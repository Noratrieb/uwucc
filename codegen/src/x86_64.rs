//! Basic codegen for the x86-64 architecture.
//!
//! We use the [`iced_x86`] crate as our assembler.
//!
//! Then, all IR basic blocks and statements are lowered in a straightforward way.
//! No optimizations are done. There is some basic register allocation.
//!
//! ## Registers
//! <https://gitlab.com/x86-psABIs/x86-64-ABI>
//!
//! | name     | description          | callee-saved |
//! | -------- | -------------------- | ------------ |
//! | %rax     | temporary register; with variable arguments passes information about the number of vector registers used; 1st return register | No |
//! | %rbx     | callee-saved register | Yes |
//! | %rcx     | used to pass 4th integer argument to functions | No |
//! | %rdx     | used to pass 3rd argument to functions; 2nd return register | No |
//! | %rsp     | stack pointer | Yes |
//! | %rbp     | callee-saved register; optionally used as frame pointer | Yes |
//! | %rsi     | used to pass 2nd argument to functions | No |
//! | %rdi     | used to pass 1st argument to functions | No |
//! | %r8      | used to pass 5th argument to functions | No |
//! | %r9      | used to pass 6th argument to functions | No |
//! | %r10     | temporary register, used for passing a function’s static chain pointer | No |
//! | %r11     | temporary register | No |
//! | %r12-r14 | callee-saved registers | Yes |
//! | %r15     | callee-saved register; optionally used as GOT base pointer | Yes |
use analysis::{
    ir::{self, BbIdx, Branch, Func, Location, Operand, Register, Statement, StatementKind},
    LoweringCx,
};
use iced_x86::{
    code_asm::{self as x, AsmRegister64, CodeAssembler},
    Formatter, IcedError, NasmFormatter,
};
use parser::{Error, Span};
use rustc_hash::FxHashMap;

use crate::{
    registers::{MachineReg, RegValue},
    stack, Result,
};

trait IcedErrExt {
    type T;
    fn sp(self, cx: &AsmCtxt<'_, '_>) -> Result<Self::T, Error>;
}

impl<T> IcedErrExt for Result<T, IcedError> {
    type T = T;

    fn sp(self, cx: &AsmCtxt<'_, '_>) -> Result<Self::T, Error> {
        self.map_err(|e| Error::new(e.to_string(), cx.current_span))
    }
}

struct AsmCtxt<'f, 'cx> {
    lcx: &'cx LoweringCx<'cx>,
    func: &'f Func<'cx>,
    a: CodeAssembler,
    reg_map: FxHashMap<Register, RegValue>,
    reg_occupancy: Vec<Option<Register>>,
    bb_idx: BbIdx,

    stack_layout: stack::StackLayout,

    current_span: Span,

    // caches
    last_register_uses: Vec<Option<Location>>,
}

impl<'cx> AsmCtxt<'_, 'cx> {
    fn allocate_result_ssa_reg(&mut self, reg: Register, location: Location) -> RegValue {
        for (i, opt_reg) in self.reg_occupancy.iter_mut().enumerate() {
            if let Some(reg) = opt_reg.as_mut() {
                if let Some(last_use) = self.last_register_uses[reg.as_usize()] {
                    if ir::info::dominates_location(self.func, last_use, location) {
                        // The last use dominates our location - the SSA reg is dead now.
                        *opt_reg = None;
                    }
                }
            }

            if opt_reg.is_none() {
                *opt_reg = Some(reg);
                let value = RegValue::MachineReg(MachineReg(i));
                self.reg_map.insert(reg, value);
                return value;
            }
        }

        todo!("spill.")
    }

    fn gen_store(&mut self, ptr: Operand, value: Operand, size: u64, _align: u64) -> Result<()> {
        if size != 8 {
            todo!("stores of less or more than 8 bytes: {size}");
        }
        match ptr {
            Operand::Const(_) => todo!("const stores not implemented"),
            Operand::Reg(reg) => {
                let ptr_value = self.reg_map[&reg];
                match (ptr_value, value) {
                    (RegValue::StackRelativePtr { offset }, Operand::Const(c)) => {
                        self.a
                            .mov(x::qword_ptr(x::rsp + offset), c.as_i32())
                            .sp(self)?;
                    }
                    (RegValue::StackRelativePtr { offset: offset_ptr }, Operand::Reg(value)) => {
                        let value = self.reg_map[&value];
                        match value {
                            RegValue::StackRelativePtr {
                                offset: offset_value,
                            } => {
                                self.a.mov(x::rax, x::rsp + offset_value).sp(self)?;
                                self.a.mov(x::rsp + offset_ptr, x::rax).sp(self)?;
                            }
                            RegValue::MachineReg(reg) => {
                                self.a.mov(x::rsp + offset_ptr, machine_reg_to_reg(reg)).sp(self)?;
                            }
                            RegValue::Spilled { .. } => todo!("spills"),
                        }
                    }
                    (RegValue::Spilled { .. }, _) => todo!("spilled"),
                    (RegValue::MachineReg(_), _) => todo!("machine reg"),
                };
            }
        }
        Ok(())
    }

    fn gen_load(
        &mut self,
        loc: Location,
        result: Register,
        ptr: Operand,
        size: u64,
        _align: u64,
    ) -> Result {
        assert_eq!(size, 8);
        let result = self.allocate_result_ssa_reg(result, loc);

        match (result, ptr) {
            (RegValue::MachineReg(num), Operand::Reg(ptr)) => {
                let into = machine_reg_to_reg(num);

                match self.reg_map[&ptr] {
                    RegValue::StackRelativePtr { offset } => {
                        self.a.mov(into, x::qword_ptr(x::rsp + offset)).sp(self)?;
                    }
                    _ => {}
                }
            }
            _ => todo!("loading into a not-reg or from a not-reg"),
        }

        Ok(())
    }

    fn generate_func(&mut self, func: &Func<'cx>) -> Result {
        // Prologue: Save rbp and save rsp in rbp.
        self.a.push(x::rbp).sp(self)?;
        self.a.mov(x::rbp, x::rsp).sp(self)?;
        self.a
            .sub(x::rsp, self.stack_layout.total_size as i32)
            .sp(self)?;

        loop {
            let bb = &func.bbs[self.bb_idx.as_usize()];
            for (idx, stmt) in bb.statements.iter().enumerate() {
                let loc = Location::stmt(self.bb_idx, idx);
                let Statement { span, ref kind } = *stmt;
                self.current_span = span;

                match *kind {
                    StatementKind::Alloca { result, .. } => {
                        self.reg_map.insert(
                            result,
                            RegValue::StackRelativePtr {
                                offset: *self
                                    .stack_layout
                                    .allocas
                                    .get(&result)
                                    .expect("no stack layout slot present for alloc register"),
                            },
                        );
                    }
                    StatementKind::Store {
                        ptr,
                        value,
                        size,
                        align,
                    } => {
                        self.gen_store(ptr, value, size, align)?;
                    }
                    StatementKind::Load {
                        result,
                        ptr,
                        size,
                        align,
                    } => {
                        self.gen_load(loc, result, ptr, size, align)?;
                    }
                    StatementKind::BinOp { .. } => todo!("binary operations"),
                    StatementKind::UnaryOperation { .. } => {
                        todo!("unary operations")
                    }
                    StatementKind::PtrOffset { .. } => todo!("pointer offset :D"),
                    StatementKind::Call { .. } => todo!("function calls 💀"),
                }
            }

            match bb.term {
                Branch::Ret(_) => {
                    // Epilogue: Restore rsp, rbp and return.
                    self.a.mov(x::rsp, x::rbp).sp(self)?;
                    self.a.pop(x::rbp).sp(self)?;
                    self.a.mov(x::rax, 0_u64).sp(self)?;
                    self.a.ret().sp(self)?;
                    break;
                }
                Branch::Switch { .. } => todo!("switch"),
                Branch::Goto(_) => todo!("goto"),
            }
        }

        Ok(())
    }
}

pub fn generate_func<'cx>(lcx: &'cx LoweringCx<'cx>, func: &Func<'cx>) -> Result<Vec<u8>> {
    assert_eq!(func.arity, 0, "arguments??? in MY uwucc????");

    let layout = crate::registers::compute_layout(func);
    crate::registers::debug_layout(func, &layout);

    let fn_sp = func.def_span;
    let a = CodeAssembler::new(64).unwrap();

    let stack_layout = stack::allocate_stack_space(8, func);
    dbg!(&stack_layout);

    let mut cx = AsmCtxt {
        lcx,
        a,
        func,
        reg_map: FxHashMap::default(),
        reg_occupancy: vec![None; 8],
        bb_idx: BbIdx(0),
        stack_layout,
        current_span: fn_sp,
        last_register_uses: ir::info::last_register_uses(func),
    };

    cx.generate_func(func)?;

    let code = cx.a.assemble(0x4000).sp(&cx)?;

    print!("{}:\n---", func.name);
    let mut output = String::new();
    let mut formatter = NasmFormatter::new();
    for instr in cx.a.instructions() {
        output.push('\n');
        formatter.format(instr, &mut output);
    }
    println!("{output}\n---");

    Ok(code)
}

fn machine_reg_to_reg(reg: MachineReg) -> AsmRegister64 {
    use x::*;
    [rcx, rdx, rsi, rdi, r8, r9, r11][reg.0]
}
