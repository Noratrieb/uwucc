//! Basic codegen for the x86-64 architecture.
//!
//! We use the [`iced_x86`] crate as our assembler.
//!
//! Then, all IR basic blocks and statements are lowered in a straightforward way.
//! No optimizations are done. There is some basic register allocation.
//!
//! # Register allocation
//!
//! Register allocation is not very smart, but also not too stupid. It tries to put SSA
//! registers into machine registers as much as possible.
//!
//! ```text
//! bb0:
//!   %0 = 0
//!   %1 = 1
//!   %2 = add %0 %1
//!   switch %2, then bb1, else bb2
//!
//! bb1:
//!   %3 = add %1, 1
//!   
//! bb2:
//!   %4 = add %2, 2
//! ```
//!
//! For all SSA registers, we establish their "point of last use". This is the bb,stmt where their last usage occurs.
//!
//! First, we establish a list of possible registers to allocate.
//! Since we immediately alloca all parameters, all the param registers are free real estate.
//! Also, `rbx` is always saved on the stack at the start and end.
//!
//! ```text
//! rax, rbx, rdi, rsi, rcx, rdx, r8, r9
//! ```
//!
//! This forms our priority list of registers.
//!
//! Every time a statement has a return value, we try to assign that SSA register into a new machine register.
//! For this, we iterate through the register list above and find the first register that's free. If we see a register
//! that is not used anymore at the current location, we throw it out and use that new slot.
//!
//! When codegening an SSA register, we look into a lookup table from SSA register to machine register/stack spill and use that.
//!
//! When the list above is full, we spill the register to the stack. This should be rare. If the register doesn't fit into a machine
//! register, it's also spilled.
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

#![allow(unused_variables, dead_code)]

use analysis::{
    ir::{self, BbIdx, Branch, Func, Location, Operand, Register, Statement, StatementKind},
    LoweringCx,
};
use iced_x86::{
    code_asm::{self as x, CodeAssembler},
    Formatter, IcedError, NasmFormatter,
};
use parser::{Error, Span};
use rustc_hash::FxHashMap;

use crate::Result;

trait IcedErrExt {
    type T;
    fn sp(self, span: Span) -> Result<Self::T, Error>;
}

impl<T> IcedErrExt for Result<T, IcedError> {
    type T = T;

    fn sp(self, span: Span) -> Result<Self::T, Error> {
        self.map_err(|e| Error::new(e.to_string(), span))
    }
}

/// A machine register from our register list described in the module documentation.
#[derive(Debug, Clone, Copy)]
struct MachineReg(usize);

#[derive(Debug, Clone, Copy)]
enum RegValue {
    /// The SSA register contains an address on the stack.
    /// The offest is the offset from the start of the function.
    StackRelative { offset: u64 },
    /// The SSA register resides on the stack as it has been spilled.
    /// This should be rather rare in practice.
    Spilled { offset: u64 },
    /// The SSA register resides in a machine register.
    MachineReg(MachineReg),
}

struct AsmCtxt<'cx> {
    lcx: &'cx LoweringCx<'cx>,
    a: CodeAssembler,
    reg_map: FxHashMap<Register, RegValue>,
    reg_occupancy: Vec<Option<Register>>,
    current_stack_offset: u64,
    bb_idx: BbIdx,

    // caches
    last_register_uses: Vec<Option<Location>>,
}

impl<'cx> AsmCtxt<'cx> {
    fn allocate_result_ssa_reg(
        &mut self,
        f: &Func<'_>,
        reg: Register,
        location: Location,
    ) -> RegValue {
        for (i, opt_reg) in self.reg_occupancy.iter_mut().enumerate() {
            if let Some(reg) = opt_reg.as_mut() {
                if let Some(last_use) = self.last_register_uses[reg.as_usize()] {
                    if ir::info::dominates_location(f, last_use, location) {
                        // The last use dominates our location - the SSA reg is dead now.
                        *opt_reg = None;
                    }
                }
            }

            if opt_reg.is_none() {
                *opt_reg = Some(reg);
                return RegValue::MachineReg(MachineReg(i));
            }
        }

        todo!("spill.")
    }

    fn generate_func(&mut self, func: &Func<'cx>) -> Result<()> {
        // TODO: Prologue
        self.a.push(x::rbx).sp(func.def_span)?;
        self.a.push(x::rsp).sp(func.def_span)?;

        loop {
            let bb = &func.bbs[self.bb_idx.as_usize()];
            for stmt in &bb.statements {
                let Statement {
                    span: st_sp,
                    ref kind,
                } = *stmt;

                match *kind {
                    StatementKind::Alloca {
                        result: reg,
                        size,
                        align: _,
                    } => {
                        // For alloca, we allocate some space on the stack by subtracting from RSP.
                        // TODO: Align
                        match size {
                            Operand::Const(c) => {
                                let offset = c.as_i32();
                                self.a.sub(x::rsp, offset).sp(st_sp)?;
                                self.current_stack_offset += offset as u64;
                            }
                            Operand::Reg(_) => {
                                todo!("dynamic alloca is not supported. get a better computer")
                            }
                        };
                        self.reg_map.insert(
                            reg,
                            RegValue::StackRelative {
                                offset: self.current_stack_offset,
                            },
                        );
                    }
                    StatementKind::Store {
                        ptr,
                        value,
                        size,
                        align,
                    } => {
                        let Operand::Const(size) = size else {
                            todo!("non const size");
                        };
                        if size.as_i32() != 8 {
                            todo!("stores of less or more than 8 bytes: {size}");
                        }
                        match ptr {
                            Operand::Const(_) => todo!("const stores not implemented"),
                            Operand::Reg(reg) => {
                                let ptr_value = self.reg_map[&reg];
                                match (ptr_value, value) {
                                    (RegValue::StackRelative { offset }, Operand::Const(c)) => {
                                        let offset_from_cur = self.current_stack_offset - offset;
                                        dbg!(offset_from_cur, c);

                                        self.a
                                            .mov(x::qword_ptr(x::rsp + offset_from_cur), c.as_i32())
                                            .sp(st_sp)?;
                                    }
                                    (RegValue::StackRelative { offset }, Operand::Reg(value)) => {
                                        todo!("stack relative ptr + reg value")
                                    }
                                    (RegValue::Spilled { .. }, _) => todo!("spilled"),
                                    (RegValue::MachineReg(_), _) => todo!("machine reg"),
                                };
                                //let rhs = match value {
                                //    Operand::Const(c) => {}
                                //    Operand::Reg(reg) => {}
                                //};

                                // mov [rsp + OFFSET], RHS

                                //self.a.add_instruction(Instruction::with2(Code::Mov, op0, op1))
                                // self.a.mov(x::ptr(x::rax), x::rbx).sp(st_sp);
                            }
                        }
                    }
                    StatementKind::Load {
                        result,
                        ptr,
                        size,
                        align,
                    } => todo!("loads."),
                    StatementKind::BinOp {
                        kind,
                        lhs,
                        rhs,
                        result,
                    } => todo!("binary operations"),
                    StatementKind::UnaryOperation { rhs, kind, result } => {
                        todo!("unary operations")
                    }
                    StatementKind::PtrOffset {
                        result,
                        ptr: reg,
                        amount,
                    } => todo!("pointer offset :D"),
                    StatementKind::Call {
                        result,
                        func,
                        ref args,
                    } => todo!("function calls 💀"),
                }
            }

            match bb.term {
                Branch::Ret(_) => {
                    self.a
                        .add(x::rsp, i32::try_from(self.current_stack_offset).unwrap())
                        .sp(func.def_span)?;
                    self.a.pop(x::rsp).sp(func.def_span)?;
                    self.a.pop(x::rbx).sp(func.def_span)?;
                    self.a.mov(x::rax, 0_u64).sp(func.def_span)?;
                    self.a.ret().sp(func.def_span)?;
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

    let fn_sp = func.def_span;
    let a = CodeAssembler::new(64).sp(fn_sp)?;

    let mut cx = AsmCtxt {
        lcx,
        a,
        reg_map: FxHashMap::default(),
        reg_occupancy: vec![None; 8],
        current_stack_offset: 0,
        bb_idx: BbIdx(0),
        last_register_uses: ir::info::last_register_uses(func),
    };

    cx.generate_func(func)?;

    let code = cx.a.assemble(0x4000).sp(fn_sp)?;

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
