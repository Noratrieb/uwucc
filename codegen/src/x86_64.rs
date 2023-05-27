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

use analysis::{
    ir::{BbIdx, Func, Operand, Register, Statement, StatementKind},
    LoweringCx,
};
use iced_x86::{
    code_asm::{self as x, CodeAssembler},
    IcedError, 
};
use parser::Span;
use rustc_hash::FxHashMap;

use crate::Result;

trait IcedErrExt {
    type T;
    fn sp(self, span: Span) -> Result<Self::T, analysis::Error>;
}

impl<T> IcedErrExt for Result<T, IcedError> {
    type T = T;

    fn sp(self, span: Span) -> Result<Self::T, analysis::Error> {
        self.map_err(|e| analysis::Error::new(e.to_string(), span))
    }
}

#[derive(Debug, Clone, Copy)]
enum RegValue {
    Stack { offset: u64 },
}

struct AsmCtxt<'cx> {
    lcx: &'cx LoweringCx<'cx>,
    a: CodeAssembler,
    reg_map: FxHashMap<Register, RegValue>,
    current_stack_offset: u64,
    bb_idx: BbIdx,
}

impl<'cx> AsmCtxt<'cx> {
    fn generate_func(&mut self, func: &Func<'cx>) -> Result<()> {
        // TODO: Prologue

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
                        // TODO: Align
                        match size {
                            Operand::Const(c) => {
                                let offset = c.as_i32();
                                self.a.sub(x::rsp, offset).sp(st_sp)?;
                                self.current_stack_offset += offset as u64;
                            }
                            Operand::Reg(_) => todo!("dynamic alloca is not supported"),
                        };
                        self.reg_map.insert(
                            reg,
                            RegValue::Stack {
                                offset: self.current_stack_offset,
                            },
                        );
                    }
                    StatementKind::Store {
                        ptr,
                        value,
                        size,
                        align,
                    } => match ptr {
                        Operand::Const(_) => todo!("const stores not implemented"),
                        Operand::Reg(reg) => {
                            let value = self.reg_map[&reg];
                            let stack_offset = match value {
                                RegValue::Stack { offset } => offset,
                            };
                            //let rhs = match value {
                            //    Operand::Const(c) => {}
                            //    Operand::Reg(reg) => {}
                            //};

                            // mov [rbp + OFFSET], RHS

                            //self.a.add_instruction(Instruction::with2(Code::Mov, op0, op1))

                            self.a.mov(x::ptr(x::rax), x::rbx);
                        }
                    },
                    StatementKind::Load {
                        result,
                        ptr,
                        size,
                        align,
                    } => todo!(),
                    StatementKind::BinOp {
                        kind,
                        lhs,
                        rhs,
                        result,
                    } => todo!(),
                    StatementKind::UnaryOperation { rhs, kind, result } => todo!(),
                    StatementKind::PtrOffset {
                        result,
                        ptr: reg,
                        amount,
                    } => todo!(),
                    StatementKind::Call {
                        result,
                        func,
                        ref args,
                    } => todo!(),
                }
            }

            todo!("next bbs");
        }

        Ok(())
    }
}

pub fn generate_func<'cx>(lcx: &'cx LoweringCx<'cx>, func: &Func<'cx>) -> Result<Vec<u8>> {
    assert_eq!(func.arity, 0, "arguments??? in MY uwucc????");

    let fn_sp = func.def_span;
    let mut a = CodeAssembler::new(64).sp(fn_sp)?;

    let mut cx = AsmCtxt {
        lcx,
        a,
        reg_map: FxHashMap::default(),
        current_stack_offset: 0,
        bb_idx: BbIdx(0),
    };

    cx.generate_func(func)?;

    let code = cx.a.assemble(0x4000).sp(fn_sp)?;

    Ok(code)
}