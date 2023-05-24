use analysis::{
    ir::{BbIdx, ConstValue, Func, Operand, Register, Statement, StatementKind},
    LoweringCx,
};
use iced_x86::{
    code_asm::{self as x, CodeAssembler},
    IcedError, Instruction,
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
                        reg,
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
                        reg,
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
