use parser::{Span, Symbol};

use super::layout_of;
use crate::{
    ir::{
        self, BasicBlock, BinKind, Branch, ConstValue, Func, Layout, Operand, Register,
        RegisterData, Statement, StatementKind, TyLayout,
    },
    ty::Ty,
};

#[derive(Debug)]
pub struct FuncBuilder {
    pub ir: Func,
    current_bb: u32,
}

impl FuncBuilder {
    pub fn new(name: Symbol, def_span: Span, ret_ty: Ty) -> Self {
        Self {
            ir: Func {
                regs: Vec::new(),
                bbs: vec![BasicBlock {
                    statements: Vec::new(),
                    term: Branch::Goto(0),
                }],
                name,
                def_span,
                ret_ty,
            },
            current_bb: 0,
        }
    }

    pub fn new_reg(&mut self, name: Option<Symbol>, tyl: TyLayout) -> Register {
        let reg = Register(self.ir.regs.len().try_into().unwrap());
        self.ir.regs.push(RegisterData { name, tyl });
        reg
    }

    pub fn alloca(&mut self, layout: &Layout, name: Option<Symbol>, span: Span) -> Register {
        let reg = self.new_reg(name, layout_of(Ty::Ptr(Box::new(Ty::Void))));
        let stmt = Statement {
            span,
            kind: StatementKind::Alloca {
                reg,
                size: Operand::Const(ConstValue::u64(layout.size)),
                align: Operand::Const(ConstValue::u64(layout.align)),
            },
        };
        self.bb_mut().statements.push(stmt);
        reg
    }

    pub fn binary(
        &mut self,
        kind: BinKind,
        lhs: Operand,
        rhs: Operand,
        span: Span,
        result_tyl: TyLayout,
    ) -> Register {
        let reg = self.new_reg(None, result_tyl);
        let stmt = StatementKind::BinOp {
            kind,
            lhs,
            rhs,
            result: reg,
        };
        self.bb_mut()
            .statements
            .push(Statement { span, kind: stmt });
        reg
    }

    pub fn load(&mut self, tyl: TyLayout, ptr_reg: Register, span: Span) -> Register {
        let reg = self.new_reg(None, tyl.clone());
        let stmt = StatementKind::Load {
            result: reg,
            ptr_reg,
            size: Operand::const_u64(tyl.layout.size),
            align: Operand::const_u64(tyl.layout.align),
        };
        self.bb_mut()
            .statements
            .push(Statement { span, kind: stmt });
        reg
    }

    pub fn store(&mut self, ptr_reg: Register, rhs: Operand, layout: Layout, span: Span) {
        let stmt = StatementKind::Store {
            ptr_reg,
            value: rhs,
            size: Operand::const_u64(layout.size),
            align: Operand::const_u64(layout.align),
        };
        self.bb_mut()
            .statements
            .push(Statement { span, kind: stmt });
    }

    pub fn call(
        &mut self,
        ret_tyl: TyLayout,
        func: Operand,
        args: Vec<Operand>,
        span: Span,
    ) -> Register {
        let reg = self.new_reg(None, ret_tyl);
        let stmt = StatementKind::Call {
            result: reg,
            func,
            args,
        };
        self.bb_mut()
            .statements
            .push(Statement { span, kind: stmt });
        reg
    }

    pub fn bb_mut(&mut self) -> &mut BasicBlock {
        &mut self.ir.bbs[self.current_bb as usize]
    }

    pub fn finish(mut self) -> Func {
        self.bb_mut().term = Branch::Ret(Operand::Const(ConstValue::Void));

        println!("{}", ir::func_to_string(&self.ir));

        self.ir
    }
}
