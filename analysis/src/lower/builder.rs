use parser::{Span, Symbol};

use super::LoweringCx;
use crate::{
    ir::{
        self, BasicBlock, BbIdx, BinKind, Branch, ConstValue, Func, Layout, Operand, Register,
        RegisterData, Statement, StatementKind, TyLayout, UnaryKind,
    },
    ty::{Ty, TyKind},
};

#[derive(Debug)]
pub(super) struct FuncBuilder<'a, 'cx> {
    pub lcx: &'a LoweringCx<'cx>,
    pub ir: Func<'cx>,
    pub current_bb: BbIdx,
}

impl<'a, 'cx> FuncBuilder<'a, 'cx> {
    pub fn new(
        name: Symbol,
        def_span: Span,
        ret_ty: Ty<'cx>,
        lcx: &'a LoweringCx<'cx>,
        arity: usize,
    ) -> Self {
        Self {
            ir: Func {
                regs: Vec::new(),
                bbs: vec![BasicBlock {
                    statements: Vec::new(),
                    term: Branch::dummy(),
                }],
                name,
                def_span,
                ret_ty,
                arity,
            },
            current_bb: BbIdx(0),
            lcx,
        }
    }

    pub fn new_reg(&mut self, name: Option<Symbol>, tyl: TyLayout<'cx>) -> Register {
        let reg = Register(self.ir.regs.len().try_into().unwrap());
        self.ir.regs.push(RegisterData { name, tyl });
        reg
    }

    pub fn alloca(&mut self, layout: &Layout, name: Option<Symbol>, span: Span) -> Register {
        let void_ptr = self
            .lcx
            .intern_ty(TyKind::Ptr(self.lcx.intern_ty(TyKind::Void)));
        let reg = self.new_reg(name, self.lcx.layout_of(void_ptr));
        let stmt = Statement {
            span,
            kind: StatementKind::Alloca {
                result: reg,
                size: Operand::Const(ConstValue::u64(layout.size)),
                align: Operand::Const(ConstValue::u64(layout.align)),
            },
        };
        self.cur_bb_mut().statements.push(stmt);
        reg
    }

    pub fn binary(
        &mut self,
        kind: BinKind,
        lhs: Operand,
        rhs: Operand,
        span: Span,
        result_tyl: TyLayout<'cx>,
    ) -> Register {
        let reg = self.new_reg(None, result_tyl);
        let stmt = StatementKind::BinOp {
            kind,
            lhs,
            rhs,
            result: reg,
        };
        self.cur_bb_mut()
            .statements
            .push(Statement { span, kind: stmt });
        reg
    }

    pub fn unary(
        &mut self,
        kind: UnaryKind,
        rhs: Operand,
        span: Span,
        result_tyl: TyLayout<'cx>,
    ) -> Register {
        let reg = self.new_reg(None, result_tyl);
        let stmt = StatementKind::UnaryOperation {
            kind,
            rhs,
            result: reg,
        };
        self.cur_bb_mut()
            .statements
            .push(Statement { span, kind: stmt });
        reg
    }

    pub fn load(&mut self, tyl: TyLayout<'cx>, ptr: Operand, span: Span) -> Register {
        let reg = self.new_reg(None, tyl);
        let stmt = StatementKind::Load {
            result: reg,
            ptr,
            size: Operand::const_u64(tyl.layout.size),
            align: Operand::const_u64(tyl.layout.align),
        };
        self.cur_bb_mut()
            .statements
            .push(Statement { span, kind: stmt });
        reg
    }

    pub fn store(&mut self, ptr: Operand, rhs: Operand, layout: &Layout, span: Span) {
        let stmt = StatementKind::Store {
            ptr,
            value: rhs,
            size: Operand::const_u64(layout.size),
            align: Operand::const_u64(layout.align),
        };
        self.cur_bb_mut()
            .statements
            .push(Statement { span, kind: stmt });
    }

    pub fn call(
        &mut self,
        ret_tyl: TyLayout<'cx>,
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
        self.cur_bb_mut()
            .statements
            .push(Statement { span, kind: stmt });
        reg
    }

    pub fn bb_mut(&mut self, bb: BbIdx) -> &mut BasicBlock {
        self.ir.bb_mut(bb)
    }

    pub fn cur_bb_mut(&mut self) -> &mut BasicBlock {
        &mut self.ir.bbs[self.current_bb.as_usize()]
    }

    pub fn new_block(&mut self) -> BbIdx {
        self.ir.bbs.push(BasicBlock {
            statements: vec![],
            term: Branch::dummy(),
        });
        BbIdx::from_usize(self.ir.bbs.len() - 1)
    }

    pub fn finish(self) -> Func<'cx> {
        println!(
            "{}",
            ir::func_to_string(&self.ir, &ir::pretty::DefaultCustomizer::default())
        );

        self.ir
    }
}
