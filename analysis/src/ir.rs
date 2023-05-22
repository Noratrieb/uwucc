/// A low level IR used for codegen.
///
/// The following expression is lowered to the following IR:
///
/// ```c
/// int i = 0;
/// long l = 1;
/// if (true) {
///     i = 1;
/// } else {
///     i = 2;
/// }
/// yeet(i);
/// ```
///
/// ```c
/// bb0:
///   %0 = alloca 4, 4
///   store _0, 0
///   %1 = alloca 8, 8
///   store %1, 1
///   branch true, bb1, bb2
/// bb1:
///   store %0, 1
///   branch bb3
/// bb2:
///   store %0, 2
///   branch bb3
/// bb3:
///   %val = load %0
///   call yeet(%val)
/// ```
use parser::{Span, Symbol};
use rustc_hash::FxHashMap;

use crate::ty::Ty;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(u32);

#[derive(Debug, Clone)]
pub struct Layout {
    pub size: u64,
    pub align: u64,
}

pub struct Ir {
    pub funcs: FxHashMap<DefId, Func>,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub bbs: Vec<BasicBlock>,
    pub def_span: Span,
    pub ret_ty: Ty,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub regs: Vec<RegisterData>,
    pub statements: Vec<Statement>,
    pub term: Branch,
}

#[derive(Debug, Clone)]
pub struct RegisterData {
    pub name: Option<Symbol>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Register(pub u32);

#[derive(Debug, Clone)]
pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Alloca {
        reg: Register,
        size: Operand,
        align: Operand,
    },
    Store {
        ptr_reg: Register,
        size: Operand,
        align: Operand,
    },
    Load {
        result: Register,
        ptr_reg: Register,
        size: Operand,
        align: Operand,
    },
    Arith {
        kind: ArithKind,
        lhs: Register,
        rhs: Register,
        result: Register,
    },
    Comp {
        kind: CompKind,
        lhs: Register,
        rhs: Register,
        result: Register,
    },
    PtrOffset {
        reg: Register,
        amount: Operand,
    },
}

#[derive(Debug, Clone)]
pub enum Operand {
    Reg(Register),
    Const(ConstValue),
}

#[derive(Debug, Clone)]
pub enum Branch {
    Goto(u32),
    Switch {
        cond: Option<RValue>,
        yes: u32,
        no: u32,
    },
}

#[derive(Debug, Clone)]
pub enum ArithKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone)]
pub enum CompKind {
    Eq,
    Neq,
    Gt,
    Geq,
    Lt,
    Leq,
}

#[derive(Debug, Clone)]
pub enum RValue {
    Register(u32),
    Constant(ConstValue),
}

#[derive(Debug, Clone)]
pub enum ConstValue {
    Int(u128),
}

impl Layout {
    pub fn size_align(size: u64, align: u64) -> Self {
        Self { size, align }
    }
}

impl ConstValue {
    pub fn u64(int: u64) -> Self {
        Self::Int(int.into())
    }
}
