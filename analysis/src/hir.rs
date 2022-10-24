use indexmap::IndexMap;
use lasso::Spur;
use parser::Spanned;

pub type Symbol = Spur;

pub type Ident = Spanned<Symbol>;

pub struct Hir<'hir> {
    defs: Vec<Def>,
    __: &'hir (),
}

#[derive(Clone, Copy)]
pub struct DefId(u32);

pub enum IntTySignedness {
    Signed,
    Unsigned,
}

impl Default for IntTySignedness {
    fn default() -> Self {
        // C defaults to unsigned for integers.
        Self::Signed
    }
}

pub enum IntTyKind {
    Short,
    Int,
    Long,
    LongLong,
}

pub struct IntTy {
    pub sign: IntTySignedness,
    pub kind: IntTyKind,
}

pub struct Def {
    pub name: Ident,
    pub def_id: DefId,
    pub kind: DefKind,
}

pub enum DefKind {
    Union(UnionTy),
    Enum(EnumTy),
    Struct(StructTy),
}

pub struct UnionTy {
    pub def_id: DefId,
    pub variants: IndexMap<Symbol, Ty>,
}

pub struct StructTy {
    pub def_id: DefId,
    pub fields: IndexMap<Symbol, Ty>,
}

pub struct EnumTy {
    pub def_id: DefId,
    pub variants: IndexMap<Symbol, i128>,
}

pub enum TyKind {
    Void,
    Char,
    SChar,
    UChar,
    Integer(IntTy),
    Float,
    Double,
    LongDouble,
    Bool,
    Union(UnionTy),
    Struct(StructTy),
    Enum(EnumTy),
}

pub struct Ty {
    kind: TyKind,
}

pub enum NodeKind {
    FunctionDef(FunctionDef),
}

pub struct ExternalDecl;

pub struct FunctionDef {
    name: Symbol,
}

pub struct Expr<'hir> {
    kind: ExprKind<'hir>,
    ty: Ty,
}

pub enum ExprKind<'hir> {
    Var(DefId),
    Binary(BinaryOp, &'hir Expr<'hir>, &'hir Expr<'hir>),
    Unary(UnaryOp, &'hir Expr<'hir>),
    Cast(CastExpr<'hir>),
}

pub struct CastExpr<'hir> {
    from: Ty,
    to: Ty,
    expr: &'hir Expr<'hir>,
}

pub enum UnaryOp {
    AddrOf,
    Deref,
    Plus,
    Minus,
    Tilde,
    Bang,
}

#[derive(Debug)]
pub enum ArithOpKind {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Shl,
    Shr,
    BitAnd,
    BitXor,
    BitOr,
}

#[derive(Debug)]
pub enum ComparisonKind {
    Lt,
    Gt,
    LtEq,
    GtEq,
    Eq,
    Neq,
}

#[derive(Debug)]
pub enum BinaryOp {
    Arith(ArithOpKind),
    LogicalAnd,
    LogicalOr,
    Comparison(ComparisonKind),
    Comma,
    Index, // lhs[rhs]
    Assign(Option<ArithOpKind>),
}
