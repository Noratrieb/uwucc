/// A low level IR used for codegen.
/// 
/// The following expression is lowered to the following IR:
/// 
/// ```c
/// int i = 0;
/// long l = 1;
/// int y = ((int)&i)+l;
/// ```
/// 
/// ```c
/// int _0;   // i
/// long _1;  // l
/// int *_2;  // tmp &i
/// int _3;   // tmp (int)&i
/// int _4;   // tmp l (implicit cast to int)
/// int _5;   // y
/// 
/// _0 = Const(0)
/// _1 = Const(1)
/// _2 = AddrOf(_0)
/// _3 = Cast(Ptr, Int, _2)
/// _4 = Cast(Long, Int, _1)
/// _5 = _3 + _4
/// ```

use parser::Span;

use crate::hir::Ty;

struct Body {
    locals: Vec<LocalDecl>,
    statements: Vec<Statement>,
}

struct LocalDecl {
    pub ty: Ty,
}

struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

enum StatementKind {
    Assign(LValue, RValue),
}

enum RValue {
    BinOp(BinOpKind, Operand),
    Const,
}

enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

enum Operand {
    Local(usize),
}

enum LValue {}
