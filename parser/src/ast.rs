use std::fmt::Debug;

use bitflags::bitflags;
use dbg_pls::DebugPls;

use crate::Spanned;

//
// --- Expr
//

#[derive(Debug, DebugPls)]
pub enum Atom {
    Ident(String),
    Int(i128),
    Float(f64),
    String(String),
    Char(u8),
}

#[derive(Debug, DebugPls)]
pub enum UnaryOp {
    AddrOf,
    Deref,
    Plus,
    Minus,
    Tilde,
    Bang,
}

#[derive(Debug, DebugPls)]
pub enum BinaryOp {
    Add,
    Sub,
    Comma,
    Index, // lhs[rhs]
}

#[derive(Debug, DebugPls)]
pub struct ExprUnary {
    pub rhs: Box<Spanned<Expr>>,
    pub op: UnaryOp,
}

#[derive(Debug, DebugPls)]
pub struct ExprBinary {
    pub lhs: Box<Spanned<Expr>>,
    pub rhs: Box<Spanned<Expr>>,
    pub op: BinaryOp,
}

#[derive(Debug, DebugPls)]
pub enum Expr {
    Atom(Atom),
    Unary(ExprUnary),
    Binary(ExprBinary),
}

//
// --- Types and decls and garbage whatever
//

#[derive(Debug, DebugPls)]
pub enum TypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Bool,
    Complex,
    // TODO
    // atomic-type-specifier
    // struct-or-union-specifier
    // enum-specifier
    // typedef-name
}

pub type Ident = Spanned<String>;

bitflags! {
    pub struct DeclAttr: u8 {
        const EXTERN = 0b00000001;
        const STATIC = 0b00000010;
        const THREAD_LOCAL = 0b00000100;
    }
}

impl DebugPls for DeclAttr {
    fn fmt(&self, f: dbg_pls::Formatter<'_>) {
        use std::fmt::Write;
        let mut string = String::new();
        write!(string, "{:?}", self).unwrap();
        DebugPls::fmt(&string, f);
    }
}

#[derive(Debug, DebugPls)]
pub struct DeclSpec {
    pub ty: TypeSpecifier,
    pub attrs: DeclAttr,
}

#[derive(Debug, DebugPls)]
pub enum Decl {
    Normal(NormalDecl),
    StaticAssert,
}

#[derive(Debug, DebugPls)]
pub struct InitDecl {
    pub declarator: Declarator,
    pub init: Option<Spanned<Expr>>,
}

#[derive(Debug, DebugPls)]
pub struct NormalDecl {
    pub decl_spec: DeclSpec,
    pub init_declarators: Vec<Spanned<InitDecl>>,
}

#[derive(Debug, DebugPls)]
pub struct FunctionParamDecl {
    pub decl_spec: Spanned<DeclSpec>,
    pub declarator: Spanned<Declarator>,
}

#[derive(Debug, DebugPls)]
pub enum DirectDeclarator {
    Ident(Ident),
    WithParams {
        ident: Ident,
        params: Vec<FunctionParamDecl>,
    },
}

#[derive(Debug, DebugPls)]
pub struct Declarator {
    pub decl: DirectDeclarator,
    pub pointer: bool,
}

#[derive(Debug, DebugPls)]
pub struct FunctionDef {
    pub declaration: Decl,
    pub body: Vec<Expr>,
}

#[derive(Debug, DebugPls)]
pub enum ExternalDecl {
    Decl(Decl),
    FunctionDef(FunctionDef),
}
