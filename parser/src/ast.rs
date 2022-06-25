use dbg_pls::DebugPls;

use crate::{Span, Spanned};

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

#[derive(Debug, Default, DebugPls)]
pub struct DeclAttr {
    pub is_extern: bool,
    pub is_static: bool,
    pub is_thread_local: bool,
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
    pub init: Option<()>,
}

#[derive(Debug, DebugPls)]
pub struct NormalDecl {
    pub decl_spec: DeclSpec,
    pub init_declarators: Vec<Spanned<InitDecl>>,
}

#[derive(Debug, DebugPls)]
pub enum DirectDeclarator {
    Ident(Ident),
    WithParams {
        ident: Ident,
        params: Vec<NormalDecl>,
    },
}

#[derive(Debug, DebugPls)]
pub struct Declarator {
    pub decl: DirectDeclarator,
    pub pointer: bool,
}

#[derive(Debug, DebugPls)]
pub struct FunctionParamDecl {
    pub decl_spec: DeclSpec,
    pub declarator: Declarator,
}

#[derive(Debug, DebugPls)]
pub enum FunctionParams {
    Void(Span),
    List(Vec<Spanned<NormalDecl>>),
}

#[derive(Debug, DebugPls)]
pub struct FunctionDef {
    pub declaration: Decl,
    pub body: Vec<()>,
}

#[derive(Debug, DebugPls)]
pub enum ExternalDecl {
    Decl(Decl),
    FunctionDef(FunctionDef),
}
