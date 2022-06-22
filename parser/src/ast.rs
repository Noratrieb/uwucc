use dbg_pls::DebugPls;

use crate::Span;

pub type Spanned<T> = (T, Span);

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
pub enum Declaration {
    Normal {
        decl_spec: DeclSpec,
        name: Option<String>,
        initializer: Option<()>,
        pointer: bool,
    },
    StaticAssert,
}

#[derive(Debug, DebugPls)]
pub struct Declarator {
    pub identifier: String,
    pub pointer: bool,
}

#[derive(Debug, DebugPls)]
pub struct FunctionParamDecl {
    pub decl_spec: DeclSpec,
    pub declarator: Declarator,
}

#[derive(Debug, DebugPls)]
pub enum FunctionParameters {
    Void(Span),
    List(Vec<Spanned<FunctionParamDecl>>),
}

#[derive(Debug, DebugPls)]
pub struct FunctionDefinition {
    pub decl_spec: Spanned<DeclSpec>,
    pub declarator: Spanned<String>,
    pub declaration_list: FunctionParameters,
    pub body: Vec<()>,
}
