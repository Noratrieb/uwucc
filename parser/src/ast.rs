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
pub enum Declaration {
    Normal(NormalDeclaration),
    StaticAssert,
}

#[derive(Debug, DebugPls)]
pub struct NormalDeclaration {
    pub decl_spec: DeclSpec,
    pub declarator: Declarator,
    pub initializer: Option<()>,
}

#[derive(Debug, DebugPls)]
pub enum DirectDeclarator {
    Ident(Ident),
    WithParams {
        ident: Ident,
        params: Vec<NormalDeclaration>,
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
pub enum FunctionParameters {
    Void(Span),
    List(Vec<Spanned<NormalDeclaration>>),
}

#[derive(Debug, DebugPls)]
pub struct FunctionDefinition {
    pub decl_spec: Spanned<DeclSpec>,
    pub name: Ident,
    pub parameter_list: FunctionParameters,
    pub body: Vec<()>,
}
