use std::fmt::{Debug, Formatter};

use dbg_pls::DebugPls;

use crate::Spanned;

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

#[derive(Default)]
pub struct DeclAttr {
    pub is_extern: bool,
    pub is_static: bool,
    pub is_thread_local: bool,
}

impl Debug for DeclAttr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut d = f.debug_set();
        if self.is_extern {
            d.entry(&"extern");
        }
        if self.is_static {
            d.entry(&"static");
        }
        if self.is_thread_local {
            d.entry(&"thread_local");
        }
        d.finish()
    }
}

impl DebugPls for DeclAttr {
    fn fmt(&self, f: dbg_pls::Formatter<'_>) {
        let mut d = f.debug_set();
        if self.is_extern {
            d = d.entry(&"extern");
        }
        if self.is_static {
            d = d.entry(&"static");
        }
        if self.is_thread_local {
            d = d.entry(&"thread_local");
        }
        d.finish();
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
    pub init: Option<()>,
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
    pub body: Vec<()>,
}

#[derive(Debug, DebugPls)]
pub enum ExternalDecl {
    Decl(Decl),
    FunctionDef(FunctionDef),
}
