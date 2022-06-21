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
