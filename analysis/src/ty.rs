use indexmap::IndexMap;
use parser::{ast::IntTy, Symbol};

use crate::ir::DefId;

#[derive(Debug, Clone)]
pub enum Ty {
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

#[derive(Debug, Clone)]
pub struct UnionTy {
    pub def_id: DefId,
    pub variants: IndexMap<Symbol, Ty>,
}

#[derive(Debug, Clone)]
pub struct StructTy {
    pub def_id: DefId,
    pub fields: IndexMap<Symbol, Ty>,
}

#[derive(Debug, Clone)]
pub struct EnumTy {
    pub def_id: DefId,
    pub variants: IndexMap<Symbol, i128>,
}
