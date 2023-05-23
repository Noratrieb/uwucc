use std::{
    hash::{Hash, Hasher},
    ops::Deref,
};

use indexmap::IndexMap;
use parser::{ast::IntTy, Symbol};

use crate::ir::DefId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty<'cx>(&'cx TyKind<'cx>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind<'cx> {
    Void,
    Char,
    SChar,
    UChar,
    Integer(IntTy),
    Float,
    Double,
    LongDouble,
    Bool,
    Ptr(Ty<'cx>),
    Union(UnionTy<'cx>),
    Struct(StructTy<'cx>),
    Enum(EnumTy),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnionTy<'cx> {
    pub def_id: DefId,
    pub variants: IndexMap<Symbol, Ty<'cx>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructTy<'cx> {
    pub def_id: DefId,
    pub fields: IndexMap<Symbol, Ty<'cx>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumTy {
    pub def_id: DefId,
    pub variants: IndexMap<Symbol, i128>,
}

impl<'cx> Ty<'cx> {
    pub fn new_unchecked(kind: &'cx TyKind<'cx>) -> Self {
        Self(kind)
    }
}

impl<'cx> Deref for Ty<'cx> {
    type Target = &'cx TyKind<'cx>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Hash for UnionTy<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.def_id.hash(state)
    }
}
impl Hash for StructTy<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.def_id.hash(state)
    }
}
impl Hash for EnumTy {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.def_id.hash(state)
    }
}
