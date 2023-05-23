use std::{
    fmt::Display,
    hash::{Hash, Hasher},
    ops::Deref,
};

use indexmap::IndexMap;
use parser::{
    ast::{IntSign, IntTy, IntTyKind},
    Symbol,
};

use crate::ir::DefId;

#[derive(Debug, Clone, Copy, Eq)]
pub struct Ty<'cx>(&'cx TyKind<'cx>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind<'cx> {
    Void,
    Char,
    Int(IntTy),
    Float,
    Double,
    LongDouble,
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

impl Display for Ty<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match **self {
            TyKind::Void => f.write_str("void"),
            TyKind::Char => f.write_str("char"),
            TyKind::Int(int) => {
                match int.0 {
                    IntSign::Signed => f.write_str("signed "),
                    IntSign::Unsigned => f.write_str("unsigned "),
                }?;
                match int.1 {
                    IntTyKind::Bool => f.write_str("_Bool"),
                    IntTyKind::Char => f.write_str("char"),
                    IntTyKind::Short => f.write_str("short"),
                    IntTyKind::Int => f.write_str("int"),
                    IntTyKind::Long => f.write_str("long"),
                    IntTyKind::LongLong => f.write_str("long long"),
                }?;
                Ok(())
            }
            TyKind::Float => f.write_str("float"),
            TyKind::Double => f.write_str("double"),
            TyKind::LongDouble => f.write_str("long double"),
            TyKind::Ptr(ty) => {
                write!(f, "{ty}*")
            }
            TyKind::Union(_) => todo!(),
            TyKind::Struct(_) => todo!(),
            TyKind::Enum(_) => todo!(),
        }
    }
}

impl PartialEq for Ty<'_> {
    fn eq(&self, other: &Self) -> bool {
        // Interning.
        std::ptr::eq(self.0, other.0)
    }
}

impl Hash for Ty<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Interning.
        std::ptr::hash(self.0, state)
    }
}

impl<'cx> Ty<'cx> {
    pub fn new_unchecked(kind: &'cx TyKind<'cx>) -> Self {
        Self(kind)
    }
    pub fn is_integral(self) -> bool {
        matches!(*self, TyKind::Char | TyKind::Int(_))
    }

    pub fn unwrap_int(self) -> IntTy {
        match *self {
            TyKind::Int(int) => *int,
            _ => panic!("expected integer type, found {self}"),
        }
    }
}
