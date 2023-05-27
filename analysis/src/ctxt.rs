use std::{
    cell::{Cell, RefCell},
    fmt::Debug,
};

use parser::{
    ast::{self, IntSign, IntTyKind, IntTy},
    Symbol,
};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    ir::{DefId, Layout, TyLayout, VariableInfo},
    ty::{Ty, TyKind},
};

pub struct LoweringCx<'cx> {
    tys: RefCell<FxHashSet<&'cx TyKind<'cx>>>,
    layouts: RefCell<FxHashSet<&'cx Layout>>,
    string_literals: RefCell<FxHashMap<&'cx [u8], DefId>>,
    pub(crate) arena: &'cx bumpalo::Bump,
    next_def_id: Cell<DefId>,
    pub types: CommonTypes<'cx>,
    /**/
    pub(crate) global_decls: FxHashMap<Symbol, VariableInfo<'cx>>,
}

impl Debug for LoweringCx<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("lcx")
    }
}

pub struct CommonInt<'cx> {
    pub signed: Ty<'cx>,
    pub unsigned: Ty<'cx>,
}

pub struct CommonTypes<'cx> {
    pub void: Ty<'cx>,
    pub char: Ty<'cx>,
    pub su_char: CommonInt<'cx>,
    pub short: CommonInt<'cx>,
    pub int: CommonInt<'cx>,
    pub long: CommonInt<'cx>,
}

pub(crate) fn intern_ty_inner<'cx>(
    tys: &RefCell<FxHashSet<&'cx TyKind<'cx>>>,
    arena: &'cx bumpalo::Bump,
    kind: TyKind<'cx>,
) -> Ty<'cx> {
    let opt_kind = tys.borrow().get(&kind).copied();
    match opt_kind {
        Some(ty) => Ty::new_unchecked(ty),
        None => {
            let kind = arena.alloc(kind);
            tys.borrow_mut().insert(kind);
            Ty::new_unchecked(kind)
        }
    }
}

impl<'cx> CommonTypes<'cx> {
    fn new(tys: &RefCell<FxHashSet<&'cx TyKind<'cx>>>, arena: &'cx bumpalo::Bump) -> Self {
        let int = |sign, kind| intern_ty_inner(tys, arena, TyKind::Int(IntTy(sign, kind)));
        let int_pair = |kind| CommonInt {
            signed: int(IntSign::Signed, kind),
            unsigned: int(IntSign::Unsigned, kind),
        };

        Self {
            void: intern_ty_inner(tys, arena, TyKind::Void),
            char: intern_ty_inner(tys, arena, TyKind::Char),
            su_char: int_pair(IntTyKind::Char),
            short: int_pair(IntTyKind::Short),
            int: int_pair(IntTyKind::Int),
            long: int_pair(IntTyKind::Long),
        }
    }
}

impl<'cx> LoweringCx<'cx> {
    pub fn new(arena: &'cx bumpalo::Bump) -> Self {
        let tys = RefCell::default();
        let types = CommonTypes::new(&tys, arena);
        LoweringCx {
            tys,
            layouts: RefCell::default(),
            string_literals: RefCell::default(),
            arena,
            next_def_id: Cell::new(DefId(0)),
            global_decls: FxHashMap::default(),
            types,
        }
    }

    pub(crate) fn next_def_id(&self) -> DefId {
        let def_id = self.next_def_id.get();
        self.next_def_id.set(DefId(def_id.0 + 1));
        def_id
    }

    pub(crate) fn lower_ty(&self, ty: &ast::TypeSpecifier) -> Ty<'cx> {
        let kind = match ty {
            ast::TypeSpecifier::Void => TyKind::Void,
            ast::TypeSpecifier::Char => TyKind::Char,
            ast::TypeSpecifier::Integer(int) => TyKind::Int(*int),
            ast::TypeSpecifier::Float => TyKind::Float,
            ast::TypeSpecifier::Double => TyKind::Double,
            ast::TypeSpecifier::LongDouble => TyKind::LongDouble,
        };
        self.intern_ty(kind)
    }

    pub(crate) fn intern_ty(&self, kind: TyKind<'cx>) -> Ty<'cx> {
        intern_ty_inner(&self.tys, self.arena, kind)
    }

    fn intern_layout(&self, layout: Layout) -> &'cx Layout {
        let opt_layout = self.layouts.borrow().get(&layout).copied();
        match opt_layout {
            Some(layout) => layout,
            None => {
                let layout = self.arena.alloc(layout);
                self.layouts.borrow_mut().insert(layout);
                layout
            }
        }
    }

    pub(crate) fn intern_str_lit(&self, str: &[u8]) -> DefId {
        let opt_str = self.string_literals.borrow().get(str).copied();
        match opt_str {
            Some(lit_def_id) => lit_def_id,
            None => {
                let str = self.arena.alloc_slice_copy(str);
                let lit_def_id = self.next_def_id();
                self.string_literals.borrow_mut().insert(str, lit_def_id);
                lit_def_id
            }
        }
    }

    pub(crate) fn layout_of(&self, ty: Ty<'cx>) -> TyLayout<'cx> {
        let layout = match *ty {
            TyKind::Void => Layout::size_align(0, 1),
            TyKind::Char => Layout::size_align(1, 1),
            TyKind::Int(int) => match int.1 {
                IntTyKind::Bool => Layout::size_align(1, 1),
                IntTyKind::Char => Layout::size_align(1, 1),
                IntTyKind::Short => Layout::size_align(2, 2),
                IntTyKind::Int => Layout::size_align(4, 4),
                IntTyKind::Long => Layout::size_align(8, 8),
                IntTyKind::LongLong => Layout::size_align(8, 8),
            },
            TyKind::Float => Layout::size_align(4, 4),
            TyKind::Double => Layout::size_align(8, 8),
            TyKind::LongDouble => Layout::size_align(8, 8),
            TyKind::Func(_, _) => Layout::size_align(8, 8),
            TyKind::Struct(_) => todo!("layout_of struct"),
            TyKind::Union(_) => todo!("layout_of union"),
            TyKind::Enum(_) => todo!("layout_of enum"),
            TyKind::Ptr(_) => Layout::size_align(8, 8),
        };
        let layout = self.intern_layout(layout);
        TyLayout { ty, layout }
    }
}
