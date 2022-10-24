use parser::{ast, Span};
use rustc_hash::FxHashMap;

use crate::hir::{self, DefId, Symbol};

pub struct LowerCtx<'hir> {
    hir_symbol_intern: lasso::Rodeo,
    hir_arena: &'hir bumpalo::Bump,
    global_symbols: FxHashMap<Symbol, &'hir hir::ExternalDecl>,
    scope: Scope,
}

struct Scope {
    parent: Option<Box<Scope>>,
    variables: FxHashMap<Symbol, DefId>,
}

impl Scope {
    fn new() -> Self {
        Self {
            parent: None,
            variables: FxHashMap::default(),
        }
    }

    fn insert(&mut self, symbol: Symbol, def_id: DefId) {
        self.variables.insert(symbol, def_id);
    }

    fn enter_new(&mut self) {
        let new = Self::new();
        let this = std::mem::replace(self, new);
        self.parent = Some(Box::new(this));
    }

    fn leave(&mut self) {
        let old = std::mem::replace(self, Self::new());
        let parent = old.parent.expect("parent not found when leaving scope");
        *self = *parent;
    }

    fn lookup(&self, sym: Symbol) -> Option<DefId> {
        self.variables
            .get(&sym)
            .copied()
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.lookup(sym)))
    }
}

impl<'hir> LowerCtx<'hir> {
    pub fn lower_translation_unit(&mut self, unit: &ast::TranslationUnit) -> hir::Hir<'hir> {
        for _decl in unit {}

        todo!()
    }

    fn lower_decl(&mut self, decl: &ast::ExternalDecl, span: Span) -> hir::ExternalDecl {
        match decl {
            ast::ExternalDecl::Decl(_) => todo!(),
            ast::ExternalDecl::FunctionDef(def) => {
                let _fn_def = self.lower_function_def(def, span);
                hir::ExternalDecl
            }
        }
    }

    fn lower_function_def(&mut self, def: &ast::FunctionDef, _span: Span) -> hir::FunctionDef {
        let decl = def.decl.uwnrap_normal();
        let (init_declarator, _declarator_span) = decl
            .init_declarators
            .get(0)
            .expect("single init declarator in function definition");
        let declarator = &init_declarator.declarator;

        let ((name_ident, _name_span), _param_decls) = declarator.decl.unwrap_with_params();

        let name_sym = self.hir_symbol_intern.get_or_intern(name_ident);

        if self.global_symbols.contains_key(&name_sym) {
            panic!("function declarated twice! return this error properly! lol!")
        }

        todo!()
    }
}
