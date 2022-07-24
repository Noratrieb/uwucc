use parser::{ast, Span};
use rustc_hash::FxHashMap;

use crate::hir::{self, Symbol};

pub struct LowerCtx<'hir> {
    hir_symbol_intern: lasso::Rodeo,
    hir_arena: &'hir bumpalo::Bump,
    global_symbols: FxHashMap<Symbol, &'hir hir::ExternalDecl>,
}

impl<'hir> LowerCtx<'hir> {
    pub fn lower_translation_unit(&mut self, unit: &ast::TranslationUnit) -> hir::Hir<'hir> {
        for _decl in unit {}

        todo!()
    }

    fn lower_decl(&mut self, decl: &ast::ExternalDecl, _span: Span) -> hir::ExternalDecl {
        match decl {
            ast::ExternalDecl::Decl(_) => todo!(),
            ast::ExternalDecl::FunctionDef(_func) => todo!(),
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
