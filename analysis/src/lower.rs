use parser::{
    ast::{self, Ident},
    Span,
};
use rustc_hash::FxHashMap;

use crate::hir;

pub struct LowerCtx<'hir> {
    hir_arena: &'hir bumpalo::Bump,
    global_symbols: FxHashMap<Ident, &'hir hir::ExternalDecl>,
}

impl<'hir> LowerCtx<'hir> {
    pub fn lower_translation_unit(&mut self, unit: &ast::TranslationUnit) -> hir::Hir<'hir> {
        for decl in unit {}

        todo!()
    }

    fn lower_decl(&mut self, decl: &ast::ExternalDecl, span: Span) -> hir::ExternalDecl {
        match decl {
            ast::ExternalDecl::Decl(_) => todo!(),
            ast::ExternalDecl::FunctionDef(func) => todo!(),
        }
    }

    fn lower_function_def(&mut self, def: &ast::FunctionDef, span: Span) -> hir::FunctionDef {
        todo!()
    }
}
