use lasso::Spur;
use parser::Spanned;

pub type Symbol = Spur;

pub type Ident = Spanned<Symbol>;

pub struct Hir<'hir> {
    x: &'hir (),
}

pub struct ExternalDecl;

pub struct FunctionDef;
