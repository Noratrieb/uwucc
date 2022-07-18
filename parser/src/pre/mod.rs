mod lexer;

use lexer::PLexer;
pub use lexer::{PToken, Punctuator};

use crate::Span;

pub struct Preprocessor<L> {
    lexer: L,
}

impl<'src, L> Iterator for Preprocessor<L>
where
    L: Iterator<Item = (PToken<'src>, Span)>,
{
    type Item = (PToken<'src>, Span);

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next()
    }
}

pub fn preprocess_tokens(src: &str) -> impl Iterator<Item = (PToken<'_>, Span)> {
    let lexer = PLexer::new(src);

    let preprocessor = Preprocessor { lexer };

    preprocessor
}
