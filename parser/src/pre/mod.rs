mod lexer;

use std::{io, path::Path};

use lexer::PLexer;
pub use lexer::{PToken, Punctuator};

use crate::Span;

enum IncludeKind {
    AngleBracketed,
    Quoted,
}

trait FileResolver {
    fn resolve_file(&self, file_name: &Path, kind: IncludeKind) -> io::Result<Vec<u8>>;
}

struct Todo;

impl FileResolver for Todo {
    fn resolve_file(&self, file_name: &Path, kind: IncludeKind) -> io::Result<Vec<u8>> {
        todo!()
    }
}

pub struct Preprocessor<L> {
    lexer: L,
    resolver: Box<dyn FileResolver>,
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

    let preprocessor = Preprocessor {
        lexer,
        resolver: Box::new(Todo),
    };

    preprocessor
}
