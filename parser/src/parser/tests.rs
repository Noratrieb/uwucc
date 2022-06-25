use std::fmt::Debug;

use super::{Parser, Tok};
use crate::Span;

fn lex_and_pre<'src>(src: &'src str) -> impl Iterator<Item = (Tok<'src>, Span)> + 'src {
    let pre_tokens = crate::pre::preprocess_tokens(src);
    crate::token::pre_tokens_to_tokens(pre_tokens)
}

fn the_current_root_parse_thing<'src>(src: impl Iterator<Item = (Tok<'src>, Span)>) -> impl Debug {
    use peekmore::PeekMore;

    let mut parser = Parser {
        lex: src.peekmore(),
    };

    parser.external_declarations()
}

macro_rules! parse_test {
    ($src:expr) => {
        let lexer = lex_and_pre($src);
        let parsed = the_current_root_parse_thing(lexer);
        insta::assert_debug_snapshot!(parsed);
    };
}

#[test]
fn empty_void_function() {
    let src = r#"
void uwu() {}
    "#;

    parse_test!(src);
}

#[test]
fn empty_funky_attributes_no_params_function() {
    let src = r#"
extern volatile _Thread_local int uwu() {}
    "#;

    parse_test!(src);
}

#[test]
#[ignore = "fix declarator mess"]
fn empty_function_with_params() {
    let src = r#"
int uwu(long owo, unsigned qwq) {}
    "#;

    parse_test!(src);
}

#[test]
fn global_variable_declarations() {
    let src = r#"
int test;
_Thread_local double uwu, owo;
    "#;

    parse_test!(src);
}
