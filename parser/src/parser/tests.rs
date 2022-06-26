use std::fmt::Debug;

use super::{Parser, Tok};
use crate::Span;

fn lex_and_pre(src: &str) -> impl Iterator<Item = (Tok<'_>, Span)> + '_ {
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
    parse_test!(
        r#"
void uwu() {}
    "#
    );
}

#[test]
fn empty_funky_attributes_no_params_function() {
    parse_test!(
        r#"
extern volatile _Thread_local int uwu() {}
    "#
    );
}

#[test]
fn empty_function_with_params() {
    parse_test!(
        r#"
int uwu(long owo, int qwq) {}
    "#
    );
}

#[test]
fn global_variable_declarations() {
    parse_test!(
        r#"
int test;
_Thread_local double uwu, owo;

// oh no, a function
int function();
    "#
    );
}

#[test]
fn small_expression() {
    parse_test!(
        r#"
int x = 1 + 1;
    "#
    );
}
