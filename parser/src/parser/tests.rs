use super::Tok;
use crate::{ast::ExternalDecl, parser::ParserError, Span, Spanned};

fn lex_and_pre(src: &str) -> impl Iterator<Item = (Tok<'_>, Span)> + '_ {
    let pre_tokens = crate::pre::preprocess_tokens(src);
    crate::token::pre_tokens_to_tokens(pre_tokens)
}

fn pretty_print(ast: &Result<Vec<Spanned<ExternalDecl>>, ParserError>) -> String {
    let mut vec = Vec::new();

    match ast {
        Ok(ast) => {
            let mut printer = crate::pretty::PrettyPrinter::new(&mut vec, true);
            printer.translation_unit(ast).unwrap();
        }
        Err(err) => vec.extend_from_slice(format!("{err:?}").as_bytes()),
    }

    String::from_utf8_lossy(&vec).into_owned()
}

macro_rules! parse_test {
    ($src:expr) => {
        let lexer = lex_and_pre($src);
        let parsed = super::parse_declarations(lexer);
        let parsed_pretty = dbg_pls::pretty(&parsed);
        let pretty_printed_source = pretty_print(&parsed);

        insta::assert_debug_snapshot!((parsed_pretty, pretty_printed_source));
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
fn small_expressions() {
    parse_test!(
        r#"
int x = 1 + 1;

int y = (1 + (2 - 3));

int z = (array[9]);
    "#
    );
}
