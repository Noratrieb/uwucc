use super::Tok;
use crate::{ast::ExternalDecl, parser::Error, Span, Spanned};

fn lex_and_pre(src: &str) -> impl Iterator<Item = (Tok<'_>, Span)> + '_ {
    let pre_tokens = crate::pre::preprocess_tokens(src);
    crate::token::pre_tokens_to_tokens(pre_tokens)
}

fn pretty_print(ast: &Result<Vec<Spanned<ExternalDecl>>, Error>) -> String {
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

#[test]
fn integer_types() {
    parse_test!(
        r#"
         char          a = 1;
signed   char          b = 2;
unsigned char          c = 3;

         short         d = 4;
signed   short         e = 6;
         short     int f = 5;
signed   short     int g = 7;
unsigned short         h = 8;
unsigned short     int i = 9;

signed                 j = 10;
                   int k = 11;
signed             int l = 12;
unsigned               m = 13;
unsigned           int n = 14;

         long          o = 15;
signed   long          p = 16;
         long      int q = 17;
signed   long      int r = 18;
unsigned long          s = 19;
unsigned long      int t = 20;

         long long     u = 21;
signed   long long     v = 22;
         long long int w = 23;
signed   long long int x = 24;
unsigned long long     y = 25;
unsigned long long int z = 26;
    "#
    );
}

#[test]
fn decl_stmts() {
    parse_test!(
        r#"
int main() {
    int i = 0;
    float f = 1 + 32;
}
    "#
    );
}

#[test]
fn expr_stmt() {
    parse_test!(
        r#"
int main() {
    1 + 1;
    "hello world!";
}
    "#
    );
}

#[test]
fn hello_world() {
    parse_test!(
        r#"
int main() {
    puts("Hello, world!");
}
    "#
    );
}

#[test]
fn if_else() {
    parse_test!(
        r#"
int main() {
    if (1) {
        "a";
    } else {
        "b";
    }
}
    "#
    );
}

#[test]
fn if_else_braceless() {
    parse_test!(
        r#"
int main() {
    if (1)
        "a";
    else
        "b";
}
    "#
    );
}

#[test]
fn else_if() {
    parse_test!(
        r#"
int main() {
    if (1) {
        "a";
    } else if (2) {
        "b";
    } else {
        "c";
    }
}
    "#
    );
}

#[test]
fn return_empty() {
    parse_test!(
        r#"
int main() {
    return;
}
    "#
    );
}

#[test]
fn return_expr() {
    parse_test!(
        r#"
int main() {
    return 0;
}
    "#
    );
}
