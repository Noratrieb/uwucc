use std::io::Read;

use analysis::LoweringCx;
use parser::Error;

fn main() {
    let input_file = std::env::args().nth(1).expect("first argument");

    let (filename, src) = if input_file == "-" {
        let mut buf = String::new();
        std::io::stdin()
            .lock()
            .read_to_string(&mut buf)
            .unwrap_or_else(|err| {
                eprintln!("failed to read file {input_file}: {err}");
                std::process::exit(1);
            });
        ("<stdin>".into(), buf)
    } else {
        let src = std::fs::read_to_string(&input_file).unwrap_or_else(|err| {
            eprintln!("failed to read file {input_file}: {err}");
            std::process::exit(1);
        });
        (input_file, src)
    };

    let ast = parser::parse_file(&src);
    // dbg_pls::color!(&ast);
    let ast = ast.unwrap_or_else(|err| report_fatal(&filename, &src, err));
    let mut printer = parser::pretty::PrettyPrinter::new(std::io::stdout().lock(), false);
    println!("-------- AST pretty");
    printer.translation_unit(&ast).unwrap();

    let arena = bumpalo::Bump::new();
    let mut lcx = LoweringCx::new(&arena);

    println!("-------- IR");
    let ir = analysis::lower_translation_unit(&mut lcx, &ast)
        .unwrap_or_else(|err| report_fatal(&filename, &src, err));

    println!("-------- ASM");
    codegen::generate(&lcx, &ir).unwrap_or_else(|err| report_fatal(&filename, &src, err));
}

fn report_fatal(filename: &str, source: &str, error: Error) -> ! {
    use ariadne::{Label, Report, ReportKind, Source};

    let line = match error.span {
        Some(span) => {
            let mut line = 0;
            source.char_indices().find(|(i, c)| {
                if *c == '\n' {
                    line += 1;
                }
                // exit if we have found the start
                *i >= span.start
            });
            line
        }
        None => 0,
    };

    let mut rep = Report::build(ReportKind::Error, filename, line).with_message(&error.msg);

    if let Some(span) = error.span {
        rep = rep
            .with_label(Label::new((filename, span.start..span.end)))
            .with_message(&error.msg);
    }

    rep.finish()
        .eprint((filename, Source::from(source)))
        .unwrap();

    std::process::exit(1);
}
