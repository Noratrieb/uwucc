use std::io::Write;

type Result = std::io::Result<()>;

use crate::{
    ast::{
        ArithOpKind, Atom, BinaryOp, ComparisonKind, Decl, DeclAttr, DeclSpec, Declarator,
        DirectDeclarator, Expr, ExprBinary, ExprUnary, ExternalDecl, FunctionDef,
        FunctionParamDecl, InitDecl, IntTyKind, IntTySignedness, NormalDecl, TypeSpecifier,
        UnaryOp,
    },
    Spanned,
};

pub struct PrettyPrinter<W> {
    indent: usize,
    output: W,
    force_parens: bool,
}

const INDENT: &str = "    ";

impl<W: Write> PrettyPrinter<W> {
    pub fn new(output: W, force_parens: bool) -> Self {
        Self {
            indent: 0,
            output,
            force_parens,
        }
    }

    fn string(&mut self, str: &str) -> Result {
        write!(self.output, "{}", str)
    }

    fn print_indent(&mut self) -> Result {
        self.string(&INDENT.repeat(self.indent))
    }

    fn linebreak(&mut self) -> Result {
        self.string("\n")?;
        self.print_indent()?;
        Ok(())
    }

    fn indent(&mut self) {
        self.indent += 1;
    }

    fn dedent(&mut self) {
        self.indent -= 1;
    }

    pub fn translation_unit(&mut self, unit: &[Spanned<ExternalDecl>]) -> Result {
        for decl in unit {
            self.external_decl(&decl.0)?;
        }
        Ok(())
    }

    fn external_decl(&mut self, decl: &ExternalDecl) -> Result {
        match decl {
            ExternalDecl::FunctionDef(def) => self.function_def(def),
            ExternalDecl::Decl(decl) => self.decl(decl, false),
        }
    }

    fn function_def(&mut self, def: &FunctionDef) -> Result {
        self.decl(&def.decl, true)?;
        self.string(" {")?;
        self.linebreak()?;
        self.indent();
        // TODO: body
        self.dedent();
        self.string("}")?;
        self.linebreak()?;

        Ok(())
    }

    fn decl(&mut self, decl: &Decl, func: bool) -> Result {
        match decl {
            Decl::StaticAssert => todo!(),
            Decl::Normal(normal_decl) => self.normal_decl(normal_decl)?,
        }
        if !func {
            self.string(";")?;
            self.linebreak()?;
        }
        Ok(())
    }

    fn normal_decl(&mut self, decl: &NormalDecl) -> Result {
        self.decl_spec(&decl.decl_spec)?;
        self.string(" ")?;
        let mut first = true;
        for declarator in &decl.init_declarators {
            if !first {
                self.string(", ")?;
            }
            first = false;
            self.init_declarator(&declarator.0)?;
        }

        Ok(())
    }

    fn decl_spec(&mut self, decl_spec: &DeclSpec) -> Result {
        self.decl_attr(&decl_spec.attrs)?;
        self.type_specifier(&decl_spec.ty)?;
        Ok(())
    }

    fn init_declarator(&mut self, init_declarator: &InitDecl) -> Result {
        self.declarator(&init_declarator.declarator)?;
        if let Some(init) = &init_declarator.init {
            self.string(" = ")?;
            self.expr(&init.0)?;
        }
        Ok(())
    }

    fn type_specifier(&mut self, spec: &TypeSpecifier) -> Result {
        match spec {
            TypeSpecifier::Void => self.string("void"),
            TypeSpecifier::Char => self.string("char"),
            TypeSpecifier::SChar => self.string("signed char"),
            TypeSpecifier::UChar => self.string("unsigned char"),
            TypeSpecifier::Integer(int) => {
                // prefix the unsignedness if desired
                if let IntTySignedness::Unsigned = int.sign {
                    self.string("unsigned ")?;
                }

                match int.kind {
                    IntTyKind::Short => self.string("short"),
                    IntTyKind::Int => self.string("int"),
                    IntTyKind::Long => self.string("long"),
                    IntTyKind::LongLong => self.string("long long"),
                }
            }
            TypeSpecifier::Float => self.string("float"),
            TypeSpecifier::Double => self.string("double"),
            TypeSpecifier::LongDouble => self.string("long double"),
            TypeSpecifier::Bool => self.string("_Bool"),
        }
    }

    fn decl_attr(&mut self, attr: &DeclAttr) -> Result {
        let mut attrs = Vec::new();
        if attr.contains(DeclAttr::EXTERN) {
            attrs.push("extern");
        }
        if attr.contains(DeclAttr::STATIC) {
            attrs.push("static");
        }
        if attr.contains(DeclAttr::THREAD_LOCAL) {
            attrs.push("_Thread_local");
        }
        self.string(&attrs.join(" "))?;
        if !attrs.is_empty() {
            self.string(" ")?;
        }

        Ok(())
    }

    fn declarator(&mut self, declarator: &Declarator) -> Result {
        if declarator.pointer {
            self.string("*")?;
        }
        self.direct_declarator(&declarator.decl)?;
        Ok(())
    }

    fn direct_declarator(&mut self, declarator: &DirectDeclarator) -> Result {
        match declarator {
            DirectDeclarator::Ident(ident) => self.string(&ident.0),
            DirectDeclarator::WithParams { ident, params } => {
                self.string(&ident.0)?;
                self.string("(")?;
                self.function_param_decls(params)?;
                self.string(")")?;
                Ok(())
            }
        }
    }

    fn function_param_decls(&mut self, decls: &[FunctionParamDecl]) -> Result {
        let mut first = true;
        for decl in decls {
            if !first {
                self.string(", ")?;
            }
            first = false;
            self.decl_spec(&decl.decl_spec.0)?;
            self.string(" ")?;
            self.declarator(&decl.declarator.0)?;
        }
        Ok(())
    }

    fn expr(&mut self, expr: &Expr) -> Result {
        match expr {
            Expr::Atom(atom) => match atom {
                Atom::Ident((ident, _)) => self.string(ident),
                Atom::Int(int) => write!(self.output, "{}", int),
                Atom::Float(float) => write!(self.output, "{}", float),
                Atom::String(string) => {
                    self.string("\"")?;
                    // bare attempt at escpaing
                    self.string(&string.replace('\"', "\\\""))?;
                    self.string("\"")?;
                    Ok(())
                }
                Atom::Char(char) => {
                    self.string("'")?;
                    let ascii = *char as char;
                    write!(self.output, "{}", ascii)?;
                    self.string("'")?;
                    Ok(())
                }
            },
            Expr::Unary(unary) => self.unary(unary),
            Expr::Binary(ExprBinary {
                op: BinaryOp::Index,
                lhs,
                rhs,
            }) => {
                self.expr(&lhs.as_ref().0)?;
                self.string("[")?;
                self.expr(&rhs.as_ref().0)?;
                self.string("]")?;
                Ok(())
            }
            Expr::Binary(binary) => self.binary(binary),
        }
    }

    fn unary(&mut self, unary: &ExprUnary) -> Result {
        self.string(match unary.op {
            UnaryOp::AddrOf => "&",
            UnaryOp::Deref => "*",
            UnaryOp::Plus => "+",
            UnaryOp::Minus => "-",
            UnaryOp::Tilde => "~",
            UnaryOp::Bang => "!",
        })?;
        if self.force_parens {
            self.string("(")?;
        }
        self.expr(&unary.rhs.as_ref().0)?;
        if self.force_parens {
            self.string(")")?;
        }
        Ok(())
    }

    fn binary(&mut self, binary: &ExprBinary) -> Result {
        if self.force_parens {
            self.string("(")?;
        }
        self.expr(&binary.lhs.as_ref().0)?;
        self.string(" ")?;
        self.string(match binary.op {
            BinaryOp::Arith(ArithOpKind::Mul) => "*",
            BinaryOp::Arith(ArithOpKind::Div) => "/",
            BinaryOp::Arith(ArithOpKind::Mod) => "%",
            BinaryOp::Arith(ArithOpKind::Add) => "+",
            BinaryOp::Arith(ArithOpKind::Sub) => "-",
            BinaryOp::Arith(ArithOpKind::Shl) => "<<",
            BinaryOp::Arith(ArithOpKind::Shr) => ">>",
            BinaryOp::Arith(ArithOpKind::BitAnd) => "&",
            BinaryOp::Arith(ArithOpKind::BitXor) => "^",
            BinaryOp::Arith(ArithOpKind::BitOr) => "|",
            BinaryOp::LogicalAnd => "&&",
            BinaryOp::LogicalOr => "||",
            BinaryOp::Comparison(ComparisonKind::Lt) => "<",
            BinaryOp::Comparison(ComparisonKind::Gt) => ">",
            BinaryOp::Comparison(ComparisonKind::LtEq) => "<=",
            BinaryOp::Comparison(ComparisonKind::GtEq) => ">=",
            BinaryOp::Comparison(ComparisonKind::Eq) => "==",
            BinaryOp::Comparison(ComparisonKind::Neq) => "!=",
            BinaryOp::Comma => ",",
            BinaryOp::Index => unreachable!("special case outside"),
            BinaryOp::Assign(None) => "=",
            BinaryOp::Assign(Some(ArithOpKind::Mul)) => "*=",
            BinaryOp::Assign(Some(ArithOpKind::Div)) => "/=",
            BinaryOp::Assign(Some(ArithOpKind::Mod)) => "%=",
            BinaryOp::Assign(Some(ArithOpKind::Add)) => "+=",
            BinaryOp::Assign(Some(ArithOpKind::Sub)) => "-=",
            BinaryOp::Assign(Some(ArithOpKind::Shl)) => "<<=",
            BinaryOp::Assign(Some(ArithOpKind::Shr)) => ">>=",
            BinaryOp::Assign(Some(ArithOpKind::BitAnd)) => "&=",
            BinaryOp::Assign(Some(ArithOpKind::BitXor)) => "^=",
            BinaryOp::Assign(Some(ArithOpKind::BitOr)) => "|=",
        })?;
        self.string(" ")?;
        self.expr(&binary.rhs.as_ref().0)?;
        if self.force_parens {
            self.string(")")?;
        }
        Ok(())
    }
}
