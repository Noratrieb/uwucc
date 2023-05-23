use std::io::Write;

type Result = std::io::Result<()>;

use crate::{
    ast::{
        ArithOpKind, Atom, BinaryOp, ComparisonKind, Decl, DeclAttr, DeclSpec, Declarator,
        DirectDeclarator, Expr, ExprBinary, ExprPostfix, ExprUnary, ExternalDecl, FunctionDef,
        FunctionParamDecl, InitDecl, IntSign, IntTyKind, NormalDecl, PostfixOp, Stmt,
        TypeSpecifier, UnaryOp,
    },
    sym::Symbol,
    Span, Spanned,
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

    fn sym(&mut self, sym: Symbol) -> Result {
        sym.as_str(|sym| self.string(sym))
    }

    fn print_indent(&mut self) -> Result {
        self.string(&INDENT.repeat(self.indent))
    }

    fn linebreak(&mut self) -> Result {
        self.string("\n")?;
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
            ExternalDecl::Decl(decl) => {
                self.decl(decl, false)?;
                self.linebreak()?;
                Ok(())
            }
        }
    }

    fn function_def(&mut self, def: &FunctionDef) -> Result {
        self.decl(&def.decl, true)?;
        self.string(" ")?;
        self.block(&def.body)?;
        self.linebreak()?;

        Ok(())
    }

    /// prints a block at the current location, stops right after the last `}`
    fn block(&mut self, body: &[(Stmt, Span)]) -> Result {
        self.string("{")?;
        self.linebreak()?;
        self.indent();
        for (stmt, _) in body {
            self.print_indent()?;
            self.stmt(stmt)?;
            self.linebreak()?;
        }
        self.dedent();
        self.print_indent()?;
        self.string("}")?;
        Ok(())
    }

    fn stmt(&mut self, stmt: &Stmt) -> Result {
        match stmt {
            Stmt::Decl(decl) => self.decl(decl, false),
            Stmt::Labeled { label, stmt } => {
                self.sym(label.0)?;
                self.string(":")?;
                self.linebreak()?;
                self.print_indent()?;
                self.stmt(&stmt.0)?;
                Ok(())
            }
            Stmt::Compound(body) => self.block(body),
            Stmt::If {
                cond: (cond, _),
                then,
                otherwise,
            } => {
                self.string("if (")?;
                self.expr(cond)?;
                self.string(") ")?;
                self.block(then)?;
                if let Some(block) = otherwise {
                    self.string(" else ")?;
                    self.block(block)?;
                }
                Ok(())
            }
            Stmt::Switch => todo!(),
            Stmt::While { cond, body } => {
                self.string("while (")?;
                self.expr(cond)?;
                self.string(") ")?;
                self.block(body)?;
                Ok(())
            }
            Stmt::For {
                init_decl,
                init_expr,
                cond,
                post,
                body,
            } => {
                if init_expr.is_some() {
                    todo!()
                }

                self.string("for (")?;
                if let Some((decl, _)) = init_decl {
                    self.decl(decl, false)?;
                } else {
                    self.string(";")?;
                }

                if let Some((cond, _)) = cond {
                    self.string(" ")?;
                    self.expr(cond)?;
                }
                self.string(";")?;

                if let Some((post, _)) = post {
                    self.string(" ")?;
                    self.expr(post)?;
                }

                self.string(") ")?;
                self.block(body)?;

                Ok(())
            }
            Stmt::Goto((label, _)) => {
                self.string("goto ")?;
                self.sym(*label)?;
                Ok(())
            }
            Stmt::Continue => self.string("continue"),
            Stmt::Break => self.string("break"),
            Stmt::Return(expr) => {
                self.string("return")?;
                if let Some((expr, _)) = expr {
                    self.string(" ")?;
                    self.expr(expr)?;
                }
                Ok(())
            }
            Stmt::Expr(expr) => self.expr(expr),
        }
    }

    fn decl(&mut self, decl: &Decl, func: bool) -> Result {
        match decl {
            Decl::StaticAssert => todo!(),
            Decl::Normal(normal_decl) => self.normal_decl(normal_decl)?,
        }
        if !func {
            self.string(";")?;
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
            TypeSpecifier::Integer(int) => {
                // prefix the unsignedness if desired
                if let IntSign::Unsigned = int.0 {
                    self.string("unsigned ")?;
                }

                match int.1 {
                    IntTyKind::Bool => self.string("_Bool"),
                    IntTyKind::Char => self.string("char"),
                    IntTyKind::Short => self.string("short"),
                    IntTyKind::Int => self.string("int"),
                    IntTyKind::Long => self.string("long"),
                    IntTyKind::LongLong => self.string("long long"),
                }
            }
            TypeSpecifier::Float => self.string("float"),
            TypeSpecifier::Double => self.string("double"),
            TypeSpecifier::LongDouble => self.string("long double"),
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
            DirectDeclarator::Ident(ident) => self.sym(ident.0),
            DirectDeclarator::WithParams { ident, params } => {
                self.sym(ident.0)?;
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
                Atom::Ident((ident, _)) => self.sym(*ident),
                Atom::Int(int) => write!(self.output, "{}", int),
                Atom::Float(float) => write!(self.output, "{}", float),
                Atom::String(string) => {
                    self.string("\"")?;
                    // bare attempt at escpaing
                    self.string(
                        &std::str::from_utf8(string)
                            .unwrap_or("<invalid utf-8>")
                            .replace('\"', "\\\""),
                    )?;
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
            Expr::Postfix(ExprPostfix { lhs, op }) => {
                self.expr(&lhs.0)?;
                match op {
                    PostfixOp::Call(args) => {
                        self.string("(")?;
                        let mut first = true;
                        for (expr, _) in args {
                            if !first {
                                self.string(", ")?;
                            }
                            first = false;
                            self.expr(expr)?;
                        }
                        self.string(")")?;
                        Ok(())
                    }
                    PostfixOp::Member((ident, _)) => {
                        self.string(".")?;
                        self.sym(*ident)?;
                        Ok(())
                    }
                    PostfixOp::ArrowMember((ident, _)) => {
                        self.string("->")?;
                        self.sym(*ident)?;
                        Ok(())
                    }
                    PostfixOp::Increment => self.string("++"),
                    PostfixOp::Decrement => self.string("--"),
                }
            }
        }
    }

    fn unary(&mut self, unary: &ExprUnary) -> Result {
        self.string(match unary.op {
            UnaryOp::Increment => "++",
            UnaryOp::Decrement => "--",
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
