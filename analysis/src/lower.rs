mod builder;
mod typeck;

use parser::{
    ast::{self, ExprBinary},
    Error, Span, Symbol,
};
use rustc_hash::FxHashMap;

use self::{builder::FuncBuilder, typeck::Coercion};
use crate::{
    ctxt::LoweringCx,
    ir::{
        self, BbIdx, BinKind, Branch, ConstValue, Func, Ir, Operand, Register, TyLayout, UnaryKind,
        VariableInfo, VariableInfoKind,
    },
    ty::{Ty, TyKind},
};

type Result<T, E = Error> = std::result::Result<T, E>;

pub fn lower_translation_unit<'cx>(
    lcx: &mut LoweringCx<'cx>,
    ast: &ast::TranslationUnit,
) -> Result<Ir<'cx>, Error> {
    let mut ir = Ir {
        funcs: FxHashMap::default(),
    };

    for (decl, _) in ast {
        match decl {
            ast::ExternalDecl::Decl(_) => todo!("decl is unsupported"),
            ast::ExternalDecl::FunctionDef(def) => {
                let decl = def.decl.unwrap_normal();
                let body = &def.body;
                let ret_ty = lcx.lower_ty(&decl.decl_spec.ty);

                let (ref declarator, def_span) = decl.init_declarators[0];

                let ast::DirectDeclarator::WithParams { ident, params } = &declarator.declarator.decl else {
                    unreachable!("function def needs withparams declarator");
                };

                let def_id = lcx.next_def_id();
                let func = lower_func(lcx, body, def_span, ident.0, ret_ty, params)?;

                let args = &*lcx
                    .arena
                    .alloc_slice_fill_iter(func.regs[..func.arity].iter().map(|data| data.tyl.ty));

                let ty = lcx.intern_ty(TyKind::Func(args, func.ret_ty));

                let def_span = func.def_span;

                ir.funcs.insert(def_id, func);

                lcx.global_decls.insert(
                    ident.0,
                    VariableInfo {
                        def_span,
                        decl_attr: ast::DeclAttr::empty(),
                        tyl: lcx.layout_of(ty),
                        kind: VariableInfoKind::FnDef { def_id },
                    },
                );
            }
        }
    }

    for func in ir.funcs.values() {
        ir::validate(func);
    }

    Ok(ir)
}

struct FnLoweringCtxt<'a, 'cx> {
    scopes: Vec<FxHashMap<Symbol, VariableInfo<'cx>>>,
    build: FuncBuilder<'a, 'cx>,
    lcx: &'a LoweringCx<'cx>,
}

impl<'a, 'cx> FnLoweringCtxt<'a, 'cx> {
    fn dummy_tyl(&self) -> TyLayout<'cx> {
        self.ty_layout(TyKind::Void)
    }

    fn ty_layout(&self, ty_kind: TyKind<'cx>) -> TyLayout<'cx> {
        self.lcx.layout_of(self.lcx.intern_ty(ty_kind))
    }

    fn resolve_ident(&self, ident: Symbol) -> Option<&VariableInfo<'cx>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|s| s.get(&ident))
            .or_else(|| self.lcx.global_decls.get(&ident))
    }

    fn lower_block(&mut self, body: &[(ast::Stmt, Span)]) -> Result<()> {
        self.scopes.push(Default::default());
        for (stmt, stmt_span) in body {
            self.lower_stmt(stmt, *stmt_span)?;
        }
        self.scopes.pop();
        Ok(())
    }

    fn declare_local(&mut self, decl: &ast::Decl, span: Span) -> Result<()> {
        let decl = decl.unwrap_normal();
        let ty = self.lcx.lower_ty(&decl.decl_spec.ty);
        let decl_attr = decl.decl_spec.attrs;

        for (var, def_span) in &decl.init_declarators {
            let tyl = self.lcx.layout_of(ty);
            let (name, name_span) = var.declarator.decl.name();
            let ptr_to = self.build.alloca(tyl.layout, Some(name), span);

            let variable_info = VariableInfo {
                def_span: *def_span,
                decl_attr,
                tyl,
                kind: VariableInfoKind::Local { ptr_to },
            };
            let predeclared = self.scopes.last_mut().unwrap().insert(name, variable_info);
            if let Some(predeclared) = predeclared {
                return Err(Error::new(
                    format!("variable {name} has already been declared"),
                    name_span,
                )
                .note_spanned("already declared here", predeclared.def_span));
            }
            if let Some((init, init_span)) = &var.init {
                let init = self.lower_expr(init, *init_span)?;
                self.build
                    .store(Operand::Reg(ptr_to), init.0, tyl.layout, *init_span);
            }
        }
        Ok(())
    }

    fn expr_as_lvalue(&mut self, expr: &ast::Expr) -> Result<(Operand, TyLayout<'cx>)> {
        let ast::Expr::Atom(ast::Atom::Ident((ident, ident_span))) = *expr else {
            todo!("complex lvalues")
        };
        let Some(var) = self.resolve_ident(ident) else {
            return Err(Error::new(format!("cannot find variable {ident}"), ident_span));
        };
        Ok(match var.kind {
            VariableInfoKind::Local { ptr_to } => (Operand::Reg(ptr_to), var.tyl),
            VariableInfoKind::FnDef { def_id } => {
                (Operand::Const(ConstValue::StaticPtr(def_id)), var.tyl)
            }
            VariableInfoKind::Static { def_id } => {
                (Operand::Const(ConstValue::StaticPtr(def_id)), var.tyl)
            }
        })
    }

    fn lower_stmt(&mut self, stmt: &ast::Stmt, stmt_span: Span) -> Result<()> {
        match stmt {
            ast::Stmt::Decl(decl) => {
                self.declare_local(decl, stmt_span)?;
            }
            ast::Stmt::Labeled { .. } => todo!("labels are not implemented"),
            ast::Stmt::Compound(block) => {
                self.lower_block(block)?;
            }
            ast::Stmt::If {
                cond,
                then: then_body,
                otherwise,
            } => {
                let cond = self.lower_expr(&cond.0, cond.1)?;
                let pred = self.build.current_bb;
                let then = self.build.new_block();
                let els = otherwise
                    .as_deref()
                    .map(|oth| (oth, self.build.new_block()));
                let cont = self.build.new_block();

                self.build.current_bb = then;
                self.lower_block(then_body)?;
                self.build.cur_bb_mut().term = Branch::Goto(cont);

                let false_branch = match els {
                    Some((otherwise, els)) => {
                        self.build.current_bb = els;
                        self.lower_block(otherwise)?;
                        self.build.cur_bb_mut().term = Branch::Goto(cont);
                        els
                    }
                    None => cont,
                };
                self.build.bb_mut(pred).term = Branch::Switch {
                    cond: cond.0,
                    yes: then,
                    no: false_branch,
                };
                self.build.current_bb = cont;
            }
            ast::Stmt::Switch => todo!(),
            ast::Stmt::While { .. } => todo!(),
            ast::Stmt::For { .. } => todo!(),
            ast::Stmt::Goto(_) => todo!(),
            ast::Stmt::Continue => todo!(),
            ast::Stmt::Break => todo!(),
            ast::Stmt::Return(expr) => {
                let ret = match expr {
                    Some(expr) => self.lower_expr(&expr.0, expr.1)?.0,
                    None => Operand::Const(ConstValue::Void),
                };
                self.build.cur_bb_mut().term = Branch::Ret(ret);
            }
            ast::Stmt::Expr(ast::Expr::Binary(ast::ExprBinary {
                op: ast::BinaryOp::Assign(assign),
                lhs,
                rhs,
            })) => {
                if assign.is_some() {
                    todo!("assign operation");
                }
                let rhs = self.lower_expr(&rhs.0, rhs.1)?;
                let (ptr_to, tyl) = self.expr_as_lvalue(&lhs.0)?;
                self.build.store(ptr_to, rhs.0, tyl.layout, stmt_span);
            }
            ast::Stmt::Expr(expr) => {
                self.lower_expr(expr, stmt_span)?;
            }
        }

        Ok(())
    }

    fn lower_expr(&mut self, expr: &ast::Expr, span: Span) -> Result<(Operand, TyLayout<'cx>)> {
        let op_tyl = match expr {
            ast::Expr::Atom(ast::Atom::Char(c)) => (
                Operand::Const(ConstValue::Int((*c).into())),
                self.lcx.layout_of(self.lcx.types.char),
            ),
            ast::Expr::Atom(ast::Atom::Int(i)) => (
                Operand::Const(ConstValue::Int(*i as _)),
                self.lcx.layout_of(self.lcx.types.int.signed),
            ),
            ast::Expr::Atom(ast::Atom::Float(_)) => todo!("no floats"),
            ast::Expr::Atom(ast::Atom::Ident((ident, ident_span))) => {
                let Some(var) = self.resolve_ident(*ident) else {
                    return Err(Error::new(format!("cannot find variable {ident}"), *ident_span));
                };
                let tyl = var.tyl;
                match var.kind {
                    VariableInfoKind::Local { ptr_to } => {
                        let op = self.build.load(var.tyl, Operand::Reg(ptr_to), span);
                        (Operand::Reg(op), tyl)
                    }
                    VariableInfoKind::FnDef { def_id } => {
                        (Operand::Const(ConstValue::StaticPtr(def_id)), tyl)
                    }
                    VariableInfoKind::Static { def_id } => {
                        let op = self.build.load(
                            var.tyl,
                            Operand::Const(ConstValue::StaticPtr(def_id)),
                            span,
                        );
                        (Operand::Reg(op), tyl)
                    }
                }
            }
            ast::Expr::Atom(ast::Atom::String(string)) => {
                let lit_def_id = self.lcx.intern_str_lit(string);
                (
                    Operand::Const(ConstValue::StaticPtr(lit_def_id)),
                    self.ty_layout(TyKind::Ptr(self.lcx.types.char)),
                )
            }
            ast::Expr::Unary(ast::ExprUnary {
                op: op @ (ast::UnaryOp::Increment | ast::UnaryOp::Decrement),
                rhs: rhs_expr,
            }) => {
                // First increment/decrement, then return the value.
                let (rhs, rhs_tyl) = self.lower_expr(&rhs_expr.0, rhs_expr.1)?;
                let is_incr = matches!(op, ast::UnaryOp::Increment);

                if !rhs_tyl.ty.is_integral() {
                    return Err(Error::new(
                        format!(
                            "cannot {} {}",
                            if is_incr { "increment" } else { "decrement" },
                            rhs_tyl.ty
                        ),
                        rhs_expr.1,
                    ));
                }

                let (lvalue, tyl) = self.expr_as_lvalue(&rhs_expr.0)?;
                let bin_kind = if is_incr { BinKind::Add } else { BinKind::Sub };
                let lhs = self.build.load(tyl, lvalue, span);
                let result = self
                    .build
                    .binary(bin_kind, Operand::Reg(lhs), rhs, span, tyl);
                self.build.store(Operand::Reg(lhs), rhs, tyl.layout, span);

                (Operand::Reg(result), tyl)
            }
            ast::Expr::Unary(unary) => {
                let rhs = self.lower_expr(&unary.rhs.0, unary.rhs.1)?;
                let kind = match unary.op {
                    ast::UnaryOp::Increment => unreachable!("handled prefix increment above"),
                    ast::UnaryOp::Decrement => unreachable!("handled prefix increment above"),
                    ast::UnaryOp::AddrOf => todo!("addr of"),
                    ast::UnaryOp::Deref => todo!("deref?"),
                    ast::UnaryOp::Plus => todo!("unary plus lol"),
                    ast::UnaryOp::Minus => UnaryKind::Negate,
                    ast::UnaryOp::Tilde => UnaryKind::BitNot,
                    ast::UnaryOp::Bang => UnaryKind::LogicalNot,
                };

                let reg = self.build.unary(kind, rhs.0, span, self.dummy_tyl());
                (Operand::Reg(reg), self.dummy_tyl())
            }
            ast::Expr::Binary(ast::ExprBinary {
                lhs,
                rhs,
                op: ast::BinaryOp::Assign(assign),
            }) => {
                if assign.is_some() {
                    todo!("assign operation");
                }
                let rhs = self.lower_expr(&rhs.0, rhs.1)?;

                let (ptr_to, tyl) = self.expr_as_lvalue(&lhs.0)?;
                self.build.store(ptr_to, rhs.0, tyl.layout, span);
                rhs
            }
            ast::Expr::Binary(ExprBinary {
                op: ast::BinaryOp::Arith(arith),
                lhs: lhs_expr,
                rhs: rhs_expr,
            }) => {
                let (mut lhs, lhs_tyl) = self.lower_expr(&lhs_expr.0, lhs_expr.1)?;
                let (mut rhs, rhs_tyl) = self.lower_expr(&rhs_expr.0, rhs_expr.1)?;

                let (result, lhs_coerce, rhs_coerce) =
                    self.arith_op(lhs_tyl.ty, rhs_tyl.ty, span)?;

                let mut do_coerce = |reg, coerce, span, ty| {
                    let kind = match coerce {
                        Coercion::ZeroExt => UnaryKind::Zext,
                        Coercion::SignExt => UnaryKind::Sext,
                        Coercion::SignToUnsigned => todo!("hm, what should this do"),
                    };
                    Operand::Reg(self.build.unary(kind, reg, span, self.lcx.layout_of(ty)))
                };

                for (coerce, ty) in lhs_coerce {
                    lhs = do_coerce(lhs, coerce, span, ty);
                }
                for (coerce, ty) in rhs_coerce {
                    rhs = do_coerce(rhs, coerce, span, ty);
                }

                let kind = match arith {
                    ast::ArithOpKind::Mul => BinKind::Mul,
                    ast::ArithOpKind::Div => BinKind::Div,
                    ast::ArithOpKind::Mod => BinKind::Mod,
                    ast::ArithOpKind::Add => BinKind::Add,
                    ast::ArithOpKind::Sub => BinKind::Sub,
                    ast::ArithOpKind::Shl => BinKind::Shl,
                    ast::ArithOpKind::Shr => BinKind::Shr,
                    ast::ArithOpKind::BitAnd => BinKind::BitAnd,
                    ast::ArithOpKind::BitXor => BinKind::BitXor,
                    ast::ArithOpKind::BitOr => BinKind::BitOr,
                };

                let result = self.lcx.layout_of(result);
                let reg = self.build.binary(kind, lhs, rhs, span, result);

                (Operand::Reg(reg), result)
            }
            ast::Expr::Binary(ExprBinary {
                op: ast::BinaryOp::Comparison(comp),
                lhs,
                rhs,
            }) => {
                let lhs = self.lower_expr(&lhs.0, lhs.1)?;
                let rhs = self.lower_expr(&rhs.0, rhs.1)?;
                let kind = match comp {
                    ast::ComparisonKind::Lt => BinKind::Lt,
                    ast::ComparisonKind::Gt => BinKind::Gt,
                    ast::ComparisonKind::LtEq => BinKind::Leq,
                    ast::ComparisonKind::GtEq => BinKind::Geq,
                    ast::ComparisonKind::Eq => BinKind::Eq,
                    ast::ComparisonKind::Neq => BinKind::Neq,
                };

                let reg = self
                    .build
                    .binary(kind, lhs.0, rhs.0, span, self.dummy_tyl());

                (Operand::Reg(reg), self.dummy_tyl())
            }
            ast::Expr::Binary(ExprBinary {
                op: ast::BinaryOp::Comma,
                lhs,
                rhs,
            }) => {
                let _lhs = self.lower_expr(&lhs.0, lhs.1)?;
                // Discard the lhs, evaluate to the rhs.
                self.lower_expr(&rhs.0, rhs.1)?
            }
            ast::Expr::Binary(_) => todo!("other binary"),
            ast::Expr::Postfix(postfix) => {
                let lhs = self.lower_expr(&postfix.lhs.0, postfix.lhs.1)?;
                match &postfix.op {
                    ast::PostfixOp::Call(args) => {
                        let args = args
                            .iter()
                            .map(|(arg, sp)| self.lower_expr(arg, *sp).map(|o| o.0))
                            .collect::<Result<_, _>>()?;

                        let reg = self.build.call(self.dummy_tyl(), lhs.0, args, span);
                        (Operand::Reg(reg), self.dummy_tyl())
                    }
                    ast::PostfixOp::Member(_) => todo!("member expr"),
                    ast::PostfixOp::ArrowMember(_) => todo!("arrow member expr"),
                    ast::PostfixOp::Increment => {
                        todo!("gotta have lvalues")
                    }
                    ast::PostfixOp::Decrement => todo!(),
                }
            }
        };
        Ok(op_tyl)
    }
}

fn lower_func<'cx>(
    // may be used later
    lcx: &LoweringCx<'cx>,
    body: &[(ast::Stmt, Span)],
    def_span: Span,
    name: Symbol,
    ret_ty: Ty<'cx>,
    params: &[ast::FunctionParamDecl],
) -> Result<Func<'cx>, Error> {
    let mut cx = FnLoweringCtxt {
        scopes: vec![Default::default()],
        build: FuncBuilder::new(
            name,
            def_span,
            ret_ty,
            lcx,
            params.len(),
        ),
        lcx,
    };

    for param in params {
        let decl_spec = &param.decl_spec.0;
        let ty = lcx.lower_ty(&decl_spec.ty);
        let tyl = lcx.layout_of(ty);
        // Create all the parameter registers.
        let _ = cx
            .build
            .new_reg(Some(param.declarator.0.decl.name().0), tyl);
    }

    for (i, param) in params.iter().enumerate() {
        // For every param, we create an allocation and store the register into it.
        let param_reg_data = &cx.build.ir.regs[i];
        let name = param.declarator.0.decl.name().0;

        let decl_spec = &param.decl_spec.0;
        let decl_attr = decl_spec.attrs;
        let tyl = param_reg_data.tyl;
        let span = param.declarator.1;

        let alloca_name = Symbol::intern(&format!("{}.local", name));
        let ptr_to = cx.build.alloca(tyl.layout, Some(alloca_name), span);

        let variable_info = VariableInfo {
            def_span: span,
            decl_attr,
            tyl,
            kind: VariableInfoKind::Local { ptr_to },
        };
        let predeclared = cx.scopes.last_mut().unwrap().insert(name, variable_info);
        if let Some(predeclared) = predeclared {
            return Err(
                Error::new(format!("parameter {name} has already been declared"), span)
                    .note_spanned("already declared here", predeclared.def_span),
            );
        }

        cx.build.store(
            Operand::Reg(ptr_to),
            Operand::Reg(Register(i as _)),
            tyl.layout,
            span,
        );
    }

    cx.lower_block(body)?;

    if let Branch::Goto(BbIdx(u32::MAX)) = cx.build.cur_bb_mut().term {
        cx.build.cur_bb_mut().term = Branch::Ret(Operand::Const(ConstValue::Void));
    }

    Ok(cx.build.finish())
}
