use parser::{
    ast::{DeclAttr, ExternalDecl, Stmt, TranslationUnit, TypeSpecifier},
    Span, Symbol,
};
use rustc_hash::FxHashMap;

use crate::{
    ir::{
        BasicBlock, Branch, ConstValue, Func, Ir, Layout, Operand, Register, RegisterData,
        Statement, StatementKind,
    },
    ty::Ty,
    Error,
};

struct LoweringCx {}

pub fn lower_translation_unit(ast: &TranslationUnit) -> Result<Ir, Error> {
    let mut lcx = LoweringCx {};

    for (decl, _) in ast {
        match decl {
            ExternalDecl::Decl(_) => todo!("decl is unsupported"),
            ExternalDecl::FunctionDef(def) => {
                let decl = def.decl.uwnrap_normal();
                let body = &def.body;
                let ret_ty = lower_ty(&decl.decl_spec.ty);
                lower_body(&mut lcx, body, decl.init_declarators[0].1, ret_ty)?;
            }
        }
    }

    todo!()
}

#[derive(Debug)]
struct FnLoweringCtxt {
    scopes: Vec<FxHashMap<Symbol, VariableInfo>>,
    ir: Func,
    current_bb: u32,
}

impl FnLoweringCtxt {
    fn alloca(&mut self, layout: &Layout, name: Option<Symbol>, span: Span) -> Register {
        let bb = self.bb_mut();
        let reg = Register(bb.regs.len().try_into().unwrap());
        bb.regs.push(RegisterData { name });
        let stmt = Statement {
            span,
            kind: StatementKind::Alloca {
                reg,
                size: Operand::Const(ConstValue::u64(layout.size)),
                align: Operand::Const(ConstValue::u64(layout.align)),
            },
        };
        bb.statements.push(stmt);
        reg
    }

    fn bb_mut(&mut self) -> &mut BasicBlock {
        &mut self.ir.bbs[self.current_bb as usize]
    }
}

#[derive(Debug)]
struct VariableInfo {
    def_span: Span,
    ty: Ty,
    ptr_to: Register,
    decl_attr: DeclAttr,
    layout: Layout,
}

fn lower_body(
    // may be used later
    _lcx: &mut LoweringCx,
    body: &[(Stmt, Span)],
    def_span: Span,
    ret_ty: Ty,
) -> Result<Func, Error> {
    let mut cx: FnLoweringCtxt = FnLoweringCtxt {
        scopes: vec![FxHashMap::default()],
        ir: Func {
            bbs: vec![BasicBlock {
                regs: Vec::new(),
                statements: Vec::new(),
                term: Branch::Goto(0),
            }],
            def_span,
            ret_ty,
        },
        current_bb: 0,
    };

    for (stmt, stmt_span) in body {
        match stmt {
            Stmt::Decl(decl) => {
                let decl = decl.uwnrap_normal();
                let ty = lower_ty(&decl.decl_spec.ty);
                let decl_attr = decl.decl_spec.attrs;

                for (var, def_span) in &decl.init_declarators {
                    let layout = layout_of(&ty);
                    let (name, _) = var.declarator.decl.name();
                    let ptr_to = cx.alloca(&layout, Some(name), *stmt_span);

                    let variable_info = VariableInfo {
                        def_span: *def_span,
                        ty: ty.clone(),
                        ptr_to,
                        decl_attr,
                        layout,
                    };
                    cx.scopes.last_mut().unwrap().insert(name, variable_info);
                }
            }
            Stmt::Labeled { .. } => todo!("labels are not implemented"),
            Stmt::Compound(_) => todo!("blocks are not implemented"),
            Stmt::If {
                cond,
                then,
                otherwise,
            } => todo!(),
            Stmt::Switch => todo!(),
            Stmt::While { cond, body } => todo!(),
            Stmt::For {
                init_decl,
                init_expr,
                cond,
                post,
                body,
            } => todo!(),
            Stmt::Goto(_) => todo!(),
            Stmt::Continue => todo!(),
            Stmt::Break => todo!(),
            Stmt::Return(_) => todo!(),
            Stmt::Expr(_) => todo!(),
        }
    }

    dbg!(&cx);

    Ok(cx.ir)
}

fn lower_ty(ty: &TypeSpecifier) -> Ty {
    match ty {
        TypeSpecifier::Void => Ty::Void,
        TypeSpecifier::Char => Ty::Char,
        TypeSpecifier::SChar => Ty::SChar,
        TypeSpecifier::UChar => Ty::UChar,
        TypeSpecifier::Integer(int) => Ty::Integer(*int),
        TypeSpecifier::Float => Ty::Float,
        TypeSpecifier::Double => Ty::Double,
        TypeSpecifier::LongDouble => Ty::LongDouble,
        TypeSpecifier::Bool => Ty::Bool,
    }
}

fn layout_of(ty: &Ty) -> Layout {
    match ty {
        Ty::Void => Layout::size_align(0, 1),
        Ty::Char => Layout::size_align(1, 1),
        Ty::SChar => Layout::size_align(1, 1),
        Ty::UChar => Layout::size_align(1, 1),
        Ty::Integer(int) => match int.kind {
            parser::ast::IntTyKind::Short => Layout::size_align(2, 2),
            parser::ast::IntTyKind::Int => Layout::size_align(4, 4),
            parser::ast::IntTyKind::Long => Layout::size_align(8, 8),
            parser::ast::IntTyKind::LongLong => Layout::size_align(8, 8),
        },
        Ty::Float => Layout::size_align(4, 4),
        Ty::Double => Layout::size_align(8, 8),
        Ty::LongDouble => Layout::size_align(8, 8),
        Ty::Bool => Layout::size_align(1, 1),
        Ty::Struct(_) => todo!("layout_of struct"),
        Ty::Union(_) => todo!("layout_of union"),
        Ty::Enum(_) => todo!("layout_of enum"),
    }
}
