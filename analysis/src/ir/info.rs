use rustc_hash::FxHashSet;

use super::{BbIdx, Branch, Func, Location, Operand};
use crate::ir::visit::Visitor;

pub fn traverse_postorder(func: &Func<'_>) -> Vec<BbIdx> {
    // the final traversial, backwards.
    // the starting bb has to be visited last.
    let mut traversal = vec![BbIdx(0)];
    let mut i = 0;
    let mut seen = FxHashSet::default();

    seen.insert(BbIdx(0));

    while let Some(&next) = traversal.get(i) {
        let successors = func.bb(next).term.successors();
        for succ in successors.filter(|&s| seen.insert(s)) {
            traversal.push(succ);
        }

        i += 1;
    }

    traversal.reverse();
    traversal
}

/// The last usage of each SSA register. After that location, the SSA register is not used anymore
/// and can be discarded. Registers with `None` are never used.
pub fn last_register_uses(func: &Func<'_>) -> Vec<Option<Location>> {
    // TODO: This does not work when registers are used after backedges.
    // When mem2reg/stack2ssa is implemented, this will be very bad. Right now, it's totally fine!
    let mut uses = vec![None; func.regs.len()];

    for bb in traverse_postorder(func) {
        let uses = &mut *uses;

        let mut check_op = |op: Operand, stmt| match op {
            Operand::Reg(reg) => {
                if uses[reg.as_usize()].is_none() {
                    uses[reg.as_usize()] = Some(Location { bb, stmt })
                }
            }
            Operand::Const(_) => {}
        };

        if let Branch::Ret(op) = func.bb(bb).term {
            check_op(op, None);
        }
        for (i, stmt) in func.bb(bb).statements.iter().enumerate() {
            let check_op = |op| {
                check_op(op, Some(i));
            };

            struct StmtVisitor<F> {
                check_op: F,
            }
            impl<F: FnMut(Operand)> Visitor for StmtVisitor<F> {
                fn visit_operand(&mut self, op: Operand) {
                    (self.check_op)(op);
                }
            }
            StmtVisitor { check_op }.visit_statement(stmt);
        }
    }

    uses
}

pub fn dominates_location(f: &Func<'_>, dom: Location, sub: Location) -> bool {
    // TODO: Can this be made more efficient by caching renumberings of bbs?
    if dom.bb == sub.bb {
        return match (dom.stmt, sub.stmt) {
            (None, Some(_)) => true,
            (Some(d_i), Some(s_i)) if d_i < s_i => true,
            _ => false,
        };
    }

    let mut seen = FxHashSet::default();
    let mut worklist = vec![dom.bb];

    while let Some(bb) = worklist.pop() {
        if !seen.insert(bb) {
            continue;
        }
        worklist.extend(f.bb(bb).term.successors());
    }

    seen.contains(&sub.bb)
}

#[cfg(test)]
mod tests {
    use crate::{
        define_ir_func,
        ir::{BbIdx, Location},
        LoweringCx,
    };

    #[test]
    fn postorder_graph() {
        let arena = bumpalo::Bump::new();
        let lcx = LoweringCx::new(&arena);

        let f = define_ir_func! {
            def(lcx) name (), regs(0) {
                0: {
                    => Goto(BbIdx(1))
                }
                1: {
                    => Switch { cond: op(0), yes: BbIdx(2), no: BbIdx(3) }
                }
                2: {
                    => Ret(op(0))
                }
                3: {
                    => Goto(BbIdx(0))
                }
            }
        };

        let traverse = super::traverse_postorder(&f);

        assert_eq!(traverse, vec![BbIdx(3), BbIdx(2), BbIdx(1), BbIdx(0)]);
    }

    #[test]
    fn single_bb() {
        let arena = bumpalo::Bump::new();
        let lcx = LoweringCx::new(&arena);

        let f = define_ir_func! {
            // %0 = add 0, 2
            // %1 = add %0, 1
            // %2 = add %0, 2
            // ret %0
            def(lcx) name (), regs(3) {
                0: {
                    BinOp { kind: BinKind::Add, lhs: op(0), rhs: op(2), result: Register(0) };
                    BinOp { kind: BinKind::Add, lhs: op(Register(0)), rhs: op(1), result: Register(1) };
                    BinOp { kind: BinKind::Add, lhs: op(Register(1)), rhs: op(2), result: Register(2) };
                    => Ret(op(Register(0)))
                }
            }
        };

        let uses = super::last_register_uses(&f);

        assert_eq!(
            uses,
            vec![
                Some(Location {
                    bb: BbIdx(0),
                    stmt: None
                }),
                Some(Location {
                    bb: BbIdx(0),
                    stmt: Some(2)
                }),
                None,
            ]
        );
    }
}
