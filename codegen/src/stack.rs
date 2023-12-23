use std::collections::HashMap;

use analysis::ir::{BbIdx, Func, Register, StatementKind};

#[derive(Debug)]
pub struct StackLayout {
    pub allocas: HashMap<Register, u64>,
    pub total_size: u64,
}

/// Based on the alloca's in the initial block, this calculcates stack pointer offsets for every allocation.
pub fn allocate_stack_space<'cx>(start_align: u64, func: &Func<'cx>) -> StackLayout {
    assert_eq!(start_align, 8);
    // REMEMBER: The stack grows down (on x86-64).

    let mut temp_layout = HashMap::new();
    let mut offset_backwards = 0;

    for stmt in &func.bb(BbIdx::ZERO).statements {
        if let StatementKind::Alloca {
            result,
            size,
            align,
        } = stmt.kind
        {
            if size != 8 || align != 8 {
                todo!("non 8 integer {size} {align}")
            }
            offset_backwards += 8;
            temp_layout.insert(result, offset_backwards);
        }
    }

    let total_size = offset_backwards;

    StackLayout {
        allocas: temp_layout
            .into_iter()
            .map(|(reg, offset_back)| (reg, total_size - offset_back))
            .collect(),
        total_size,
    }
}
