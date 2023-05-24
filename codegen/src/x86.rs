use analysis::{ir::Func, LoweringCx};
use iced_x86::{code_asm as x, IcedError};
use parser::Span;

use crate::Result;

trait IcedErrExt {
    type T;
    fn sp(self, span: Span) -> Result<Self::T, analysis::Error>;
}

impl<T> IcedErrExt for Result<T, IcedError> {
    type T = T;

    fn sp(self, span: Span) -> Result<Self::T, analysis::Error> {
        self.map_err(|e| analysis::Error::new(e.to_string(), span))
    }
}

pub fn generate_func<'cx>(_lcx: &LoweringCx<'cx>, func: &Func<'cx>) -> Result<Vec<u8>> {
    assert_eq!(func.arity, 0, "arguments??? in MY uwucc????");

    let sp = func.def_span;
    let mut a = x::CodeAssembler::new(64).sp(sp)?;

    a.xor(x::rax, x::rax).sp(sp)?;
    a.ret().sp(sp)?;

    let code = a.assemble(0x4000).sp(sp)?;

    Ok(code)
}
