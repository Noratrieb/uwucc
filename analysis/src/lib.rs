#![allow(dead_code)] // TODO: no
#![warn(rust_2018_idioms)]

mod ctxt;
pub mod ir;
mod lower;
pub mod ty;

pub use ctxt::LoweringCx;
pub use lower::lower_translation_unit;
