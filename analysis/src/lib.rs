#![allow(dead_code)] // TODO: no
#![warn(rust_2018_idioms)]

struct Ctx<'src, P> {
    parser: P,
}