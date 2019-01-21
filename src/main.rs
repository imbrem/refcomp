#[cfg_attr(test, macro_use)]
extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

extern crate inkwell;

pub mod parser;
pub mod ast;
pub mod codegen;

fn main() {
    println!("Hello world!");
}
