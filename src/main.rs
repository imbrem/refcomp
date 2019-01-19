#[cfg_attr(test, macro_use)]
extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

pub mod parser;
pub mod ast;

pub struct Configuration {
    
}

fn main() {
    println!("Hello world!");
}
