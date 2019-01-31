#[cfg_attr(test, macro_use)]
extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

extern crate inkwell;
extern crate by_address;

pub mod parser;
pub mod ast;
pub mod codegen;

use std::io::{self, Read};
use pest::Parser;

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    println!(
        "Parsing result: {:#?}",
        parser::CSC488Parser::parse(parser::Rule::bare_scope, &buffer)
    );
    Ok(())
}
