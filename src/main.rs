#[cfg_attr(test, macro_use)]
extern crate pest;
#[macro_use]
extern crate pest_derive;

#[derive(Parser)]
#[grammar="grammar488.pest"]
pub struct CSC488Parser;

fn main() {
    println!("Hello world!");
}
