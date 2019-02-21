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

use self::codegen::*;
use inkwell::context::Context;

use self::ast::parse_bare_scope;
use self::ast::table::SymbolTable;

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;

    let scope = match parser::CSC488Parser::parse(parser::Rule::bare_scope, &buffer) {
        Ok(mut scope) => match scope.next( ){
            Some(scope) => {
                println!("Parsing result:\n{:?}", scope);
                scope
            },
            None => {
                println!("Error: empty scope parsed!");
                return Ok(());
            }
        }
        Err(err) => {
            println!("Parsing failed with error {:?}", err);
            return Ok(());
        }
    };

    println!("Converting to AST...");

    let scope = {
        let mut sym = SymbolTable::new();
        match parse_bare_scope(scope, &mut sym) {
            Some(scope) => {
                println!("AST:\n{:?}", scope);
                scope
            },
            None => {
                println!("AST construction failed!");
                return Ok(());
            }
        }
    };

    println!("Initializing compiler...");

    let context = Context::create();
    let module = context.create_module("repl");

    let mut compiler = Compiler::new(context, module);

    println!("Compiler initialized!");

    println!("Registering globals...");

    {
        let mut cnt = 0;
        for variable in scope.get_variables().iter().cloned() {
            compiler.register_global(variable); cnt += 1;
        }
        println!("Registered {} globals!", cnt);
    }

    println!("Registering functions...");

    {
        let mut cnt = 0;
        for function in scope.get_functions().iter().cloned() {

        }
    }

    Ok(())
}
