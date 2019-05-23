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

use crate::ast::table::Callable;
use std::io::{self, Read};
use pest::Parser;

use self::codegen::*;
use inkwell::context::Context;
//use inkwell::passes::PassManager;
use inkwell::targets::{Target, InitializationConfig};

use self::ast::parse_bare_scope;
use self::ast::table::SymbolTable;

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;

    let scope = match parser::CSC488Parser::parse(parser::Rule::bare_scope, &buffer) {
        Ok(mut scope) => match scope.next( ){
            Some(scope) => {
                println!("Parsing result:\n{:#}\n\n\n", scope);
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
            Ok(scope) => {
                println!("AST:\n{:#?}\n\n\n", scope);
                scope
            },
            Err(s) => {
                println!("AST construction failed with: {}", s);
                return Ok(());
            }
        }
    };

    println!("Initializing compiler...");

    let context = Context::create();
    let module = context.create_module("repl");
    let passes = |fpm : &mut inkwell::passes::PassManager| {
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
    };

    let mut compiler = Compiler::new(context, module, passes);

    println!("Compiler initialized!");

    println!("Initializing target...");
    match Target::initialize_native(&InitializationConfig::default()) {
        Ok(()) => println!("Target initialized!"),
        Err(s) => {
            println!("Error initializing target: {}", s);
            return Ok(());
        }
    }

    println!("Registering globals...");

    {
        let mut cnt = 0;
        for variable in scope.get_variables().iter().cloned() {
            compiler.register_global(variable); cnt += 1;
        }
        println!("Registered {} globals!\n", cnt);
    }

    println!("Registering procedures...");

    {
        let mut cnt = 0;
        for function in scope.get_procedures().iter().cloned() {
            match compiler.register_function(function.clone()) {
                Ok(_) => println!("Registered procedure {}", function.get_name()),
                Err(s) => println!(
                    "Error registering procedure {}: {}\nProcedure Details: {:#?}",
                    function.get_name(), s, function
                )
            }
            cnt += 1;
        }
        println!("Registered {} procedures!\n", cnt);
    }

    println!("Registering functions...");

    {
        let mut cnt = 0;
        for function in scope.get_functions().iter().cloned() {
            match compiler.register_function(function.clone()) {
                Ok(_) => println!("Registered function {}", function.get_name()),
                Err(s) => println!(
                    "Error registering function {}: {}\nFunction Details: {:#?}",
                    function.get_name(), s, function
                )
            }
            cnt += 1;
        }
        println!("Registered {} functions!\n", cnt);
    }

    println!("Compiling procedures...");

    {
        let mut cnt = 0;
        for function in scope.get_procedures().iter().cloned() {
            match compiler.compile_fn(function.clone()) {
                Ok(p) => {
                    println!("Compiled procedure {} to IR:\n----------\n", function.get_name());
                    p.print_to_stderr();
                    println!("\n----------\n");
                },
                Err(s) => println!(
                    "Error registering procedure {}: {}\nProcedure Details: {:#?}",
                    function.get_name(), s, function
                )
            }
            cnt += 1;
        }
        println!("Compiled {} procedures!\n", cnt);
    }

    println!("Compiling functions...");

    {
        let mut cnt = 0;
        for function in scope.get_functions().iter().cloned() {
            match compiler.compile_fn(function.clone()) {
                Ok(f) => {
                    println!("Compiled function {} to IR:\n", function.get_name());
                    f.print_to_stderr();
                },
                Err(s) => println!(
                    "Error registering function {}: {}\nFunction Details: {:#?}",
                    function.get_name(), s, function
                )
            }
            cnt += 1;
        }
        println!("Compiled {} functions!\n", cnt);
    }

    println!("Generating main function...");

    let m = std::rc::Rc::new(scope.into_main("main".to_string()));

    let fm = match compiler.compile_fn(m) {
        Ok(f) => {
            println!("Generated main function! IR\n----------\n");
            f.print_to_stderr();
            println!("\n----------\n");
            f
        },
        Err(e) => {
            println!("Error generating main function: {}\n", e);
            return Ok(());
        }
    };

    // Verifying the module
    println!("Verifying module...");
    match compiler.module.verify() {
        Err(e) => {
            println!("Module error: {}", e);
            return Ok(())
        },
        _ => {
            println!("Module verified! Generated LLVM:\n");
            compiler.module.print_to_stderr();
        }
    }

    println!("Initializing execution engine...");
    let ee = compiler.module
        .create_jit_execution_engine(inkwell::OptimizationLevel::None)
        .unwrap();
    println!("Execution engine initialized!");
    println!("Running main function....\n----------\n");

    unsafe {
        ee.run_function(&fm, &[]);
    }

    println!("\n----------\n");
    Ok(())
}
