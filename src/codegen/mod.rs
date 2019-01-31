// Inspired by, and bassed off, the Kaleidoscope tutorial in Inkwell found at
// https://github.com/TheDan64/inkwell/blob/master/examples/kaleidoscope/main.rs
// by TheDan64

use inkwell::types::BasicTypeEnum;
use std::rc::Rc;
use std::collections::HashMap;

use crate::ast::table::{Variable, Function, Callable};
use crate::ast::types::{Type, ScalarType, Typed};
use crate::ast::statement::{Scope};
use by_address::ByAddress;

use inkwell::{
    context::Context,
    builder::Builder,
    passes::PassManager,
    module::Module,
    basic_block::BasicBlock};
use inkwell::values::{
    PointerValue,
    FunctionValue,
    BasicValueEnum
};

pub struct Compiler {
    pub context : Context,
    pub builder : Builder,
    pub fpm : PassManager,
    pub module : Module,
    variables : HashMap<ByAddress<Rc<Variable>>, PointerValue>,
    curr_fn : Option<FunctionValue>
}

impl Compiler {

    pub fn new(
        context : Context,
        builder : Builder,
        fpm : PassManager,
        module : Module
    ) -> Compiler {
        Compiler {
            context : context,
            builder: builder,
            fpm : fpm,
            module : module,
            variables : HashMap::new(),
            curr_fn : None
        }
    }

    #[inline]
    fn get_fn(&self, name : &str) -> Option<FunctionValue> {
        self.module.get_function(name)
    }

    // Code mostly taken from:
    // https://github.com/TheDan64/inkwell/blob/master/examples/kaleidoscope/main.rs
    fn create_entry_block_alloca(&self, var : &Variable, entry : Option<&BasicBlock>)
    -> PointerValue {
        let builder = self.context.create_builder();
        let owned_entry = self.curr_fn.unwrap().get_entry_basic_block();
        let entry = owned_entry.as_ref().or(entry).unwrap();

        match entry.get_first_instruction() {
            Some(instruction) => builder.position_before(&instruction),
            None => builder.position_at_end(entry)
        };

        match var.get_type() {
            Type::ScalarType(s) => match s {
                ScalarType::Integer =>
                    builder.build_alloca(self.context.i64_type(), var.get_name()),
                ScalarType::Boolean =>
                    builder.build_alloca(self.context.bool_type(), var.get_name())
            },
            Type::ArrayType(_) => panic!("Array types not yet implemented!"),
            Type::Null => panic!("Cannot allocate null variable"),
            Type::Void => panic!("Cannot allocate void variable")
        }
    }

    fn register_variable(&mut self, var : Rc<Variable>, entry : Option<&BasicBlock>)
    -> PointerValue {
        let pointer_val = self.create_entry_block_alloca(var.as_ref(), entry);
        self.variables.insert(ByAddress(var), pointer_val);
        pointer_val
    }

    fn get_param_types<T: Callable>(&self, func : &Rc<T>)
    -> Result<Vec<BasicTypeEnum>, &'static str> {
        func.get_params().iter().map(|t| match t.get_type() {
            Type::Null => Err("Null types not implemented"),
            Type::Void => Err("Void type in parameter list"),
            Type::ArrayType(_) => Err("Cannot pass array type"),
            Type::ScalarType(s) => match s {
                ScalarType::Integer => Ok(self.context.i64_type().into()),
                ScalarType::Boolean => Ok(self.context.bool_type().into())
            }
        }).collect()
    }

    fn compile_prototype<T: Callable>(&mut self, func : Rc<T>)
    -> Result<FunctionValue, &'static str> {
        // Get function type
        let atypes = self.get_param_types(&func)?;
        let atypes = atypes.as_slice();
        let function_type = match func.get_return() {
            Type::Void => self.context.void_type().fn_type(atypes, false),
            Type::ScalarType(s) => match s {
                ScalarType::Integer => self.context.i64_type().fn_type(atypes, false),
                ScalarType::Boolean => self.context.bool_type().fn_type(atypes, false),
            },
            Type::ArrayType(_) => {return Err("Cannot return array from function!");},
            Type::Null => {return Err("Null functions not yet implemented!");}
        };
        // Create the function
        let fn_val = self.module.add_function(func.get_name(), function_type, None);
        // Set it as the current function
        self.curr_fn = Some(fn_val);
        // Create an entry block and point the builder to it's end
        let entry = self.context.append_basic_block(&fn_val, "entry");
        self.builder.position_at_end(&entry);
        // Register argument variables and name associated arguments, then store to variables
        for (var, param) in func.get_params().iter().zip(fn_val.get_param_iter()) {
            match param {
                BasicValueEnum::FloatValue(f) => f.set_name(var.get_name()),
                BasicValueEnum::IntValue(i) => i.set_name(var.get_name()),
                _ => {return Err("Invalid parameter type!");}
            };
            let alloca = self.register_variable(var.clone(), Some(&entry));
            self.builder.build_store(alloca, param);
        }
        Ok(fn_val)
    }

    fn implement_scope(&mut self, scope : &Scope, func : FunctionValue)
    -> Result<FunctionValue, &'static str> {
        Err("Not yet implemented")
    }

    pub fn compile_fn<T: Callable>(&mut self, func : Rc<T>) -> Result<FunctionValue, &'static str>
    {
        // Compile the prototype
        let proto = self.compile_prototype(func.clone())?;
        // Compile the body
        let scope = match func.get_scope() {
            Some(scope) => Ok(scope),
            None => Err("Tried to compile function without scope!")
        }?;
        self.implement_scope(scope, proto)
    }
}
