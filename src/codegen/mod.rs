// Inspired by, and bassed off, the Kaleidoscope tutorial in Inkwell found at
// https://github.com/TheDan64/inkwell/blob/master/examples/kaleidoscope/main.rs
// by TheDan64

use crate::ast::expression::{Constant, Expression};
use crate::ast::statement::Statement;
use inkwell::types::BasicTypeEnum;
use inkwell::AddressSpace;
use std::rc::Rc;
use std::collections::HashMap;

use crate::ast::table::{Variable, Function, Callable};
use crate::ast::types::{Type, ScalarType, Typed};
use crate::ast::statement::*;
use by_address::ByAddress;

use inkwell::{
    context::Context,
    builder::Builder,
    module::Module,
    basic_block::BasicBlock};
use inkwell::values::{
    PointerValue,
    FunctionValue,
    GlobalValue,
    BasicValueEnum
};

pub struct Compiler {
    pub context : Context,
    pub module : Module,
    builder : Builder,
    variables : HashMap<ByAddress<Rc<Variable>>, PointerValue>,
    functions : HashMap<ByAddress<Rc<Function>>, FunctionValue>,
    globals : HashMap<ByAddress<Rc<Variable>>, GlobalValue>,
    curr_fn : Option<FunctionValue>
}

impl Compiler {

    pub fn get_llvm_type(&self, typ : Type) -> Option<BasicTypeEnum> {
        match typ {
            Type::Null => None,
            Type::Void => None,
            Type::ArrayType(_) => panic!("Array types not implemented!"),
            Type::ScalarType(s) => match s {
                ScalarType::Integer => Some(self.context.i64_type().into()),
                ScalarType::Boolean => Some(self.context.bool_type().into())
            }
        }
    }

    pub fn get_param_type(&self, typ : Type) -> Result<BasicTypeEnum, &'static str> {
        match typ {
            Type::Null => Err("Null types not implemented"),
            Type::Void => Err("Cannot pass void type"),
            Type::ArrayType(_) => Err("Cannot pass array types"),
            Type::ScalarType(s) => match s {
                ScalarType::Integer => Ok(self.context.i64_type().into()),
                ScalarType::Boolean => Ok(self.context.bool_type().into())
            }
        }
    }

    pub fn new(
        context : Context,
        module : Module
    ) -> Compiler {
        let builder = context.create_builder();
        Compiler {
            context : context,
            module : module,
            builder : builder,
            variables : HashMap::new(),
            functions : HashMap::new(),
            globals : HashMap::new(),
            curr_fn : None
        }
    }

    // Code mostly taken from:
    // https://github.com/TheDan64/inkwell/blob/master/examples/kaleidoscope/main.rs
    fn create_entry_block_alloca(&self, var : &Variable, entry : Option<&BasicBlock>)
    -> PointerValue {
        let builder = self.context.create_builder();
        let owned_entry = match self.curr_fn {
            Some(f) => f.get_entry_basic_block(),
            None => None
        };
        let entry = match entry {
            Some(entry) => entry,
            None => owned_entry.as_ref().unwrap()
        };

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

    pub fn register_global(&mut self, var : Rc<Variable>) {
        let new_global = self.module.add_global(
            self.get_llvm_type(var.get_type()).unwrap(),
            Some(AddressSpace::Generic), var.get_name());
        let pointer_to_global = new_global.as_pointer_value();
        self.globals.insert(ByAddress(var.clone()), new_global);
        self.variables.insert(ByAddress(var), pointer_to_global);
    }

    fn get_param_types(&self, func : &Rc<Function>)
    -> Result<Vec<BasicTypeEnum>, &'static str> {
        func.get_params().iter().map(|t| self.get_param_type(t.get_type())).collect()
    }

    pub fn register_function(&mut self, func : Rc<Function>)
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
        // Register it
        self.functions.insert(ByAddress(func), fn_val);
        Ok(fn_val)
    }

    fn implement_expression(&mut self, expr : &Expression) -> Result<BasicValueEnum, &'static str> {
        match expr {
            Expression::Constant(c) => {
                let typ = self.get_llvm_type(c.get_type()).unwrap();
                Ok(match c {
                    Constant::Integer(i) => {
                        typ.as_int_type().const_int(*i as u64, true)
                    },
                    Constant::Boolean(b) => {
                        typ.as_int_type().const_int(*b as u64, false)
                    }
                }.into())
            },
            Expression::Negation(n) => {
                Err("Negation not yet implemented")
            },
            Expression::Arithmetic(a) => {
                Err("Arithmetic operations not yet implemented")
            },
            Expression::Not(n) => {
                Err("Logical negation not yet implemented")
            },
            Expression::Logical(l) => {
                Err("Logical operations not yet implemented")
            },
            Expression::Comparison(c) => {
                Err("Comparisons not yet implemented")
            },
            Expression::Variable(v) => {
                Err("Variables not yet implemented")
            },
            Expression::ArrayIndex(a) => {
                Err("Array indices not yet implemented")
            },
            Expression::FunctionCall(f) => {
                Err("Function calls not yet implemented")
            }
        }
    }

    fn get_variable(&mut self, variable : Rc<Variable>) -> PointerValue {
        let byaddress = ByAddress(variable);
        match self.variables.get(&byaddress) {
            Some(p) => *p,
            None => self.register_variable(byaddress.0, None)
        }
    }

    fn get_destination(&mut self, destination : &AssignmentDestination) -> Result<PointerValue, &'static str> {
        match destination {
            AssignmentDestination::Variable(v) => Ok(self.get_variable(v.clone())),
            AssignmentDestination::ArrayIndex(_) => Err("Array indices not yet implemented!")
        }
    }

    fn implement_statement(&mut self, statement : &Statement) -> Result<(), &'static str> {
        match statement {
            Statement::Assignment(a) => {
                let value = self.implement_expression(&a.value)?;
                let destination = self.get_destination(&a.destination)?;
                self.builder.build_store(destination, value);
                Ok(())
            },
            Statement::Conditional(c) => {
                Err("Conditionals not yet implemented")
            },
            Statement::While(w) => {
                Err("While loops not yet implemented")
            },
            Statement::Repeat(r) => {
                Err("Repeat loops not yet implemented")
            },
            Statement::Break(b) => {
                Err("Break statements not yet implemented")
            },
            Statement::Return(r) => {
                Err("Return statements not yet implemented")
            },
            Statement::Print(p) => {
                Err("Print statements not yet implemented")
            },
            Statement::Input(i) => {
                Err("Input statements not yet implemented")
            },
            Statement::ProcedureCall(p) => {
                Err("Procedure calls not yet implemented")
            },
            Statement::Scope(s) => {
                Err("Scope not yet implemented")
            }
        }
    }

    fn implement_scope(&mut self, scope : &Scope, func : FunctionValue)
    -> Result<FunctionValue, &'static str> {
        // Register all variables
        for var in scope.get_variables().iter().cloned() {self.register_variable(var, None);}
        for statement in scope.get_statements() {self.implement_statement(statement)?}
        Ok(func)
    }

    pub fn get_function(&mut self, func : Rc<Function>) -> Result<FunctionValue, &'static str> {
        let byaddress = ByAddress(func);
        match self.functions.get(&byaddress) {
            Some(f) => Ok(*f),
            None => self.register_function(byaddress.0)
        }
    }

    pub fn compile_fn(&mut self, func : Rc<Function>) -> Result<FunctionValue, &'static str>
    {
        // Search for the prototype, or compile a new one
        let proto = self.get_function(func.clone()).unwrap();

        // Prepare compiler
        // Set it as the current function
        self.curr_fn = Some(proto);
        // Create an entry block and point the builder to it's end
        let entry = self.context.append_basic_block(&proto, "entry");
        self.builder.position_at_end(&entry);
        // Register argument variables and name associated arguments, then store to variables
        for (var, param) in func.get_params().iter().zip(proto.get_param_iter()) {
            match param {
                BasicValueEnum::FloatValue(f) => f.set_name(var.get_name()),
                BasicValueEnum::IntValue(i) => i.set_name(var.get_name()),
                _ => {return Err("Invalid parameter type!");}
            };
            let alloca = self.register_variable(var.clone(), Some(&entry));
            self.builder.build_store(alloca, param);
        }

        // Compile the body
        let scope = match func.get_scope() {
            Some(scope) => Ok(scope),
            None => Err("Tried to compile function without scope!")
        }?;
        self.implement_scope(scope, proto)
    }
}
