// Inspired by, and bassed off, the Kaleidoscope tutorial in Inkwell found at
// https://github.com/TheDan64/inkwell/blob/master/examples/kaleidoscope/main.rs
// by TheDan64

use std::rc::Rc;
use std::collections::HashMap;

use crate::ast::table::{Variable};
use crate::ast::types::{Type, ScalarType, Typed};
use by_address::ByAddress;

use inkwell::{
    context::Context,
    builder::Builder,
    passes::PassManager,
    module::Module,
    basic_block::BasicBlock};
use inkwell::values::{
    PointerValue,
    FunctionValue
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

}
