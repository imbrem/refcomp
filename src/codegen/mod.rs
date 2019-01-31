// Inspired by, and bassed off, the Kaleidoscope tutorial in Inkwell found at
// https://github.com/TheDan64/inkwell/blob/master/examples/kaleidoscope/main.rs
// by TheDan64

use std::rc::Rc;
use std::collections::HashMap;

use crate::ast::table::{Variable};
use by_address::ByAddress;

use inkwell::{
    context::Context,
    builder::Builder,
    passes::PassManager,
    module::Module};
use inkwell::values::{
    PointerValue
};

pub struct Compiler {
    pub context : Context,
    pub builder : Builder,
    pub fpm : PassManager,
    pub module : Module,
    variables : HashMap<ByAddress<Rc<Variable>>, PointerValue>
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
            variables : HashMap::new()
        }
    }
}
