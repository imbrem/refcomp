// Inspired by, and bassed off, the Kaleidoscope tutorial in Inkwell found at
// https://github.com/TheDan64/inkwell/blob/master/examples/kaleidoscope/main.rs
// by TheDan64

use inkwell::{
    context::Context,
    builder::Builder,
    passes::PassManager,
    module::Module};

pub struct Compiler {
    context : Context,
    builder : Builder,
    fpm : PassManager,
    module : Module
}

impl Compiler {
    pub fn from_context(context : Context, filename : &str) -> Compiler {
        // Initialization code copied from abovementioned tutorial
        let module = context.create_module(filename);
        let builder = context.create_builder();
        let fpm = PassManager::create_for_function(&module);

        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();

        fpm.initialize();

        Compiler {
            context : context,
            builder : builder,
            fpm : fpm,
            module : module
        }
    }
}
