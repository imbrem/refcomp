// Inspired by, and based off, the Kaleidoscope tutorial in Inkwell found at
// https://github.com/TheDan64/inkwell/blob/master/examples/kaleidoscope/main.rs
// by TheDan64

use crate::ast::expression::{
    Constant, Expression, UnaryExpression, ArithmeticOp, ComparisonOp, ArrayIndex
};
use crate::ast::statement::Statement;
use inkwell::IntPredicate;
use inkwell::types::BasicTypeEnum;
use inkwell::passes::PassManager;
use inkwell::AddressSpace;
use std::rc::Rc;
use std::collections::HashMap;

use crate::ast::table::{Variable, Function, Callable};
use crate::ast::types::{Type, ScalarType, Typed};
use crate::ast::statement::*;
use by_address::ByAddress;

use std::ops::Deref;

use inkwell::{
    context::Context,
    builder::Builder,
    module::Module,
    basic_block::BasicBlock
};
use inkwell::values::{
    PointerValue,
    FunctionValue,
    GlobalValue,
    IntValue,
    BasicValueEnum,
    CallSiteValue
};

pub struct Compiler {
    pub context : Context,
    pub module : Module,
    fpm : PassManager,
    builder : Builder,
    variables : HashMap<ByAddress<Rc<Variable>>, PointerValue>,
    functions : HashMap<ByAddress<Rc<Function>>, FunctionValue>,
    used : Vec<Rc<Function>>,
    globals : HashMap<ByAddress<Rc<Variable>>, GlobalValue>,
    curr_fn : Option<FunctionValue>,
    printf_val : FunctionValue,
    scanf_val : FunctionValue,
    breaks : Vec<BasicBlock>,
    strings : u64
}

impl Compiler {

    pub fn get_llvm_type(&self, typ : Type) -> Option<BasicTypeEnum> {
        match typ {
            Type::Null => None,
            Type::Void => None,
            Type::ArrayType(a) => {
                let ty = self.get_llvm_type(Type::ScalarType(a.get_element_type()))?;
                let mut dims = a.get_dims().iter();
                let mut aty = ty.as_int_type().array_type(*dims.next().unwrap());
                for dim in dims {
                    aty = aty.array_type(*dim);
                }
                Some(aty.into())
            },
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

    pub fn new<F : FnOnce(&mut PassManager)>(
        context : Context,
        module : Module,
        passes : F
    ) -> Compiler {
        let builder = context.create_builder();
        let printf = Self::gen_printf(&context, &module);
        let scanf = Self::gen_scanf(&context, &module);
        let mut fpm = inkwell::passes::PassManager::create_for_function(&module);
        passes(&mut fpm);
        fpm.initialize();
        Compiler {
            context : context,
            module : module,
            builder : builder,
            fpm : fpm,
            variables : HashMap::new(),
            functions : HashMap::new(),
            used : Vec::new(),
            globals : HashMap::new(),
            curr_fn : None,
            printf_val : printf,
            scanf_val : scanf,
            breaks : Vec::new(),
            strings : 0
        }
    }

    fn get_curr(&self) -> FunctionValue {
        self.curr_fn.unwrap()
    }

    fn gen_printf(context : &Context, module : &Module) -> FunctionValue {
        let printf_ty = context.i32_type().fn_type(
            &[context.i8_type().ptr_type(AddressSpace::Generic).into()],
            true
        );
        module.add_function("printf", printf_ty, None)
    }

    fn gen_scanf(context : &Context, module : &Module) -> FunctionValue {
        let scanf_ty = context.i32_type().fn_type(
            &[context.i8_type().ptr_type(AddressSpace::Generic).into()],
            true
        );
        module.add_function("scanf", scanf_ty, None)
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
        let byaddress = ByAddress(var);
        match self.globals.get(&byaddress) {
            Some(g) => {
                return g.as_pointer_value();
            }
            None => {}
        }
        let pointer_val = self.create_entry_block_alloca(byaddress.as_ref(), entry);
        self.variables.insert(byaddress, pointer_val);
        pointer_val
    }

    fn zero_initializer(ty : BasicTypeEnum) -> BasicValueEnum {
        match ty {
            BasicTypeEnum::FloatType(f) => f.const_zero().into(),
            BasicTypeEnum::IntType(i) => i.const_zero().into(),
            BasicTypeEnum::ArrayType(a) => a.const_zero().into(),
            t => panic!("Type {:?} not supported!", t)
        }
    }

    pub fn register_global(&mut self, var : Rc<Variable>) {
        let ty = self.get_llvm_type(var.get_type()).unwrap();
        let new_global = self.module.add_global(ty, None, var.get_name());
        new_global.set_initializer(&Self::zero_initializer(ty));
        let apv = new_global.as_pointer_value();
        self.globals.insert(ByAddress(var.clone()), new_global);
        self.variables.insert(ByAddress(var.clone()), apv);
    }

    fn get_implicit_arg_type(&self, typ : Type) -> Result<BasicTypeEnum, &'static str> {
        let base_type = match self.get_llvm_type(typ) {
            Some(typ) => typ,
            None => {return Err("Error!");}
        };
        match base_type {
            BasicTypeEnum::IntType(i) => Ok(i.ptr_type(AddressSpace::Generic).into()),
            _ => Ok(base_type)
        }
    }

    fn get_param_types(&self, func : &Rc<Function>)
    -> Result<Vec<BasicTypeEnum>, &'static str> {
        let res : Result<Vec<BasicTypeEnum>, &'static str> = func.get_params().iter()
            .map(|t| self.get_param_type(t.get_type()))
            .collect();
        let mut res = res?;
        for arg in func.get_implicit_args().iter().cloned().map(|v| ByAddress(v)) {
            if self.globals.get(&arg) == None {
                res.push(self.get_implicit_arg_type(arg.get_type())?)
            }
        }
        Ok(res)
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
        self.used.push(func.clone());
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
                match n.get_type() {
                    Type::ScalarType(ScalarType::Integer) => {
                        let arg_expr = self.implement_expression(n.get())?;
                        let int_expr = arg_expr.into_int_value();
                        Ok(self.builder.build_int_neg(int_expr, "negtmp").into())
                    },
                    _ => Err("Error constructing negation: invalid type")
                }
            },
            Expression::Arithmetic(a) => {
                let lhs_expr = self.implement_expression(&a.lhs)?;
                let rhs_expr = self.implement_expression(&a.rhs)?;
                match (a.lhs.get_type(), a.rhs.get_type()) {
                    (Type::ScalarType(ScalarType::Integer), Type::ScalarType(ScalarType::Integer))
                    => {
                        let ilhs = lhs_expr.into_int_value();
                        let irhs = rhs_expr.into_int_value();
                        Ok(match a.op {
                            ArithmeticOp::Add => {
                                self.builder.build_int_add(ilhs, irhs, "iaddtmp")
                            },
                            ArithmeticOp::Mul => {
                                self.builder.build_int_mul(ilhs, irhs, "imultmp")
                            },
                            ArithmeticOp::Sub => {
                                self.builder.build_int_sub(ilhs, irhs, "isubtmp")
                            },
                            ArithmeticOp::Div => {
                                self.builder.build_int_signed_div(ilhs, irhs, "idivtmp")
                            }
                        }.into())
                    },
                    _ => Err("Error constructing arithmetic operation: invalid types")
                }
            },
            Expression::Not(n) => {
                match n.get_type() {
                    Type::ScalarType(ScalarType::Boolean) => {
                        let arg_expr = self.implement_expression(n.get())?;
                        let int_expr = arg_expr.into_int_value();
                        Ok(self.builder.build_int_compare(
                            IntPredicate::EQ,
                            int_expr,
                            self.context.bool_type().const_int(0, false),
                            "nottmp"
                        ).into())
                    },
                    _ => Err("Logical not is not defined for non-boolean types")
                }
            },
            Expression::Logical(_l) => {
                Err("Logical operations not yet implemented")
            },
            Expression::Comparison(c) => {
                let lt = c.lhs.get_type();
                let rt = c.rhs.get_type();
                let le = self.implement_expression(&c.lhs)?;
                let re = self.implement_expression(&c.rhs)?;
                match (lt, rt) {
                    (Type::ScalarType(ScalarType::Boolean), Type::ScalarType(ScalarType::Boolean))
                    => {
                        match c.op {
                            ComparisonOp::EQ => {
                                Ok(self.builder.build_int_compare(
                                    IntPredicate::EQ, le.into_int_value(), re.into_int_value(),
                                    "bcmptmp"
                                ).into())
                            },
                            ComparisonOp::NE => {
                                Ok(self.builder.build_int_compare(
                                    IntPredicate::NE, le.into_int_value(), re.into_int_value(),
                                    "bcmptmp"
                                ).into())
                            },
                            _ => {
                                Err("Invalid comparison operation between booleans!")
                            }
                        }
                    },
                    (Type::ScalarType(ScalarType::Integer), Type::ScalarType(ScalarType::Integer))
                    => {
                        match c.op {
                            ComparisonOp::EQ => {
                                Ok(self.builder.build_int_compare(
                                    IntPredicate::EQ, le.into_int_value(), re.into_int_value(),
                                    "icmptmp"
                                ).into())
                            },
                            ComparisonOp::NE => {
                                Ok(self.builder.build_int_compare(
                                    IntPredicate::NE, le.into_int_value(), re.into_int_value(),
                                    "icmptmp"
                                ).into())
                            },
                            ComparisonOp::LT => {
                                Ok(self.builder.build_int_compare(
                                    IntPredicate::SLT, le.into_int_value(), re.into_int_value(),
                                    "icmptmp"
                                ).into())
                            },
                            ComparisonOp::LE => {
                                Ok(self.builder.build_int_compare(
                                    IntPredicate::SLE, le.into_int_value(), re.into_int_value(),
                                    "icmptmp"
                                ).into())
                            },
                            ComparisonOp::GT => {
                                Ok(self.builder.build_int_compare(
                                    IntPredicate::SGT, le.into_int_value(), re.into_int_value(),
                                    "icmptmp"
                                ).into())
                            },
                            ComparisonOp::GE => {
                                Ok(self.builder.build_int_compare(
                                    IntPredicate::SGE, le.into_int_value(), re.into_int_value(),
                                    "icmptmp"
                                ).into())
                            }
                        }
                    },
                    (x, y) => {
                        if x == y {
                            Err("Cannot compare incomparable types!")
                        } else {
                            Err("Cannot compare unequal types!")
                        }
                    }
                }
            },
            Expression::Variable(v) => {
                let ptr = self.get_variable(v.clone());
                Ok(self.builder.build_load(ptr, "loadtmp"))
            },
            Expression::ArrayIndex(a) => {
                let ptr = self.get_index(&a)?;
                Ok(self.builder.build_load(ptr, "arr_loadtmp"))
            },
            Expression::FunctionCall(f) => {
                Ok(
                    self.implement_function_call(f.get_function(), f.get_args(), "function_call")?
                    .try_as_basic_value()
                    .left().unwrap()
                )
            }
        }
    }

    fn implement_function_call(&mut self, func : Rc<Function>, args : &[Expression], name : &str)
    -> Result<CallSiteValue, &'static str> {
        let called = self.get_function(func)?;
        let args : Result<Vec<BasicValueEnum>, &'static str> =  args.iter()
            .map(|arg| self.implement_expression(arg)).collect();
        Ok(self.builder.build_call(called, &(args?), name))
    }

    fn get_variable(&mut self, variable : Rc<Variable>) -> PointerValue {
        let byaddress = ByAddress(variable);

        match self.variables.get(&byaddress) {
            Some(p) => *p,
            None => self.register_variable(byaddress.0, None)
        }
    }

    fn get_index(&mut self, index : &ArrayIndex) -> Result<PointerValue, &'static str> {
        let var = self.get_variable(index.get_variable());
        let maybe_indices : Result<Vec<IntValue>, &'static str> = index.get_indices().iter()
            .map(|e| self.implement_expression(e).map(|r| r.into_int_value()))
            .collect();
        let mut indices = vec![self.context.i64_type().const_zero()];
        indices.extend(maybe_indices?);
        unsafe {
            Ok(self.builder.build_in_bounds_gep(var, &indices, "gep_arr"))
        }
    }

    fn get_destination(&mut self, destination : &AssignmentDestination)
    -> Result<PointerValue, &'static str> {
        match destination {
            AssignmentDestination::Variable(v) => Ok(self.get_variable(v.clone())),
            AssignmentDestination::ArrayIndex(a) => self.get_index(a)
        }
    }

    fn global_string(&mut self, string : Vec<u8>) -> GlobalValue {
        let ct = self.context.i8_type();
        let chars : Vec<IntValue> =
            string.into_iter().map(|char| {ct.const_int(char.into(), false)}).collect();
        let string = ct.const_array(&chars);
        let mut name = "$string_".to_owned();
        name.push_str(&self.strings.to_string());
        self.strings += 1;
        let global = self.module.add_global(string.get_type(), None, &name);
        global.set_initializer(&string);
        global.set_constant(true);
        global.set_visibility(inkwell::GlobalVisibility::Hidden);
        global
    }

    fn gep_first(&mut self, ptr : PointerValue) -> PointerValue {
        let sz = self.context.i64_type();
        unsafe {
            self.builder.build_in_bounds_gep(ptr, &[sz.const_zero(), sz.const_zero()], "gep_first")
        }
    }

    fn implement_statement(&mut self, statement : &Statement) -> Result<bool, &'static str> {
        match statement {
            Statement::Assignment(a) => {
                let value = self.implement_expression(&a.value)?;
                let destination = self.get_destination(&a.destination)?;
                self.builder.build_store(destination, value);
                Ok(false)
            },
            Statement::Conditional(c) => {
                let parent = self.get_curr();

                for (i, branch) in c.conditional_branches.iter().enumerate() {
                    let last = i + 1 >= c.conditional_branches.len();
                    let condition = self.implement_expression(&branch.condition)?;
                    let then_bb = self.context.append_basic_block(&parent, "then");
                    let else_bb = self.context.append_basic_block(&parent, "else");
                    let cont_bb = self.context.append_basic_block(&parent, "cont");
                    self.builder.build_conditional_branch(
                        *condition.as_int_value(),
                        &then_bb, &else_bb);
                    self.builder.position_at_end(&then_bb);
                    let jumped = self.implement_scope(&branch.scope)?;
                    if !jumped {self.builder.build_unconditional_branch(&cont_bb);}
                    self.builder.position_at_end(&else_bb);
                    if last {
                        if let Some(scope) = &c.else_branch {
                            self.implement_scope(scope)?;
                        }
                        self.builder.build_unconditional_branch(&cont_bb);
                    }
                    self.builder.position_at_end(&cont_bb);
                }
                Ok(false)
            },
            Statement::While(w) => {
                let parent = self.get_curr();
                let while_bb = self.context.append_basic_block(&parent, "while");
                self.builder.build_unconditional_branch(&while_bb);
                self.builder.position_at_end(&while_bb);
                let condition = self.implement_expression(&w.condition)?;
                let body_bb = self.context.append_basic_block(&parent, "body");
                // TODO: "loop stack" for break statements
                let break_bb = self.context.append_basic_block(&parent, "break");
                self.breaks.push(break_bb);
                self.builder.build_conditional_branch(
                        *condition.as_int_value(),
                        &body_bb, self.breaks.last().unwrap()
                );
                self.builder.position_at_end(&body_bb);
                let jumped = self.implement_scope(&w.scope)?;
                if !jumped {self.builder.build_unconditional_branch(&while_bb);}
                self.builder.position_at_end(&self.breaks.pop().unwrap());
                Ok(false)
            },
            Statement::Repeat(r) => {
                let parent = self.get_curr();
                let repeat_bb = self.context.append_basic_block(&parent, "repeat");
                let cont_bb = self.context.append_basic_block(&parent, "cont");
                self.breaks.push(cont_bb);
                self.builder.build_unconditional_branch(&repeat_bb);
                self.builder.position_at_end(&repeat_bb);
                let jumped = self.implement_scope(&r.scope)?;
                if !jumped {
                    let condition = self.implement_expression(&r.condition)?;
                    self.builder.build_conditional_branch(
                        *condition.as_int_value(), self.breaks.last().unwrap(), &repeat_bb
                    );
                }
                self.builder.position_at_end(&self.breaks.pop().unwrap());
                Ok(false)
            },
            Statement::Break(b) => {
                match b {
                    0 => Ok(false),
                    n => {
                        match self.breaks.get(self.breaks.len() - *n as usize) {
                            Some(br) => {
                                self.builder.build_unconditional_branch(br);
                                Ok(true)
                            },
                            None => Err("Break too deep!")
                        }
                    }
                }
            },
            Statement::Return(r) => {
                match r {
                    Some(e) => {
                        let rexpr = self.implement_expression(e)?;
                        self.builder.build_return(Some(&rexpr))
                    },
                    None => self.builder.build_return(None)
                };
                Ok(true)
            },
            Statement::Print(p) => {
                let mut fmt = Vec::new();
                for output in p {
                    match output {
                        //TODO: generalize
                        OutputElement::Expression(_) => fmt.extend_from_slice("%d".as_bytes()),
                        OutputElement::Text(t) =>  fmt.extend_from_slice(t.as_bytes()),
                        OutputElement::Newline => fmt.extend_from_slice("\n".as_bytes())
                    }
                }
                fmt.push(0); // Null terminator
                let global_str = self.global_string(fmt);
                let global_ptr = self.gep_first(global_str.as_pointer_value());
                let mut args = vec![global_ptr.into()];
                for output in p {
                    match output {
                        OutputElement::Expression(e) => args.push(self.implement_expression(e)?),
                        OutputElement::Text(_) | OutputElement::Newline => {}
                    }
                }
                self.builder.build_call(
                    self.printf_val,
                    &args,
                    "print_statement"
                );
                Ok(false)
            },
            Statement::Input(i) => {
                // Formatter for inputs
                let base = " %d".as_bytes();
                // Format string
                let mut fmt : Vec<u8> = base.iter().cloned().cycle().take(base.len() * i.len()).collect();
                fmt.push(0);
                let global_str = self.global_string(fmt);
                let global_ptr = self.gep_first(global_str.as_pointer_value());
                let mut args = vec![global_ptr.into()];
                for var in i.iter().cloned() {
                    args.push(self.get_variable(var).into())
                }
                self.builder.build_call(
                    self.scanf_val,
                    &args,
                    "input_statement"
                );
                Ok(false)
            },
            Statement::ProcedureCall(p) => {
                self.implement_function_call(p.get_proc(), p.get_args(), "proc_call")?;
                Ok(false)
            },
            Statement::Scope(s) => {
                self.implement_scope(s)
            }
        }
    }

    fn implement_scope(&mut self, scope : &Scope)
    -> Result<bool, &'static str> {
        // Register all variables
        for var in scope.get_variables().iter().cloned() {self.register_variable(var, None);}
        // TODO: deal with nested functions and procedures
        let mut flow = false;
        for statement in scope.get_statements() {
            flow = self.implement_statement(statement)?;
            if flow {break;}
        }
        Ok(flow)
    }

    pub fn get_function(&mut self, func : Rc<Function>) -> Result<FunctionValue, &'static str> {
        let byaddress = ByAddress(func);
        match self.functions.get(&byaddress) {
            Some(f) => Ok(*f),
            None => self.register_function(byaddress.0)
        }
    }

    fn clear_variables(&mut self) {
        self.variables.clear();
    }

    pub fn compile_fn(&mut self, func : Rc<Function>) -> Result<FunctionValue, &'static str>
    {
        // Get the scope
        let sref = func.get_scope();
        let scope = match sref.deref() {
            Some(scope) => Ok(scope),
            None => Err("Tried to compile function without scope!")
        }?;

        // Search for the prototype, or compile a new one
        let proto = {
            let byaddress = ByAddress(func.clone());
            match self.functions.get(&byaddress) {
                Some(f) => *f,
                None => {
                    let res = self.register_function(byaddress.0);
                    self.used.pop();
                    res?
                }
            }
        };

        // Prepare compiler
        // Set it as the current function
        self.curr_fn = Some(proto);
        // Create an entry block and point the builder to it's end
        let entry = self.context.append_basic_block(&proto, "entry");
        self.builder.position_at_end(&entry);
        // Register argument variables and name associated arguments, then store to variables
        let mut param_iter = proto.get_param_iter();
        for var in func.get_params().iter() {
            let param = param_iter.next().unwrap();
            match param {
                BasicValueEnum::FloatValue(f) => f.set_name(var.get_name()),
                BasicValueEnum::IntValue(i) => i.set_name(var.get_name()),
                _ => {return Err("Invalid parameter type!");}
            };
            let alloca = self.register_variable(var.clone(), Some(&entry));
            self.builder.build_store(alloca, param);
        }
        // Now, for each implicit argument, if it's a global, put the global in the symbol table,
        // otherwise, make a store
        for var in func.get_implicit_args().iter().cloned() {
            let ba = ByAddress(var);
            if let Some(global) = self.globals.get(&ba) {
                self.variables.insert(ba, global.as_pointer_value());
            } else {
                let param = param_iter.next().unwrap();
                let pv = match param {
                    BasicValueEnum::PointerValue(p) => {
                        p.set_name(ba.get_name()); p
                    },
                    _ => {return Err("Invalid implicit parameter type!")}
                };
                self.variables.insert(ba, pv);
            }
        }

        // Compile the body
        let flow = self.implement_scope(scope)?;
        if !flow { // No return statement
            match func.get_type() {
                Type::Null | Type::Void => { // Void or unit type, so append one
                    self.builder.build_return(None);
                },
                _ => {
                    return Err("Error building function: function without return statement");
                }
            }
        }
        // Clean up by de-registering all variables
        self.clear_variables();

        // Verify the function
        if !proto.verify(false) {
            return Err("Error building function: generated invalid LLVM. Check for return statements!");
        }
        // Run the FPM on the function
        self.fpm.run_on_function(&proto);

        // Now, compile all newly registered functions
        while !self.used.is_empty() {
            let func = self.used.pop().unwrap();
            self.compile_fn(func)?;
        }

        Ok(proto)
    }
}
