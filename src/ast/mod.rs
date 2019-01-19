pub mod types;
pub mod statement;
pub mod expression;

use self::types::{Type, Typed};

trait Callable {
    fn get_arity(&self) -> usize;
}

#[derive(Debug, Eq, PartialEq)]
pub struct Variable {
    var_type : Type,
    name : String
}

impl Variable {
    pub fn new(name : String, var_type : Type) -> Variable {
        Variable{name : name, var_type : var_type}
    }
}

impl Typed for Variable {
    fn get_type(&self) -> Type {return self.var_type.clone()}
}

#[derive(Debug, Eq, PartialEq)]
pub struct Function {
    name : String,
    arity : u32,
    ret_type : Type
}

impl Function {
    pub fn new(name : String, arity : u32, ret_type : Type) -> Function {
        Function{name : name, arity : arity, ret_type : ret_type}
    }
}

impl Callable for Function {
    fn get_arity(&self) -> usize {self.arity as usize}
}

impl Typed for Function {
    fn get_type(&self) -> Type {self.ret_type.clone()}
}

#[derive(Debug, Eq, PartialEq)]
pub struct Procedure {
    name : String,
    arity : u32
}

impl Procedure {
    pub fn new(name : String, arity : u32) -> Procedure {
        Procedure{name : name, arity : arity}
    }
}

impl Callable for Procedure {
    fn get_arity(&self) -> usize {self.arity as usize}
}
