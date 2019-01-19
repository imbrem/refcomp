pub mod types;
pub mod statement;
pub mod expression;

use self::types::{Type, Typed};

trait Callable {
    fn get_arity(&self) -> usize;
}

pub struct Variable {
    var_type : Type
}

impl Variable {
    pub fn new(var_type : Type) -> Variable {
        Variable{var_type : var_type}
    }
}

impl Typed for Variable {
    fn get_type(&self) -> Type {return self.var_type.clone()}
}

pub struct Function {
    arity : u32,
    ret_type : Type
}

impl Function {
    pub fn new(arity : u32, ret_type : Type) -> Function {
        Function{arity : arity, ret_type : ret_type}
    }
}

impl Callable for Function {
    fn get_arity(&self) -> usize {self.arity as usize}
}

impl Typed for Function {
    fn get_type(&self) -> Type {self.ret_type.clone()}
}

pub struct Procedure {

}
