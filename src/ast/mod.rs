pub mod types;
pub mod statement;
pub mod expression;

use self::types::{Type, Typed};

trait Callable {
    fn get_arity(&self) -> usize;
}

pub struct Variable {

}

pub struct Function {
    arity : u32,
    ret_type : Type
}

impl Callable for Function {
    fn get_arity(&self) -> usize {self.arity as usize}
}

impl Typed for Function {
    fn get_type(&self) -> Type {self.ret_type.clone()}
}

pub struct Procedure {

}
