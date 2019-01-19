use super::types::{Type, Typed};
use super::statement::Scope;
use std::collections::BTreeMap as SymbolMap;

pub trait Callable {
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

#[derive(Debug, PartialEq)]
pub struct Function {
    name : String,
    arity : u32,
    ret_type : Type,
    fn_impl : Option<Scope>
}

impl Function {
    pub fn new(name : String, arity : u32, ret_type : Type, fn_impl : Scope)
    -> Function {Function{
        name : name, arity : arity, ret_type : ret_type,
        fn_impl : Some(fn_impl)
    }}
}

impl Callable for Function {
    fn get_arity(&self) -> usize {self.arity as usize}
}

impl Typed for Function {
    fn get_type(&self) -> Type {self.ret_type.clone()}
}

#[derive(Debug, PartialEq)]
pub struct Procedure {
    name : String,
    arity : u32,
    pr_impl : Option<Scope>
}

impl Procedure {
    pub fn new(name : String, arity : u32, pr_impl : Scope) -> Procedure {
        Procedure{name : name, arity : arity, pr_impl : Some(pr_impl)}
    }
}

impl Callable for Procedure {
    fn get_arity(&self) -> usize {self.arity as usize}
}

#[derive(PartialEq, Debug)]
pub enum Symbol {
    Variable(Variable),
    Function(Function),
    Procedure(Procedure)
}

#[derive(PartialEq, Debug)]
pub struct SymbolTable {
    symbols : SymbolMap<String, Symbol>
}

impl SymbolTable {
    pub fn new() -> SymbolTable {SymbolTable{symbols : SymbolMap::new()}}
}
