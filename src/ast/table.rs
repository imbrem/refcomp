use std::rc::Rc;
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
    pub fn integer(name : String) -> Variable {
        Self::new(name, Type::integer())
    }
    pub fn boolean(name : String) -> Variable {
        Self::new(name, Type::boolean())
    }
    pub fn get_name(&self) -> &str {&self.name}
}

impl Typed for Variable {
    fn get_type(&self) -> Type {return self.var_type.clone()}
}

#[derive(Debug, PartialEq)]
pub struct Function {
    name : String,
    args : Vec<Rc<Variable>>,
    ret_type : Type,
    fn_impl : Option<Scope>
}

impl Function {
    pub fn new(name : String, args : Vec<Rc<Variable>>, ret_type : Type, fn_impl : Scope)
    -> Function {Function{
        name : name, args : args, ret_type : ret_type,
        fn_impl : Some(fn_impl)
    }}
    pub fn get_name(&self) -> &str {&self.name}
}

impl Callable for Function {
    fn get_arity(&self) -> usize {self.args.len()}
}

impl Typed for Function {
    fn get_type(&self) -> Type {self.ret_type.clone()}
}

#[derive(Debug, PartialEq)]
pub struct Procedure {
    name : String,
    args : Vec<Rc<Variable>>,
    pr_impl : Option<Scope>
}

impl Procedure {
    pub fn new(name : String, args : Vec<Rc<Variable>>, pr_impl : Scope) -> Procedure {
        Procedure{name : name, args : args, pr_impl : Some(pr_impl)}
    }
    pub fn get_name(&self) -> &str {&self.name}
}

impl Callable for Procedure {
    fn get_arity(&self) -> usize {self.args.len()}
}

#[derive(Clone, PartialEq, Debug)]
pub enum Symbol {
    Variable(Rc<Variable>),
    Function(Rc<Function>),
    Procedure(Rc<Procedure>)
}

impl Symbol {
    pub fn get_name(&self) -> &str {
        match self {
            Symbol::Variable(v) => v.get_name(),
            Symbol::Function(f) => f.get_name(),
            Symbol::Procedure(p) => p.get_name()
        }
    }
    pub fn variable(name : String, var_type : Type) -> Symbol {
        Symbol::Variable(Rc::new(Variable::new(name, var_type)))
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct SymbolTable {
    symbols : SymbolMap<String, Symbol>
}

impl SymbolTable {
    pub fn new() -> SymbolTable {SymbolTable{symbols : SymbolMap::new()}}
    pub fn with_init(symbols : SymbolMap<String, Symbol>) -> SymbolTable {
        SymbolTable{symbols : symbols}
    }
    pub fn dereference(&self, symbol : &str) -> Option<Symbol> {
        match self.symbols.get(symbol) {
            Some(r) => Some(r.clone()),
            None => None
        }
    }
    pub fn define(&mut self, symbol : Symbol) -> Option<Symbol> {
        self.reference(symbol.get_name().to_string(), symbol)
    }
    pub fn reference(&mut self, name : String, symbol : Symbol) -> Option<Symbol> {
        self.symbols.insert(name, symbol)
    }
}
