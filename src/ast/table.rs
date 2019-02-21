use by_address::ByAddress;
use std::rc::Rc;
use super::types::{Type, Typed};
use super::statement::Scope;
use std::collections::HashSet;
use std::iter::FromIterator;
use std::collections::BTreeMap as SymbolMap;

pub trait Callable {
    fn get_name(&self) -> &str;
    fn get_arity(&self) -> usize;
    fn get_params(&self) -> &Vec<Rc<Variable>>;
    fn get_scope(&self) -> Option<&Scope>;
    fn get_return(&self) -> Type;
}

pub trait Scoped {
    fn enter_scope(&self, sym : &mut SymbolTable);
    fn leave_scope(&self, sym : &mut SymbolTable);
}

pub trait DependencyVisitor {
    fn visit_dependencies<T: FnMut(Rc<Variable>)>(&self, visitor : T) -> T;
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Variable {
    var_type : Type,
    name : String
}

impl DependencyVisitor for Rc<Variable> {
    fn visit_dependencies<T: FnMut(Rc<Variable>)>(&self, mut visitor: T) -> T {
        visitor(self.clone());
        visitor
    }
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

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    name : String,
    args : Vec<Rc<Variable>>,
    implicit_args : Option<Vec<Rc<Variable>>>,
    ret_type : Type,
    fn_impl : Option<Scope>
}

impl Function {
    pub fn new(name : String, args : Vec<Rc<Variable>>, ret_type : Type)
    -> Function {Function{
        name : name, args : args, ret_type : ret_type, implicit_args : None, fn_impl : None
    }}
    pub fn procedure(name : String, args : Vec<Rc<Variable>>) -> Function {
        Function::new(name, args, Type::Void)
    }
    pub fn with_impl(name : String, args : Vec<Rc<Variable>>, ret_type : Type, fn_impl : Scope)
    -> Function {
        let mut result = Function::new(name, args, ret_type);
        result.implement(fn_impl);
        return result
    }
    pub fn get_implicit_args(&self) -> Option<&Vec<Rc<Variable>>> {
        match &self.implicit_args {
            Some(args) => Some(args),
            None => None
        }
    }
    pub fn implement(&mut self, scope : Scope) {
        let mut argument_set : HashSet<ByAddress<Rc<Variable>>>
            = HashSet::from_iter(self.args.iter().cloned().map(|i| ByAddress(i)));
        let mut iargs = vec![];
        argument_set.extend(scope.get_variables().iter().cloned().map(|i| ByAddress(i)));
        scope.visit_dependencies(|v : Rc<Variable>| {
            let b = ByAddress(v);
            if argument_set.contains(&b) {
                iargs.push(b.0)
            }
        });
        self.implicit_args = Some(iargs);
        self.fn_impl = Some(scope);
    }
}

impl DependencyVisitor for Function {
    fn visit_dependencies<T: FnMut(Rc<Variable>)>(&self, mut visitor : T) -> T {
        if let Some(args) = &self.implicit_args {
            for arg in args {visitor = arg.visit_dependencies(visitor)}
        }
        visitor
    }
}

impl Callable for Function {
    fn get_arity(&self) -> usize {self.args.len()}
    fn get_params(&self) -> &Vec<Rc<Variable>> {&self.args}
    fn get_scope(&self) -> Option<&Scope> {self.fn_impl.as_ref()}
    fn get_return(&self) -> Type {self.ret_type.clone()}
    fn get_name(&self) -> &str {&self.name}
}

impl Typed for Function {
    fn get_type(&self) -> Type {self.ret_type.clone()}
}

impl Scoped for Function {
    fn enter_scope(&self, sym: &mut SymbolTable) {
        for arg in &self.args {sym.define(Symbol::Variable(arg.clone()))}
        if let Some(s) = &self.fn_impl {s.enter_scope(sym);}
    }
    fn leave_scope(&self, sym : &mut SymbolTable) {
        if let Some(s) = &self.fn_impl {s.leave_scope(sym);}
        for arg in &self.args {sym.undef(arg.get_name());}
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Symbol {
    Variable(Rc<Variable>),
    Function(Rc<Function>),
    Procedure(Rc<Function>)
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
    symbols : SymbolMap<String, Vec<Symbol>>
}

impl SymbolTable {
    pub fn new() -> SymbolTable {SymbolTable{symbols : SymbolMap::new()}}
    pub fn with_init(symbols : SymbolMap<String, Vec<Symbol>>) -> SymbolTable {
        SymbolTable{symbols : symbols}
    }
    pub fn dereference(&self, symbol : &str) -> Option<Symbol> {
        match self.symbols.get(symbol) {
            Some(v) => match v.last() {
                Some(r) => Some(r.clone()),
                None => None
            },
            None => None
        }
    }
    pub fn define(&mut self, symbol : Symbol) {
        self.reference(symbol.get_name().to_string(), symbol)
    }
    pub fn reference(&mut self, name : String, symbol : Symbol) {
        self.symbols.entry(name).or_insert(Vec::with_capacity(1)).push(symbol);
    }
    pub fn undef(&mut self, name : &str) -> Option<Symbol> {
        match self.symbols.get_mut(name) {
            Some(v) => v.pop(),
            None => None
        }
    }
}
