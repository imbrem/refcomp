use by_address::ByAddress;
use std::rc::Rc;
use super::types::{Type, Typed};
use super::statement::Scope;
use std::cell::{Ref, RefCell, Cell};
use std::collections::BTreeMap as SymbolMap;
use std::ops::Deref;

pub trait Callable {
    fn get_name(&self) -> &str;
    fn get_arity(&self) -> usize;
    fn get_params(&self) -> &Vec<Rc<Variable>>;
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
    name : String,
    level : Cell<Option<u32>>
}

impl DependencyVisitor for Rc<Variable> {
    fn visit_dependencies<T: FnMut(Rc<Variable>)>(&self, mut visitor: T) -> T {
        visitor(self.clone());
        visitor
    }
}

impl Variable {
    pub fn new(name : String, var_type : Type) -> Variable {
        Variable{ name : name, var_type : var_type, level : Cell::new(None) }
    }
    pub fn integer(name : String) -> Variable {
        Self::new(name, Type::integer())
    }
    pub fn boolean(name : String) -> Variable {
        Self::new(name, Type::boolean())
    }
    pub fn get_name(&self) -> &str {&self.name}
    pub fn update_level(&self, level : u32) {
        let sl = self.level.get();
        if sl.is_none() || sl.unwrap() > level {
            self.level.set(Some(level))
        }
    }
    pub fn get_level(&self) -> Option<u32> {self.level.get()}
    pub fn to_level(self, level : u32) -> Variable {
        self.update_level(level);
        self
    }
 }

impl Typed for Variable {
    fn get_type(&self) -> Type {return self.var_type.clone()}
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    name : String,
    args : Vec<Rc<Variable>>,
    implicit_args : RefCell<Vec<Rc<Variable>>>,
    ret_type : Type,
    fn_impl : RefCell<Option<Scope>>,
    level : Cell<Option<u32>>
}

impl Function {
    pub fn new(name : String, args : Vec<Rc<Variable>>, ret_type : Type)
    -> Function {Function{
        name : name,
        args : args,
        ret_type : ret_type,
        implicit_args : RefCell::new(vec![]),
        fn_impl : RefCell::new(None),
        level : Cell::new(None)
    }}
    pub fn procedure(name : String, args : Vec<Rc<Variable>>) -> Function {
        Function::new(name, args, Type::Void)
    }
    pub fn with_impl(name : String, args : Vec<Rc<Variable>>, ret_type : Type, fn_impl : Scope)
    -> Function {
        let result = Function::new(name, args, ret_type);
        result.implement(fn_impl);
        return result
    }
    pub fn get_implicit_args(&self) -> Ref<Vec<Rc<Variable>>> {
        self.implicit_args.borrow()
    }
    pub fn implement(&self, scope : Scope) {
        let mut iargs = vec![];
        scope.visit_dependencies(|v : Rc<Variable>| {
            let b = ByAddress(v);
            if b.get_level().unwrap() < self.get_level().unwrap() {
                iargs.push(b.0)
            } else if b.get_level().unwrap() > self.get_level().unwrap() + 1 {
                panic!(
                    "Level of variable {:#?} too high to dereference from this scope ({:#?})!",
                    b, self
                );
            }
        });
        self.implicit_args.replace(iargs);
        self.fn_impl.replace(Some(scope));
    }
    pub fn get_scope(&self) -> Ref<Option<Scope>> {
        self.fn_impl.borrow()
    }
    pub fn update_level(&self, level : u32) {
        let sl = self.level.get();
        if sl.is_none() || sl.unwrap() > level {
            self.level.set(Some(level));
            for arg in &self.args {arg.update_level(level + 1);}
        }
    }
    pub fn get_level(&self) -> Option<u32> {self.level.get()}
}

impl DependencyVisitor for Function {
    fn visit_dependencies<T: FnMut(Rc<Variable>)>(&self, mut visitor : T) -> T {
        for arg in self.get_implicit_args().iter() {visitor = arg.visit_dependencies(visitor)}
        visitor
    }
}

impl Callable for Function {
    fn get_arity(&self) -> usize {self.args.len()}
    fn get_params(&self) -> &Vec<Rc<Variable>> {&self.args}
    fn get_return(&self) -> Type {self.ret_type.clone()}
    fn get_name(&self) -> &str {&self.name}
}

impl Typed for Function {
    fn get_type(&self) -> Type {self.ret_type.clone()}
}

impl Scoped for Function {
    fn enter_scope(&self, sym: &mut SymbolTable) {
        for arg in &self.args {sym.define(Symbol::Variable(arg.clone()))}
        if let Some(s) = self.fn_impl.borrow().deref() {s.enter_scope(sym);}
    }
    fn leave_scope(&self, sym : &mut SymbolTable) {
        if let Some(s) = self.fn_impl.borrow().deref() {s.leave_scope(sym);}
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
    pub fn update_level(&self, level : u32) {
        match self {
            Symbol::Variable(v) => v.update_level(level),
            Symbol::Function(f) => f.update_level(level),
            Symbol::Procedure(p) => p.update_level(level)
        }
    }
    pub fn get_level(&self) -> Option<u32> {
        match self {
            Symbol::Variable(v) => v.get_level(),
            Symbol::Function(f) => f.get_level(),
            Symbol::Procedure(p) => p.get_level()
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct SymbolTable {
    symbols : SymbolMap<String, Vec<Symbol>>,
    level : u32
}

impl SymbolTable {
    pub fn new() -> SymbolTable {SymbolTable{symbols : SymbolMap::new(), level : 0}}
    pub fn with_init(symbols : SymbolMap<String, Vec<Symbol>>) -> SymbolTable {
        SymbolTable{symbols : symbols, level : 0}
    }

    pub fn enter_level(&mut self) {
        self.level += 1;
    }
    pub fn leave_level(&mut self) {
        self.level -= 1;
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
        println!("Defining symbol {:?}", symbol);
        self.reference(symbol.get_name().to_string(), symbol)
    }
    pub fn reference(&mut self, name : String, symbol : Symbol) {
        println!("Referencing name {} to symbol {:?}", name, symbol);
        symbol.update_level(self.level);
        self.symbols.entry(name).or_insert(Vec::with_capacity(1)).push(symbol);
    }
    pub fn undef(&mut self, name : &str) -> Option<Symbol> {
        println!("Undefining name {}", name);
        match self.symbols.get_mut(name) {
            Some(v) => v.pop(),
            None => None
        }
    }
}
