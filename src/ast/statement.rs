use super::table::{Procedure, Function, Variable, Scoped, Symbol, SymbolTable};
use super::expression::{Expression, ArrayIndex};
use super::declaration::{Declaration};
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum AssignmentDestination {
    Variable(Rc<Variable>),
    ArrayIndex(ArrayIndex)
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub destination : AssignmentDestination,
    pub value : Expression
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConditionalBranch {
    pub condition : Expression,
    pub scope : Scope
}

#[derive(Clone, Debug, PartialEq)]
pub struct Conditional {
    pub if_branch : ConditionalBranch,
    pub elif_branches : Vec<ConditionalBranch>,
    pub else_branch : Option<ConditionalBranch>
}

#[derive(Clone, Debug, PartialEq)]
pub struct While {
    pub condition : Expression,
    pub scope : Scope
}

#[derive(Clone, Debug, PartialEq)]
pub struct Repeat {
    pub condition : Expression,
    pub scope : Scope
}

#[derive(Clone, Debug, PartialEq)]
pub struct ProcedureCall {
    pub procedure : Rc<Procedure>
}

#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    variables : Vec<Rc<Variable>>,
    functions : Vec<Rc<Function>>,
    procedures : Vec<Rc<Procedure>>,
    statements : Vec<Statement>
}

impl Scope {
    pub fn new(statements : Vec<Statement>, declarations : Vec<Declaration>) -> Scope {
        let mut result = {
            Scope{
                variables : vec![],
                functions : vec![],
                procedures : vec![],
                statements : statements
            }
        };
        for decl in declarations {
            match decl {
                Declaration::Variable(vars) => for var in vars {result.variables.push(var)},
                Declaration::Function(f) => result.functions.push(Rc::new(f)),
                Declaration::Procedure(p) => result.procedures.push(Rc::new(p))
            }
        }
        result
    }
    pub fn empty() -> Scope {
        Scope{
            variables : vec![],
            functions : vec![],
            procedures : vec![],
            statements : vec![]
        }
    }
}

impl Scoped for Scope {
    fn enter_scope(&self, sym : &mut SymbolTable) {
        for var in &self.variables {
            sym.define(Symbol::Variable(var.clone()))
        }
        for func in &self.functions {
            sym.define(Symbol::Function(func.clone()))
        }
        for proc in &self.procedures {
            sym.define(Symbol::Procedure(proc.clone()))
        }
    }
    fn leave_scope(&self, sym : &mut SymbolTable) {
        for var in &self.variables {
            sym.undef(var.get_name());
        }
        for func in &self.functions {
            sym.undef(func.get_name());
        }
        for proc in &self.procedures {
            sym.undef(proc.get_name());
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Assignment(Assignment),
    Conditional(Conditional),
    While(While),
    Repeat(Repeat),
    Break(u32),
    Return(Expression),
    Print(Vec<OutputElement>),
    Input(Vec<String>),
    ProcedureCall(ProcedureCall),
    Scope(Scope)
}

#[derive(Clone, Debug, PartialEq)]
pub enum OutputElement {
    Expression(Expression),
    Text(String),
    Newline
}
