use super::table::{Procedure, Function, Variable, Scoped, Symbol, SymbolTable};
use super::expression::{Expression, ArrayIndex};
use super::declaration::Declaration;
use super::parse_bare_scope;
use crate::parser::Rule;
use pest::iterators::Pair;
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

impl Assignment {
    pub fn to_variable(var : Rc<Variable>, val : Expression) -> Assignment {
        Assignment{destination : AssignmentDestination::Variable(var), value : val}
    }
    pub fn to_index(arr : ArrayIndex, val : Expression) -> Assignment {
        Assignment{destination : AssignmentDestination::ArrayIndex(arr), value : val}
    }
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
                Declaration::Function(f) => result.functions.push(f),
                Declaration::Procedure(p) => result.procedures.push(p)
            }
        }
        result
    }
    pub fn new_from_symbols(statements : Vec<Statement>, symbols : Vec<Symbol>) -> Scope {
        let mut result = {
            Scope{
                variables : vec![],
                functions : vec![],
                procedures : vec![],
                statements : statements
            }
        };
        for sym in symbols {
            match sym {
                Symbol::Variable(v) => result.variables.push(v),
                Symbol::Function(f) => result.functions.push(f),
                Symbol::Procedure(p) => result.procedures.push(p)
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

fn parse_assignment(pair : Pair<Rule>, sym : &SymbolTable) -> Option<Statement> {
    let mut pairs = pair.into_inner();
    let variable = pairs.next().unwrap();
    let expression = match Expression::from_pair(pairs.next().unwrap(), sym) {
        Ok(exp) => exp,
        _ => {return None}
    };
    match variable.as_rule() {
        Rule::array_index => panic!("Array index assignment not yet implemented!"),
        Rule::identifier => match sym.dereference(variable.as_str()) {
            Some(Symbol::Variable(v)) =>
                Some(Statement::Assignment(Assignment::to_variable(v, expression))),
            _ => None
        },
        _ => None
    }
}

pub fn parse_statement(pair : Pair<Rule>, sym : &mut SymbolTable) -> Option<Statement> {
    match pair.as_rule() {
        Rule::assignment => parse_assignment(pair, sym),
        Rule::conditional => {
            panic!("Conditionals are not yet implemented!")
        },
        Rule::while_loop => {
            panic!("While loops are not yet implemented!")
        },
        Rule::repeat_loop => {
            panic!("Repetition loops are not yet implemented!")
        },
        Rule::return_statement => {
            panic!("Return statements are not yet implemented!")
        },
        Rule::print_statement => {
            let outputs = pair.into_inner().next().unwrap().into_inner().map(|o| {
                    let o = o.into_inner().next();
                    if let Some(o) = o {
                        match o.as_rule() {
                            Rule::expression =>
                                OutputElement::Expression(Expression::from_pair(o, sym).unwrap()),
                            Rule::text =>
                                OutputElement::Text(
                                    o.into_inner().next().unwrap().as_str().to_string()
                                ),
                            _ => unreachable!()
                        }
                    } else {
                        OutputElement::Newline
                    }
                });
            Some(Statement::Print(outputs.collect()))
        },
        Rule::input_statement => {
            panic!("Input is not yet implemented!")
        },
        Rule::procedure_call => {
            panic!("Procedure calls are not yet implemented!")
        },
        Rule::scope => Some(
            Statement::Scope(parse_bare_scope(pair.into_inner().next().unwrap(), sym).unwrap())
        ),
        _ => None
    }
}
