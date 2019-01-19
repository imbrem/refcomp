use super::table::{Procedure, Variable};
use super::expression::{Expression, ArrayIndex};
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
    pub statements : Vec<Statement>
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
