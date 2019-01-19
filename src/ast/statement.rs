use super::{Procedure, Variable};
use super::expression::Expression;
use std::rc::Rc;

pub struct Assignment {
    pub destination : Rc<Variable>,
    pub value : Expression
}

pub struct ConditionalBranch {
    pub condition : Expression,
    pub scope : Scope
}

pub struct Conditional {
    pub if_branch : ConditionalBranch,
    pub elif_branches : Vec<ConditionalBranch>,
    pub else_branch : Option<ConditionalBranch>
}

pub struct While {
    pub condition : Expression,
    pub scope : Scope
}
pub struct Repeat {
    pub condition : Expression,
    pub scope : Scope
}
pub struct ProcedureCall {
    pub procedure : Rc<Procedure>
}
pub struct Scope {
    pub statements : Vec<Statement>
}

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

pub enum OutputElement {
    Expression(Expression),
    Text(String),
    Newline
}
