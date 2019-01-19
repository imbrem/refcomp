use super::{Procedure, Variable};
use super::expression::Expression;

pub struct Assignment<'a> {
    pub destination : &'a Variable,
    pub value : Expression<'a>
}

pub struct ConditionalBranch<'a> {
    pub condition : Expression<'a>,
    pub scope : Scope<'a>
}

pub struct Conditional<'a> {
    pub if_branch : ConditionalBranch<'a>,
    pub elif_branches : Vec<ConditionalBranch<'a>>,
    pub else_branch : Option<ConditionalBranch<'a>>
}

pub struct While<'a> {
    pub condition : Expression<'a>,
    pub scope : Scope<'a>
}
pub struct Repeat<'a> {
    pub condition : Expression<'a>,
    pub scope : Scope<'a>
}
pub struct ProcedureCall<'a> {
    pub procedure : &'a Procedure
}
pub struct Scope<'a> {
    pub statements : Vec<Statement<'a>>
}

pub enum Statement<'a> {
    Assignment(Assignment<'a>),
    Conditional(Conditional<'a>),
    While(While<'a>),
    Repeat(Repeat<'a>),
    Break(u32),
    Return(Expression<'a>),
    Print(Vec<OutputElement<'a>>),
    Input(Vec<String>),
    ProcedureCall(ProcedureCall<'a>),
    Scope(Scope<'a>)
}

pub enum OutputElement<'a> {
    Expression(Expression<'a>),
    Text(String),
    Newline
}
