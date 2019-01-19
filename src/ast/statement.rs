use super::expression::Expression;

pub struct Assignment {

}
pub struct Conditional {

}
pub struct While {

}
pub struct Repeat {

}
pub struct ProcedureCall {

}
pub struct Scope {

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
