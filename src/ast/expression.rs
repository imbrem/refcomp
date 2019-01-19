use super::Variable;

pub struct Negation {

}
pub struct Addition {

}
pub struct Subtraction {

}
pub struct Multiplication {

}
pub struct Division {

}
pub struct Not {

}
pub struct And {

}
pub struct Or {

}
pub struct Comparison {
    pub op : ComparisonOp
}
pub enum ComparisonOp {
    EQ, LT, LEQ, GT, GEQ
}

pub struct FunctionCall {

}
pub enum Constant {
    Integer(i64),
    Boolean(bool)
}

pub enum Expression {
    Constant(Constant),
    Negation(Negation),
    Addition(Addition),
    Subtraction(Subtraction),
    Multiplication(Multiplication),
    Division(Division),
    Not(Not),
    And(And),
    Or(Or),
    Comparison(Comparison),
    Variable(Variable),
    FunctionCall(FunctionCall)
}
