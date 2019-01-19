use super::Variable;
use super::types::{Type, Typed, ScalarType};
use crate::Configuration;

pub trait UnaryFolder {
    fn fold(c: Constant, cfg : Configuration) -> Option<Constant>;
}
pub trait UnaryExpression {
    fn new(expr : Expression, cfg : Configuration) -> Option<Expression>;
}

pub struct Negation {
    arg : Box<Expression>
}

impl UnaryExpression for Negation {
    fn new(expr : Expression, cfg : Configuration) -> Option<Expression> {
        match expr {
            Expression::Constant(c) => match Self::fold(c, cfg) {
                Some(c) => Some(Expression::Constant(c)),
                None => None
            },
            e => match e.get_type() {
                Type::ScalarType(s) => match s {
                    ScalarType::Integer => Some(Expression::Negation(Negation{arg : Box::new(e)})),
                    _ => None
                },
                _ => None
            }
        }
    }
}

impl UnaryFolder for Negation {
    fn fold(c : Constant, _cfg : Configuration) -> Option<Constant> {
        match c {
            Constant::Integer(i) => Some(Constant::Integer(-i)),
            _ => None
        }
    }
}

impl Typed for Negation {
    fn get_type(&self) -> Type {self.arg.get_type()}
}

pub enum ArithmeticOp {
    Add, Sub, Mul, Div
}
pub struct Arithmetic {
    lhs : Box<Expression>,
    rhs : Box<Expression>,
    op : ArithmeticOp
}

impl Arithmetic {
    pub fn new(lhs : Expression, rhs : Expression, op : ArithmeticOp, cfg : Configuration)
    -> Option<Expression> {
        match (lhs, rhs) {
            (Expression::Constant(c), Expression::Constant(d)) => match Self::fold(c, d, op, cfg) {
                Some(c) => Some(Expression::Constant(c)),
                None => None
            },
            (lhs, rhs) => match (lhs.get_type(), rhs.get_type()) {
                (Type::ScalarType(s), Type::ScalarType(t)) => match (s, t) {
                    (ScalarType::Integer, ScalarType::Integer) => Some(Expression::Arithmetic(
                        Arithmetic{lhs : Box::new(lhs), rhs : Box::new(rhs), op : op})),
                    _ => None
                },
                _ => None
            }
        }

    }
    pub fn fold(c : Constant, d : Constant, op : ArithmeticOp, _cfg : Configuration)
    -> Option<Constant> {
        match (c, d) {
            (Constant::Integer(c), Constant::Integer(d)) => match op {
                ArithmeticOp::Add => Some(Constant::Integer(c + d)),
                ArithmeticOp::Sub => Some(Constant::Integer(c - d)),
                ArithmeticOp::Mul => Some(Constant::Integer(c * d)),
                ArithmeticOp::Div => Some(Constant::Integer(c / d))
            },
            _ => None
        }
    }
}

impl Typed for Arithmetic {
    fn get_type(&self) -> Type {self.rhs.get_type()}
}

pub struct Not {
    arg : Box<Expression>
}

impl UnaryExpression for Not {
    fn new(expr : Expression, cfg : Configuration) -> Option<Expression> {
        match expr {
            Expression::Constant(c) => match Self::fold(c, cfg) {
                Some(c) => Some(Expression::Constant(c)),
                None => None
            },
            Expression::Not(ne) => Some(*ne.arg), // Double negation optimization
            e => match e.get_type() {
                Type::ScalarType(s) => match s {
                    ScalarType::Boolean => Some(Expression::Not(Not{arg : Box::new(e)})),
                    _ => None
                },
                _ => None
            }
        }
    }
}

impl UnaryFolder for Not {
    fn fold(c : Constant, _cfg : Configuration) -> Option<Constant> {
        match c {
            Constant::Boolean(b) => Some(Constant::Boolean(!b)),
            _ => None
        }
    }
}

impl Typed for Not {
    fn get_type(&self) -> Type {Type::ScalarType(ScalarType::Boolean)}
}

pub enum LogicalOp {
    And, Or
}
pub struct Logical {
    lhs : Box<Expression>,
    rhs : Box<Expression>,
    op : LogicalOp
}

impl Logical {
    pub fn new(lhs : Expression, rhs : Expression, op : LogicalOp, cfg : Configuration)
    -> Option<Expression> {
        match (lhs, rhs) {
            (Expression::Constant(c), Expression::Constant(d)) => match Self::fold(c, d, op, cfg) {
                Some(c) => Some(Expression::Constant(c)),
                None => None
            },
            (lhs, rhs) => match (lhs.get_type(), rhs.get_type()) {
                (Type::ScalarType(s), Type::ScalarType(t)) => match (s, t) {
                    (ScalarType::Boolean, ScalarType::Boolean) => Some(Expression::Logical(
                        Logical{lhs : Box::new(lhs), rhs : Box::new(rhs), op : op})),
                    _ => None
                },
                _ => None
            }
        }

    }
    pub fn fold(c : Constant, d : Constant, op : LogicalOp, _cfg : Configuration)
    -> Option<Constant> {
        match (c, d) {
            (Constant::Boolean(c), Constant::Boolean(d)) => match op {
                LogicalOp::And => Some(Constant::Boolean(c && d)),
                LogicalOp::Or => Some(Constant::Boolean(c || d))
            },
            _ => None
        }
    }
}

impl Typed for Logical {
    fn get_type(&self) -> Type {Type::ScalarType(ScalarType::Boolean)}
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
    Integer(i64), // Should be 32 bit, but is reduced based off configuration later
    Boolean(bool)
}

impl Typed for Constant {
    fn get_type(&self) -> Type {
        match self {
            Constant::Integer(_) => Type::ScalarType(ScalarType::Integer),
            Constant::Boolean(_) => Type::ScalarType(ScalarType::Boolean)
        }
    }
}

pub enum Expression {
    Constant(Constant),
    Negation(Negation),
    Arithmetic(Arithmetic),
    Not(Not),
    Logical(Logical),
    Comparison(Comparison),
    Variable(Variable),
    FunctionCall(FunctionCall)
}

impl Typed for Expression {
    //TODO: implement
    fn get_type(&self) -> Type {Type::Null}
}
