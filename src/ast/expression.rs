use super::{Variable, Function, Callable};
use super::types::{Type, Typed, ScalarType};
use crate::Configuration;
use std::rc::Rc;

pub trait UnaryFolder {
    fn fold(c: Constant, cfg : Configuration) -> Option<Constant>;
}
pub trait UnaryExpression {
    fn new(expr : Expression, cfg : Configuration) -> Option<Expression>;
}

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ArithmeticOp {
    Add, Sub, Mul, Div
}

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LogicalOp {
    And, Or
}

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub struct Comparison {
    lhs : Box<Expression>,
    rhs : Box<Expression>,
    op : ComparisonOp
}

impl Comparison {
    pub fn new(lhs : Expression, rhs : Expression, op : ComparisonOp, cfg : Configuration)
    -> Option<Expression> {
        match (lhs, rhs) {
            (Expression::Constant(c), Expression::Constant(d)) => match Self::fold(c, d, op, cfg) {
                Some(c) => Some(Expression::Constant(c)),
                None => None
            },
            (lhs, rhs) => match (lhs.get_type(), rhs.get_type()) {
                (Type::ScalarType(s), Type::ScalarType(t)) => match (s, t) {
                    (ScalarType::Boolean, ScalarType::Boolean) => match op {
                        ComparisonOp::EQ | ComparisonOp::NE => Some(Expression::Comparison(
                        Comparison{lhs : Box::new(lhs), rhs : Box::new(rhs), op : op})),
                        _ => None
                    },
                    (ScalarType::Integer, ScalarType::Integer) => Some(Expression::Comparison(
                        Comparison{lhs : Box::new(lhs), rhs : Box::new(rhs), op : op})),
                    _ => None
                },
                _ => None
            }
        }

    }
    pub fn fold(c : Constant, d : Constant, op : ComparisonOp, _cfg : Configuration)
    -> Option<Constant> {
        match (c, d) {
            (Constant::Boolean(c), Constant::Boolean(d)) => match op {
                ComparisonOp::EQ => Some(Constant::Boolean(c == d)),
                ComparisonOp::NE => Some(Constant::Boolean(c != d)),
                _ => None
            },
            (Constant::Integer(c), Constant::Integer(d)) => Some(match op {
                ComparisonOp::EQ => Constant::Boolean(c == d),
                ComparisonOp::NE => Constant::Boolean(c != d),
                ComparisonOp::LT => Constant::Boolean(c < d),
                ComparisonOp::LE => Constant::Boolean(c <= d),
                ComparisonOp::GT => Constant::Boolean(c > d),
                ComparisonOp::GE => Constant::Boolean(c >= d),
            }),
            _ => None
        }
    }
}

impl Typed for Comparison {
    fn get_type(&self) -> Type {return Type::ScalarType(ScalarType::Boolean)}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ComparisonOp {
    EQ, NE, LT, LE, GT, GE
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall {
    function : Rc<Function>,
    arguments : Vec<Expression>
}

impl FunctionCall {
    pub fn new(function : Rc<Function>, arguments : Vec<Expression>) -> Option<Expression> {
        if function.get_arity() == arguments.len() {Some(
            Expression::FunctionCall(FunctionCall{function : function, arguments : arguments})
        )} else {None}
    }
}

impl Typed for FunctionCall {
    fn get_type(&self) -> Type {self.function.get_type()}
}

#[derive(Clone, Debug, Eq, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Constant(Constant),
    Negation(Negation),
    Arithmetic(Arithmetic),
    Not(Not),
    Logical(Logical),
    Comparison(Comparison),
    Variable(Rc<Variable>),
    FunctionCall(FunctionCall)
}

impl Typed for Expression {
    fn get_type(&self) -> Type {
        match self {
            Expression::Constant(c) => c.get_type(),
            Expression::Negation(n) => n.get_type(),
            Expression::Arithmetic(a) => a.get_type(),
            Expression::Not(n) => n.get_type(),
            Expression::Logical(l) => l.get_type(),
            Expression::Comparison(c) => c.get_type(),
            Expression::Variable(v) => v.get_type(),
            Expression::FunctionCall(f) => f.get_type()
        }
    }
}
