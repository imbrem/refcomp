use super::{Variable, Function, Callable};
use super::types::{Type, Typed, ScalarType};
use crate::Configuration;

pub trait UnaryFolder {
    fn fold(c: Constant, cfg : Configuration) -> Option<Constant>;
}
pub trait UnaryExpression {
    fn new(expr : Expression, cfg : Configuration) -> Option<Expression>;
}

pub struct Negation<'a> {
    arg : Box<Expression<'a>>
}

impl<'a> UnaryExpression for Negation<'a> {
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

impl<'a> UnaryFolder for Negation<'a> {
    fn fold(c : Constant, _cfg : Configuration) -> Option<Constant> {
        match c {
            Constant::Integer(i) => Some(Constant::Integer(-i)),
            _ => None
        }
    }
}

impl<'a> Typed for Negation<'a> {
    fn get_type(&self) -> Type {self.arg.get_type()}
}

pub enum ArithmeticOp {
    Add, Sub, Mul, Div
}
pub struct Arithmetic<'a> {
    lhs : Box<Expression<'a>>,
    rhs : Box<Expression<'a>>,
    op : ArithmeticOp
}

impl<'a> Arithmetic<'a> {
    pub fn new(lhs : Expression<'a>, rhs : Expression<'a>, op : ArithmeticOp, cfg : Configuration)
    -> Option<Expression<'a>> {
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

impl<'a> Typed for Arithmetic<'a> {
    fn get_type(&self) -> Type {self.rhs.get_type()}
}

pub struct Not<'a> {
    arg : Box<Expression<'a>>
}

impl<'a> UnaryExpression for Not<'a> {
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

impl<'a> UnaryFolder for Not<'a> {
    fn fold(c : Constant, _cfg : Configuration) -> Option<Constant> {
        match c {
            Constant::Boolean(b) => Some(Constant::Boolean(!b)),
            _ => None
        }
    }
}

impl<'a> Typed for Not<'a> {
    fn get_type(&self) -> Type {Type::ScalarType(ScalarType::Boolean)}
}

pub enum LogicalOp {
    And, Or
}
pub struct Logical<'a> {
    lhs : Box<Expression<'a>>,
    rhs : Box<Expression<'a>>,
    op : LogicalOp
}

impl<'a> Logical<'a> {
    pub fn new(lhs : Expression<'a>, rhs : Expression<'a>, op : LogicalOp, cfg : Configuration)
    -> Option<Expression<'a>> {
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

impl<'a> Typed for Logical<'a> {
    fn get_type(&self) -> Type {Type::ScalarType(ScalarType::Boolean)}
}

pub struct Comparison<'a> {
    lhs : Box<Expression<'a>>,
    rhs : Box<Expression<'a>>,
    op : ComparisonOp
}

impl<'a> Comparison<'a> {
    pub fn new(lhs : Expression<'a>, rhs : Expression<'a>, op : ComparisonOp, cfg : Configuration)
    -> Option<Expression<'a>> {
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

pub enum ComparisonOp {
    EQ, NE, LT, LE, GT, GE
}

pub struct FunctionCall<'a> {
    function : &'a Function,
    arguments : Vec<Expression<'a>>
}

impl<'a> FunctionCall<'a> {
    pub fn new(function : &'a Function, arguments : Vec<Expression<'a>>) -> Option<Expression<'a>> {
        if function.get_arity() == arguments.len() {Some(
            Expression::FunctionCall(FunctionCall{function : function, arguments : arguments})
        )} else {None}
    }
}

impl<'a> Typed for FunctionCall<'a> {
    fn get_type(&self) -> Type {self.function.get_type()}
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

pub enum Expression<'a> {
    Constant(Constant),
    Negation(Negation<'a>),
    Arithmetic(Arithmetic<'a>),
    Not(Not<'a>),
    Logical(Logical<'a>),
    Comparison(Comparison<'a>),
    Variable(&'a Variable),
    FunctionCall(FunctionCall<'a>)
}

impl<'a> Typed for Expression<'a> {
    //TODO: implement
    fn get_type(&self) -> Type {Type::Null}
}
