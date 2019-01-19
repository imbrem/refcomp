use super::{Variable, Function, Callable};
use super::types::{Type, Typed, ScalarType};
use crate::Configuration;
use crate::parser::{Rule, BINARY_PRECEDENCE_CLIMBER, LOGICAL_PRECEDENCE_CLIMBER};
use pest::iterators::{Pair};
use std::rc::Rc;

//TODO: implement
pub struct SymbolTable {

}

pub trait UnaryFolder {
    fn fold(c: Constant, cfg : &Configuration) -> Option<Constant>;
}
pub trait UnaryExpression {
    fn new(expr : Expression, cfg : &Configuration) -> Option<Expression>;
}

#[derive(Clone, Debug, PartialEq)]
pub struct Negation {
    arg : Box<Expression>
}

impl UnaryExpression for Negation {
    fn new(expr : Expression, cfg : &Configuration) -> Option<Expression> {
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
    fn fold(c : Constant, _cfg : &Configuration) -> Option<Constant> {
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
    pub fn new(lhs : Expression, rhs : Expression, op : ArithmeticOp, cfg : &Configuration)
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
    pub fn fold(c : Constant, d : Constant, op : ArithmeticOp, _cfg : &Configuration)
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
    fn new(expr : Expression, cfg : &Configuration) -> Option<Expression> {
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
    fn fold(c : Constant, _cfg : &Configuration) -> Option<Constant> {
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
    pub fn new(lhs : Expression, rhs : Expression, op : LogicalOp, cfg : &Configuration)
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
    pub fn fold(c : Constant, d : Constant, op : LogicalOp, _cfg : &Configuration)
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
    pub fn new(lhs : Expression, rhs : Expression, op : ComparisonOp, cfg : &Configuration)
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
    pub fn fold(c : Constant, d : Constant, op : ComparisonOp, _cfg : &Configuration)
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

impl Expression {

    fn parse_primary(pair : Pair<Rule>, sym : &SymbolTable, cfg : &Configuration) -> Option<Expression> {
        if pair.as_rule() != Rule::primary_expression {return None;}
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::integer => match str::parse::<i64>(pair.as_str()) {
                Ok(i) => Some(Expression::Constant(Constant::Integer(i))),
                _ => None
            },
            Rule::expression => Self::from_pair(pair, sym, cfg),
            Rule::ternary_expression => panic!("Not yet implemented!"),
            Rule::function_call => panic!("Not yet implemented!"),
            Rule::kw_true => Some(Expression::Constant(Constant::Boolean(true))),
            Rule::kw_false => Some(Expression::Constant(Constant::Boolean(false))),
            Rule::variable => panic!("Not yet implemented!"),

            _ => panic!("{:?} is not a valid primary expression!", pair)
        }
    }

    fn parse_binary(pair : Pair<Rule>, sym : &SymbolTable, cfg : &Configuration) -> Option<Expression> {
        let parse_primary = |pair : Pair<Rule>| {Self::parse_primary(pair, sym, cfg)};
        let merge_arith = |lhs : Option<Expression>, op : Pair<Rule>, rhs : Option<Expression>|
        -> Option<Expression> {
            match (lhs, rhs) {
                (Some(lhs), Some(rhs)) => match op.as_rule() {
                    Rule::op_plus => Arithmetic::new(lhs, rhs, ArithmeticOp::Add, cfg),
                    Rule::op_minus => Arithmetic::new(lhs, rhs, ArithmeticOp::Sub, cfg),
                    Rule::op_times => Arithmetic::new(lhs, rhs, ArithmeticOp::Mul, cfg),
                    Rule::op_divides => Arithmetic::new(lhs, rhs, ArithmeticOp::Div, cfg),
                    op => panic!("{:?} is not a valid arithmetic binary operator!", op)
                },
                _ => None
            }
        };
        BINARY_PRECEDENCE_CLIMBER.climb(pair.into_inner(), parse_primary, merge_arith)
    }

    fn parse_logical_primary(pair : Pair<Rule>, sym : &SymbolTable, cfg : &Configuration) -> Option<Expression> {
        match pair.as_rule() {
            Rule::comparison_expression => {
                let mut pairs = pair.into_inner();
                let lhs = match Self::parse_binary(pairs.next().unwrap(), sym, cfg) {
                    Some(s) => s,
                    None => {return None}
                };
                let op = match pairs.next().unwrap().as_rule() {
                    Rule::op_eq => ComparisonOp::EQ,
                    Rule::op_neq => ComparisonOp::NE,
                    Rule::op_lt => ComparisonOp::LT,
                    Rule::op_leq => ComparisonOp::LE,
                    Rule::op_gt => ComparisonOp::GT,
                    Rule::op_geq => ComparisonOp::GE,
                    op => panic!("{:?} is not a valid comparison opertor", op)
                };
                let rhs = match Self::parse_binary(pairs.next().unwrap(), sym, cfg) {
                    Some(s) => s,
                    None => {return None}
                };
                Comparison::new(lhs, rhs, op, cfg)
            },
            Rule::binary_expression => Self::parse_binary(pair, sym, cfg),
            Rule::not_expression => Not::new(
                match Self::parse_logical_primary(pair, sym, cfg) {
                    Some(s) => s,
                    None => {return None}
                },
                cfg
            ),
            _ => panic!("{:?} is not a valid logical primary!", pair)
        }
    }

    pub fn from_pair(pair : Pair<Rule>, sym : &SymbolTable, cfg : &Configuration)
    -> Option<Expression> {
        if pair.as_rule() != Rule::expression {return None;}
        let logical_primary = |pair : Pair<Rule>| {Self::parse_logical_primary(pair, sym, cfg)};
        let merge_logical = |lhs : Option<Expression>, op : Pair<Rule>, rhs : Option<Expression>|
            -> Option<Expression>{
            match (lhs, rhs) {
                (Some(lhs), Some(rhs)) => match op.as_rule() {
                    Rule::kw_and => Logical::new(lhs, rhs, LogicalOp::And, cfg),
                    Rule::kw_or => Logical::new(lhs, rhs, LogicalOp::Or, cfg),
                    op => panic!("{:?} is not a valid logical binary operator!", op)
                },
                _ => None
            }
        };
        LOGICAL_PRECEDENCE_CLIMBER.climb(pair.into_inner(), logical_primary, merge_logical)
    }
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::CSC488Parser;
    use pest::Parser;

    #[test]
    fn integer_expressions_are_folded_correctly() {
        assert_eq!(
            Arithmetic::new(
                Expression::Constant(Constant::Integer(1)),
                Expression::Constant(Constant::Integer(1)),
                ArithmeticOp::Add,
                &Configuration{}
            ),
            Some(Expression::Constant(Constant::Integer(2)))
        )
    }

    #[test]
    fn integer_expressions_are_parsed_correctly() {
        let sym = SymbolTable{};
        let cfg = Configuration{};
        assert_eq!(
            Expression::from_pair(
                CSC488Parser::parse(Rule::expression, "1 + 1 * (3 + 3)").unwrap().next().unwrap(),
                &sym, &cfg
            ).unwrap(),
            Expression::Constant(Constant::Integer(7))
        )
    }

    #[test]
    fn boolean_expressions_parse_correctly() {
        let sym = SymbolTable{};
        let cfg = Configuration{};
        assert_eq!(
            Expression::from_pair(
                CSC488Parser::parse(Rule::expression, "1 + 1 * (3 + 3) > 3 * 3 or 1 < 1 + 1").unwrap().next().unwrap(),
                &sym, &cfg
            ).unwrap(),
            Expression::Constant(Constant::Boolean(true))
        )
    }
}
