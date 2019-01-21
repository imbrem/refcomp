use super::table::{Variable, Function, Callable, Symbol, SymbolTable};
use super::types::{Type, Typed, ScalarType, ArrayType};
use crate::parser::{Rule, BINARY_PRECEDENCE_CLIMBER, LOGICAL_PRECEDENCE_CLIMBER};
use pest::iterators::{Pair};
use std::rc::Rc;
type EPResult = Result<Expression, &'static str>;
type FResult = Result<Constant, &'static str>;

pub trait UnaryFolder {
    fn fold(c: Constant) -> FResult;
}
pub trait UnaryExpression {
    fn new(expr : Expression) -> EPResult;
}

#[derive(Clone, Debug, PartialEq)]
pub struct Negation {
    arg : Box<Expression>
}

impl UnaryExpression for Negation {
    fn new(expr : Expression) -> EPResult {
        match expr {
            Expression::Constant(c) => Ok(Expression::Constant(Self::fold(c)?)),
            e => match e.get_type() {
                Type::ScalarType(s) => match s {
                    ScalarType::Integer => Ok(Expression::Negation(Negation{arg : Box::new(e)})),
                    _ => Err("Cannot negate non-numeric types")
                },
                _ => Err("Cannot negate non-scalar types")
            }
        }
    }
}

impl UnaryFolder for Negation {
    fn fold(c : Constant) -> FResult {
        match c {
            Constant::Integer(i) => Ok(Constant::Integer(-i)),
            _ => Err("Cannot negate non-numeric constants")
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
    pub fn new(lhs : Expression, rhs : Expression, op : ArithmeticOp) -> EPResult {
        match (lhs, rhs) {
            (Expression::Constant(c), Expression::Constant(d)) =>
                Ok(Expression::Constant(Self::fold(c, d, op)?)),
            (lhs, rhs) => match (lhs.get_type(), rhs.get_type()) {
                (Type::ScalarType(s), Type::ScalarType(t)) => match (s, t) {
                    (ScalarType::Integer, ScalarType::Integer) => Ok(Expression::Arithmetic(
                        Arithmetic{lhs : Box::new(lhs), rhs : Box::new(rhs), op : op})),
                    _ => Err("Cannot add non-numeric types")
                },
                _ => Err("Cannot add array types")
            }
        }

    }
    pub fn fold(c : Constant, d : Constant, op : ArithmeticOp) -> FResult {
        match (c, d) {
            (Constant::Integer(c), Constant::Integer(d)) => match op {
                ArithmeticOp::Add => Ok(Constant::Integer(c + d)),
                ArithmeticOp::Sub => Ok(Constant::Integer(c - d)),
                ArithmeticOp::Mul => Ok(Constant::Integer(c * d)),
                ArithmeticOp::Div => Ok(Constant::Integer(c / d))
            },
            _ => Err("Cannot add incompatible types")
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
    fn new(expr : Expression) -> EPResult {
        match expr {
            Expression::Constant(c) => Ok(Expression::Constant(Self::fold(c)?)),
            Expression::Not(ne) => Ok(*ne.arg), // Double negation optimization
            e => match e.get_type() {
                Type::ScalarType(s) => match s {
                    ScalarType::Boolean => Ok(Expression::Not(Not{arg : Box::new(e)})),
                    _ => Err("Cannot perform logical negation on non-Boolean type")
                },
                _ => Err("Cannot perform logical negation on a vector")
            }
        }
    }
}

impl UnaryFolder for Not {
    fn fold(c : Constant) -> FResult {
        match c {
            Constant::Boolean(b) => Ok(Constant::Boolean(!b)),
            _ => Err("Cannot perform logical negation on non-Boolean type")
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
    pub fn new(lhs : Expression, rhs : Expression, op : LogicalOp) -> EPResult {
        match (lhs, rhs) {
            (Expression::Constant(c), Expression::Constant(d)) =>
                Ok(Expression::Constant(Self::fold(c, d, op)?)),
            (lhs, rhs) => match (lhs.get_type(), rhs.get_type()) {
                (Type::ScalarType(s), Type::ScalarType(t)) => match (s, t) {
                    (ScalarType::Boolean, ScalarType::Boolean) => Ok(Expression::Logical(
                        Logical{lhs : Box::new(lhs), rhs : Box::new(rhs), op : op})),
                    _ => Err("Cannot perform logical operations on non-Boolean type")
                },
                _ => Err("Cannot perform logical operations on vectors")
            }
        }

    }
    pub fn fold(c : Constant, d : Constant, op : LogicalOp)
    -> FResult {
        match (c, d) {
            (Constant::Boolean(c), Constant::Boolean(d)) => match op {
                LogicalOp::And => Ok(Constant::Boolean(c && d)),
                LogicalOp::Or => Ok(Constant::Boolean(c || d))
            },
            _ => Err("Cannot perform logical operations on non-Boolean constants")
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
    pub fn new(lhs : Expression, rhs : Expression, op : ComparisonOp) -> EPResult {
        match (lhs, rhs) {
            (Expression::Constant(c), Expression::Constant(d)) =>
                Ok(Expression::Constant(Self::fold(c, d, op)?)),
            (lhs, rhs) => match (lhs.get_type(), rhs.get_type()) {
                (Type::ScalarType(s), Type::ScalarType(t)) => match (s, t) {
                    (ScalarType::Boolean, ScalarType::Boolean) => match op {
                        ComparisonOp::EQ | ComparisonOp::NE => Ok(Expression::Comparison(
                        Comparison{lhs : Box::new(lhs), rhs : Box::new(rhs), op : op})),
                        _ => Err("Cannot compare booleans by order")
                    },
                    (ScalarType::Integer, ScalarType::Integer) => Ok(Expression::Comparison(
                        Comparison{lhs : Box::new(lhs), rhs : Box::new(rhs), op : op})),
                    _ => Err("Cannot compare incompatible types")
                }
                _ => Err("Cannot compare non-scalar types")
            }
        }

    }
    pub fn fold(c : Constant, d : Constant, op : ComparisonOp) -> FResult {
        match (c, d) {
            (Constant::Boolean(c), Constant::Boolean(d)) => match op {
                ComparisonOp::EQ => Ok(Constant::Boolean(c == d)),
                ComparisonOp::NE => Ok(Constant::Boolean(c != d)),
                _ => Err("Cannot compare boolean constants by order")
            },
            (Constant::Integer(c), Constant::Integer(d)) => Ok(match op {
                ComparisonOp::EQ => Constant::Boolean(c == d),
                ComparisonOp::NE => Constant::Boolean(c != d),
                ComparisonOp::LT => Constant::Boolean(c < d),
                ComparisonOp::LE => Constant::Boolean(c <= d),
                ComparisonOp::GT => Constant::Boolean(c > d),
                ComparisonOp::GE => Constant::Boolean(c >= d),
            }),
            _ => Err("Cannot compare incompatible constants")
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
    pub fn new(function : Rc<Function>, arguments : Vec<Expression>) -> EPResult {
        if function.get_arity() == arguments.len() {
            for (arg, typ) in arguments.iter()
                .zip(function.get_params().iter().map(|p| p.get_type())) {
                if arg.get_type() != typ {return Err("Argument type mismatch");}
            }
            Ok(Expression::FunctionCall(FunctionCall{function : function, arguments : arguments})
        )} else {Err("Invalid number of arguments to function call")}
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
pub struct ArrayIndex {
    variable : Rc<Variable>,
    index : Vec<Expression>,
    idx_type : Type
}

impl ArrayIndex {
    pub fn new(variable : Rc<Variable>, index : Vec<Expression>) -> EPResult {
        match variable.get_type() {
            Type::ArrayType(a) => {
                let new_type = if index.len() == a.get_dims().len() {
                    Type::ScalarType(a.get_element_type())
                } else if index.len() < a.get_dims().len() {
                    Type::ArrayType(Rc::new(ArrayType::new(
                        a.get_element_type(),
                        a.get_dims()[..a.get_dims().len() - index.len()].to_vec())))
                } else {
                    return Err("Invalid array dimensions")
                };
                for expr in &index {
                    if expr.get_type() != Type::ScalarType(ScalarType::Integer) {
                        return Err("Array indices must be integers")}
                }
                Ok(Expression::ArrayIndex(ArrayIndex{
                        variable : variable, index : index, idx_type : new_type
                }))
            },
            _ => Err("Tried to array index non array type")
        }
    }
}

impl Typed for ArrayIndex {
    fn get_type(&self) -> Type {self.idx_type.clone()}
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
    ArrayIndex(ArrayIndex),
    FunctionCall(FunctionCall)
}

pub fn parse_arguments(pair : Pair<Rule>, sym : &SymbolTable)
-> Result<Vec<Expression>, &'static str> {
    if pair.as_rule() != Rule::arguments {return Err("Not arguments");}
    pair.into_inner()
            .map(|arg| Expression::from_pair(arg, sym))
            .collect()
}

impl Expression {

    fn parse_primary(pair : Pair<Rule>, sym : &SymbolTable) -> EPResult {
        if pair.as_rule() != Rule::primary_expression {return Err("Not a primary expression")}
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::integer => match str::parse::<i64>(pair.as_str()) {
                Ok(i) => Ok(Expression::Constant(Constant::Integer(i))),
                _ => Err("Could not parse integer")
            },
            Rule::expression => Self::from_pair(pair, sym),
            Rule::ternary_expression => panic!("Not yet implemented!"),
            Rule::function_call => {
                let mut pairs = pair.into_inner();
                let fn_name = pairs.next().unwrap().as_str();
                let arguments = match pairs.next() {
                    Some(args) => parse_arguments(args, sym)?,
                    None => Vec::new()
                };
                FunctionCall::new(
                    match sym.dereference(fn_name) {
                        Some(Symbol::Function(f)) => f,
                        Some(Symbol::Procedure(_)) => {
                            return Err("Cannot call procedure in expression")},
                        Some(Symbol::Variable(_)) => {
                            return Err("Cannot call variable as function")},
                        _ => {return Err("Function undefined")}
                    },
                    arguments
            )},
            Rule::kw_true => Ok(Expression::Constant(Constant::Boolean(true))),
            Rule::kw_false => Ok(Expression::Constant(Constant::Boolean(false))),
            Rule::array_index => {
                let mut pairs = pair.into_inner();
                let var = match sym.dereference(pairs.next().unwrap().as_str()) {
                    Some(v) => match v {
                        Symbol::Variable(v) => v,
                        Symbol::Function(_) => {
                            return Err("Expected array variable, got function")},
                        Symbol::Procedure(_) => {
                            return Err("Expected array variable, got procedure")}
                    },
                    None => {return Err("Could not dereference array variable")}
                };
                let mut idxers = Vec::new();
                for pair in pairs {
                    assert_eq!(pair.as_rule(), Rule::array_indexer);
                    idxers.push(Self::from_pair(pair, sym)?)
                }
                ArrayIndex::new(var, idxers)
            }
            Rule::identifier => match sym.dereference(pair.as_str()) {
                Some(v) => match v {
                    Symbol::Variable(v) => Ok(Expression::Variable(v)),
                    Symbol::Function(_) => Err("Expected variable, got function"),
                    Symbol::Procedure(_) => Err("Expected variable, got procedure")
                },
                _ => Err("Could not dereference variable")
            },
            _ => panic!("{:?} is not a valid primary expression!", pair)
        }
    }

    fn parse_binary(pair : Pair<Rule>, sym : &SymbolTable)
    -> EPResult {
        let parse_primary = |pair : Pair<Rule>| {Self::parse_primary(pair, sym)};
        let merge_arith = |lhs : EPResult, op : Pair<Rule>, rhs : EPResult|
        -> EPResult {
            match (lhs, rhs) {
                (Ok(lhs), Ok(rhs)) => match op.as_rule() {
                    Rule::op_plus => Arithmetic::new(lhs, rhs, ArithmeticOp::Add),
                    Rule::op_minus => Arithmetic::new(lhs, rhs, ArithmeticOp::Sub),
                    Rule::op_times => Arithmetic::new(lhs, rhs, ArithmeticOp::Mul),
                    Rule::op_divides => Arithmetic::new(lhs, rhs, ArithmeticOp::Div),
                    op => panic!("{:?} is not a valid arithmetic binary operator!", op)
                },
                (Err(lhs), _) => Err(lhs),
                (_, Err(rhs)) => Err(rhs)
            }
        };
        BINARY_PRECEDENCE_CLIMBER.climb(pair.into_inner(), parse_primary, merge_arith)
    }

    fn parse_logical_primary(pair : Pair<Rule>, sym : &SymbolTable)
    -> EPResult {
        match pair.as_rule() {
            Rule::comparison_expression => {
                let mut pairs = pair.into_inner();
                let lhs = Self::parse_binary(pairs.next().unwrap(), sym)?;
                let op = match pairs.next().unwrap().as_rule() {
                    Rule::op_eq => ComparisonOp::EQ,
                    Rule::op_neq => ComparisonOp::NE,
                    Rule::op_lt => ComparisonOp::LT,
                    Rule::op_leq => ComparisonOp::LE,
                    Rule::op_gt => ComparisonOp::GT,
                    Rule::op_geq => ComparisonOp::GE,
                    op => panic!("{:?} is not a valid comparison opertor", op)
                };
                let rhs = Self::parse_binary(pairs.next().unwrap(), sym)?;
                Comparison::new(lhs, rhs, op)
            },
            Rule::binary_expression => Self::parse_binary(pair, sym),
            Rule::not_expression => Not::new(Self::parse_logical_primary(pair, sym)?),
            _ => panic!("{:?} is not a valid logical primary!", pair)
        }
    }

    pub fn from_pair(pair : Pair<Rule>, sym : &SymbolTable) -> EPResult {
        if pair.as_rule() != Rule::expression {return Err("Not an expression");}
        let logical_primary = |pair : Pair<Rule>| {Self::parse_logical_primary(pair, sym)};
        let merge_logical = |lhs : EPResult, op : Pair<Rule>, rhs : EPResult|
            -> EPResult {
            match (lhs, rhs) {
                (Ok(lhs), Ok(rhs)) => match op.as_rule() {
                    Rule::kw_and => Logical::new(lhs, rhs, LogicalOp::And),
                    Rule::kw_or => Logical::new(lhs, rhs, LogicalOp::Or),
                    op => panic!("{:?} is not a valid logical binary operator!", op)
                },
                (Err(lhs), _) => Err(lhs),
                (_, Err(rhs)) => Err(rhs)
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
            Expression::ArrayIndex(a) => a.get_type(),
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
                ArithmeticOp::Add
            ),
            Ok(Expression::Constant(Constant::Integer(2)))
        )
    }

    #[test]
    fn integer_expressions_are_parsed_correctly() {
        let sym = SymbolTable::new();
        assert_eq!(
            Expression::from_pair(
                CSC488Parser::parse(Rule::expression, "1 + 1 * (3 + 3)").unwrap().next().unwrap(),
                &sym
            ).unwrap(),
            Expression::Constant(Constant::Integer(7))
        )
    }

    #[test]
    fn boolean_expressions_parse_correctly() {
        let sym = SymbolTable::new();
        assert_eq!(
            Expression::from_pair(
                CSC488Parser::parse(Rule::expression, "1 + 1 * (3 + 3) > 3 * 3 or 1 < 1 + 1").unwrap().next().unwrap(),
                &sym
            ).unwrap(),
            Expression::Constant(Constant::Boolean(true))
        )
    }

    #[test]
    fn variables_parse_correctly() {
        let mut sym = SymbolTable::new();
        let vx = Rc::new(Variable::new("x".to_string(), Type::ScalarType(ScalarType::Integer)));
        let x = Symbol::Variable(vx.clone());
        sym.define(x);
        assert_eq!(
            Expression::from_pair(
                CSC488Parser::parse(Rule::expression, "x").unwrap().next().unwrap(),
                &sym
            ).unwrap(),
            Expression::Variable(vx)
        )
    }

    #[test]
    fn function_calls_parse_correctly() {
        let mut sym = SymbolTable::new();
        let f = Rc::new(Function::new("f".to_string(), vec![
            Rc::new(Variable::integer("x".to_string())),
            Rc::new(Variable::boolean("flag".to_string()))],
            Type::integer()
        ));
        sym.define(Symbol::Function(f.clone()));
        let result = Expression::from_pair(
                CSC488Parser::parse(Rule::expression, "f(5, true)").unwrap().next().unwrap(),
                &sym
            ).unwrap();
        assert_eq!(
            result,
            FunctionCall::new(
                f,
                vec![
                    Expression::Constant(Constant::Integer(5)),
                    Expression::Constant(Constant::Boolean(true))]
            ).unwrap()
        );
        assert_eq!(result.get_type(), Type::integer());
    }
}
