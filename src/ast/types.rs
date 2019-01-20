use std::rc::Rc;
use crate::parser::Rule;
use pest::iterators::Pair;

pub trait Typed {
    fn get_type(&self) -> Type;
}
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ScalarType {
    Integer,
    Boolean
}
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ArrayType {
    element_type : ScalarType,
    dims : Vec<u32>
}

impl ArrayType {
    pub fn new(element_type : ScalarType, dims : Vec<u32>) -> ArrayType {
        ArrayType{element_type : element_type, dims : dims}
    }
    pub fn get_element_type(&self) -> ScalarType {self.element_type}
    pub fn get_dims(&self) -> &Vec<u32> {&self.dims}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
    ScalarType(ScalarType),
    ArrayType(Rc<ArrayType>),
    Null,
    Void
}

impl Type {
    pub fn integer() -> Type {Type::ScalarType(ScalarType::Integer)}
    pub fn boolean() -> Type {Type::ScalarType(ScalarType::Boolean)}
}

pub fn parse_scalar_type(pair : Pair<Rule>) -> Option<ScalarType> {
    match pair.as_rule() {
        Rule::scalar_type => match pair.into_inner().next().unwrap().as_rule() {
            Rule::kw_integer => Some(ScalarType::Integer),
            Rule::kw_boolean => Some(ScalarType::Boolean),
            _ => unreachable!()
        },
        _ => None
    }
}

pub fn parse_type(pair : Pair<Rule>) -> Option<Type> {
    match pair.as_rule() {
        Rule::scalar_type => Some(Type::ScalarType(parse_scalar_type(pair).unwrap())),
        Rule::array_type => {
            let mut pairs = pair.into_inner();
            let dims = pairs.next().unwrap().into_inner().map(
                |p| p.into_inner().next().unwrap().as_str().parse::<u32>().unwrap()
            ).collect();
            Some(Type::ArrayType(Rc::new(ArrayType::new(
                parse_scalar_type(pairs.next().unwrap()).unwrap(),
                dims
            ))))
        },
        Rule::optional_type => match pair.into_inner().next() {
                Some(t) => parse_type(t),
                None => None
        },
        _ => None
    }
}
