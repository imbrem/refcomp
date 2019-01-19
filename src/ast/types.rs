use std::rc::Rc;

pub trait Typed {
    fn get_type(&self) -> Type;
}
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ScalarType {
    Integer,
    Boolean
}
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ArrayType {
    pub element_type : ScalarType,
    pub dims : Vec<u32>
}
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
    ScalarType(ScalarType),
    ArrayType(Rc<ArrayType>),
    Null,
    Void
}
