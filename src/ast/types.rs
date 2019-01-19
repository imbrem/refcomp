use std::rc::Rc;

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
