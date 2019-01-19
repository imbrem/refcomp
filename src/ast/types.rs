use std::rc::Rc;

pub trait Typed {
    fn get_type(&self) -> Type;
}
#[derive(Clone)]
pub enum ScalarType {
    Integer,
    Boolean
}
#[derive(Clone)]
pub struct ArrayType {
    pub element_type : ScalarType,
    pub dims : Vec<u32>
}
#[derive(Clone)]
pub enum Type {
    ScalarType(ScalarType),
    ArrayType(Rc<ArrayType>),
    Null,
    Void
}
