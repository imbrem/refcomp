use std::rc::Rc;

pub trait Typed {
    fn get_type(&self) -> Type;
}

pub enum ScalarType {
    Integer,
    Boolean
}
pub struct ArrayType {
    pub element_type : ScalarType,
    pub dims : Vec<u32>
}
pub enum Type {
    ScalarType(ScalarType),
    ArrayType(Rc<ArrayType>),
    Null,
    Void
}
