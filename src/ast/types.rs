pub enum ScalarType {
    Integer,
    Boolean
}
pub struct ArrayType {
    pub element_type : ScalarType,
    pub dims : Vec<u32>
}
pub enum ExpressionType {
    ScalarType(ScalarType),
    ArrayType(ArrayType)
}
