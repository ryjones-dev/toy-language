use crate::parser::types::DataType;

// Implement conversion to Cranelift's "Type" type here so that the parser
// doesn't need to know about Cranelift at all.
impl From<DataType> for cranelift::codegen::ir::Type {
    fn from(value: DataType) -> Self {
        match value {
            DataType::Int => cranelift::codegen::ir::Type::int(64).unwrap(),
            DataType::Bool => cranelift::codegen::ir::Type::int(8).unwrap(),
        }
    }
}
