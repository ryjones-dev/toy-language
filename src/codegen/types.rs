use crate::parser::types::Type;

// Implement conversion to Cranelift's "Type" type here so that the parser
// doesn't need to know about Cranelift at all.
impl From<Type> for cranelift::codegen::ir::Type {
    fn from(value: Type) -> Self {
        match value {
            Type::Int => cranelift::codegen::ir::Type::int(64).unwrap(),
            Type::Bool => cranelift::codegen::ir::Type::int(8).unwrap(),
        }
    }
}
