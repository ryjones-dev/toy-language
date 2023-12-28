//! Implement conversions to Cranelift's "Type" type so that the parser
//! doesn't need to know about Cranelift at all.
use crate::parser::{
    function::FunctionParameter,
    types::{DataType, Type},
};

impl From<DataType> for cranelift::codegen::ir::Type {
    fn from(value: DataType) -> Self {
        match value {
            DataType::Int => cranelift::codegen::ir::Type::int(64).unwrap(),
            DataType::Bool => cranelift::codegen::ir::Type::int(8).unwrap(),
        }
    }
}

impl From<Type> for cranelift::codegen::ir::Type {
    fn from(value: Type) -> Self {
        let ty: DataType = value.into();
        ty.into()
    }
}

impl From<&Type> for cranelift::codegen::ir::Type {
    fn from(value: &Type) -> Self {
        let ty: DataType = (*value).into();
        ty.into()
    }
}

impl From<&FunctionParameter> for cranelift::codegen::ir::Type {
    fn from(value: &FunctionParameter) -> Self {
        value.ty.into()
    }
}
