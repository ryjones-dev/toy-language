//! Implement conversions to Cranelift's "Type" type so that the parser
//! doesn't need to know about Cranelift at all.
use crate::{
    parser::{
        types::{DataType, Type},
        variable::Variable,
    },
    semantic::EXPECT_VAR_TYPE,
};

impl From<DataType> for cranelift::codegen::ir::Type {
    fn from(value: DataType) -> Self {
        match value {
            DataType::Int => cranelift::codegen::ir::types::I64,
            DataType::Float => cranelift::codegen::ir::types::F64,
            DataType::Bool => cranelift::codegen::ir::types::I8,
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

impl From<&Variable> for cranelift::codegen::ir::Type {
    fn from(value: &Variable) -> Self {
        value.get_type().expect(EXPECT_VAR_TYPE).into()
    }
}
