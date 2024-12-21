//! Implement conversions to Cranelift's "Type" type so that the parser
//! doesn't need to know about Cranelift at all.
use crate::{
    parser::{
        types::{DataType, Type},
        variable::Variable,
    },
    semantic::EXPECT_VAR_TYPE,
};

const EXPECT_STRUCT: &str = "struct type should have been linked to a struct by this point";

impl DataType {
    // TODO: This doesn't work yet but isn't needed
    // pub(crate) fn size(&self) -> u32 {
    //     match self {
    //         DataType::Int => 8,
    //         DataType::Float => 8,
    //         DataType::Bool => 1,
    //         DataType::Struct { _struct, .. } => {
    //             let alignment = self.alignment() as u32;

    //             let mut total_size = 0;
    //             for member_type in _struct
    //                 .as_ref()
    //                 .expect(EXPECT_STRUCT)
    //                 .members()
    //                 .iter()
    //                 .map(|member| Into::<&DataType>::into(member.get_type()))
    //             {
    //                 let size = member_type.size();
    //                 total_size += size + (alignment - size % alignment);
    //             }
    //         }
    //     }
    // }

    // pub(crate) fn alignment(&self) -> u8 {
    //     match self {
    //         DataType::Int => 8,
    //         DataType::Float => 8,
    //         DataType::Bool => 1,
    //         DataType::Struct { _struct, .. } => {
    //             let _struct = _struct.as_ref().expect(EXPECT_STRUCT);
    //             let mut max_alignment = 0;
    //             for member_type in _struct
    //                 .members()
    //                 .iter()
    //                 .map(|member| Into::<&DataType>::into(member.get_type()))
    //             {
    //                 let member_alignment = member_type.alignment();
    //                 if member_alignment > max_alignment {
    //                     max_alignment = member_alignment;
    //                 }
    //             }

    //             max_alignment
    //         }
    //     }
    // }

    /// Returns a representation of this data type as a list of primitive data types.
    ///
    /// For primitive data types like int and float, this is just a list of 1 element.
    /// For composite data types like structs, this is a list of all of the primitive types
    /// that make up that composite data type.
    pub(crate) fn primitive_types(self) -> Vec<DataType> {
        match self {
            DataType::Int => vec![self],
            DataType::Float => vec![self],
            DataType::Bool => vec![self],
            DataType::Struct { _struct, .. } => {
                let mut types = Vec::new();
                for member in _struct.expect(EXPECT_STRUCT).into_members() {
                    types.append(&mut DataType::from(member.into_type()).primitive_types());
                }
                types
            }
        }
    }
}

impl From<DataType> for cranelift::codegen::ir::Type {
    fn from(value: DataType) -> Self {
        match value {
            DataType::Int => cranelift::codegen::ir::types::I64,
            DataType::Float => cranelift::codegen::ir::types::F64,
            DataType::Bool => cranelift::codegen::ir::types::I8,
            DataType::Struct { .. } => {
                panic!("structs do not have a cranelift native type mapping")
            }
        }
    }
}

impl From<Type> for cranelift::codegen::ir::Type {
    fn from(value: Type) -> Self {
        let ty: DataType = value.into();
        ty.into()
    }
}

impl From<Variable> for cranelift::codegen::ir::Type {
    fn from(value: Variable) -> Self {
        value.get_type().clone().expect(EXPECT_VAR_TYPE).into()
    }
}
