//! Implement conversions to Cranelift's "Type" type so that the parser
//! doesn't need to know about Cranelift at all.
use crate::{
    parser::{
        types::{DataType, Type},
        variable::Variable,
    },
    semantic::{EXPECT_STRUCT, EXPECT_VAR_TYPE},
};

/// Defines how the parameter is used in the function.
pub(super) enum ParameterMode {
    Input,
    Output,
}

/// Defines a function parameter used in function codegen.
pub(super) struct FunctionParam<'a> {
    data_type: &'a DataType,
    mode: ParameterMode,
}

impl<'a> FunctionParam<'a> {
    pub(super) fn new(data_type: &'a DataType, mode: ParameterMode) -> FunctionParam<'a> {
        Self { data_type, mode }
    }
}

impl From<FunctionParam<'_>> for cranelift::codegen::ir::AbiParam {
    fn from(value: FunctionParam) -> Self {
        match value.data_type {
            DataType::Int => cranelift::codegen::ir::AbiParam::new(value.data_type.into()),
            DataType::Float => cranelift::codegen::ir::AbiParam::new(value.data_type.into()),
            DataType::Bool => cranelift::codegen::ir::AbiParam::new(value.data_type.into()),
            DataType::Struct { .. } => match value.mode {
                ParameterMode::Input => cranelift::codegen::ir::AbiParam::special(
                    cranelift::codegen::ir::types::I64, // Pointer type
                    cranelift::codegen::ir::ArgumentPurpose::StructArgument(
                        value.data_type.clone().layout(0).size,
                    ),
                ),
                ParameterMode::Output => cranelift::codegen::ir::AbiParam::special(
                    cranelift::codegen::ir::types::I64, // Pointer type
                    cranelift::codegen::ir::ArgumentPurpose::StructReturn,
                ),
            },
        }
    }
}

/// Describes the size and offset information for a [`DataType`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct DataTypeLayout {
    pub(super) data_type: DataType,
    pub(super) size: u32,
    pub(super) offset: u32,
    pub(super) members: Vec<DataTypeLayout>,
}

impl DataType {
    /// Calculates the layout of the data type, including the size in bytes
    /// as well as each member's offset if the data type is a [`DataType::Struct`].
    ///
    /// For primitive data types, the size is a well defined value and the offset is the given base offset.
    /// For struct data types, the size is calculated based on the size of each of its members, factoring in alignment rules.
    /// The offset of each member will be calculated based on each member's size, factoring in alignment rules,
    /// shifted by the given base offset.
    pub(super) fn layout(self, base_offset: u32) -> DataTypeLayout {
        match self {
            DataType::Int => DataTypeLayout {
                data_type: self,
                size: 8,
                offset: base_offset,
                members: vec![],
            },
            DataType::Float => DataTypeLayout {
                data_type: self,
                size: 8,
                offset: base_offset,
                members: vec![],
            },
            DataType::Bool => DataTypeLayout {
                data_type: self,
                size: 1,
                offset: base_offset,
                members: vec![],
            },
            DataType::Struct {
                ref struct_data_types,
                ..
            } => {
                let mut members = Vec::new();
                let mut total_size = 0;
                for data_type in struct_data_types.as_ref().expect(EXPECT_STRUCT) {
                    let mut layout = data_type.clone().layout(base_offset + total_size);
                    members.append(&mut layout.members);
                    let size = layout.size;

                    // Add padding if this data type would cause misalignment
                    total_size += self.padding(total_size, size);

                    total_size += size;
                }

                // Add padding if the struct ends misaligned
                total_size += self.padding(total_size, 0);

                DataTypeLayout {
                    data_type: self,
                    size: total_size,
                    offset: base_offset,
                    members,
                }
            }
        }
    }

    /// Calculates the alignment of the data type in bytes.
    ///
    /// For primitive data types, this is a well defined value.
    /// For composite data types like structs, this is the largest alignment of its members.
    pub(super) fn alignment(&self) -> u8 {
        match self {
            DataType::Int => 8,
            DataType::Float => 8,
            DataType::Bool => 1,
            DataType::Struct {
                struct_data_types, ..
            } => {
                let struct_data_types = struct_data_types.as_ref().expect(EXPECT_STRUCT);
                let mut max_alignment = 0;
                for data_type in struct_data_types {
                    let member_alignment = data_type.alignment();
                    if member_alignment > max_alignment {
                        max_alignment = member_alignment;
                    }
                }

                max_alignment
            }
        }
    }

    /// Returns an amount to pad to align the struct member given the current total struct size.
    fn padding(&self, total_size: u32, member_size: u32) -> u32 {
        let alignment = self.alignment() as u32;

        let remaining_size_in_alignment = alignment - total_size % alignment;
        if remaining_size_in_alignment + member_size > alignment {
            remaining_size_in_alignment
        } else {
            0
        }
    }
}

impl From<DataType> for cranelift::codegen::ir::Type {
    fn from(value: DataType) -> Self {
        (&value).into()
    }
}

impl From<&DataType> for cranelift::codegen::ir::Type {
    fn from(value: &DataType) -> Self {
        match value {
            &DataType::Int => cranelift::codegen::ir::types::I64,
            &DataType::Float => cranelift::codegen::ir::types::F64,
            &DataType::Bool => cranelift::codegen::ir::types::I8,
            &DataType::Struct { .. } => {
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
