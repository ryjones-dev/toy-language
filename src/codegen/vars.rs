use std::collections::HashMap;

use crate::{
    parser::{types::DataType, variable::Variable},
    semantic::EXPECT_VAR_TYPE,
};

use super::types::DataTypeLayout;

/// Keeps track of data need to generate a [`Variable`].
///
/// Code generation between primitive types (ex. ints and floats) are handled differently
/// than composite types (ex. structs).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum CodegenVariable {
    Primitive {
        data_type: DataType,
        index: u32,
    },
    Composite {
        layout: DataTypeLayout,
        indices: Vec<u32>,
    },
}

pub(super) struct CodegenVariables {
    variables: HashMap<String, CodegenVariable>,
    index_counter: u32,
}

impl CodegenVariables {
    pub(super) fn new() -> Self {
        Self {
            variables: HashMap::new(),
            index_counter: 0,
        }
    }

    /// Returns a [`CodegenVariable`] that is associated with this [`Variable`].
    ///
    /// A [`CodegenVariable`] could represent either a primitive type or a composite type,
    /// depending on the variable's type.
    ///
    /// If the [`Variable`] doesn't yet have a [`CodegenVariable`], this method will create and return it.
    pub(super) fn codegen_var(&mut self, variable: Variable) -> &CodegenVariable {
        // Key the variable on its name and type so that variables with the same name that are used
        // during the other's assignment can have different types.
        let var_key = format!(
            "{}:{}",
            variable.name(),
            variable.get_type().as_ref().expect(EXPECT_VAR_TYPE)
        );

        // TODO: It would be more convenient to only access the map once,
        // but borrow checker limitations prevent accessing the map in the Some block
        // but inserting into the map in the None block.
        // When Polonius is released this should be fixed.
        // See https://rust-lang.github.io/rfcs/2094-nll.html#problem-case-3-conditional-control-flow-across-functions
        // for a description of the problem.
        if self.variables.contains_key(&var_key) {
            return self
                .variables
                .get(&var_key)
                .expect("codegen var just checked");
        } else {
            let data_type: DataType = variable.into_type().expect(EXPECT_VAR_TYPE).into();
            let layout = data_type.layout(0);

            let codegen_var;
            match layout.data_type {
                DataType::Struct { .. } => {
                    let indices = layout
                        .members
                        .iter()
                        .map(|_| {
                            let index = self.index_counter;
                            self.index_counter += 1;
                            index
                        })
                        .collect();

                    codegen_var = CodegenVariable::Composite { layout, indices };
                }
                data_type => {
                    codegen_var = CodegenVariable::Primitive {
                        data_type,
                        index: self.index_counter,
                    };
                    self.index_counter += 1;
                }
            }

            self.variables.insert(var_key.clone(), codegen_var);
        }

        self.variables
            .get(&var_key)
            .expect("codegen var just added")
    }
}
