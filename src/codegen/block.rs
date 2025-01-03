use std::collections::HashMap;

use crate::{
    parser::{types::DataType, variable::Variable},
    semantic::EXPECT_VAR_TYPE,
};

/// Keeps track of data need to generate a variable.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct BlockVariable {
    pub(super) index: u32,
    pub(super) ty: DataType,
}

pub(super) struct BlockVariables {
    variables: HashMap<String, Vec<BlockVariable>>,
    index_counter: u32,
}

impl BlockVariables {
    pub(super) fn new() -> Self {
        Self {
            variables: HashMap::new(),
            index_counter: 0,
        }
    }

    /// Returns a list of [`BlockVariable`]s that are associated with this [`Variable`].
    ///
    /// A semantic [`Variable`] may decompose into multiple [`BlockVariable`]s
    /// if the [`Variable`] represents a composite type such as a [`Struct`].
    pub(super) fn block_vars(&mut self, variable: Variable) -> Vec<BlockVariable> {
        // Key the variable on its name and type so that variables with the same name that are used
        // during the other's assignment can have different types.
        let var_key = format!(
            "{}:{}",
            variable.name(),
            variable.get_type().as_ref().expect(EXPECT_VAR_TYPE)
        );
        match self.variables.get(&var_key) {
            Some(var_indices) => return var_indices.clone(),
            None => {
                let block_vars =
                    DataType::from(variable.get_type().as_ref().expect(EXPECT_VAR_TYPE).clone())
                        .primitive_types()
                        .into_iter()
                        .enumerate()
                        .map(|(i, data_type)| BlockVariable {
                            index: self.index_counter + i as u32,
                            ty: data_type,
                        })
                        .collect::<Vec<BlockVariable>>();

                self.index_counter += block_vars.len() as u32;
                self.variables.insert(var_key, block_vars.clone());

                block_vars
            }
        }
    }
}
