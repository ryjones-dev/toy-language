use std::collections::HashMap;

use crate::{
    parser::{identifier::Identifier, types::DataType, variable::Variable},
    semantic::EXPECT_VAR_TYPE,
};

/// Keeps track of data need to generate a variable.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct BlockVariable {
    pub(super) index: u32,
    pub(super) ty: DataType,
}

pub(super) struct BlockVariables {
    variables: HashMap<Identifier, Vec<BlockVariable>>,
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
        match self.variables.get(variable.name()) {
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
                self.variables
                    .insert(variable.name().clone(), block_vars.clone());

                block_vars
            }
        }
    }
}
