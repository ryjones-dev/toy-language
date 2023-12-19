use std::collections::HashMap;

use crate::parser::types::Identifier;

pub(super) struct BlockVariables {
    variables: HashMap<Identifier, u32>,
    var_index: u32,
}

impl BlockVariables {
    pub(super) fn new() -> Self {
        Self {
            variables: HashMap::new(),
            var_index: 0,
        }
    }

    pub(super) fn var(&mut self, name: Identifier) -> u32 {
        match self.variables.get(&name) {
            Some(var_index) => *var_index,
            None => {
                let index = self.var_index;
                self.variables.insert(name, index);
                self.var_index += 1;
                index
            }
        }
    }
}
