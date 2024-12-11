use super::definition::Definition;

/// Represents the complete abstract syntax tree parsed from source code.
///
/// Having a type that represents the tree is more convenient and expressive than passing around a list of nodes.
pub(crate) struct AbstractSyntaxTree(pub(super) Vec<Definition>);

impl IntoIterator for AbstractSyntaxTree {
    type Item = Definition;
    type IntoIter = std::vec::IntoIter<Definition>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl std::ops::Deref for AbstractSyntaxTree {
    type Target = Vec<Definition>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for AbstractSyntaxTree {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl std::fmt::Display for AbstractSyntaxTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for definition in self.iter() {
            match definition {
                Definition::Struct(_struct) => writeln!(f, "Struct: {}", _struct.name())?,
                Definition::Function(function) => {
                    writeln!(f, "Function: {}", function.signature.name)?
                }
            }

            writeln!(f, "{:?}\n", definition)?;
        }

        Ok(())
    }
}
