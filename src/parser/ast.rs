use super::function::Function;

/// Represents the complete abstract syntax tree parsed from source code.
///
/// Having a type that represents the tree is more convenient and expressive than passing around a list of nodes.
pub(crate) struct AbstractSyntaxTree(pub(super) Vec<Function>);

impl IntoIterator for AbstractSyntaxTree {
    type Item = Function;
    type IntoIter = std::vec::IntoIter<Function>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl std::ops::Deref for AbstractSyntaxTree {
    type Target = Vec<Function>;

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
        for function in self.iter() {
            writeln!(f, "Function: {}", function.signature.name)?;
            writeln!(f, "{:?}\n", function)?;
        }

        Ok(())
    }
}
