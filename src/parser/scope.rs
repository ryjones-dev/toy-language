use super::statement::Statement;

#[derive(Debug)]
pub(crate) struct Scope(Vec<Statement>);

impl Scope {
    pub(super) fn new(statements: Vec<Statement>) -> Self {
        Self(statements)
    }

    pub(crate) fn num_statements(&self) -> usize {
        self.0.len()
    }
}

impl std::ops::Deref for Scope {
    type Target = Vec<Statement>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for Scope {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl IntoIterator for Scope {
    type Item = Statement;
    type IntoIter = std::vec::IntoIter<Statement>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
