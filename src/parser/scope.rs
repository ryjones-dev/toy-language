use super::expression::Expression;

#[derive(Debug)]
pub(crate) struct Scope(Vec<Expression>);

impl Scope {
    pub(super) fn new(expressions: Vec<Expression>) -> Self {
        Self(expressions)
    }

    /// Split the scope's return expression from the rest of the scope's body.
    ///
    /// # Panics
    /// Panics if the scope is empty.
    pub(crate) fn split_return_mut(&mut self) -> (&mut [Expression], &mut Expression) {
        let len = self.len();
        let (body, returns) = self.split_at_mut(len - 1);
        let returns = returns.first_mut().unwrap();

        (body, returns)
    }
}

impl std::ops::Deref for Scope {
    type Target = Vec<Expression>;

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
    type Item = Expression;
    type IntoIter = std::vec::IntoIter<Expression>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
