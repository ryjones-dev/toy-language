use super::expression::Expression;

#[derive(Debug, Clone)]
pub(crate) struct Scope(Vec<Expression>);

impl Scope {
    pub(super) fn new(expressions: Vec<Expression>) -> Self {
        Self(expressions)
    }

    /// Split the scope's return expression from the rest of the scope's body.
    ///
    /// # Panics
    /// Panics if the scope is empty.
    pub(crate) fn split_return(&self) -> (&[Expression], &Expression) {
        let len = self.len();
        let (body, returns) = self.split_at(len - 1);
        let returns = returns.first().unwrap();

        (body, returns)
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

    /// Wrap the last expression in the scope in a [`Expression::FunctionReturn`].
    ///
    /// This is helpful when dealing with function scopes to make codegen more consistent.
    ///
    /// If the scope is empty, an empty function return is inserted.
    pub(crate) fn wrap_function_return(&mut self) {
        // Early out if the scope is empty
        if self.len() == 0 {
            self.0.push(Expression::FunctionReturn {
                expression: Box::new(Expression::ExpressionList {
                    expressions: Vec::new(),
                    source: (0..=0).into(),
                }),
                source: (0..=0).into(),
            });
            return;
        }

        let (_, returns) = self.split_return_mut();
        *returns = Expression::FunctionReturn {
            expression: Box::new(returns.clone()),
            source: returns.source(),
        };
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
