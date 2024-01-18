use super::expression::Expression;

#[derive(Debug, Clone)]
pub(crate) struct Scope(Vec<Expression>);

impl Scope {
    pub(super) fn new(expressions: Vec<Expression>) -> Self {
        Self(expressions)
    }

    pub(crate) fn split_into_return(mut self) -> (Vec<Expression>, Option<Expression>) {
        let returns = self.pop();
        (self.0, returns)
    }

    /// Split the scope's return expression from the rest of the scope's body.
    ///
    /// Returns [`None`] if the scope is empty.
    pub(crate) fn split_return(&self) -> Option<(&Expression, &[Expression])> {
        self.split_last()
    }

    /// Split the scope's return expression from the rest of the scope's body.
    ///
    /// Returns [`None`] if the scope is empty.
    pub(crate) fn split_return_mut(&mut self) -> Option<(&mut Expression, &mut [Expression])> {
        self.split_last_mut()
    }

    /// Wrap the last expression in the scope in a [`Expression::FunctionReturn`].
    ///
    /// This is helpful when dealing with function scopes to make codegen more consistent.
    ///
    /// If the scope is empty, an empty function return is inserted.
    pub(crate) fn wrap_function_return(&mut self) {
        if let Some((returns, _)) = self.split_return_mut() {
            let inner = returns.unwrap_transparent();
            match inner {
                // Don't wrap the expression if it's already a function return
                Expression::FunctionReturn { .. } => {}
                _ => {
                    *returns = Expression::FunctionReturn {
                        expression: Box::new(returns.clone()),
                        source: returns.source(),
                    };
                }
            }
        } else {
            self.0.push(Expression::FunctionReturn {
                expression: Box::new(Expression::ExpressionList {
                    expressions: Vec::new(),
                    source: (0..=0).into(),
                }),
                source: (0..=0).into(),
            });
        }
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
