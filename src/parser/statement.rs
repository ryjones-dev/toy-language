use super::{expression::Expression, source_range::SourceRange, variable::Variables};

/// A TODO_LANG_NAME statement is a single unit of a function's task.
///
/// In concept, a statement can be thought of as a single line of code. In many other languages,
/// a line-ending separator character such as ';' is used to distinguish statements from each other.
/// In TODO_LANG_NAME, an explicit line-ending separator character is not needed, instead opting for whitespace
/// (either a newline character '\n', or a space character ' ' if the code is all on one line).
///
/// In practice, statements often encompass things like variable assignments or function returns.
/// Anything in a function that is not just an expression is often a statement.
#[derive(Debug, Clone)]
pub(crate) enum Statement {
    /// Assignments must have an equal number of variables compared with the expression's return values.
    ///
    /// If the results of an expression are not needed, they still must be assigned to a variable.
    /// A discarded variable can be used to signify that the results are intentionally being ignored.
    Assignment {
        variables: Variables,
        expression: Expression,
        source: SourceRange,
    },
    /// Represents expression values returned from a scope. This is different than [`Statement::FunctionReturn`],
    /// although can be used instead of [`Statement::FunctionReturn`] at the end of a function scope.
    ScopeReturn {
        expressions: Vec<Expression>,
        source: SourceRange,
    },
    /// Represents returning from a function. This is more specific than [`Statement::ScopeReturn`],
    /// and allows for early returning from a function scope.
    FunctionReturn {
        expressions: Vec<Expression>,
        source: SourceRange,
    },
}

impl Statement {
    pub(crate) fn source(&self) -> SourceRange {
        match self {
            Statement::Assignment { source, .. } => *source,
            Statement::ScopeReturn { source, .. } => *source,
            Statement::FunctionReturn { source, .. } => *source,
        }
    }
}
