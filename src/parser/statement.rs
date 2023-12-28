use super::{
    expression::Expression, function::FunctionCall, source_range::SourceRange, variable::Variable,
};

/// A TODO_LANG_NAME statement is a single unit of a function's task.
///
/// In concept, a statement can be thought of as a single line of code. In many other languages,
/// a line-ending separator character such as ';' is used to distinguish statements from each other.
/// In TODO_LANG_NAME, an explicit line-ending separator character is not needed, instead opting for whitespace
/// (either a newline character '\n', or a space character ' ' if the code is all on one line).
///
/// In practice, statements often encompasses variable assignments or control flow.
/// Anything in a function that is not just an expression is often a statement.
#[derive(Debug)]
pub(crate) enum Statement {
    /// Assignments must have an equal number of variables compared with the expression's return values.
    ///
    /// If the results of an expression are not needed, they still must be assigned to a variable.
    /// A discarded variable can be used to signify that the results are intentionally being ignored.
    Assignment(Vec<Variable>, Expression),
    /// Call a function with no return values as a free-standing statement.
    /// The function must return no values, otherwise a [`Statement::Assignment`] must be used.
    FunctionCall(FunctionCall),
    Return(Vec<Expression>),
}

impl Statement {
    /// Returns a [`SourceRange`] that captures the entire statement's source code.
    pub(crate) fn source(&self) -> SourceRange {
        match self {
            Statement::Assignment(variables, expression) => {
                todo!("Need to refactor variables that might be discarded")
            }
            Statement::FunctionCall(function_call) => function_call.source(),
            Statement::Return(expressions) => expressions
                .first()
                .expect("there must be at least one return expression")
                .source()
                .combine(
                    expressions
                        .last()
                        .expect("there must be at least one return expression")
                        .source(),
                ),
        }
    }
}
