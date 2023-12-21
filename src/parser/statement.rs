use super::{expression::Expression, function::FunctionCall, variable::Variable};

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
    /// An assignment variable can either be a [`Variable`], or the discard identifier ("_"),
    /// which is represented by [`Option::None`.
    Assignment(Vec<Option<Variable>>, Expression),
    /// Call a function with no return values as a free-standing statement.
    /// The function must return no values, otherwise a [`Statement::Assignment`] must be used.
    FunctionCall(FunctionCall),
    Return(Vec<Expression>),
}
