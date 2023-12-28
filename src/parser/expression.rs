use super::{
    function::FunctionCall,
    literals::{BoolLiteral, IntLiteral},
    variable::Variable,
};

/// Each type of boolean comparison that can be used in an expression.
///
/// This struct is useful when a boolean comparison needs to be addressed separately from an expression.
/// Using a separate comparsion type instead of binding the operands to this enum
/// makes the expression generation code cleaner.
#[derive(Debug, Clone, Copy)]
pub(crate) enum BooleanComparisonType {
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

/// Each type of binary math operation that can be used in an expression.
///
/// This works similarly to [`BooleanComparisonType`], but is used for math operations with two operands.
#[derive(Debug, Clone, Copy)]
pub(crate) enum BinaryMathOperationType {
    Add,
    Subtract,
    Multiply,
    Divide,
}

/// Each type of unary math operation that can be used in an expression.
///
/// This works similarly to [`BinaryMathOperationType`], but is used for math operations with only one operand.
#[derive(Debug, Clone, Copy)]
pub(crate) enum UnaryMathOperationType {
    Negate,
}

/// A TODO_LANG_NAME expression that, when evaluated, can return zero or more values.
///
/// An [`Expression`] can be thought of in terms of rvalues in C++ semantics -
/// it represents any source code that doesn't necessarily have any storage associated with it after evaluation.
/// This is commonly paired with a [`Statement::Assignment`], so that the result of the expression
/// can be stored in a variable for later use in a function.
/// In fact, in most cases a freestanding expression will not compile without an assignment to a variable.
/// An exception to this is [`Expression::FunctionCall`], which may not return any value and makes sense to
/// write without an assignment.
///
/// It is worth noting that variables and literals are expressions as well.
/// [`Expression::IntLiteral`] is a more obvious example as it literally represents a value, but a [`Expression::Variable`]
/// is also an expression, since evaluating the variable means to return its currently stored value.
///
/// The [`Expression`] type itself does not contain any information on what the expression returns.
/// Its value can only be determined when the expression is evaluated at runtime.
/// Instead, the type stores the information needed to generate the Cranelift IR to represent the expression.
///
/// Expressions support recursive generation, and in those cases the inner expression must be wrapped
/// in a container type such as [`Box`] or [`Vec`].
#[derive(Debug, Clone)]
pub(crate) enum Expression {
    BooleanComparison(BooleanComparisonType, Box<Expression>, Box<Expression>),
    BinaryMathOperation(BinaryMathOperationType, Box<Expression>, Box<Expression>),
    UnaryMathOperation(UnaryMathOperationType, Box<Expression>),

    FunctionCall(FunctionCall),

    Variable(Variable),
    IntLiteral(IntLiteral),
    BoolLiteral(BoolLiteral),
}
