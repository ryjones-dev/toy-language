use super::{
    function::FunctionSignature,
    identifier::Identifier,
    source_range::SourceRange,
    variable::{Variable, Variables},
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
/// Everything is an expression in TODO_LANG_NAME. This is different compared to other languages like C++ that have rvalue semantics.
/// Even assignment "statements" are expressions, they just return no values. See [`Expression::Assignment`].
///
/// In TODO_LANG_NAME, if an expression returns a value, that value must be assigned to a variable using an assignment expression,
/// otherwise the program won't compile.
/// If an expression such as a [`Expression::FunctionCall`] returns no values, then it does not need to be assigned to a variable
/// as there is nothing to assign.
///
/// It is worth noting that variables and literals are expressions as well.
/// [`Expression::IntLiteral`] is a more obvious example as it literally represents a value, but a [`Expression::Variable`]
/// is also an expression, since evaluating the variable means to return its currently stored value.
///
/// The [`Expression`] type itself does not contain any information on what value the expression returns.
/// Its value can only be determined when the expression is evaluated at runtime.
/// Instead, the type stores the information needed to perform semantic analysis and to
/// generate the Cranelift IR representing the expression.
///
/// Expressions support recursive evaluation, and in those cases the inner expression must be wrapped
/// in a container type such as [`Box`] or [`Vec`].
#[derive(Debug, Clone)]
pub(crate) enum Expression {
    /// A wrapped list of inner expressions.
    ///
    /// Having an [`Expression`] be recursive like this allows for simpler parsing of scope return values,
    /// and can be used in other places like function call arguments or assignments.
    ExpressionList {
        expressions: Vec<Expression>,
        source: SourceRange,
    },
    /// Assignments must have an equal number of variables compared with the expression's return values.
    ///
    /// If the results of an expression are not needed, they still must be assigned to a variable.
    /// A discarded variable can be used to signify that the results are intentionally being ignored.
    Assignment {
        variables: Variables,
        expression: Box<Expression>,
        source: SourceRange,
    },
    /// Represents returning from a function. This is useful for early returning from an outer function scope.
    FunctionReturn {
        expression: Box<Expression>,
        source: SourceRange,
    },
    /// The called function signature can't be parsed from the function call expression itself,
    /// but can be deduced during semantic analysis.
    /// Until then, the function signature will have a value of [`None`].
    FunctionCall {
        name: Identifier,
        argument_expression: Box<Option<Expression>>,
        source: SourceRange,
        function_signature: Option<FunctionSignature>,
    },
    BooleanComparison {
        comparison_type: BooleanComparisonType,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        source: SourceRange,
    },
    BinaryMathOperation {
        operation_type: BinaryMathOperationType,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        source: SourceRange,
    },
    UnaryMathOperation {
        operation_type: UnaryMathOperationType,
        expression: Box<Expression>,
        source: SourceRange,
    },
    Variable(Variable),
    IntLiteral(i64, SourceRange),
    BoolLiteral(bool, SourceRange),
}

impl Expression {
    /// Returns a [`SourceRange`] that captures the expression.
    pub(crate) fn source(&self) -> SourceRange {
        match self {
            Expression::ExpressionList { source, .. } => *source,
            Expression::Assignment { source, .. } => *source,
            Expression::FunctionReturn { source, .. } => *source,
            Expression::BooleanComparison { source, .. } => *source,
            Expression::BinaryMathOperation { source, .. } => *source,
            Expression::UnaryMathOperation { source, .. } => *source,
            Expression::FunctionCall { source, .. } => *source,
            Expression::Variable(variable) => variable.source(),
            Expression::IntLiteral(_, source) => *source,
            Expression::BoolLiteral(_, source) => *source,
        }
    }
}
