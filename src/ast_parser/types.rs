use std::{fmt::Display, num::ParseIntError, str::FromStr};

/// A distinct type that is used to represent names of functions and variables.
///
/// This is just a wrapper around [`String`] and is completely convertable to and from [`String`].
/// It is intended to be used in places where it semantically makes sense to represent
/// a parsed identifier from source code.
/// If [`String`] was used directly, this context would be lost on the reader.
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Identifier(String);

impl From<Identifier> for String {
    fn from(value: Identifier) -> Self {
        value.0
    }
}

impl From<String> for Identifier {
    fn from(value: String) -> Self {
        Self { 0: value }
    }
}

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Self {
            0: value.to_owned(),
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A distinct type that is used to represent integer literal values.
///
/// This is just a wrapper around [`i64`] and is completely convertable to and from [`i64`].
/// Parsing directly from a string to [`IntLiteral`] is also supported.
/// It is intended to be used in places where it semantically makes sense to represent
/// a parsed integer literal from source code.
/// If [`i64`] was used directly, this context would be lost on the reader.
#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub(crate) struct IntLiteral(i64);

impl FromStr for IntLiteral {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let value = s.parse()?;
        Ok(Self { 0: value })
    }
}

impl From<IntLiteral> for i64 {
    fn from(value: IntLiteral) -> Self {
        value.0
    }
}

impl From<i64> for IntLiteral {
    fn from(value: i64) -> Self {
        Self(value)
    }
}

/// Each type of boolean comparison that can be used in an expression.
///
/// This struct is useful when a boolean comparison needs to be addressed separately from an expression.
/// Using a separate comparsion type instead of binding the operands to this enum
/// makes the expression evaluation code cleaner.
#[derive(Debug)]
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
#[derive(Debug)]
pub(crate) enum BinaryMathOperationType {
    Add,
    Subtract,
    Multiply,
    Divide,
}

/// Each type of unary math operation that can be used in an expression.
///
/// This works similarly to [`BinaryMathOperationType`], but is used for math operations with only one operand.
#[derive(Debug)]
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
/// Its value can only be determined when the expression is evaluated.
/// Instead, the type stores the information needed to evaluate the expression.
///
/// Expressions support recursive evaluation, and in those cases the inner expression must be wrapped
/// in a container type such as [`Box`] or [`Vec`].
#[derive(Debug)]
pub(crate) enum Expression {
    BooleanComparison(BooleanComparisonType, Box<Expression>, Box<Expression>),
    BinaryMathOperation(BinaryMathOperationType, Box<Expression>, Box<Expression>),
    UnaryMathOperation(UnaryMathOperationType, Box<Expression>),
    FunctionCall(Identifier, Vec<Expression>),
    Variable(Identifier),
    IntLiteral(IntLiteral),
}

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
    Assignment(Vec<Identifier>, Expression),
    FunctionCall(Vec<Expression>),
    Return(Vec<Expression>),
}

/// A TODO_LANG_NAME function is a set of parameterized statements that can be executed from other parts of the program.
///
/// This struct contains all of the information that can be parsed from the source code directly.
/// This includes the function's signature, which is the function's name and parameters, as well as all of the statements
/// in the function's body. Note that the return values are not part of the function's signature in TODO_LANG_NAME,
/// so the return values and types of the function must be determined by evaluating the return expressions at compile time.
#[derive(Debug)]
pub struct Function {
    pub(crate) name: Identifier,
    pub(crate) params: Vec<Identifier>,
    pub(crate) body: Vec<Statement>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_convert_string_identifier() {
        let str = "identifier";
        let string = String::from(str);

        let identifier_string = Identifier::from(string.clone());
        let converted_string = String::from(identifier_string.clone());
        assert_eq!(string, converted_string);

        let identifier_str = Identifier::from(str);
        assert_eq!(identifier_string, identifier_str);
    }

    #[test]
    fn test_display_identifier() {
        let expected = String::from("identifier");
        let identifier = Identifier::from(expected.clone());
        let actual = format!("{}", identifier);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_convert_i64_int_literal() {
        let num = 10;

        let int_literal = IntLiteral::from(num);
        let converted_num = i64::from(int_literal);
        assert_eq!(num, converted_num,);
    }

    #[test]
    fn test_parse_int_literal() -> Result<(), ParseIntError> {
        let num_str = "10";
        let int_literal = IntLiteral::from_str(num_str)?;
        assert_eq!(int_literal.0, 10);
        Ok(())
    }
}
