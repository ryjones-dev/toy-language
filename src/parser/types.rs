use thiserror::Error;

/// An error that is thrown when parsing source code fails.
///
/// In the implementation, [`peg`] is used to parse source code.
/// Because [`peg::error::ParseError`] has a generic parameter, it makes it difficult
/// to use in this API. Therefore, [`ParseError`] here is a target type that the underlying
/// [`peg::error::ParseError`] can be mapped to for easier error handling.
/// [`ParseError`] is transparent and just outputs whatever error message [`peg::error::ParseError`] would have.
#[derive(Debug, Error)]
#[error("{0}")]
pub struct ParseError(String);

impl<L: std::fmt::Display> From<peg::error::ParseError<L>> for ParseError {
    fn from(value: peg::error::ParseError<L>) -> Self {
        ParseError(value.to_string())
    }
}

#[derive(Debug, Error)]
#[error("\"{0}\" is not a valid type")]
pub struct ParseTypeError(String);

/// A helper type that keeps track of a region of source code.
///
/// Other AST types will contain one of these so that they can keep track
/// of which part of the source code they represent.
/// This is needed for emiting helpful error messages.
///
/// [`SourceRange`] is convertible to and from [`std::ops::RangeInclusive<usize>`].
/// To construct a [`SourceRange`], convert from a [`std::ops::RangeInclusive<usize>`] (`(1..=3).into()`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct SourceRange {
    start: usize,
    end: usize,
}

impl From<SourceRange> for std::ops::RangeInclusive<usize> {
    fn from(value: SourceRange) -> Self {
        value.start..=value.end
    }
}

impl From<std::ops::RangeInclusive<usize>> for SourceRange {
    fn from(value: std::ops::RangeInclusive<usize>) -> Self {
        Self {
            start: *value.start(),
            end: *value.end(),
        }
    }
}

/// TODO_LANG_NAME built-in data types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
}

impl std::str::FromStr for Type {
    type Err = ParseTypeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "int" => Ok(Type::Int),
            "bool" => Ok(Type::Bool),
            _ => Err(ParseTypeError(s.to_string())),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
        }
    }
}

/// A list of related types.
///
/// A custom type is helpful to display better output in error messages when listing types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Types(Vec<Type>);

impl Types {
    pub(crate) fn new() -> Self {
        Self(Vec::new())
    }
}

impl From<Vec<Type>> for Types {
    fn from(value: Vec<Type>) -> Self {
        Self(value)
    }
}

impl FromIterator<Type> for Types {
    fn from_iter<T: IntoIterator<Item = Type>>(iter: T) -> Self {
        Self(Vec::from_iter(iter))
    }
}

impl<'a> IntoIterator for &'a Types {
    type Item = &'a Type;
    type IntoIter = std::slice::Iter<'a, Type>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl std::ops::Deref for Types {
    type Target = Vec<Type>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for Types {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl std::fmt::Display for Types {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.len() == 1 {
            return write!(f, "{}", self.0[0]);
        }

        write!(f, "(")?;
        for (i, ty) in self.0.iter().enumerate() {
            write!(f, "{}", ty)?;

            if i < self.0.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")?;

        Ok(())
    }
}

/// A distinct type that is used to represent names of functions and variables.
///
/// It contains extra information about where in the source code it is located,
/// which is needed for helpful error messages.
///
/// While there are contexts where using the discard identifier (`"_"`) makes sense, it is not universal,
/// so instead of building it in to the type, the discard identifier is individually considered in the contexts
/// it make sense.
#[derive(Debug, Clone, Eq)]
pub struct Identifier {
    val: String,
    source: SourceRange,
}

impl Identifier {
    pub(super) fn new(val: String, source: SourceRange) -> Self {
        Self { val, source }
    }
}

// The source range should not affect equivalence or hashing
impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.val == other.val
    }
}

impl std::hash::Hash for Identifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.val.hash(state);
    }
}

impl From<Identifier> for String {
    fn from(value: Identifier) -> Self {
        value.val
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.val)
    }
}

/// A distinct type that is used to represent a variable.
///
/// Only the name is parsable from the source code. The type will be [`Option::None`]
/// until semantic analysis can determine the type.
#[derive(Debug, Clone)]
pub(crate) struct Variable {
    pub(crate) name: Identifier,
    pub(crate) ty: Option<Type>,
}

impl Variable {
    pub(crate) fn new(name: Identifier) -> Self {
        Self { name, ty: None }
    }

    pub(super) fn with_type(self, ty: Type) -> Self {
        Self {
            name: self.name,
            ty: Some(ty),
        }
    }
}

/// A distinct type that is used to represent integer literal values.
///
/// It contains extra information about where in the source code it is located,
/// which is needed for helpful error messages.
#[derive(Debug, Clone, Copy)]
pub(crate) struct IntLiteral {
    val: i64,
    source: SourceRange,
}

impl IntLiteral {
    pub(super) fn new(val: i64, source: SourceRange) -> Self {
        Self { val, source }
    }
}

impl From<IntLiteral> for i64 {
    fn from(value: IntLiteral) -> Self {
        value.val
    }
}

/// A distinct type that is used to represent boolean literal values.
///
/// It contains extra information about where in the source code it is located,
/// which is needed for helpful error messages.
#[derive(Debug, Clone, Copy)]
pub(crate) struct BoolLiteral {
    val: bool,
    source: SourceRange,
}

impl BoolLiteral {
    pub(super) fn new(val: bool, source: SourceRange) -> Self {
        Self { val, source }
    }
}

impl From<BoolLiteral> for bool {
    fn from(value: BoolLiteral) -> Self {
        value.val
    }
}

/// Each type of boolean comparison that can be used in an expression.
///
/// This struct is useful when a boolean comparison needs to be addressed separately from an expression.
/// Using a separate comparsion type instead of binding the operands to this enum
/// makes the expression generation code cleaner.
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

/// A container type that represents a function call.
///
/// Because a function call can either be an expression or statement depending on the context,
/// it is helpful to have an underlying type that captures the info needed to make the function call.
///
/// The argument types and return types can't be parsed from the function call expression itself,
/// but can be deduced during semantic analysis.
/// Until then, the argument types and return types will have a value of [`Option::None`].
#[derive(Debug)]
pub(crate) struct FunctionCall {
    pub(crate) name: Identifier,
    pub(crate) arguments: Vec<Expression>,

    pub(crate) argument_types: Option<Types>,
    pub(crate) return_types: Option<Types>,
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
#[derive(Debug)]
pub(crate) enum Expression {
    BooleanComparison(BooleanComparisonType, Box<Expression>, Box<Expression>),
    BinaryMathOperation(BinaryMathOperationType, Box<Expression>, Box<Expression>),
    UnaryMathOperation(UnaryMathOperationType, Box<Expression>),

    FunctionCall(FunctionCall),

    Variable(Variable),
    IntLiteral(IntLiteral),
    BoolLiteral(BoolLiteral),
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
    /// An assignment variable can either be a [`Variable`], or the discard identifier ("_"),
    /// which is represented by [`Option::None`.
    Assignment(Vec<Option<Variable>>, Expression),
    /// Call a function with no return values as a free-standing statement.
    /// The function must return no values, otherwise a [`Statement::Assignment`] must be used.
    FunctionCall(FunctionCall),
    Return(Vec<Expression>),
}

/// The signature of a TODO_LANG_NAME function.
///
/// This includes the function's name, parameters, and return types.
#[derive(Debug, Clone)]
pub(crate) struct FunctionSignature {
    pub(crate) name: Identifier,
    pub(crate) params: Vec<Variable>,
    pub(crate) returns: Types,
}

/// A TODO_LANG_NAME function is a set of parameterized statements that can be executed from other parts of the program.
///
/// This struct contains all of the information that can be parsed from the source code directly.
/// This includes the function's signature as well as all of the statements in the function's body.
#[derive(Debug)]
pub(crate) struct Function {
    pub(crate) signature: FunctionSignature,
    pub(crate) body: Vec<Statement>,
}

/// Represents the complete abstract syntax tree parsed from source code.
///
/// Having a type that represents the tree is more convenient and expressive than passing around a list of nodes.
pub(crate) struct AbstractSyntaxTree(pub(super) Vec<Function>);

impl IntoIterator for AbstractSyntaxTree {
    type Item = Function;
    type IntoIter = std::vec::IntoIter<Function>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl std::ops::Deref for AbstractSyntaxTree {
    type Target = Vec<Function>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for AbstractSyntaxTree {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl std::fmt::Display for AbstractSyntaxTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for function in self.iter() {
            writeln!(f, "Function: {}", function.signature.name)?;
            writeln!(f, "{:?}\n", function)?;
        }

        Ok(())
    }
}
