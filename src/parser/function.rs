use super::{
    expression::Expression, identifier::Identifier, source_range::SourceRange,
    statement::Statement, types::Types, variable::Variables,
};

/// The signature of a TODO_LANG_NAME function.
///
/// This includes the function's name, parameters, and return types.
#[derive(Debug, Clone)]
pub(crate) struct FunctionSignature {
    pub(crate) name: Identifier,
    pub(crate) params: Variables,
    pub(crate) returns: Types,
    pub(crate) source: SourceRange,
}

impl FunctionSignature {
    pub(crate) fn is_discarded(&self) -> bool {
        self.name.is_discarded()
    }
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

/// A container type that represents a function call.
///
/// Because a function call can either be an expression or statement depending on the context,
/// it is helpful to have an underlying type that captures the info needed to make the function call.
///
/// The called function signature can't be parsed from the function call expression itself,
/// but can be deduced during semantic analysis.
/// Until then, the function signature will have a value of [`None`].
#[derive(Debug, Clone)]
pub(crate) struct FunctionCall {
    pub(crate) name: Identifier,
    pub(crate) arguments: Vec<Expression>,
    pub(crate) source: SourceRange,

    pub(crate) function_signature: Option<FunctionSignature>,
}
