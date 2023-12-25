use super::{
    expression::Expression,
    identifier::Identifier,
    statement::Statement,
    types::{Type, Types},
};

#[derive(Debug, Clone)]
pub(crate) struct FunctionParameter {
    pub(crate) name: Identifier,
    pub(crate) ty: Type,
}

impl FunctionParameter {
    pub(super) fn new(name: Identifier, ty: Type) -> Self {
        Self { name, ty }
    }
}

/// The signature of a TODO_LANG_NAME function.
///
/// This includes the function's name, parameters, and return types.
#[derive(Debug, Clone)]
pub(crate) struct FunctionSignature {
    pub(crate) name: Identifier,
    pub(crate) params: Vec<FunctionParameter>,
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
