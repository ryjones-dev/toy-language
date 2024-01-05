use super::{
    identifier::Identifier, scope::Scope, source_range::SourceRange, types::Types,
    variable::Variables,
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
    pub(crate) body: Scope,
}
