use super::{
    expression::Expression,
    identifier::Identifier,
    source_range::SourceRange,
    statement::Statement,
    types::{Type, Types},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FunctionParameter {
    pub(crate) name: Identifier,
    pub(crate) ty: Type,
}

impl FunctionParameter {
    pub(super) fn new(name: Identifier, ty: Type) -> Self {
        Self { name, ty }
    }

    /// Returns a [`SourceRange`] starting at the parameter's name and ending at the parameter's type.
    pub(crate) fn source(&self) -> SourceRange {
        self.name.source().combine(self.ty.source())
    }
}

impl std::fmt::Display for FunctionParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

/// A list of related TODO_LANG_NAME parameters.
///
/// Wrapping the list is needed to display better output in error messages when listing parameters.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FunctionParameters(Vec<FunctionParameter>);

impl FunctionParameters {
    pub(crate) fn new() -> Self {
        Self(Vec::new())
    }

    /// Returns a [`SourceRange`] from the beginning of the parameter list to the end.
    /// Returns [`None`] if the parameter list is empty.
    pub(crate) fn source(&self) -> Option<SourceRange> {
        if self.len() > 0 {
            Some(
                self.first()
                    .unwrap()
                    .source()
                    .combine(self.last().unwrap().source()),
            )
        } else {
            None
        }
    }

    pub(crate) fn types(&self) -> Types {
        self.iter().map(|param| param.ty).collect()
    }
}

impl FromIterator<FunctionParameter> for FunctionParameters {
    fn from_iter<T: IntoIterator<Item = FunctionParameter>>(iter: T) -> Self {
        Self(Vec::from_iter(iter))
    }
}

impl<'a> IntoIterator for &'a FunctionParameters {
    type Item = &'a FunctionParameter;
    type IntoIter = std::slice::Iter<'a, FunctionParameter>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl std::ops::Deref for FunctionParameters {
    type Target = Vec<FunctionParameter>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::fmt::Display for FunctionParameters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "`(")?;

        for (i, param) in self.0.iter().enumerate() {
            write!(f, "{}", param)?;

            if i < self.0.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")`")?;

        Ok(())
    }
}

/// The signature of a TODO_LANG_NAME function.
///
/// This includes the function's name, parameters, and return types.
#[derive(Debug, Clone)]
pub(crate) struct FunctionSignature {
    pub(crate) name: Identifier,
    pub(crate) params: FunctionParameters,
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
