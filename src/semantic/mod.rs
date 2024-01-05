use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, DiagnosticContext, DiagnosticLevel, DiagnosticMessage},
    parser::{ast::AbstractSyntaxTree, function::FunctionSignature},
};

use self::{
    diagnostic::{diag_newly_defined, diag_originally_defined},
    function::{analyze_function, FunctionError},
    scope_tracker::{ScopeError, ScopeTracker},
};

mod diagnostic;
mod expression;
mod function;
mod scope_tracker;
mod statement;

pub(crate) const EXPECT_VAR_TYPE: &str = "variable should have a type by this point";
pub(crate) const EXPECT_FUNC_SIG: &str = "function signature should be set by this point";

#[derive(Debug, Error)]
pub(super) enum SemanticError {
    #[error("main function is not defined")]
    MissingMainError,
    #[error("duplicate function definition")]
    FunctionAlreadyDefinedError {
        original: FunctionSignature,
        new: FunctionSignature,
    },
    #[error(transparent)]
    FunctionError(#[from] FunctionError),
    #[error(transparent)]
    ScopeError(#[from] ScopeError),
}

impl From<SemanticError> for Diagnostic {
    fn from(err: SemanticError) -> Self {
        match err {
            SemanticError::MissingMainError => Self::new(&err, DiagnosticLevel::Error),
            SemanticError::FunctionAlreadyDefinedError {
                ref original,
                ref new,
            } => Self::new(&err, DiagnosticLevel::Error)
                .with_context(
                    DiagnosticContext::new(DiagnosticMessage::new(
                        format!("function `{}` already defined in this scope", new.name),
                        new.source,
                    ))
                    .with_labels(vec![
                        diag_originally_defined(original.source, None),
                        diag_newly_defined(new.source, None),
                    ]),
                )
                .with_suggestions({
                    let mut suggestions = Vec::new();
                    if original.params != new.params {
                        suggestions.push("TODO_LANG_NAME does not support parameter overloading");
                    }
                    if original.returns != new.returns {
                        suggestions.push("TODO_LANG_NAME does not support return type overloading");
                    }
                    suggestions
                }),
            SemanticError::FunctionError(err) => err.into(),
            SemanticError::ScopeError(err) => err.into(),
        }
    }
}

pub(crate) fn semantic_analysis(ast: &mut AbstractSyntaxTree) -> Vec<SemanticError> {
    let mut errors = Vec::new();

    let mut global_scope_tracker = ScopeTracker::new(None);

    // Add each function to the global scope.
    // This needs to be done first so that function definition order does not matter.
    for function in ast.iter() {
        if let Some(func_sig) = global_scope_tracker.insert_func_sig(function.signature.clone()) {
            errors.push(SemanticError::FunctionAlreadyDefinedError {
                original: func_sig.clone(),
                new: function.signature.clone(),
            });
        }
    }

    // Analyze each function
    for function in ast.iter_mut() {
        let errs = analyze_function(function, &mut global_scope_tracker);
        errors.append(
            &mut errs
                .into_iter()
                .map(|err| SemanticError::FunctionError(err))
                .collect(),
        );
    }

    if !global_scope_tracker.has_main_func() {
        errors.push(SemanticError::MissingMainError);
    }

    // Check for any unused variables or functions in the global scope that have not been used
    let (unused_variables, function_signatures) = global_scope_tracker.get_unused();
    for variable in unused_variables {
        errors.push(SemanticError::ScopeError(ScopeError::UnusedVariableError {
            variable,
        }))
    }
    for func_sig in function_signatures {
        // main function can be unused
        if func_sig.name.to_string() != "main" {
            errors.push(SemanticError::ScopeError(ScopeError::UnusedFunctionError {
                function_signature: func_sig,
            }))
        }
    }

    errors
}
