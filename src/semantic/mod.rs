use expression::ExpressionError;
use r#struct::{analyze_struct, StructError};
use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, DiagnosticContext, DiagnosticLevel, DiagnosticMessage},
    parser::{
        ast::AbstractSyntaxTree, definition::Definition, function::FunctionSignature,
        r#struct::Struct, types::DataType,
    },
};

use self::{
    diagnostic::{diag_newly_defined, diag_originally_defined},
    function::{analyze_function, FunctionError},
    scope::ScopeError,
    scope_tracker::ScopeTracker,
};

mod diagnostic;
mod expression;
mod function;
mod scope;
mod scope_tracker;
mod r#struct;

pub(crate) const EXPECT_VAR_TYPE: &str = "variable should have a type by this point";

#[derive(Debug, Error)]
pub(super) enum SemanticError {
    #[error("main function is not defined")]
    MissingMainError,
    #[error("duplicate function definition")]
    FunctionAlreadyDefinedError {
        original: FunctionSignature,
        new: FunctionSignature,
    },
    #[error("duplicate struct definition")]
    StructAlreadyDefinedError { original: Struct, new: Struct },
    #[error(transparent)]
    FunctionError(#[from] FunctionError),
    #[error(transparent)]
    StructError(#[from] StructError),
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
            SemanticError::StructAlreadyDefinedError {
                ref original,
                ref new,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(DiagnosticMessage::new(
                    format!("struct `{}` already defined in this scope", new),
                    new.name().source(),
                ))
                .with_labels(vec![
                    diag_originally_defined(original.name().source(), None),
                    diag_newly_defined(new.name().source(), None),
                ]),
            ),
            SemanticError::FunctionError(err) => err.into(),
            SemanticError::StructError(err) => err.into(),
            SemanticError::ScopeError(err) => err.into(),
        }
    }
}

pub(crate) fn semantic_analysis(ast: &mut AbstractSyntaxTree) -> Vec<SemanticError> {
    let mut errors = Vec::new();

    let mut global_scope_tracker = ScopeTracker::new(None);

    // Analyze each struct definition and add them to the global scope.
    // This needs to be done first so that function parameters and return types
    // can refer to the struct.
    // This currently doesn't allow for arbitrary struct definition ordering.
    for definition in ast.iter_mut() {
        match definition {
            Definition::Struct(_struct) => {
                let errs = analyze_struct(_struct, &global_scope_tracker);
                errors.append(
                    &mut errs
                        .into_iter()
                        .map(|err| SemanticError::StructError(err))
                        .collect(),
                );

                if let Some(s) = global_scope_tracker.insert_struct(_struct.clone()) {
                    errors.push(SemanticError::StructAlreadyDefinedError {
                        original: s.clone(),
                        new: _struct.clone(),
                    });
                }
            }
            Definition::Function(_) => {}
        }
    }

    // Now that all structs have been added to the global scope, populate function
    // parameters and return types with those struct types, and add the function
    // to the global scope.
    for definition in ast.iter_mut() {
        match definition {
            Definition::Struct(_) => {}
            Definition::Function(function) => {
                // Populate the parameter's data type with the referenced struct if applicable
                for param in function.signature.params.iter_mut() {
                    match param.get_type().as_ref().expect(EXPECT_VAR_TYPE).into() {
                        &DataType::Struct { ref name, .. } => match global_scope_tracker
                            .get_struct(&**name)
                        {
                            Some(existing_struct) => {
                                param.update_struct_data_type(existing_struct.clone())
                            }
                            None => errors.push(SemanticError::ScopeError(
                                ScopeError::ExpressionError(ExpressionError::StructUnknownError(
                                    name.clone(),
                                    param.source(),
                                )),
                            )),
                        },
                        _ => {}
                    };
                }

                // Populate the return data type with the referenced struct if applicable
                for return_type in function.signature.returns.iter_mut() {
                    match return_type.into() {
                        &mut DataType::Struct {
                            ref name,
                            ref mut struct_data_types,
                        } => match global_scope_tracker.get_struct(&**name) {
                            Some(existing_struct) => {
                                *struct_data_types = Some(
                                    existing_struct
                                        .clone()
                                        .into_members()
                                        .into_iter()
                                        .map(|member| member.into_type().into())
                                        .collect(),
                                );
                            }
                            None => errors.push(SemanticError::ScopeError(
                                ScopeError::ExpressionError(ExpressionError::StructUnknownError(
                                    name.clone(),
                                    return_type.source(),
                                )),
                            )),
                        },
                        _ => {}
                    };
                }

                if let Some(func_sig) =
                    global_scope_tracker.insert_func_sig(function.signature.clone())
                {
                    errors.push(SemanticError::FunctionAlreadyDefinedError {
                        original: func_sig.clone(),
                        new: function.signature.clone(),
                    });
                }
            }
        }
    }

    // Analyze each function definition.
    // Doing this in a separate loop allows for arbitrary function definition order.
    for definition in ast.iter_mut() {
        match definition {
            Definition::Struct(_struct) => {}
            Definition::Function(function) => {
                let errs = analyze_function(function, &global_scope_tracker);
                errors.append(
                    &mut errs
                        .into_iter()
                        .map(|err| SemanticError::FunctionError(err))
                        .collect(),
                );
            }
        }
    }

    if !global_scope_tracker.has_main_func() {
        errors.push(SemanticError::MissingMainError);
    }

    // Check for any unused variables, functions, or structs in the global scope that have not been used
    let (unused_variables, function_signatures, structs) = global_scope_tracker.get_unused();
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
    for _struct in structs {
        errors.push(SemanticError::ScopeError(ScopeError::UnusedStructError {
            _struct,
        }))
    }
    errors
}
