use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, DiagnosticLevel},
    parser::{
        ast::AbstractSyntaxTree,
        function::{FunctionParameter, FunctionSignature},
        statement::Statement,
        types::Types,
    },
};

use self::{
    scope::Scope,
    statement::{analyze_statement, StatementError},
};

mod diagnostic;
mod expression;
pub(super) mod scope;
mod statement;

pub(crate) const EXPECT_VAR_TYPE: &str = "variable should have a type by this point";

#[derive(Debug, Error)]
pub(super) enum SemanticError {
    #[error("main function is not defined")]
    MissingMainError,
    #[error("function \"{}\" is already defined in this scope", .new.name)]
    FunctionAlreadyDefinedError {
        original: FunctionSignature,
        new: FunctionSignature,
    },
    #[error("duplicate function parameter \"{new}\"")]
    DuplicateParameterError {
        original: FunctionParameter,
        new: FunctionParameter,
    },
    #[error(transparent)]
    StatementError(#[from] StatementError),
}

impl From<SemanticError> for Diagnostic {
    fn from(err: SemanticError) -> Self {
        match err {
            SemanticError::MissingMainError => Self::new(&err, DiagnosticLevel::Error),
            SemanticError::FunctionAlreadyDefinedError { original, new } => todo!(),
            SemanticError::DuplicateParameterError { original, new } => todo!(),
            SemanticError::StatementError(err) => err.into(),
        }
    }
}

pub(crate) fn semantic_analysis(ast: &mut AbstractSyntaxTree) -> Result<(), Vec<SemanticError>> {
    let mut errors = Vec::new();

    let mut global_scope = Scope::new(None);

    let mut has_main_function = false;

    // Insert each function signature in the global scope
    for function in ast.iter() {
        if let Some(func_sig) = global_scope.insert_func_sig(function.signature.clone()) {
            errors.push(SemanticError::FunctionAlreadyDefinedError {
                original: func_sig.clone(),
                new: function.signature.clone(),
            });
        } else {
            if function.signature.name.to_string() == "main" {
                has_main_function = true;
            }
        }
    }

    if !has_main_function {
        errors.push(SemanticError::MissingMainError);
    }

    // Check for errors in each function
    for function in ast.iter_mut() {
        let mut function_scope = Scope::new(Some(&global_scope));

        // Insert parameters into function scope
        for param in &function.signature.params {
            if let Some(variable) = function_scope.insert_var(param.clone().into()) {
                errors.push(SemanticError::DuplicateParameterError {
                    original: variable.to_param().clone(),
                    new: param.clone(),
                });
            }
        }

        let mut has_return_statement = false;

        for statement in &mut function.body {
            if let Err(errs) =
                analyze_statement(&function.signature, &mut function_scope, statement)
            {
                errors.append(
                    &mut errs
                        .into_iter()
                        .map(|err| SemanticError::StatementError(err))
                        .collect(),
                )
            }

            match statement {
                Statement::Return { .. } => has_return_statement = true,
                _ => {}
            }
        }

        // Check for a function that has return types but does not have a return statement
        if !has_return_statement && function.signature.returns.len() > 0 {
            errors.push(SemanticError::StatementError(
                StatementError::ReturnValueMismatchError {
                    func_sig: function.signature.clone(),
                    return_types: Types::new(),
                },
            ))
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}
