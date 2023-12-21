use thiserror::Error;

use crate::parser::types::AbstractSyntaxTree;

use self::{
    scope::{Scope, ScopeError},
    statement::{analyze_statement, StatementError},
};

mod expression;
pub(super) mod scope;
mod statement;

pub(crate) const EXPECT_VAR_TYPE: &str = "variable should have a type by this point";

#[derive(Debug, Error)]
pub enum SemanticError {
    #[error("main function is not defined")]
    MissingMainError,
    #[error("{0}")]
    StatementError(#[from] StatementError),
    #[error("{0}")]
    ScopeError(#[from] ScopeError),
}

pub(crate) fn semantic_analysis(ast: &mut AbstractSyntaxTree) -> Result<(), Vec<SemanticError>> {
    let mut errors = Vec::new();

    let mut global_scope = Scope::new(None);

    let mut has_main_function = false;

    // Insert each function signature in the global scope
    for function in ast.iter() {
        if let Err(err) = global_scope.insert_func_sig(function.signature.clone()) {
            errors.push(SemanticError::ScopeError(err));
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
            if let Err(err) = function_scope.insert_var(param.clone()) {
                errors.push(SemanticError::ScopeError(err));
            }
        }

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
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}
