use thiserror::Error;

use crate::ast_parser::types::{Expression, FunctionCall, Identifier, Type};

use super::scope::Scope;

#[derive(Debug, Error)]
pub(super) enum ExpressionError {
    #[error("unknown variable \"{0}\" in this scope")]
    UnknownVariableError(Identifier),
    #[error("unknown function \"{0}\" in this scope")]
    UnknownFunctionError(Identifier),
    #[error(
        "Mismatched expression results for this context. Expected: {expected:?}, Actual: {actual:?}"
    )]
    MismatchedExpressionResultsError {
        expected: Vec<Type>,
        actual: Vec<Type>,
    },
}

/// Check if the given expression is semantically correct.
///
/// Recursively validate that inner expressions have the correct number and types of values that the outer expression expects.
/// Returns the expression's value types, or an error if an inner expression was not compatible with the outer expression.
pub(super) fn check_expression(
    expression: &Expression,
    scope: &Scope,
) -> Result<Vec<Type>, ExpressionError> {
    let mut types = Vec::new();

    match expression {
        Expression::BooleanComparison(comparison_type, lhs, rhs) => {
            // TODO: Allow any comparable types, not just ints
            let lhs_types = check_expression(lhs, scope)?;
            expect_single_type(&lhs_types, Type::Int)?;

            let rhs_types = check_expression(rhs, scope)?;
            expect_single_type(&rhs_types, Type::Int)?;

            types.push(Type::Int);
        }
        Expression::BinaryMathOperation(_, lhs, rhs) => {
            // TODO: Handle floats later
            let lhs_types = check_expression(lhs, scope)?;
            expect_single_type(&lhs_types, Type::Int)?;

            let rhs_types = check_expression(rhs, scope)?;
            expect_single_type(&rhs_types, Type::Int)?;

            types.push(Type::Int);
        }
        Expression::UnaryMathOperation(_, expression) => {
            let mut ty = check_expression(expression, scope)?;
            expect_single_type(&ty, Type::Int)?;

            types.append(&mut ty);
        }
        Expression::FunctionCall(function_call) => {
            let mut tys = check_function_call(function_call, scope)?;
            types.append(&mut tys);
        }
        Expression::Variable(name) => match scope.get_var(name) {
            Some(variable) => types.push(variable.ty),
            None => return Err(ExpressionError::UnknownVariableError(name.clone())),
        },
        Expression::IntLiteral(_) => types.push(Type::Int),
    };

    Ok(types)
}

pub(super) fn check_function_call(
    function_call: &FunctionCall,
    scope: &Scope,
) -> Result<Vec<Type>, ExpressionError> {
    match scope.get_func_sig(&function_call.name) {
        Some(func_sig) => Ok(func_sig.returns.to_vec()),
        None => {
            return Err(ExpressionError::UnknownFunctionError(
                function_call.name.clone(),
            ))
        }
    }
}

fn expect_single_type(types: &Vec<Type>, expected_type: Type) -> Result<(), ExpressionError> {
    if types.len() != 1 || types[0] != expected_type {
        return Err(ExpressionError::MismatchedExpressionResultsError {
            expected: vec![expected_type],
            actual: types.to_vec(),
        });
    }

    Ok(())
}
