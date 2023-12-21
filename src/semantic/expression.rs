use thiserror::Error;

use crate::parser::types::{Expression, FunctionCall, Identifier, Type, Types};

use super::scope::Scope;

#[derive(Debug, Error)]
pub enum ExpressionError {
    #[error("unknown variable \"{0}\" in this scope")]
    UnknownVariableError(Identifier),
    #[error("unknown function \"{0}\" in this scope")]
    UnknownFunctionError(Identifier),
    #[error("wrong number of arguments in call to function \"{function_name}\". expected: {expected}, actual: {actual}")]
    WrongNumberOfArgumentsError {
        function_name: Identifier,
        expected: usize,
        actual: usize,
    },
    #[error("mismatched argument type in call to function \"{function_name}\". expected: {expected}, actual: {actual}")]
    MismatchedArgumentTypeError {
        function_name: Identifier,
        expected: Type,
        actual: Type,
    },
    #[error("expected single value, but expression returns {0}")]
    SingleValueError(usize),
    #[error("expected type {expected}, but got {actual}")]
    WrongTypeError { expected: Type, actual: Type },
}

/// Check if the given expression is semantically correct.
///
/// Recursively validate that inner expressions have the correct number and types of values that the outer expression expects.
/// Returns the expression's value types, or an error if an inner expression was not compatible with the outer expression.
pub(super) fn analyze_expression(
    expression: &Expression,
    scope: &Scope,
) -> Result<Types, ExpressionError> {
    let mut types = Types::new();

    match expression {
        Expression::BooleanComparison(comparison_type, lhs, rhs) => {
            // TODO: Allow any comparable types, not just ints
            let lhs_types = analyze_expression(lhs, scope)?;
            expect_single_type(&lhs_types, Type::Int)?;

            let rhs_types = analyze_expression(rhs, scope)?;
            expect_single_type(&rhs_types, Type::Int)?;

            types.push(Type::Int);
        }
        Expression::BinaryMathOperation(operation_type, lhs, rhs) => {
            // TODO: Handle floats later
            let lhs_types = analyze_expression(lhs, scope)?;
            expect_single_type(&lhs_types, Type::Int)?;

            let rhs_types = analyze_expression(rhs, scope)?;
            expect_single_type(&rhs_types, Type::Int)?;

            types.push(Type::Int);
        }
        Expression::UnaryMathOperation(operation_type, expression) => {
            let mut ty = analyze_expression(expression, scope)?;
            expect_single_type(&ty, Type::Int)?;

            types.append(&mut ty);
        }
        Expression::FunctionCall(function_call) => {
            let mut tys = analyze_function_call(function_call, scope)?;
            types.append(&mut tys);
        }
        Expression::Variable(variable) => match scope.get_var(&variable.name) {
            Some(variable) => types.push(variable.ty),
            None => return Err(ExpressionError::UnknownVariableError(variable.name.clone())),
        },
        Expression::IntLiteral(_) => types.push(Type::Int),
        Expression::BoolLiteral(_) => types.push(Type::Bool),
    };

    Ok(types)
}

pub(super) fn analyze_function_call(
    function_call: &FunctionCall,
    scope: &Scope,
) -> Result<Types, ExpressionError> {
    match scope.get_func_sig(&function_call.name) {
        Some(func_sig) => {
            // Analyze each argument expression to determine their types
            let mut argument_types = Types::new();
            for expression in &function_call.arguments {
                let mut args = analyze_expression(expression, scope)?;
                argument_types.append(&mut args);
            }

            // Check that the function parameters match the function call arguments
            if func_sig.params.len() != argument_types.len() {
                return Err(ExpressionError::WrongNumberOfArgumentsError {
                    function_name: func_sig.name.clone(),
                    expected: func_sig.params.len(),
                    actual: argument_types.len(),
                });
            } else {
                for (i, arg) in argument_types.iter().enumerate() {
                    if *arg != func_sig.params[i].ty {
                        return Err(ExpressionError::MismatchedArgumentTypeError {
                            function_name: func_sig.name.clone(),
                            expected: func_sig.params[i].ty,
                            actual: *arg,
                        });
                    }
                }
            }

            // The function's return types are what should be propagated up to the call site
            Ok(func_sig.returns.clone())
        }
        None => Err(ExpressionError::UnknownFunctionError(
            function_call.name.clone(),
        )),
    }
}

fn expect_single_type(types: &Types, expected_type: Type) -> Result<(), ExpressionError> {
    if types.len() != 1 {
        return Err(ExpressionError::SingleValueError(types.len()));
    }

    if types[0] != expected_type {
        return Err(ExpressionError::WrongTypeError {
            expected: expected_type,
            actual: types[0],
        });
    }

    Ok(())
}
