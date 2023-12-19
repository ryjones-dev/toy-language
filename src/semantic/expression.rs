use thiserror::Error;

use crate::parser::types::{Expression, FunctionCall, Identifier, Type, Types};

use super::scope::Scope;

#[derive(Debug, Error)]
pub enum ExpressionError {
    #[error("unknown variable \"{0}\" in this scope")]
    UnknownVariableError(Identifier),
    #[error("unknown function \"{0}\" in this scope")]
    UnknownFunctionError(Identifier),
    #[error(
        "mismatched variables in expression assignment. expected: {expected}, actual: {actual}"
    )]
    MismatchedAssignmentError { expected: Types, actual: Types },
    #[error(
        "mismatched arguments for call to function \"{function_name}\". expected: {expected}, actual: {actual}"
    )]
    MismatchedArgumentsError {
        function_name: Identifier,
        expected: Types,
        actual: Types,
    },
    #[error(
        "mismatched return types for function \"{}\". expected: {expected}, actual: {actual}",
        .function_name
    )]
    MismatchedReturnError {
        function_name: Identifier,
        expected: Types,
        actual: Types,
    },
    #[error("expected single value for boolean comparison, but expression returns {0}")]
    SingleValueError(usize),
    #[error("expected {expected} type for boolean comparison, but got {actual}")]
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
        Expression::BoolLiteral(_) => todo!(),
    };

    Ok(types)
}

pub(super) fn analyze_function_call(
    function_call: &FunctionCall,
    scope: &Scope,
) -> Result<Types, ExpressionError> {
    match scope.get_func_sig(&function_call.name) {
        Some(func_sig) => {
            // Check that the function parameters match the function call arguments
            // TODO: validate types as well
            if func_sig.params.len() != function_call.arguments.len() {
                Err(ExpressionError::MismatchedArgumentsError {
                    function_name: function_call.name.clone(),
                    expected: func_sig.params.iter().map(|_| Type::Int).collect::<Types>(),
                    actual: Types::from(vec![Type::Int; function_call.arguments.len()]),
                })
            } else {
                Ok(func_sig.returns.clone())
            }
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
