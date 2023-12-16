use crate::{
    ast_parser::types::{
        BinaryMathOperationType, BooleanComparisonType, Expression, FunctionCall, Identifier,
        UnaryMathOperationType,
    },
    semantic::scope::Scope,
};
use cranelift::prelude::*;
use thiserror::Error;

/// A distinct type that is used to represent name the value of an evaluated [`Expression`].
///
/// In the current implementation, this is just a wrapper around Cranelift's [`Value`]
/// and is completely convertable to and from [`Value`].
/// It is intended to be used in places where it semantically makes sense to represent
/// an expression resulting value after evaluation.
/// [`Value`] also conveys that meaning, but it is a library-specific type that shouldn't be exposed to users.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(super) struct ExpressionValue(Value);

impl From<ExpressionValue> for Value {
    fn from(value: ExpressionValue) -> Self {
        value.0
    }
}

impl From<Value> for ExpressionValue {
    fn from(value: Value) -> Self {
        Self { 0: value }
    }
}

#[derive(Debug, Error, PartialEq)]
pub enum EvaluateExpressionError {
    #[error("incorrect number of values: expected {expected}, got {actual}")]
    IncorrectNumValuesError { expected: usize, actual: usize },
    #[error("unknown function \"{name}\"")]
    UnknownFunctionError { name: Identifier },
    #[error("unknown variable \"{name}\"")]
    UnknownVariableError { name: Identifier },
    #[error("variable \"{name}\" used before it was defined")]
    UseVariableError { name: Identifier },
}

/// Helper type that is responsible for evaluating expressions.
///
/// [`ExpressionEvaluator`] only contains the context it needs to evaluate expressions, emit
/// the relevant IR code, and return the expression's values. This makes the wider compiler more modular.
/// TODO: This type depends on [`Module`], which would become an issue if the underlying implementation
/// moved away from Cranelift.
pub(super) struct ExpressionEvaluator<
    'module,
    'ctx: 'builder,
    'builder,
    'scope,
    M: cranelift_module::Module + 'module,
> {
    builder: &'builder mut FunctionBuilder<'ctx>,
    module: &'module mut M,
    scope: &'scope Scope<'scope>,
}

impl<'module, 'ctx: 'builder, 'builder, 'scope, M: cranelift_module::Module>
    ExpressionEvaluator<'module, 'ctx, 'builder, 'scope, M>
{
    pub(super) fn new(
        module: &'module mut M,
        builder: &'builder mut FunctionBuilder<'ctx>,
        scope: &'scope Scope<'scope>,
    ) -> Self {
        Self {
            builder,
            module,
            scope,
        }
    }
}

impl<'module, 'ctx: 'builder, 'builder, 'var, M: cranelift_module::Module + 'module>
    ExpressionEvaluator<'module, 'ctx, 'builder, 'var, M>
{
    /// Evaluate a given expression and return the expression's resulting values.
    ///
    /// An expression can return multiple values depending on the context.
    /// Because expression evaluation can be recursive, it is possible for an inner expression
    /// to return a different number of results than an outer expression expects.
    /// If that happens, an [`EvaluateExpressionError`] is returned.
    pub(super) fn evaluate(
        &mut self,
        expression: Expression,
    ) -> Result<Vec<ExpressionValue>, EvaluateExpressionError> {
        match expression {
            Expression::BooleanComparison(comparison_type, lhs, rhs) => {
                Ok(vec![self.evaluate_boolean_comparison(
                    comparison_type,
                    *lhs,
                    *rhs,
                )?])
            }
            Expression::BinaryMathOperation(operation_type, lhs, rhs) => {
                Ok(vec![self.evaluate_binary_operation(
                    operation_type,
                    *lhs,
                    *rhs,
                )?])
            }
            Expression::UnaryMathOperation(operation_type, expression) => Ok(vec![
                self.evaluate_unary_operation(operation_type, *expression)?
            ]),
            Expression::FunctionCall(function_call) => self.evaluate_call(function_call),
            Expression::Variable(name) => {
                let variable = self
                    .scope
                    .get_var(&name)
                    .ok_or(EvaluateExpressionError::UnknownVariableError { name: name.clone() })?;
                self.builder
                    .try_use_var(cranelift::frontend::Variable::from(*variable))
                    .map(|value| vec![ExpressionValue::from(value)])
                    .map_err(|_| EvaluateExpressionError::UseVariableError { name })
            }
            Expression::IntLiteral(value) => {
                // TODO: only support 64-bit integers for now
                let int_type = codegen::ir::Type::int(64).unwrap();
                Ok(vec![ExpressionValue(
                    self.builder.ins().iconst(int_type, i64::from(value)),
                )])
            }
        }
    }

    fn evaluate_boolean_comparison(
        &mut self,
        comparison_type: BooleanComparisonType,
        lhs: Expression,
        rhs: Expression,
    ) -> Result<ExpressionValue, EvaluateExpressionError> {
        let left_values = self.evaluate(lhs)?;
        let right_values = self.evaluate(rhs)?;
        let left_value = Self::expect_single_value(&left_values)?;
        let right_value = Self::expect_single_value(&right_values)?;
        match comparison_type {
            BooleanComparisonType::Equal => Ok(ExpressionValue(self.builder.ins().icmp(
                IntCC::Equal,
                Value::from(*left_value),
                Value::from(*right_value),
            ))),
            BooleanComparisonType::NotEqual => Ok(ExpressionValue(self.builder.ins().icmp(
                IntCC::NotEqual,
                Value::from(*left_value),
                Value::from(*right_value),
            ))),
            BooleanComparisonType::LessThan => Ok(ExpressionValue(self.builder.ins().icmp(
                IntCC::SignedLessThan,
                Value::from(*left_value),
                Value::from(*right_value),
            ))),
            BooleanComparisonType::LessThanEqual => Ok(ExpressionValue(self.builder.ins().icmp(
                IntCC::SignedLessThanOrEqual,
                Value::from(*left_value),
                Value::from(*right_value),
            ))),
            BooleanComparisonType::GreaterThan => Ok(ExpressionValue(self.builder.ins().icmp(
                IntCC::SignedGreaterThan,
                Value::from(*left_value),
                Value::from(*right_value),
            ))),
            BooleanComparisonType::GreaterThanEqual => {
                Ok(ExpressionValue(self.builder.ins().icmp(
                    IntCC::SignedGreaterThanOrEqual,
                    Value::from(*left_value),
                    Value::from(*right_value),
                )))
            }
        }
    }

    fn evaluate_binary_operation(
        &mut self,
        operation_type: BinaryMathOperationType,
        lhs: Expression,
        rhs: Expression,
    ) -> Result<ExpressionValue, EvaluateExpressionError> {
        let left_values = self.evaluate(lhs)?;
        let right_values = self.evaluate(rhs)?;
        let left_value = Self::expect_single_value(&left_values)?;
        let right_value = Self::expect_single_value(&right_values)?;
        match operation_type {
            BinaryMathOperationType::Add => Ok(ExpressionValue(
                self.builder
                    .ins()
                    .iadd(Value::from(*left_value), Value::from(*right_value)),
            )),
            BinaryMathOperationType::Subtract => Ok(ExpressionValue(
                self.builder
                    .ins()
                    .isub(Value::from(*left_value), Value::from(*right_value)),
            )),
            BinaryMathOperationType::Multiply => Ok(ExpressionValue(
                self.builder
                    .ins()
                    .imul(Value::from(*left_value), Value::from(*right_value)),
            )),
            BinaryMathOperationType::Divide => Ok(ExpressionValue(
                self.builder
                    .ins()
                    .udiv(Value::from(*left_value), Value::from(*right_value)),
            )),
        }
    }

    fn evaluate_unary_operation(
        &mut self,
        operation_type: UnaryMathOperationType,
        expression: Expression,
    ) -> Result<ExpressionValue, EvaluateExpressionError> {
        let values = self.evaluate(expression)?;
        let value = Self::expect_single_value(&values)?;
        match operation_type {
            UnaryMathOperationType::Negate => Ok(ExpressionValue(
                self.builder.ins().ineg(Value::from(*value)),
            )),
        }
    }

    fn evaluate_call(
        &mut self,
        FunctionCall { name, arguments }: FunctionCall,
    ) -> Result<Vec<ExpressionValue>, EvaluateExpressionError> {
        // Find the function identifier in the list of declared functions
        let func_id_to_call = self
            .module
            .declarations()
            .get_functions()
            .find_map(|(func_id, func_decl)| {
                if let Some(func_name) = &func_decl.name {
                    if *func_name == name.to_string() {
                        return Some(func_id);
                    }
                }

                None
            })
            .ok_or(EvaluateExpressionError::UnknownFunctionError { name })?;

        // Get the function to be called
        let func_ref_to_call = self
            .module
            .declare_func_in_func(func_id_to_call, self.builder.func);

        // Evaluate parameter expressions
        let mut arg_values = Vec::with_capacity(arguments.len());
        for arg in arguments {
            let values = self.evaluate(arg)?;
            let value = Self::expect_single_value(&values)?;
            arg_values.push(Value::from(*value));
        }

        // Call function
        let call = self.builder.ins().call(func_ref_to_call, &arg_values);

        // Return function return values
        Ok(self
            .builder
            .inst_results(call)
            .iter()
            .map(|value| ExpressionValue::from(*value))
            .collect::<Vec<ExpressionValue>>())
    }

    fn expect_single_value(
        values: &[ExpressionValue],
    ) -> Result<&ExpressionValue, EvaluateExpressionError> {
        Self::expect_value_len(1, values)?;
        Ok(values.first().unwrap())
    }

    fn expect_value_len(
        len: usize,
        values: &[ExpressionValue],
    ) -> Result<(), EvaluateExpressionError> {
        if values.len() != len {
            return Err(EvaluateExpressionError::IncorrectNumValuesError {
                expected: len,
                actual: values.len(),
            });
        }

        Ok(())
    }
}

// TODO: move these tests
#[cfg(test)]
mod tests {
    use std::error::Error;

    use cranelift::{
        codegen::{entity::EntityRef, isa, Context},
        frontend::FunctionBuilderContext,
    };
    use cranelift_jit::{JITBuilder, JITModule};

    use crate::ast_parser::types::IntLiteral;

    use super::*;

    struct TestHarness<'module, 'ctx, 'builder, 'var> {
        context: Context,
        function_context: FunctionBuilderContext,
        builder: FunctionBuilder<'ctx>,
        module: JITModule, // TODO: should this be mocked?
        expression_evaluator: ExpressionEvaluator<'module, 'ctx, 'builder, 'var, JITModule>,
    }

    struct NoOpModule {}

    impl cranelift_module::Module for NoOpModule {
        fn isa(&self) -> &dyn isa::TargetIsa {
            unimplemented!()
        }

        fn declarations(&self) -> &cranelift_module::ModuleDeclarations {
            unimplemented!()
        }

        fn declare_function(
            &mut self,
            name: &str,
            linkage: cranelift_module::Linkage,
            signature: &codegen::ir::Signature,
        ) -> cranelift_module::ModuleResult<cranelift_module::FuncId> {
            unimplemented!()
        }

        fn declare_anonymous_function(
            &mut self,
            signature: &codegen::ir::Signature,
        ) -> cranelift_module::ModuleResult<cranelift_module::FuncId> {
            unimplemented!()
        }

        fn declare_data(
            &mut self,
            name: &str,
            linkage: cranelift_module::Linkage,
            writable: bool,
            tls: bool,
        ) -> cranelift_module::ModuleResult<cranelift_module::DataId> {
            unimplemented!()
        }

        fn declare_anonymous_data(
            &mut self,
            writable: bool,
            tls: bool,
        ) -> cranelift_module::ModuleResult<cranelift_module::DataId> {
            unimplemented!()
        }

        fn define_function_with_control_plane(
            &mut self,
            func: cranelift_module::FuncId,
            ctx: &mut Context,
            ctrl_plane: &mut codegen::control::ControlPlane,
        ) -> cranelift_module::ModuleResult<()> {
            unimplemented!()
        }

        fn define_function_bytes(
            &mut self,
            func_id: cranelift_module::FuncId,
            func: &codegen::ir::Function,
            alignment: u64,
            bytes: &[u8],
            relocs: &[codegen::FinalizedMachReloc],
        ) -> cranelift_module::ModuleResult<()> {
            unimplemented!()
        }

        fn define_data(
            &mut self,
            data_id: cranelift_module::DataId,
            data: &cranelift_module::DataDescription,
        ) -> cranelift_module::ModuleResult<()> {
            unimplemented!()
        }
    }

    #[test]
    fn test_evaluate_expression() {}

    #[test]
    fn test_expect_values() -> Result<(), EvaluateExpressionError> {
        let mut values = Vec::new();

        // expecting values from an empty list returns an error
        let err = ExpressionEvaluator::<NoOpModule>::expect_single_value(&values);
        assert_eq!(
            Err(EvaluateExpressionError::IncorrectNumValuesError {
                expected: 1,
                actual: 0
            }),
            err
        );
        let err = ExpressionEvaluator::<NoOpModule>::expect_value_len(2, &values);
        assert_eq!(
            Err(EvaluateExpressionError::IncorrectNumValuesError {
                expected: 2,
                actual: 0
            }),
            err
        );

        // expecting no values from an empty list works correctly
        ExpressionEvaluator::<NoOpModule>::expect_value_len(0, &values)?;

        let first_expression_value = ExpressionValue(Value::new(0));
        let second_expression_value = ExpressionValue(Value::new(1));
        values.push(first_expression_value);

        // A single value should be pulled from the list
        let value = ExpressionEvaluator::<NoOpModule>::expect_single_value(&values)?;
        assert_eq!(&first_expression_value, value);

        // Adding a second value and expecting a single value returns an error
        values.push(second_expression_value);
        let err = ExpressionEvaluator::<NoOpModule>::expect_single_value(&values);
        assert_eq!(
            Err(EvaluateExpressionError::IncorrectNumValuesError {
                expected: 1,
                actual: 2
            }),
            err
        );

        // Expecting two values works correctly
        ExpressionEvaluator::<NoOpModule>::expect_value_len(2, &values)?;

        // Expecting more values than in the list returns an error
        let err = ExpressionEvaluator::<NoOpModule>::expect_value_len(3, &values);
        assert_eq!(
            Err(EvaluateExpressionError::IncorrectNumValuesError {
                expected: 3,
                actual: 2
            }),
            err
        );

        Ok(())
    }

    #[test]
    fn test_evaluate_unary_operation() -> Result<(), Box<dyn Error>> {
        let mut context = Context::new();
        let mut function_context = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut context.func, &mut function_context);
        let mut module =
            JITModule::new(JITBuilder::new(cranelift_module::default_libcall_names())?);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let scope = Scope::new(None);
        let mut expression_evaluator = ExpressionEvaluator::new(&mut module, &mut builder, &scope);

        let value = expression_evaluator.evaluate_unary_operation(
            UnaryMathOperationType::Negate,
            Expression::UnaryMathOperation(
                UnaryMathOperationType::Negate,
                Box::new(Expression::IntLiteral(IntLiteral::from(10))),
            ),
        )?;

        assert_eq!(1, Value::from(value).index());

        Ok(())
    }
}
