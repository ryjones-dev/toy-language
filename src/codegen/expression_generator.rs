use cranelift::{
    codegen::ir::{condcodes::IntCC, AbiParam, InstBuilder, Value},
    frontend::FunctionBuilder,
};

use crate::{
    parser::{
        expression::{
            BinaryMathOperationType, BooleanComparisonType, Expression, UnaryMathOperationType,
        },
        function::FunctionSignature,
        identifier::Identifier,
        types::DataType,
        variable::Variable,
    },
    semantic::{EXPECT_FUNC_SIG, EXPECT_VAR_TYPE},
    semantic_assert,
};

use super::block::BlockVariables;

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

/// Helper type that is responsible for generating Cranelift IR from expressions.
pub(super) struct ExpressionGenerator<
    'module,
    'ctx: 'builder,
    'builder,
    'block,
    M: cranelift_module::Module + 'module,
> {
    builder: &'builder mut FunctionBuilder<'ctx>,
    module: &'module mut M,
    block_vars: &'block mut BlockVariables,
}

impl<'module, 'ctx: 'builder, 'builder, 'block, M: cranelift_module::Module>
    ExpressionGenerator<'module, 'ctx, 'builder, 'block, M>
{
    pub(super) fn new(
        module: &'module mut M,
        builder: &'builder mut FunctionBuilder<'ctx>,
        block_vars: &'block mut BlockVariables,
    ) -> Self {
        Self {
            builder,
            module,
            block_vars,
        }
    }
}

impl<'module, 'ctx: 'builder, 'builder, 'var, M: cranelift_module::Module + 'module>
    ExpressionGenerator<'module, 'ctx, 'builder, 'var, M>
{
    /// Generate IR for a given expression and return the expression's resulting values.
    ///
    /// An expression can return multiple values depending on the context.
    /// This function assumes that the context has already been checked by the [`crate::semantic`] module,
    /// so no error needs to be returned.
    pub(super) fn generate(&mut self, expression: Expression) -> Vec<ExpressionValue> {
        match expression {
            Expression::Scope { scope, .. } => {
                let scope_block = self.builder.create_block();

                // Jump and switch to the new scope block
                self.builder.ins().jump(scope_block, &[]);
                self.builder.switch_to_block(scope_block);

                let mut value = Vec::new();
                for expression in scope {
                    // Because we already went through semantic analysis, we know that
                    // only the last expression in the scope will actually return a value
                    value = self.generate(expression);
                }

                self.builder.seal_block(scope_block);

                // Pass the scope's return value back as the result of the expression
                value
            }
            Expression::ExpressionList { expressions, .. } => {
                let mut values = Vec::new();
                for expression in expressions {
                    values.append(&mut self.generate(expression));
                }
                values
            }
            Expression::Assignment {
                variables,
                expression,
                ..
            } => {
                // Generate IR for the expression on the right-hand side of the equals sign
                let values = self.generate(*expression);

                // Declare and define the variables
                for (i, variable) in variables.into_iter().enumerate() {
                    // Only declare and define variables if they are not discarded
                    if !variable.is_discarded() {
                        let cranelift_variable = cranelift::frontend::Variable::from_u32(
                            self.block_vars.var(variable.name().clone()),
                        );

                        // Intentionally ignore the error, since we don't care if the variable has already been declared
                        let _ = self.builder.try_declare_var(
                            cranelift_variable,
                            variable.get_type().expect(EXPECT_VAR_TYPE).into(),
                        );
                        self.builder.def_var(cranelift_variable, values[i].into());
                    }
                }

                vec![]
            }
            Expression::FunctionReturn { expression, .. } => {
                // Generate IR for the return value expression
                let return_values = self.generate(*expression);

                // Add the IR instruction to actually return from the function
                self.builder.ins().return_(
                    &return_values
                        .iter()
                        .map(|value| Value::from(*value))
                        .collect::<Vec<Value>>(),
                );

                return_values
            }
            Expression::FunctionCall {
                name,
                argument_expression,
                function_signature,
                ..
            } => self.generate_function_call(
                name,
                *argument_expression,
                function_signature.expect(EXPECT_FUNC_SIG),
            ),
            Expression::BooleanComparison {
                comparison_type,
                lhs,
                rhs,
                ..
            } => {
                vec![self.generate_boolean_comparison(comparison_type, *lhs, *rhs)]
            }
            Expression::BinaryMathOperation {
                operation_type,
                lhs,
                rhs,
                ..
            } => {
                vec![self.generate_binary_operation(operation_type, *lhs, *rhs)]
            }
            Expression::UnaryMathOperation {
                operation_type,
                expression,
                ..
            } => {
                vec![self.generate_unary_operation(operation_type, *expression)]
            }
            Expression::Variable(variable) => vec![self.generate_variable(variable)],
            Expression::IntLiteral(value, _) => vec![self.generate_int_literal(value)],
            Expression::BoolLiteral(value, _) => {
                vec![self.generate_bool_literal(value)]
            }
        }
    }

    fn generate_boolean_comparison(
        &mut self,
        comparison_type: BooleanComparisonType,
        lhs: Expression,
        rhs: Expression,
    ) -> ExpressionValue {
        let left_values = self.generate(lhs);
        let right_values = self.generate(rhs);
        semantic_assert!(
            left_values.len() == 1,
            "left boolean operand expression did not return a single value"
        );
        semantic_assert!(
            right_values.len() == 1,
            "right boolean operand expression did not return a single value"
        );
        let left_value = left_values[0];
        let right_value = right_values[0];
        match comparison_type {
            BooleanComparisonType::Equal => ExpressionValue(self.builder.ins().icmp(
                IntCC::Equal,
                Value::from(left_value),
                Value::from(right_value),
            )),
            BooleanComparisonType::NotEqual => ExpressionValue(self.builder.ins().icmp(
                IntCC::NotEqual,
                Value::from(left_value),
                Value::from(right_value),
            )),
            BooleanComparisonType::LessThan => ExpressionValue(self.builder.ins().icmp(
                IntCC::SignedLessThan,
                Value::from(left_value),
                Value::from(right_value),
            )),
            BooleanComparisonType::LessThanEqual => ExpressionValue(self.builder.ins().icmp(
                IntCC::SignedLessThanOrEqual,
                Value::from(left_value),
                Value::from(right_value),
            )),
            BooleanComparisonType::GreaterThan => ExpressionValue(self.builder.ins().icmp(
                IntCC::SignedGreaterThan,
                Value::from(left_value),
                Value::from(right_value),
            )),
            BooleanComparisonType::GreaterThanEqual => ExpressionValue(self.builder.ins().icmp(
                IntCC::SignedGreaterThanOrEqual,
                Value::from(left_value),
                Value::from(right_value),
            )),
        }
    }

    fn generate_binary_operation(
        &mut self,
        operation_type: BinaryMathOperationType,
        lhs: Expression,
        rhs: Expression,
    ) -> ExpressionValue {
        let left_values = self.generate(lhs);
        let right_values = self.generate(rhs);
        semantic_assert!(
            left_values.len() == 1,
            "left binary operand expression did not return a single value"
        );
        semantic_assert!(
            right_values.len() == 1,
            "right binary operand expression did not return a single value"
        );
        let left_value = left_values[0];
        let right_value = right_values[0];
        match operation_type {
            BinaryMathOperationType::Add => ExpressionValue(
                self.builder
                    .ins()
                    .iadd(Value::from(left_value), Value::from(right_value)),
            ),
            BinaryMathOperationType::Subtract => ExpressionValue(
                self.builder
                    .ins()
                    .isub(Value::from(left_value), Value::from(right_value)),
            ),
            BinaryMathOperationType::Multiply => ExpressionValue(
                self.builder
                    .ins()
                    .imul(Value::from(left_value), Value::from(right_value)),
            ),
            BinaryMathOperationType::Divide => ExpressionValue(
                self.builder
                    .ins()
                    .udiv(Value::from(left_value), Value::from(right_value)),
            ),
        }
    }

    fn generate_unary_operation(
        &mut self,
        operation_type: UnaryMathOperationType,
        expression: Expression,
    ) -> ExpressionValue {
        let values = self.generate(expression);
        semantic_assert!(
            values.len() == 1,
            "unary operand expression did not return a single value"
        );
        match operation_type {
            UnaryMathOperationType::Negate => {
                ExpressionValue(self.builder.ins().ineg(Value::from(values[0])))
            }
        }
    }

    fn generate_function_call(
        &mut self,
        name: Identifier,
        argument_expression: Option<Expression>,
        func_sig: FunctionSignature,
    ) -> Vec<ExpressionValue> {
        // Because we've already done semantic analysis, we know that the function being called is defined,
        // and this call to that function is correct.
        // Therefore, we can build the function signature from the function call information.
        // This allows for any arbitrary function definition order.
        let mut sig = self.module.make_signature();

        for ty in &func_sig.params.types() {
            sig.params.push(AbiParam::new(ty.into()))
        }

        for ty in &func_sig.returns {
            sig.returns.push(AbiParam::new(ty.into()));
        }

        let func_id_to_call = self
            .module
            .declare_function(&name.to_string(), cranelift_module::Linkage::Local, &sig)
            .unwrap(); // TODO: this could have a genuine error but it's extremely unlikely, so it's not worth changing the API right now

        // Get the function to be called
        let func_ref_to_call = self
            .module
            .declare_func_in_func(func_id_to_call, self.builder.func);

        // Generate IR for the argument expression
        let mut values = Vec::new();
        if let Some(argument_expression) = argument_expression {
            values = self.generate(argument_expression);
            semantic_assert!(
                values.len() == sig.params.len(),
                "function argument expression did not return the correct number of values"
            );
        }

        // Call function
        let call = self.builder.ins().call(
            func_ref_to_call,
            &values
                .into_iter()
                .map(|val| val.into())
                .collect::<Vec<Value>>(),
        );

        // Return function return values
        self.builder
            .inst_results(call)
            .iter()
            .map(|value| ExpressionValue::from(*value))
            .collect::<Vec<ExpressionValue>>()
    }

    fn generate_variable(&mut self, variable: Variable) -> ExpressionValue {
        ExpressionValue::from(
            self.builder
                .use_var(cranelift::frontend::Variable::from_u32(
                    self.block_vars.var(variable.into_name()),
                )),
        )
    }

    fn generate_int_literal(&mut self, value: i64) -> ExpressionValue {
        ExpressionValue(self.builder.ins().iconst(DataType::Int.into(), value))
    }

    fn generate_bool_literal(&mut self, value: bool) -> ExpressionValue {
        ExpressionValue(
            self.builder
                .ins()
                .iconst(DataType::Bool.into(), if value { 1 } else { 0 }),
        )
    }
}
