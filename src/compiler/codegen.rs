use cranelift::prelude::*;
use thiserror::Error;

use crate::ast_parser::{
    grammar::parser,
    types::{Expression, Function, Identifier, Statement},
};

use super::{
    context::CompilerContext,
    expression_evaluator::{EvaluateExpressionError, ExpressionEvaluator, ExpressionValue},
    options::{CompileOptions, OptimizationLevel},
    variable_map::VariableMap,
};

/// An error that is thrown when parsing source code fails.
///
/// In the implementation, [`peg`] is used to parse source code.
/// Because [`peg::error::ParseError`] has a generic parameter, it makes it difficult
/// to use in this API. Therefore, [`ParseError`] here is a target type that the underlying
/// [`peg::error::ParseError`] can be mapped to for easier error handling.
/// [`ParseError`] is transparent and just outputs whatever error message [`peg::error::ParseError`] would have.
#[derive(Error, Debug)]
#[error("{0}")]
pub struct ParseError(String);

impl<L: std::fmt::Display> From<peg::error::ParseError<L>> for ParseError {
    fn from(value: peg::error::ParseError<L>) -> Self {
        ParseError(value.to_string())
    }
}

/// An error that is thrown when compiling [`Statement::Assignment`].
#[derive(Debug, Error)]
pub enum AssignmentError {
    #[error("expression returns {0} values but {1} variables are declared")]
    MismatchedNumVariablesError(usize, usize),
    #[error("Error evaluating expression: {0}")]
    EvaluateExpressionError(EvaluateExpressionError),
}

/// An error that is thrown when compiling [`Statement::FunctionCall`].
#[derive(Debug, Error)]
pub enum FunctionCallError {
    #[error("Error evaluating expression: {0}")]
    EvaluateExpressionError(EvaluateExpressionError),
    #[error("Function \"{0}\" returns values that are not stored in a variable. If this is intentional, use the discard identifier (\"_\")")]
    NonZeroReturnError(Identifier),
}

/// An error that is thrown when compiling [`Statement::Return`].
#[derive(Debug, Error)]
pub enum ReturnError {
    #[error("Error evaluating expression: {0}")]
    EvaluateExpressionError(EvaluateExpressionError),
}

/// An error that captures all errors that can be thrown during compilation.
#[derive(Debug, Error)]
pub enum CompileError {
    #[error("Error creating compiler: {0}")]
    CreateError(cranelift_module::ModuleError),
    #[error("Error parsing source code: {0}")]
    ParseError(ParseError),
    #[error("{0}")]
    AssignmentError(AssignmentError),
    #[error("{0}")]
    FunctionCallError(FunctionCallError),
    #[error("{0}")]
    ReturnError(ReturnError),
    #[error("Error finalizing compilation: {0}")]
    FinalizeError(cranelift_module::ModuleError),
    #[error("Main function not defined")]
    MissingMainError,
    #[error("Main function already defined")]
    DuplicateMainError,
    #[error("Error declaring function {0}: {1}")]
    FunctionDeclarationError(String, String),
    #[error("Error defining function {0}: {1}")]
    FunctionDefinitionError(String, String),
    #[error("Error declaring variable {0}: {1}")]
    VariableDeclarationError(String, String),
    #[error("Error defining variable {0}: {1}")]
    VariableDefinitionError(String, String),
}

/// Abstracts different types of compiler backends that can be used by [`Compiler`].
///
/// The main purpose for this abstraction is to support JIT and AOT compilation methods.
/// [`cranelift_module::Module`] takes care of most of this for us,
/// but some additional functions are needed to abstract specifics surrounding
/// emitting generated code, so that [`Compiler`] can work independently of the backend.
/// Currently, only [`cranelift_jit::JITModule`] is implemented.
pub trait CompilerModule: cranelift_module::Module {
    fn finalize(&mut self) -> Result<(), CompileError>;
}

impl CompilerModule for cranelift_jit::JITModule {
    fn finalize(&mut self) -> Result<(), CompileError> {
        self.finalize_definitions()
            .map_err(|err| CompileError::FinalizeError(err))?;

        Ok(())
    }
}

/// The user-facing type for compiling TODO_LANG_NAME source code.
///
/// This is the entry point of the API. After creating a [`Compiler`] with [`Compiler::new()`],
/// calling [`Compiler::compile()`] does all of the compilation.
/// Any [`CompilerModule`]-specific actions after that, to retrieve the compiled code or
/// create object files from it, are done through the underlying [`CompilerModule`].
pub struct Compiler<M: CompilerModule> {
    module: M,
    options: CompileOptions,
    main_function_id: Option<cranelift_module::FuncId>,
}

impl Compiler<cranelift_jit::JITModule> {
    /// Creates a new [`Compiler`] instance with a given [`OptimizationLevel`].
    pub fn new(options: CompileOptions) -> Result<Self, CompileError> {
        let builder = match options.optimization_level {
            OptimizationLevel::None => cranelift_jit::JITBuilder::with_flags(
                &[("opt_level", "none")],
                cranelift_module::default_libcall_names(),
            ),
            OptimizationLevel::Speed => cranelift_jit::JITBuilder::with_flags(
                &[("opt_level", "speed")],
                cranelift_module::default_libcall_names(),
            ),
            OptimizationLevel::SpeedAndSize => cranelift_jit::JITBuilder::with_flags(
                &[("opt_level", "speed_and_size")],
                cranelift_module::default_libcall_names(),
            ),
        }
        .map_err(|err| CompileError::CreateError(err))?;

        let module = cranelift_jit::JITModule::new(builder);

        Ok(Self {
            module,
            options,
            main_function_id: None,
        })
    }

    /// Returns a pointer to the main function after the source code is compiled,
    /// or an error if the source code does not include a main function.
    ///
    /// # Panics
    /// Panics if the source code has not been compiled yet.
    pub fn get_main_function(&self) -> Result<*const u8, CompileError> {
        match self.main_function_id {
            Some(main_function_id) => Ok(self.module.get_finalized_function(main_function_id)),
            None => Err(CompileError::MissingMainError),
        }
    }
}

impl<M: CompilerModule> Compiler<M> {
    /// Compiles the given source code to machine code.
    ///
    /// Accessing the resulting machine code will depend on the implementation of the underlying [`CompilerModule`].
    /// For instance, a JIT module would give access to the main function in memory, while an object module
    /// would provide the machine code in object files so that they can be linked by a linker.
    ///
    /// This function can return many different types of errors at different stages of compilation,
    /// and all of those error types are represented by a [`CompileError`].
    ///
    /// If the intermediate IR is requested, this function will return that IR as a [`String`]
    /// in the first position of the tuple.
    /// If a disassembly is requested, this function will return that disassembly as a [`String`]
    /// in the second position of the tuple.
    pub fn compile(
        &mut self,
        source_code: &str,
    ) -> Result<(Option<String>, Option<String>), CompileError> {
        let mut context = CompilerContext::new(&self.module, self.options.request_disassembly);

        let mut ir = String::new();
        let mut disassembly = String::new();
        // Parse the source code into AST nodes
        let functions =
            parser::code(source_code).map_err(|err| CompileError::ParseError(err.into()))?;

        // Compile each function individually, and build up the disassembly if requested in the context
        let mut function_context = FunctionBuilderContext::new();
        for function in functions {
            let (function_ir, function_disasm) =
                self.compile_function(&mut context, &mut function_context, function)?;

            match function_ir {
                Some(function_ir) => ir.push_str(format!("{}\n", function_ir).as_str()),
                None => {}
            }

            match function_disasm {
                Some(function_disasm) => {
                    disassembly.push_str(format!("{}\n", function_disasm).as_str())
                }
                None => {}
            }
        }

        // Ensure that the main function was defined
        if let None = self.main_function_id {
            return Err(CompileError::MissingMainError);
        }

        // Finalize the compilation in a module-independent way
        self.module.finalize()?;

        Ok((
            match ir.as_str() {
                "" => None,
                _ => Some(ir),
            },
            match disassembly.as_str() {
                "" => None,
                _ => Some(disassembly),
            },
        ))
    }

    fn compile_function(
        &mut self,
        compiler_context: &mut CompilerContext,
        function_context: &mut FunctionBuilderContext,
        function: Function,
    ) -> Result<(Option<String>, Option<String>), CompileError> {
        // Pull out the wrapped context
        let context = compiler_context.get_inner_context_mut();

        // TODO: only support 64-bit integer types for now
        let int_type = codegen::ir::Type::int(64).unwrap();

        // Set up the parameter types for the function
        for _ in &function.params {
            context.func.signature.params.push(AbiParam::new(int_type));
        }

        // Instantiate the function builder and create the function entry block where IR code will be emitted.
        // Because this is the entry block of the function, we can seal it early as no other blocks can branch to it.
        let mut builder = FunctionBuilder::new(&mut context.func, function_context);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        // In order to completely know the function's signature, the return expressions need to be evaluated.
        // This is because the TODO_LANG_NAME syntax does not provide a way to declare the number of
        // return values or their types as a part of the function's signature, by design.
        // Because of this, we need to start compiling the function body first, and then we can declare the return types.

        // To compile the function body, we first need to declare the parameter variables
        // that the function may reference.
        let mut variables = VariableMap::new();
        for (i, name) in function.params.iter().enumerate() {
            let variable = cranelift::frontend::Variable::from(*(variables.insert(name)));
            builder.declare_var(variable, int_type);

            // Because we know the values of the parameters from the calling function,
            // we can define those values right away.
            let value = builder.block_params(entry_block)[i];
            builder.def_var(variable, value)
        }

        // Copy the function name as a string since compiling the function body consumes the function
        let function_name = function.name.clone().map(|name| String::from(name));

        // Now we can compile the function body to get the number of return values
        let return_values = self.compile_function_body(builder, &mut variables, function)?;

        // Set up the return types for the function
        for _ in 0..return_values.len() {
            context.func.signature.returns.push(AbiParam::new(int_type));
        }

        // Now that the function signature is completely known, we can finally declare it.
        let function_id = match &function_name {
            Some(function_name) => self
                .module
                .declare_function(
                    function_name,
                    cranelift_module::Linkage::Local,
                    &context.func.signature,
                )
                .map_err(|err| {
                    Self::check_codegen_error(&err);
                    CompileError::FunctionDeclarationError(function_name.clone(), err.to_string())
                })?,
            None => todo!("Declare anonymous function"),
        };

        // We have already compiled the function body, so we can immediately mark the function as defined
        // and finalize the function.
        self.module
            .define_function(function_id, context)
            .map_err(|err| {
                Self::check_codegen_error(&err);
                CompileError::FunctionDefinitionError(
                    match function_name.clone() {
                        Some(function_name) => function_name,
                        None => "".to_string(),
                    },
                    err.to_string(),
                )
            })?;

        // Take note if this is the main function
        if let Some(function_name) = function_name {
            if function_name == "main" {
                if let Some(_) = self.main_function_id {
                    return Err(CompileError::DuplicateMainError);
                }

                self.main_function_id = Some(function_id);
            }
        }

        // Assume IR is requested first to appease context lifetimes
        let mut ir = Some(format!("{}", context.func));
        let disassembly = context.compiled_code().unwrap().vcode.clone();

        if !self.options.request_ir {
            ir = None;
        }

        compiler_context.clear(&self.module);

        Ok((ir, disassembly))
    }

    fn compile_function_body(
        &mut self,
        mut builder: FunctionBuilder,
        variables: &mut VariableMap,
        function: Function,
    ) -> Result<Vec<ExpressionValue>, CompileError> {
        let mut return_values = Vec::new();

        // Emit IR code for each statement in the function
        for statement in function.body {
            let mut values = self.compile_statement(&mut builder, variables, statement)?;
            return_values.append(&mut values);
        }

        builder.finalize();
        Ok(return_values)
    }

    fn compile_statement(
        &mut self,
        builder: &mut FunctionBuilder,
        variables: &mut VariableMap,
        statement: Statement,
    ) -> Result<Vec<ExpressionValue>, CompileError> {
        // TODO: only support 64-bit integer types for now
        let int_type = codegen::ir::Type::int(64).unwrap();

        match statement {
            Statement::Assignment(variable_names, expression) => {
                // Evaluate the expression on the right-hand side of the equals sign
                let values;
                {
                    let mut expression_evaluator =
                        ExpressionEvaluator::new(&mut self.module, builder, &variables);
                    values = expression_evaluator.evaluate(expression).map_err(|err| {
                        CompileError::AssignmentError(AssignmentError::EvaluateExpressionError(err))
                    })?;
                }

                // Make sure that there are a number of variable names equal to resulting values from the expression
                if values.len() != variable_names.len() {
                    return Err(CompileError::AssignmentError(
                        AssignmentError::MismatchedNumVariablesError(
                            values.len(),
                            variable_names.len(),
                        ),
                    ));
                }

                // Declare and define the variables
                for (i, variable_name) in variable_names.into_iter().enumerate() {
                    // Only declare and define variable identifiers if they are not the discard identifier.
                    if let Some(variable_name) = variable_name {
                        let variable = *match variables.get(&variable_name) {
                            Some(variable) => variable,
                            None => {
                                let variable = variables.insert(&variable_name);

                                // Declare the variable in Cranelift if it was not in the variable map
                                builder
                                    .try_declare_var(variable.clone().into(), int_type)
                                    .map_err(|err| {
                                        CompileError::VariableDeclarationError(
                                            String::from(variable_name.clone()),
                                            err.to_string(),
                                        )
                                    })?;

                                variable
                            }
                        };

                        // Define the variable's value to the corresponding return value of the expression
                        builder
                            .try_def_var(variable.into(), values[i].into())
                            .map_err(|err| {
                                CompileError::VariableDefinitionError(
                                    String::from(variable_name),
                                    err.to_string(),
                                )
                            })?;
                    }
                }

                Ok(Vec::new())
            }
            Statement::FunctionCall(function_call) => {
                let mut expression_evaluator =
                    ExpressionEvaluator::new(&mut self.module, builder, &variables);

                let function_name = function_call.name.clone();

                let return_values = expression_evaluator
                    .evaluate(Expression::FunctionCall(function_call))
                    .map_err(|err| {
                        CompileError::FunctionCallError(FunctionCallError::EvaluateExpressionError(
                            err,
                        ))
                    })?;

                // Function calls can only be treated as statements if the function does not return anything.
                if !return_values.is_empty() {
                    return Err(CompileError::FunctionCallError(
                        FunctionCallError::NonZeroReturnError(function_name),
                    ));
                }

                Ok(Vec::new())
            }
            Statement::Return(expressions) => {
                let mut return_values = Vec::new();

                for expression in expressions {
                    // Evaluate the return value expressions
                    let mut values;
                    {
                        let mut expression_evaluator =
                            ExpressionEvaluator::new(&mut self.module, builder, &variables);
                        values = expression_evaluator.evaluate(expression).map_err(|err| {
                            CompileError::ReturnError(ReturnError::EvaluateExpressionError(err))
                        })?;
                    }

                    return_values.append(&mut values);
                }

                // Add the IR instruction to actually return from the function
                builder.ins().return_(
                    &return_values
                        .iter()
                        .map(|value| Value::from(*value))
                        .collect::<Vec<Value>>(),
                );

                Ok(return_values)
            }
        }
    }

    /// Check if a given module error is caused by an error in the source code, or a bug in the compiler.
    ///
    /// Panics if the error is actually due to a bug in the compiler.
    /// This is helpful while iterating and debugging the compiler.
    fn check_codegen_error(err: &cranelift_module::ModuleError) {
        match err {
            cranelift_module::ModuleError::Compilation(ref inner_err) => match inner_err {
                codegen::CodegenError::Verifier(inner_err) => panic!(
                    "Codegen error. This is a bug in the compiler.\n{}:\n{}",
                    err.to_string(),
                    inner_err.to_string()
                ),
                _ => {}
            },
            _ => {}
        }
    }
}
