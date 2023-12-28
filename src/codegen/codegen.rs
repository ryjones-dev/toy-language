use cranelift::prelude::*;
use thiserror::Error;

use crate::{
    parser::{
        ast::AbstractSyntaxTree, expression::Expression, function::Function, statement::Statement,
    },
    semantic::EXPECT_VAR_TYPE,
};

use super::{
    block::BlockVariables,
    context::CodeGenContext,
    expression_generator::ExpressionGenerator,
    options::{CodeGenOptions, OptimizationLevel},
};

/// An error that captures all errors that can be thrown during code generation.
#[derive(Debug, Error)]
pub enum CodeGenError {
    #[error("Error creating codegen module: {0}")]
    CreateError(cranelift_module::ModuleError),
    #[error("Error finalizing compilation: {0}")]
    FinalizeError(cranelift_module::ModuleError),
    #[error("Error declaring function {0}: {1}")]
    FunctionDeclarationError(String, String),
    #[error("Error defining function {0}: {1}")]
    FunctionDefinitionError(String, String),
}

/// Abstracts different types of code generation backends that can be used by [`CodeGenerator`].
///
/// The main purpose for this abstraction is to support JIT and AOT compilation methods.
/// [`cranelift_module::Module`] takes care of most of this for us,
/// but some additional functions are needed to abstract specifics surrounding
/// emitting generated code, so that [`CodeGenerator`] can work independently of the backend.
/// Currently, only [`cranelift_jit::JITModule`] is implemented.
pub(crate) trait CodeGeneratorModule: cranelift_module::Module {
    fn finalize(&mut self) -> Result<(), CodeGenError>;
}

impl CodeGeneratorModule for cranelift_jit::JITModule {
    fn finalize(&mut self) -> Result<(), CodeGenError> {
        self.finalize_definitions()
            .map_err(|err| CodeGenError::FinalizeError(err))?;

        Ok(())
    }
}

/// A wrapper type for generating Cranelift IR, and then machine code.
pub(crate) struct CodeGenerator<M: CodeGeneratorModule> {
    module: M,
    context: CodeGenContext,
    options: CodeGenOptions,
    main_function_id: Option<cranelift_module::FuncId>,

    ir: Option<String>,
    disassembly: Option<String>,
}

pub(crate) struct JitCodeGenResults {
    pub(crate) code: *const u8,
    pub(crate) ir: Option<String>,
    pub(crate) disassembly: Option<String>,
}

impl CodeGenerator<cranelift_jit::JITModule> {
    /// Creates a new [`CodeGenerator`] instance with a given [`OptimizationLevel`].
    pub(crate) fn new(options: CodeGenOptions) -> Result<Self, CodeGenError> {
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
        .map_err(|err| CodeGenError::CreateError(err))?;

        let module = cranelift_jit::JITModule::new(builder);
        let context = CodeGenContext::new(&module, options.request_disassembly);

        Ok(Self {
            module,
            context,
            options,
            main_function_id: None,
            ir: None,
            disassembly: None,
        })
    }

    pub(crate) fn results(self) -> JitCodeGenResults {
        JitCodeGenResults {
            code: self
                .module
                .get_finalized_function(self.main_function_id.expect("missing main function")),
            ir: self.ir,
            disassembly: self.disassembly,
        }
    }

    /// Returns a pointer to the main function after the machine code is generated,
    /// or [`None`] if the machine code has not been successfully generated yet.
    pub(crate) fn get_main_function(&self) -> Option<*const u8> {
        match self.main_function_id {
            Some(main_function_id) => Some(self.module.get_finalized_function(main_function_id)),
            None => None,
        }
    }
}

impl<M: CodeGeneratorModule> CodeGenerator<M> {
    /// Generate Cranelift IR from the given [`AbstractSyntaxTree`], and then compile that to machine code.
    ///
    /// Accessing the resulting machine code will depend on the implementation of the underlying [`CodeGeneratorModule`].
    /// For instance, a JIT module would give access to the main function in memory, while an object module
    /// would provide the machine code in object files so that they can be linked by a linker.
    ///
    /// Because semantic analysis has already validated the source code, this function only returns a [`CodeGenError`]
    /// when something is fundamentally wrong. This could be due to lack of resources, or a bug in the compiler.
    ///
    /// If the intermediate IR is requested, it can be retrieved with the [`ir()`] function.
    /// If a disassembly is requested, it can be retrieved with the [`disassembly()`] function.
    pub(crate) fn generate(&mut self, ast: AbstractSyntaxTree) -> Result<(), CodeGenError> {
        let mut ir = String::new();
        let mut disassembly = String::new();

        // Generate each function individually, and build up the IR and/or disassembly if requested
        let mut function_context = FunctionBuilderContext::new();
        for function in ast {
            let (function_ir, function_disasm) =
                self.generate_function(&mut function_context, function)?;

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

        // Finalize the compilation in a module-independent way
        self.module.finalize()?;

        if !ir.is_empty() {
            self.ir = Some(ir)
        }

        if !disassembly.is_empty() {
            self.disassembly = Some(disassembly)
        }

        Ok(())
    }

    fn generate_function(
        &mut self,
        function_context: &mut FunctionBuilderContext,
        function: Function,
    ) -> Result<(Option<String>, Option<String>), CodeGenError> {
        // Pull out the wrapped context
        let context = self.context.get_inner_context_mut();

        let Function { signature, body } = function;

        // Add the function's parameters to the context
        for param in &signature.params {
            context
                .func
                .signature
                .params
                .push(AbiParam::new(param.into()));
        }

        // Add the function's return types to the context
        for return_type in &signature.returns {
            context
                .func
                .signature
                .returns
                .push(AbiParam::new(return_type.into()));
        }

        // We can now declare the function to Cranelift from the context
        let function_id = self
            .module
            .declare_function(
                &signature.name.to_string(),
                cranelift_module::Linkage::Local,
                &context.func.signature,
            )
            .map_err(|err| {
                Self::check_codegen_error(&err);
                CodeGenError::FunctionDeclarationError(signature.name.to_string(), err.to_string())
            })?;

        // Instantiate the function builder and create the function entry block where IR code will be emitted.
        // Because this is the entry block of the function, we can seal it early as no other blocks can branch to it.
        let mut builder = FunctionBuilder::new(&mut context.func, function_context);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        // Prime the block variables with the function parameters
        let mut block_vars = BlockVariables::new();
        for (i, function_param) in signature.params.iter().enumerate() {
            let cranelift_variable = cranelift::frontend::Variable::from_u32(
                block_vars.var(function_param.name.clone()),
            );
            builder.declare_var(cranelift_variable, function_param.into());
            builder.def_var(cranelift_variable, builder.block_params(entry_block)[i]);
        }

        // Now we can generate the function body
        Self::generate_function_body(&mut self.module, builder, block_vars, body);

        // Mark the function as defined to kick off Cranelift IR compilation
        self.module
            .define_function(function_id, context)
            .map_err(|err| {
                Self::check_codegen_error(&err);
                CodeGenError::FunctionDefinitionError(signature.name.to_string(), err.to_string())
            })?;

        // Take note if this is the main function
        if signature.name.to_string() == "main" {
            self.main_function_id = Some(function_id);
        }

        let mut ir = None;
        let disassembly = context.compiled_code().unwrap().vcode.clone();

        if self.options.request_ir {
            ir = Some(context.func.to_string());
        }

        self.context.clear(&self.module);

        Ok((ir, disassembly))
    }

    fn generate_function_body(
        module: &mut M,
        mut builder: FunctionBuilder,
        mut block_vars: BlockVariables,
        body: Vec<Statement>,
    ) {
        // Emit IR code for each statement in the function
        for statement in body {
            Self::generate_statement(module, &mut builder, &mut block_vars, statement);
        }

        builder.finalize();
    }

    fn generate_statement(
        module: &mut M,
        builder: &mut FunctionBuilder,
        block_vars: &mut BlockVariables,
        statement: Statement,
    ) {
        match statement {
            Statement::Assignment(variables, expression) => {
                // Generate IR for the expression on the right-hand side of the equals sign
                let values;
                {
                    let mut expression_generator =
                        ExpressionGenerator::new(module, builder, block_vars);
                    values = expression_generator.generate(expression);
                }

                // Declare and define the variables
                for (i, variable) in variables.into_iter().enumerate() {
                    // Only declare and define variable identifiers if they are not the discard identifier.
                    if let Some(variable) = variable {
                        let cranelift_variable =
                            cranelift::frontend::Variable::from_u32(block_vars.var(variable.name));

                        // Intentionally ignore the error, since we don't care if the variable has already been declared.
                        let _ = builder.try_declare_var(
                            cranelift_variable,
                            variable.ty.expect(EXPECT_VAR_TYPE).into(),
                        );
                        builder.def_var(cranelift_variable, values[i].into());
                    }
                }
            }
            Statement::FunctionCall(function_call) => {
                let mut expression_generator =
                    ExpressionGenerator::new(module, builder, block_vars);

                expression_generator.generate(Expression::FunctionCall(function_call));
            }
            Statement::Return(expressions) => {
                let mut return_values = Vec::new();

                for expression in expressions {
                    // Generate IR for the return value expressions
                    let mut values;
                    {
                        let mut expression_generator =
                            ExpressionGenerator::new(module, builder, block_vars);
                        values = expression_generator.generate(expression);
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
            }
        }
    }

    /// Check if a given module error is caused by an error in the IR code, or a bug in the code generation.
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
