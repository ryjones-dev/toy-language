use thiserror::Error;

use crate::{
    codegen::{
        codegen::{CodeGenError, CodeGenerator},
        options::CodeGenOptions,
    },
    parser::{ast::AbstractSyntaxTree, parser, ParseError},
    semantic::{semantic_analysis, SemanticError},
};

pub struct CompileOptions {
    request_ast: bool,
    codegen_options: CodeGenOptions,
}

impl CompileOptions {
    pub fn new() -> Self {
        Self {
            request_ast: false,
            codegen_options: CodeGenOptions::new(),
        }
    }

    pub fn with_ast(mut self, request_ast: bool) -> Self {
        self.request_ast = request_ast;
        self
    }

    pub fn with_codegen_options(mut self, codegen_options: CodeGenOptions) -> Self {
        self.codegen_options = codegen_options;
        self
    }
}

/// An error that captures all errors that can be thrown during compilation.
#[derive(Debug, Error)]
pub enum CompileError {
    #[error("parse error: {0}")]
    ParseError(ParseError),
    #[error("semantic error: {}", .0.iter().fold(String::new(), |acc, err| format!("{acc}\n{}", err)))]
    SemanticErrors(Vec<SemanticError>),
    #[error("codegen error: {0}")]
    CodeGenError(#[from] CodeGenError),
}

fn frontend(
    source_code: &str,
    options: &CompileOptions,
) -> Result<(AbstractSyntaxTree, Option<String>), CompileError> {
    // Parse the source code into AST nodes
    let mut ast = parser::parse(source_code).map_err(|err| CompileError::ParseError(err.into()))?;

    // Check that the code is semantically correct before attempting to generate code
    semantic_analysis(&mut ast).map_err(|errs| CompileError::SemanticErrors(errs))?;

    if options.request_ast {
        let ast_string = format!("{}", ast);
        Ok((ast, Some(ast_string)))
    } else {
        Ok((ast, None))
    }
}

fn jit_backend(
    ast: AbstractSyntaxTree,
    options: CodeGenOptions,
) -> Result<(*const u8, Option<String>, Option<String>), CompileError> {
    let mut code_generator = CodeGenerator::<cranelift_jit::JITModule>::new(options)?;

    let (ir, disassembly) = code_generator
        .generate(ast)
        .map_err(|err| CompileError::CodeGenError(err))?;

    let code = code_generator
        .get_main_function()
        .ok_or(CompileError::SemanticErrors(vec![
            SemanticError::MissingMainError,
        ]))?;

    Ok((code, ir, disassembly))
}

/// The primary function responsible for compiling TODO_LANG_NAME source code to machine code ahead of time (AOT).
///
/// This is the entry point of the API. After creating a [`CompileOptions`] with [`CompileOptions::new()`],
/// calling this function does all of the compilation.
pub fn compile(
    source_code: &str,
    options: CompileOptions,
) -> Result<(Option<String>, Option<String>), CompileError> {
    todo!("support AOT object compilation")
}

/// The primary function responsible for compiling TODO_LANG_NAME source code to machine code just in time (JIT).
///
/// This is the entry point of the API. After creating a [`CompileOptions`] with [`CompileOptions::new()`],
/// calling this function does all of the compilation.
pub fn compile_jit(
    source_code: &str,
    options: CompileOptions,
) -> Result<
    (*const u8, Option<String>, Option<String>, Option<String>),
    (CompileError, Option<String>),
> {
    let (ast, ast_string) = frontend(source_code, &options).map_err(|err| (err, None))?;

    let (code, ir, disassembly) = match jit_backend(ast, options.codegen_options) {
        Ok((code, ir, disassembly)) => (code, ir, disassembly),
        Err(err) => return Err((err, ast_string)),
    };

    Ok((code, ast_string, ir, disassembly))
}
