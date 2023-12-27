use thiserror::Error;

use crate::{
    codegen::{
        codegen::{CodeGenError, CodeGenerator, JitCodeGenResults},
        options::CodeGenOptions,
    },
    diagnostic::Diagnostic,
    parser::ast::AbstractSyntaxTree,
};

#[derive(Debug, Error)]
pub(super) enum BackendError {
    #[error(transparent)]
    CodeGenError(#[from] CodeGenError),
}

impl From<BackendError> for Diagnostic {
    fn from(err: BackendError) -> Self {
        match err {
            BackendError::CodeGenError(err) => todo!(),
        }
    }
}

pub(super) struct JitBackendResults {
    pub(super) code: *const u8,
    pub(super) ir: Option<String>,
    pub(super) disassembly: Option<String>,
}

pub(super) fn jit_backend(
    ast: AbstractSyntaxTree,
    options: CodeGenOptions,
) -> Result<JitCodeGenResults, BackendError> {
    let mut code_generator = CodeGenerator::<cranelift_jit::JITModule>::new(options)?;
    code_generator.generate(ast)?;

    Ok(code_generator.results())
}
