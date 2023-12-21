use crate::codegen::options::CodeGenOptions;

pub struct CompileOptions {
    pub(super) request_ast: bool,
    pub(super) codegen_options: CodeGenOptions,
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
