use cranelift::codegen::Context;

use super::codegen::CodeGeneratorModule;

/// CodeGenContext wraps the underlying Cranelift context.
///
/// This is done to handle some bookkeeping about setting and resetting fields in the context.
pub(crate) struct CodeGenContext {
    context: Context,
    request_disassembly: bool,
}

impl CodeGenContext {
    pub(crate) fn new<M: CodeGeneratorModule>(module: &M, request_disassembly: bool) -> Self {
        let mut context = module.make_context();
        context.set_disasm(request_disassembly);

        Self {
            context,
            request_disassembly,
        }
    }

    pub(crate) fn clear<M: cranelift_module::Module>(&mut self, module: &M) {
        module.clear_context(&mut self.context);
        self.context.set_disasm(self.request_disassembly);
    }

    pub(crate) fn get_inner_context_mut(&mut self) -> &mut Context {
        &mut self.context
    }
}
