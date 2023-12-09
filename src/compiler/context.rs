use cranelift::codegen::Context;

use super::codegen::CompilerModule;

/// CompilerContext wraps the underlying Cranelift context.
///
/// This is done to handle some bookkeeping about setting and resetting fields in the context.
pub(super) struct CompilerContext {
    context: Context,
    request_disassembly: bool,
}

impl CompilerContext {
    pub(super) fn new<M: CompilerModule>(module: &M, request_disassembly: bool) -> Self {
        // let mut context = Context::new();
        let mut context = module.make_context();
        context.set_disasm(request_disassembly);

        Self {
            context,
            request_disassembly,
        }
    }

    pub(super) fn clear<M: cranelift_module::Module>(&mut self, module: &M) {
        module.clear_context(&mut self.context);
        self.context.set_disasm(self.request_disassembly);
    }

    pub(super) fn get_inner_context_mut(&mut self) -> &mut Context {
        &mut self.context
    }
}
