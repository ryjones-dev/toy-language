/// Optimization levels used by the [`Compiler`].
///
/// These mimic Cranelift's underlying optimization levels.
#[derive(Default)]
pub enum OptimizationLevel {
    #[default]
    None,
    Speed,
    SpeedAndSize,
}

/// Code generation options to be passed to [`compile`] or [`compile_jit`].
#[derive(Default)]
pub struct CodeGenOptions {
    pub(crate) disabled: bool,
    pub(super) optimization_level: OptimizationLevel,
    pub(super) request_ir: bool,
    pub(super) request_disassembly: bool,
}

impl CodeGenOptions {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn enable(mut self, enable: bool) -> Self {
        self.disabled = !enable;
        self
    }

    pub fn with_optimization_level(mut self, optimization_level: OptimizationLevel) -> Self {
        self.optimization_level = optimization_level;
        self
    }

    pub fn with_ir(mut self, request_ir: bool) -> Self {
        self.request_ir = request_ir;
        self
    }

    pub fn with_disassembly(mut self, request_disassembly: bool) -> Self {
        self.request_disassembly = request_disassembly;
        self
    }
}
