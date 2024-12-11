use super::{Function, Struct};

/// A code primitive that can be defined in the global scope.
///
/// These are the starting points for any program.
/// All other code primitives can only be defined in local or function scopes.
#[derive(Debug)]
pub(crate) enum Definition {
    Struct(Struct),
    Function(Function),
}
