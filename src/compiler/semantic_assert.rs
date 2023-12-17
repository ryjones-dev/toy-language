/// Debug assert that ensures the [`semantic`] module catches any errors with the source code before
/// the code generation starts.
///
/// This assert is useful during development to ensure that the semantic module is implmenting enough error checks
/// to catch errors before getting to the code generation stage of compilation.
#[macro_export]
macro_rules! semantic_assert {
        ($expression:expr, $($args:expr),*) => {
            debug_assert!($expression, "{}. This should be caught by semantic analysis.", $($args)*);
        };
    }

pub(super) use semantic_assert;
