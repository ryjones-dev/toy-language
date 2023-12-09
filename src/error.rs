use thiserror::Error;

#[derive(Error, Debug)]
#[error("{0}")]
pub struct ParseError(String);

impl ParseError {
    pub fn new(error_message: String) -> Self {
        Self(error_message)
    }
}

impl<L: std::fmt::Display> From<peg::error::ParseError<L>> for ParseError {
    fn from(value: peg::error::ParseError<L>) -> Self {
        ParseError(value.to_string())
    }
}
