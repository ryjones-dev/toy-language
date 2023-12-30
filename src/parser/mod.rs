use thiserror::Error;

use crate::diagnostic::{Diagnostic, DiagnosticLevel};

use self::{
    ast::AbstractSyntaxTree,
    expression::{
        BinaryMathOperationType, BooleanComparisonType, Expression, UnaryMathOperationType,
    },
    function::{Function, FunctionCall, FunctionParameter, FunctionSignature},
    identifier::Identifier,
    statement::Statement,
    types::Type,
    variable::Variable,
};

pub(super) mod ast;
pub(super) mod expression;
pub(super) mod function;
pub(super) mod identifier;
pub(super) mod source_range;
pub(super) mod statement;
pub(super) mod types;
pub(super) mod variable;

/// An error that is thrown when parsing source code fails.
///
/// In the implementation, [`peg`] is used to parse source code.
/// Because [`peg::error::ParseError`] has a generic parameter, it makes it difficult
/// to use in this API. Therefore, [`ParseError`] here is a target type that the underlying
/// [`peg::error::ParseError`] can be mapped to for easier error handling.
/// [`ParseError`] is transparent and just outputs whatever error message [`peg::error::ParseError`] would have.
#[derive(Debug, Error)]
#[error("{0}")]
pub(super) struct ParseError(String);

impl<L: std::fmt::Display> From<peg::error::ParseError<L>> for ParseError {
    fn from(value: peg::error::ParseError<L>) -> Self {
        ParseError(value.to_string())
    }
}

impl From<ParseError> for Diagnostic {
    fn from(err: ParseError) -> Self {
        Self::new(&err, DiagnosticLevel::Error)
    }
}

peg::parser!(pub(crate) grammar parser() for str {
    /// Parses the given input source code into the relevant syntax tree types.
    ///
    /// The resulting [`AbstractSyntaxTree`] contains all of the information that the compiler needs
    /// to perform semantic analysis and generate the IR code.
    pub rule parse() -> AbstractSyntaxTree
        = f:function()* { AbstractSyntaxTree(f) }

    rule function() -> Function
        = _ s:position!() i:identifier() _ "(" _
        p:((_ i:identifier() _ t:_type() _ { (i, t) }) ** ",") _ ")"
        r:(_ "->" r:((_ t:_type() _ { t }) ++ ",") {r})? e:position!()
        _ body:scope() _ {
            Function {
                signature: FunctionSignature {
                    name: i,
                    params: p.into_iter().map(|(param_name, param_type)| FunctionParameter::new(param_name, param_type)).collect(),
                    returns: r.unwrap_or(Vec::default()).into(),
                    source: (s..=e).into()
                },
                body
            }
        }

    rule scope() -> Vec<Statement>
        = ":" _ s:statement()* _ ";" { s }

    rule statement() -> Statement
        = _ a:assignment() _  { a }
        / _ c:call_function() _ { Statement::FunctionCall(c) }
        / _ r:return_statement() _ { r }

    rule return_statement() -> Statement
        = s:position!() "->" _ r:(e:((_ e:expression() _ {e}) ++ ",")) e:position!() { Statement::Return { expressions: r, source: (s..=e).into() } }

    rule assignment() -> Statement
        = s:position!() idents:((_ i:identifier() _ { i }) ++ ",") _ "=" _ expr:expression() e:position!() {
            Statement::Assignment { variables: idents.into_iter().map(|ident|Variable::new(ident)).collect(), expression: expr, source: (s..=e).into() }
        }

    // Each level of precedence is notated by a "--" line. Precedence is in ascending order.
    // Expressions between the same "--" lines have the same level of precedence.
    rule expression() -> Expression = s:position!() expr:precedence! {
        // We can't add the start and end positions to each expression type due to the precedence!() macro,
        // so we have to grossly create each expression twice, first with all of the parsed expression info,
        // then again with the source range.
        a:(@) _ "==" _ b:@ { Expression::BooleanComparison { comparison_type: BooleanComparisonType::Equal, lhs: Box::new(a), rhs: Box::new(b), source: (0..=0).into() }}
        a:(@) _ "!=" _ b:@ { Expression::BooleanComparison { comparison_type: BooleanComparisonType::NotEqual, lhs: Box::new(a), rhs: Box::new(b), source: (0..=0).into() }}
        a:(@) _ "<" _ b:@ { Expression::BooleanComparison { comparison_type: BooleanComparisonType::LessThan, lhs: Box::new(a), rhs: Box::new(b), source: (0..=0).into() }}
        a:(@) _ "<=" _ b:@ { Expression::BooleanComparison { comparison_type: BooleanComparisonType::LessThanEqual, lhs: Box::new(a), rhs: Box::new(b), source: (0..=0).into() }}
        a:(@) _ ">" _ b:@ { Expression::BooleanComparison { comparison_type: BooleanComparisonType::GreaterThan, lhs: Box::new(a), rhs: Box::new(b), source: (0..=0).into() }}
        a:(@) _ ">=" _ b:@ { Expression::BooleanComparison { comparison_type: BooleanComparisonType::GreaterThanEqual, lhs: Box::new(a), rhs: Box::new(b), source: (0..=0).into() }}
        --
        a:(@) _ "+" _ b:@ { Expression::BinaryMathOperation { operation_type: BinaryMathOperationType::Add, lhs: Box::new(a), rhs: Box::new(b), source: (0..=0).into() }}
        a:(@) _ "-" _ b:@ { Expression::BinaryMathOperation { operation_type: BinaryMathOperationType::Subtract, lhs: Box::new(a), rhs: Box::new(b), source: (0..=0).into() }}
        --
        a:(@) _ "*" _ b:@ { Expression::BinaryMathOperation { operation_type: BinaryMathOperationType::Multiply, lhs: Box::new(a), rhs: Box::new(b), source: (0..=0).into() }}
        a:(@) _ "/" _ b:@ { Expression::BinaryMathOperation { operation_type: BinaryMathOperationType::Divide, lhs: Box::new(a), rhs: Box::new(b), source: (0..=0).into() }}
        --
        "-" e:@ { Expression::UnaryMathOperation { operation_type: UnaryMathOperationType::Negate, expression: Box::new(e), source: (0..=0).into() }}
        --
        c:call_function() { Expression::FunctionCall(c) }
        --
        "(" _ e:expression() _ ")" { e }
        i:int_literal() { i }
        b:bool_literal() { b }
        i:identifier() { Expression::Variable(Variable::new(i)) }
    } e:position!() {
        match expr {
            Expression::BooleanComparison { comparison_type, lhs, rhs, .. } => Expression::BooleanComparison { comparison_type, lhs, rhs, source: (s..=e).into() },
            Expression::BinaryMathOperation { operation_type, lhs, rhs, .. } => Expression::BinaryMathOperation { operation_type, lhs, rhs, source: (s..=e).into() },
            Expression::UnaryMathOperation { operation_type, expression, .. } => Expression::UnaryMathOperation { operation_type, expression, source: (s..=e).into() },
            Expression::FunctionCall(function_call) => Expression::FunctionCall(function_call),
            Expression::Variable(variable) => Expression::Variable(variable),
            Expression::IntLiteral(int_literal, source) => Expression::IntLiteral(int_literal, source),
            Expression::BoolLiteral(bool_literal, source) => Expression::BoolLiteral(bool_literal, source),
        }
    }

    rule call_function() -> FunctionCall
        = s:position!() i:identifier() _ "(" _ args:((_ e:expression() _ {e}) ** ",") _ ")" e:position!() {
            FunctionCall {name: i, arguments: args, source: (s..=e).into(), function_signature: None }
        }

    rule _type() -> Type
        = s:position!() t:$("int" / "bool") e:position!()  { Type::new(t.parse().expect("unknown type"), (s..=e).into()) }

    rule identifier() -> Identifier
            = quiet!{ s:position!() n:$(['_' | 'a'..='z' | 'A'..='Z']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) e:position!() {
                Identifier::new(n.to_string(), (s..=e).into())
            }}
            / expected!("identifier")

    rule int_literal() -> Expression
        = quiet!{ s:position!() n:$(['0'..='9']+) e:position!() { Expression::IntLiteral(n.parse().expect("unknown int literal"), (s..=e).into()) }}
        / expected!("integer")

    rule bool_literal() -> Expression
        = s:position!() b:$( "true" / "false" ) e:position!() { Expression::BoolLiteral(b.parse().expect("unknown bool literal"), (s..=e).into()) }

    rule comment() = "#" (!"\n" [_])* ("\n" / ![_])

    // Ignore whitespace and line comments
    rule _() = quiet!{ ([' ' | '\t' | '\n'] / comment())* }
});
