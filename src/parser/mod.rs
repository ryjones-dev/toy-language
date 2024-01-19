use thiserror::Error;

use crate::diagnostic::{Diagnostic, DiagnosticLevel};

use self::{
    ast::AbstractSyntaxTree,
    expression::{
        BinaryMathOperationType, BooleanComparisonType, Expression, UnaryMathOperationType,
    },
    function::{Function, FunctionSignature},
    identifier::Identifier,
    literal::Literal,
    scope::Scope,
    types::Type,
    variable::Variable,
};

pub(super) mod ast;
pub(super) mod expression;
pub(super) mod function;
pub(super) mod identifier;
pub(super) mod literal;
pub(super) mod scope;
pub(super) mod source_range;
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
        _ sc:scope() _ {
            Function {
                signature: FunctionSignature {
                    name: i,
                    params: p.into_iter().map(|(param_name, param_type)| Variable::new(param_name, Some(param_type))).collect(),
                    returns: r.unwrap_or(Vec::default()).into(),
                    source: (s..=e).into()
                },
                scope: sc
            }
        }

    // Each level of precedence is notated by a "--" line. Each level binds more tightly than the last.
    // Expressions between the same "--" lines have the same level of precedence.
    #[cache_left_rec]
    rule expression() -> Expression = s:position!() expr:precedence! {
        s:scope() { Expression::Scope { scope: s, source: (0..=0).into() } }
        --
        l:expression_list() { l }
        --
        a:assignment() { a }
        r:function_return() { r }
        c:function_call() { c }
        i:if_else() { i }
        --
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
        "(" _ e:expression() _ ")" { e }
        i:int_literal() { Expression::IntLiteral(i) }
        b:bool_literal() { Expression::BoolLiteral(b) }
        i:identifier() { Expression::Variable(Variable::new(i, None)) }
    } e:position!() {
        // We can't add the start and end positions to each expression type due to the precedence!() macro,
        // so we have to grossly create these expression twice, first with all of the parsed expression info,
        // then again with the source range.
        // Expressions where we can call to a separately rule don't need to do this, since that rule will parse the source range.
        match expr {
            Expression::Scope { scope, source } => Expression::Scope { scope, source: (s..=e).into() },
            Expression::BooleanComparison { comparison_type, lhs, rhs, .. } => Expression::BooleanComparison { comparison_type, lhs, rhs, source: (s..=e).into() },
            Expression::BinaryMathOperation { operation_type, lhs, rhs, .. } => Expression::BinaryMathOperation { operation_type, lhs, rhs, source: (s..=e).into() },
            Expression::UnaryMathOperation { operation_type, expression, .. } => Expression::UnaryMathOperation { operation_type, expression, source: (s..=e).into() },
            expression => expression,
        }
    }

    rule scope() -> Scope
        = "{" _ e:expression()* _ "}" { Scope::new(e) }

    #[cache_left_rec]
    rule expression_list() -> Expression
        = s:position!() exprs:((_ expr:expression() e:position!() _ { (expr, e) }) ++ ",") {
            let (_, end) = *exprs.last().unwrap();
            let expressions = exprs.into_iter().map(|(expr, _)| expr).collect();
            Expression::ExpressionList { expressions, source: (s..=end).into() }
        }

    rule assignment() -> Expression
        = s:position!() vars:((_ i:identifier() _ t:_type()? _ { (i, t) }) ++ ",") _ "=" _ expr:expression() e:position!() {
            Expression::Assignment { variables: vars.into_iter().map(|var|Variable::new(var.0, var.1)).collect(), expression: Box::new(expr), source: (s..=e).into() }
        }

    rule function_return() -> Expression
        = s:position!() "->" _ expr:expression() e:position!() _ { Expression::FunctionReturn { expression: Box::new(expr), source: (s..=e).into() } }

    rule function_call() -> Expression
        // Need to explicitly use an optional expression list for the arguments,
        // otherwise relying on the left recursion in expression() will cause issues.
        = s:position!() i:identifier() _ "(" _ l:expression_list()? _ ")" e:position!() {
            Expression::FunctionCall { name: i, argument_expression: Box::new(l), source: (s..=e).into(), function_signature: None }
        }

    rule if_else() -> Expression
        = s:position!() "if" _ c:expression() _ t:expression() _ "else"? _ el:expression()? e:position!() {
            Expression::IfElse { cond_expression: Box::new(c), then_expression: Box::new(t), else_expression: Box::new(el), source: (s..=e).into() }
        }

    rule _type() -> Type
        = s:position!() t:$("int" / "bool") e:position!()  { Type::new(t.parse().expect("unknown type"), (s..=e).into()) }

    rule identifier() -> Identifier
            = quiet!{ s:position!() n:$(['_' | 'a'..='z' | 'A'..='Z']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) e:position!() {
                Identifier::new(n.to_string(), (s..=e).into())
            }}
            / expected!("identifier")

    rule int_literal() -> Literal<i64>
        = quiet!{ s:position!() n:$(['0'..='9']+) e:position!() { Literal::new(n.parse().expect("unknown int literal"), (s..=e).into()) }}
        / expected!("integer")

    rule bool_literal() -> Literal<bool>
        = s:position!() b:$( "true" / "false" ) e:position!() { Literal::new(b.parse().expect("unknown bool literal"), (s..=e).into()) }

    rule comment() = "#" (!"\n" [_])* ("\n" / ![_])

    // Ignore whitespace and line comments
    rule _() = quiet!{ ([' ' | '\t' | '\n'] / comment())* }
});
