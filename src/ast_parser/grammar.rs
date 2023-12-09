use crate::ast_parser::types::{
    BinaryMathOperationType, BooleanComparisonType, Expression, Function, Identifier, IntLiteral,
    Statement, UnaryMathOperationType,
};

peg::parser!(pub grammar parser() for str {
    /// Parses the given input source code into the relevant syntax tree types.
    ///
    /// The resulting types contain all of the information that the compiler needs to generate the IR code.
    pub rule code() -> Vec<Function>
        = f:function()* { f }

    rule function() -> Function
        = _ i:identifier() _ "(" _ p:((_ i:identifier() _ {i}) ** ",") _ ")" _ ":" _ s:(s:statements() r:return_statement() _ {
            Vec::from_iter(s.into_iter().chain(std::iter::once(r))) // Append return statement to the vector of statements
        }) {
            Function { name: i, params: p, body: s }
        }

    rule statements() -> Vec<Statement>
        = s:statement()* { s }

    rule statement() -> Statement
        = _ a:assignment() _  { a }
        / _ c:call_function() _ { Statement::FunctionCall(c.1) }

    // Handle return statements separately, since a function should only have one at the end of the body.
    rule return_statement() -> Statement
        = _ "->" _ r:("_" {Vec::new()} / e:((_ e:expression() _ {e}) ** ",")) _ { Statement::Return(r) }

    rule assignment() -> Statement
        = idents:((_ i:identifier() _ {i}) ** ",") _ "=" _ e:expression() {Statement::Assignment(idents, e)}

    // Each level of precedence is notated by a "--" line. Precedence is in ascending order.
    // Expressions between the same "--" lines have the same level of precedence.
    rule expression() -> Expression = precedence! {
        a:(@) _ "==" _ b:@ { Expression::BooleanComparison(BooleanComparisonType::Equal, Box::new(a), Box::new(b)) }
        a:(@) _ "!=" _ b:@ { Expression::BooleanComparison(BooleanComparisonType::NotEqual, Box::new(a), Box::new(b)) }
        a:(@) _ "<" _ b:@ { Expression::BooleanComparison(BooleanComparisonType::LessThan, Box::new(a), Box::new(b)) }
        a:(@) _ "<=" _ b:@ { Expression::BooleanComparison(BooleanComparisonType::LessThanEqual, Box::new(a), Box::new(b)) }
        a:(@) _ ">" _ b:@ { Expression::BooleanComparison(BooleanComparisonType::GreaterThan, Box::new(a), Box::new(b)) }
        a:(@) _ ">=" _ b:@ { Expression::BooleanComparison(BooleanComparisonType::GreaterThanEqual, Box::new(a), Box::new(b)) }
        --
        a:(@) _ "+" _ b:@ { Expression::BinaryMathOperation(BinaryMathOperationType::Add, Box::new(a), Box::new(b)) }
        a:(@) _ "-" _ b:@ { Expression::BinaryMathOperation(BinaryMathOperationType::Subtract, Box::new(a), Box::new(b)) }
        --
        a:(@) _ "*" _ b:@ { Expression::BinaryMathOperation(BinaryMathOperationType::Multiply, Box::new(a), Box::new(b)) }
        a:(@) _ "/" _ b:@ { Expression::BinaryMathOperation(BinaryMathOperationType::Divide, Box::new(a), Box::new(b)) }
        --
        "-" e:@ { Expression::UnaryMathOperation(UnaryMathOperationType::Negate, Box::new(e)) }
        --
        c:call_function_expr() { c }
        --
        "(" _ e:expression() _ ")" { e }
        l:int_literal() { Expression::IntLiteral(l) }
        i:identifier() { Expression::Variable(i) }
    }

    rule call_function_expr() -> Expression
        = c:call_function() { Expression::FunctionCall(c.0, c.1) }

    rule call_function() -> (Identifier, Vec<Expression>)
        = i:identifier() _ "(" _ args:((_ e:expression() _ {e}) ** ",") _ ")" { (i, args) }

    rule identifier() -> Identifier
            = quiet!{ n:$(['a'..='z' | 'A'..='Z']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { Identifier::from(n.to_owned()) } }
            / expected!("identifier")

    rule int_literal() -> IntLiteral
        = quiet!{ n:$(['0'..='9']+) { <IntLiteral as std::str::FromStr>::from_str(n).unwrap() } }
        / expected!("integer")

    rule comment() = "#" (!"\n" [_])* ("\n" / ![_])

    // Ignore whitespace and line comments
    rule _() = quiet!{ ([' ' | '\t' | '\n'] / comment())* }
});
