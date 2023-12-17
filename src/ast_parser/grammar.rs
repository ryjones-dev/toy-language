use crate::ast_parser::types::{
    AbstractSyntaxTree, BinaryMathOperationType, BooleanComparisonType, Expression, Function,
    FunctionCall, FunctionSignature, Identifier, IntLiteral, Statement, Type,
    UnaryMathOperationType,
};

peg::parser!(pub grammar parser() for str {
    /// Parses the given input source code into the relevant syntax tree types.
    ///
    /// The resulting types contain all of the information that the compiler needs
    /// to perform semantic analysis and generate the IR code.
    pub rule code() -> AbstractSyntaxTree
        = f:function()* { AbstractSyntaxTree(f) }

    rule function() -> Function
        = _ i:identifier() _ "(" _ p:((_ i:identifier() _ {i}) ** ",") _ ")" r:(_ "->" r:((_ "int" _ {Type::Int}) ++ ",") {r})? _ s:scope() _ {
            // TODO: support anonymous functions
            Function { signature: FunctionSignature { name: Some(i), params: p, returns: r.unwrap_or(Vec::default()).into() }, body: s }
        }

    rule scope() -> Vec<Statement>
        = ":" _ s:statement()* _ ";" { s }

    rule statement() -> Statement
        = _ a:assignment() _  { a }
        / _ c:call_function() _ { Statement::FunctionCall(c) }
        / _ r:return_statement() _ { r }

    rule return_statement() -> Statement
        = _ "->" _ r:(e:((_ e:expression() _ {e}) ** ",")) _ { Statement::Return(r) }

    rule assignment() -> Statement
        = idents:((_ i:identifier() _ { Some(i) } / _ "_" _ { None }) ** ",") _ "=" _ e:expression() { Statement::Assignment(idents, e) }

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
        c:call_function() { Expression::FunctionCall(c) }
        --
        "(" _ e:expression() _ ")" { e }
        l:int_literal() { Expression::IntLiteral(l) }
        i:identifier() { Expression::Variable(i) }
    }

    rule call_function() -> FunctionCall
        = i:identifier() _ "(" _ args:((_ e:expression() _ {e}) ** ",") _ ")" { FunctionCall {name: i, arguments: args } }

    rule identifier() -> Identifier
            = quiet!{ n:$(['a'..='z' | 'A'..='Z']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { Identifier::from(n.to_string()) } }
            / expected!("identifier")

    rule int_literal() -> IntLiteral
        = quiet!{ n:$(['0'..='9']+) { <IntLiteral as std::str::FromStr>::from_str(n).unwrap() } }
        / expected!("integer")

    rule comment() = "#" (!"\n" [_])* ("\n" / ![_])

    // Ignore whitespace and line comments
    rule _() = quiet!{ ([' ' | '\t' | '\n'] / comment())* }
});
