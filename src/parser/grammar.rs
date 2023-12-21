use crate::parser::types::{
    AbstractSyntaxTree, BinaryMathOperationType, BoolLiteral, BooleanComparisonType, Expression,
    Function, FunctionCall, FunctionSignature, Identifier, IntLiteral, Statement,
    UnaryMathOperationType, Variable,
};

peg::parser!(pub(crate) grammar parser() for str {
    /// Parses the given input source code into the relevant syntax tree types.
    ///
    /// The resulting [`AbstractSyntaxTree`] contains all of the information that the compiler needs
    /// to perform semantic analysis and generate the IR code.
    pub rule parse() -> AbstractSyntaxTree
        = f:function()* { AbstractSyntaxTree(f) }

    rule function() -> Function
        = _ i:identifier() _ "(" _
        p:((_ i:identifier() _ t:type_name() _ { (i, t.parse().expect("unknown type")) }) ** ",") _ ")"
        r:(_ "->" r:((_ t:type_name() _ { t.parse().expect("unknown type") }) ++ ",") {r})?
        _ s:scope() _ {
            Function {
                signature: FunctionSignature {
                    name: i,
                    params: p.into_iter().map(|(param_name, param_type)| Variable::new(param_name).with_type(param_type)).collect(),
                    returns: r.unwrap_or(Vec::default()).into()
                },
                body: s
            }
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
        = idents:((_ i:identifier() _ { Some(i) } / _ "_" _ { None }) ** ",") _ "=" _ e:expression() {
            Statement::Assignment(idents.into_iter().map(|ident| ident.map(|ident| Variable::new(ident))).collect(), e)
        }

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
        b:bool_literal() { Expression::BoolLiteral(b) }
        i:identifier() { Expression::Variable(Variable::new(i)) }
    }

    rule call_function() -> FunctionCall
        = i:identifier() _ "(" _ args:((_ e:expression() _ {e}) ** ",") _ ")" { FunctionCall {name: i, arguments: args, argument_types: None, return_types: None } }

    rule type_name() -> &'input str
        = t:$("int" / "bool") { t }

    rule identifier() -> Identifier
            = quiet!{ s:position!() n:$(['a'..='z' | 'A'..='Z']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) e:position!() {
                Identifier::new(n.to_string(), (s..=e).into())
            }}
            / expected!("identifier")

    rule int_literal() -> IntLiteral
        = quiet!{ s:position!() n:$(['0'..='9']+) e:position!() { IntLiteral::new(n.parse().expect("unknown int literal"), (s..=e).into()) }}
        / expected!("integer")

    rule bool_literal() -> BoolLiteral
        = s:position!() b:$( "true" / "false" ) e:position!() { BoolLiteral::new(b.parse().expect("unknown bool literal"), (s..=e).into()) }

    rule comment() = "#" (!"\n" [_])* ("\n" / ![_])

    // Ignore whitespace and line comments
    rule _() = quiet!{ ([' ' | '\t' | '\n'] / comment())* }
});
