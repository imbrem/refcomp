
use pest::prec_climber::{PrecClimber, Operator, Assoc};

#[derive(Parser)]
#[grammar="grammar488.pest"]
pub struct CSC488Parser;

fn get_binary_precedence_climber() -> PrecClimber<Rule> {
    PrecClimber::new(vec![
        Operator::new(Rule::op_plus, Assoc::Left) | Operator::new(Rule::op_minus, Assoc::Left),
        Operator::new(Rule::op_times, Assoc::Left) | Operator::new(Rule::op_divides, Assoc::Left)
    ])
}

fn get_logical_precedence_climber() -> PrecClimber<Rule> {
    PrecClimber::new(vec![
        Operator::new(Rule::kw_and, Assoc::Left),
        Operator::new(Rule::kw_or, Assoc::Left)
    ])
}

lazy_static! {
    pub static ref BINARY_PRECEDENCE_CLIMBER : PrecClimber<Rule> = get_binary_precedence_climber();
    pub static ref LOGICAL_PRECEDENCE_CLIMBER : PrecClimber<Rule> = get_logical_precedence_climber();
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn boolean_expressions_parse_correctly() {
        parses_to!(
            parser : CSC488Parser,
            input : "true",
            rule : Rule::expression,
            tokens : [
                expression(0, 4, [
                    binary_expression(0, 4, [primary_expression(0, 4, [kw_true(0, 4)])])])]
        );
        parses_to!(
            parser : CSC488Parser,
            input : "false",
            rule : Rule::expression,
            tokens : [
                expression(0, 5, [
                    binary_expression(0, 5, [primary_expression(0, 5, [kw_false(0, 5)])])])]
        )
    }

    #[test]
    fn simple_function_calls_parse_correctly() {
        parses_to!(
            parser : CSC488Parser,
            input : "some_function(x, y, true)",
            rule : Rule::expression,
            tokens : [expression(0, 25, [binary_expression(0, 25, [primary_expression(0, 25, [
                function_call(0, 25, [
                        identifier(0, 13), arguments(14, 24, [
                        expression(14, 15, [binary_expression(14, 15,
                            [primary_expression(14, 15, [identifier(14, 15)])])]),
                        expression(17, 18, [binary_expression(17, 18,
                            [primary_expression(17, 18, [identifier(17, 18)])])]),
                        expression(20, 24, [binary_expression(20, 24,
                            [primary_expression(20, 24, [kw_true(20, 24)])])]),
                ])])
            ])])])]
        )
    }

    #[test]
    fn arithmetic_expressions_parse_correctly() {
        parses_to!(
            parser : CSC488Parser,
            input : "1 + 1 < 2",
            rule : Rule::expression,
            tokens : [expression(0, 9, [comparison_expression(0, 9, [
                binary_expression(0, 5, [
                        primary_expression(0, 1, [integer(0, 1)]),
                        op_plus(2, 3),
                        primary_expression(4, 5, [integer(4, 5)])
                    ]),
                op_lt(6, 7),
                binary_expression(8, 9, [primary_expression(8, 9, [integer(8, 9)])])
                ])])]
        )
    }

    #[test]
    fn texts_parse_correctly() {
        parses_to!(
            parser : CSC488Parser,
            input : "\"text\"",
            rule : Rule::text,
            tokens : [
                text(0, 6, [string_text(1, 5)])
            ]
        );
    }

    #[test]
    fn print_statements_parse_correctly() {
        parses_to!(
            parser : CSC488Parser,
            input : "print \"hello\"",
            rule : Rule::print_statement,
            tokens : [
                print_statement(0, 13, [output(6, 13, [
                        single_output(6, 13, [text(6, 13, [string_text(7, 12)])])])])
            ]
        );
    }

    #[test]
    fn multiple_statements_parse_correctly() {
        parses_to!(
            parser : CSC488Parser,
            input : "x = 2 print \"hello\", x",
            rule : Rule::statements,
            tokens : [statements(0, 22, [
                assignment(0, 6, [
                    identifier(0, 1),
                    expression(4, 6, [binary_expression(4, 6, [
                        primary_expression(4, 5, [integer(4, 5)])])])
                    ]),
                print_statement(6, 22, [output(12, 22, [
                    single_output(12, 19, [text(12, 19, [string_text(13, 18)])]),
                    single_output(21, 22, [
                        expression(21, 22, [
                            binary_expression(21, 22, [
                                primary_expression(21, 22, [identifier(21, 22)])])])])
                    ])])
            ])]
        )
    }
}
