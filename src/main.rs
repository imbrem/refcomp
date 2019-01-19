#[cfg_attr(test, macro_use)]
extern crate pest;
#[macro_use]
extern crate pest_derive;

#[derive(Parser)]
#[grammar="grammar488.pest"]
pub struct CSC488Parser;

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
}

fn main() {
    println!("Hello world!");
}
