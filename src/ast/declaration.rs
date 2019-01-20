use super::table::{Variable, Function, Procedure};
use super::types::{parse_type};
use crate::parser::{Rule};
use pest::iterators::Pair;

pub enum Declaration {
    Variable(Vec<Variable>),
    Function(Function),
    Procedure(Procedure)
}

fn parse_variable_declaration(pair : Pair<Rule>) -> Option<Declaration> {
    if pair.as_rule() != Rule::variable_declaration {return None;}
    let mut pairs = pair.into_inner();
    let names = pairs.next().unwrap();
    let vtype = parse_type(pairs.next().unwrap()).unwrap();
    Some(Declaration::Variable(
        names.into_inner().map(|name| Variable::new(name.as_str().to_string(), vtype.clone()))
        .collect()
    ))
}

fn parse_procedure_declaration(pair : Pair<Rule>) -> Option<Declaration> {
    None
}

fn parse_function_declaration(pair : Pair<Rule>) -> Option<Declaration> {
    None
}

pub fn parse_delcaration(pair : Pair<Rule>) -> Option<Declaration> {
    match pair.as_rule() {
        Rule::variable_declaration => parse_variable_declaration(pair),
        Rule::procedure_declaration => parse_procedure_declaration(pair),
        Rule::function_declaration => parse_function_declaration(pair),
        _ => None
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::{Rule, CSC488Parser};
    use crate::ast::types::{ArrayType, ScalarType, Type};
    use pest::Parser;
    use std::rc::Rc;

    #[test]
    fn variable_declarations_parse_properly() {
        let vtype = Type::ArrayType(Rc::new(ArrayType::new(ScalarType::Integer, vec![3])));
        let decl = parse_delcaration(
            CSC488Parser::parse(Rule::declaration, "var u, v [3] integer")
            .unwrap().next().unwrap()
        ).unwrap();
        match decl {
            Declaration::Variable(v) => {
                assert_eq!(v, [
                    Variable::new("u".to_string(), vtype.clone()),
                    Variable::new("v".to_string(), vtype.clone())
                ])
            },
            _ => panic!("Didn't parse a variable!")
        }
    }
}
