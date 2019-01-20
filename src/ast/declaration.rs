use super::table::{Variable, Function, Procedure};
use super::statement::{Scope};
use super::types::{parse_type};
use crate::parser::{Rule};
use pest::iterators::Pair;
use std::rc::Rc;

pub enum Declaration {
    Variable(Vec<Rc<Variable>>),
    Function(Function),
    Procedure(Procedure)
}

fn parse_variable_declaration(pair : Pair<Rule>) -> Option<Declaration> {
    if pair.as_rule() != Rule::variable_declaration {return None;}
    let mut pairs = pair.into_inner();
    let names = pairs.next().unwrap();
    let vtype = parse_type(pairs.next().unwrap()).unwrap();
    Some(Declaration::Variable(
        names.into_inner().map(
            |name| Rc::new(Variable::new(name.as_str().to_string(), vtype.clone()))
        ).collect()
    ))
}

fn parse_procedure_declaration(pair : Pair<Rule>) -> Option<Declaration> {
    if pair.as_rule() != Rule::procedure_declaration {return None;}
    let mut pairs = pair.into_inner();
    let name = pairs.next().unwrap().as_str().to_string();
    let parameter_pairs = pairs.next().unwrap().into_inner();
    let mut parameters = Vec::new();
    for pair in parameter_pairs {
        let mut pairs = pair.into_inner();
        let names = pairs.next().unwrap().into_inner().map(|p| p.as_str().to_string());
        let ptype = parse_type(pairs.next().unwrap()).unwrap();
        for name in names {
            parameters.push(Rc::new(Variable::new(name, ptype.clone())))
        }
    }
    let rtype = parse_type(pairs.next().unwrap());
    let scope = pairs.next().unwrap(); //TODO: parse
    match rtype {
        Some(t) => Some(Declaration::Function(Function::new(name, parameters, t, Scope::empty()))),
        None => Some(Declaration::Procedure(Procedure::new(name, parameters, Scope::empty())))
    }
}

pub fn parse_declaration(pair : Pair<Rule>) -> Option<Declaration> {
    match pair.as_rule() {
        Rule::variable_declaration => parse_variable_declaration(pair),
        Rule::procedure_declaration => parse_procedure_declaration(pair),
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
        let decl = parse_declaration(
            CSC488Parser::parse(Rule::declaration, "var u, v [3] integer")
            .unwrap().next().unwrap()
        ).unwrap();
        match decl {
            Declaration::Variable(v) => {
                assert_eq!(v, [
                    Rc::new(Variable::new("u".to_string(), vtype.clone())),
                    Rc::new(Variable::new("v".to_string(), vtype.clone()))
                ])
            },
            _ => panic!("Didn't parse a variable!")
        }
    }

    #[test]
    fn procedure_and_function_declarations_parse_properly() {
        let target_procedure = Procedure::new(
            "my_procedure".to_string(),
            vec![
                Rc::new(Variable::integer("x".to_string())),
                Rc::new(Variable::integer("y".to_string())),
                Rc::new(Variable::boolean("flag".to_string()))],
            Scope::empty()
        );
        let decl = parse_declaration(
            CSC488Parser::parse(Rule::declaration,
                "func my_procedure(x, y integer, flag boolean) {
                    func a_nested_function(x, y boolean, n integer) {
                        var an_array_variable, another_array_variable [3] boolean
                    }
                 }")
            .unwrap().next().unwrap()
        ).unwrap();
        match decl {
            Declaration::Procedure(p) => assert_eq!(p, target_procedure),
            _ => panic!("Didn't parse a procedure")
        }
    }
}
