pub mod types;
pub mod statement;
pub mod declaration;
pub mod expression;
pub mod table;

use self::table::{SymbolTable, Scoped};
use self::statement::{Scope};
use self::declaration::{parse_declaration, implement_declarations};
use self::statement::{parse_statement};
use crate::parser::Rule;
use pest::iterators::Pair;

pub fn parse_bare_scope(pair : Pair<Rule>, sym : &mut SymbolTable) -> Result<Scope, &'static str> {
    if pair.as_rule() != Rule::bare_scope {
        return Err("Error parsing bare scope: wrong rule!")
    }
    let mut pairs = pair.into_inner();
    let first = pairs.next().unwrap();
    let (declarations, statements) = match first.as_rule() {
        Rule::declarations => (Some(first), pairs.next()),
        Rule::statements => (None, Some(first)),
        _ => unreachable!()
    };
    let mut declarations = match declarations {
        Some(ds) => ds.into_inner().map(|d| parse_declaration(d, sym).unwrap()).collect(),
        None => Vec::new()
    };
    declarations.enter_scope(sym);
    implement_declarations(&mut declarations, sym)?;
    let statements = match statements {
        Some(ss) => ss.into_inner().map(|s| parse_statement(s, sym).unwrap()).collect(),
        None => Vec::new()
    };
    declarations.leave_scope(sym);
    Ok(Scope::new(statements, declarations))
}

#[cfg(test)]
mod test {
    use super::*;
    use super::table::{Variable, Symbol};
    use super::statement::{Assignment, Statement};
    use super::expression::{Expression, Constant, Arithmetic, ArithmeticOp};
    use crate::parser::CSC488Parser;
    use pest::Parser;
    use std::rc::Rc;
    #[test]
    fn simple_bare_scope_is_parsed_properly() {
        let mut sym = SymbolTable::new();
        let simple_bare_scope = parse_bare_scope(
            CSC488Parser::parse(Rule::bare_scope, "var x integer x = 5 + x")
            .unwrap().next().unwrap(),
            &mut sym);
        let goal_variable = Rc::new(Variable::integer("x".to_string()));
        let goal_assignment = Assignment::to_variable(
            goal_variable.clone(),
            Arithmetic::new(
                Expression::Constant(Constant::Integer(5)),
                Expression::Variable(goal_variable.clone()),
                ArithmeticOp::Add
            ).unwrap());
        assert_eq!(
            simple_bare_scope,
            Ok(Scope::new_from_symbols(
                vec![Statement::Assignment(goal_assignment)], vec![Symbol::Variable(goal_variable)]))
        )
    }

}
