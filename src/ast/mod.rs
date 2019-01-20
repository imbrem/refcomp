pub mod types;
pub mod statement;
pub mod declaration;
pub mod expression;
pub mod table;

use table::{SymbolTable};
use statement::{Scope};
use declaration::{parse_declaration};
use crate::parser::Rule;
use pest::iterators::Pair;

pub fn parse_bare_scope(mut pair : Pair<Rule>, sym : &mut SymbolTable) -> Option<Scope> {
    match pair.as_rule() {
        Rule::scope => {pair = pair.into_inner().next().unwrap()},
        Rule::bare_scope => {},
        _ => {return None}
    };
    let mut pairs = pair.into_inner();
    let first = pairs.next().unwrap();
    let (declarations, statements) = match first.as_rule() {
        Rule::declarations => (Some(first), pairs.next()),
        Rule::statements => (None, Some(first)),
        _ => unreachable!()
    };
    let declarations = match declarations {
        Some(ds) => ds.into_inner().map(|d| parse_declaration(d, sym).unwrap()).collect(),
        None => Vec::new()
    };
    let statements = Vec::new(); // TODO: parse statements
    Some(Scope::new(statements, declarations))
}
