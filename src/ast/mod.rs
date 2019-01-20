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

pub fn parse_bare_scope(pair : Pair<Rule>, sym : &mut SymbolTable) -> Option<Scope> {
    if pair.as_rule() != Rule::bare_scope {return None}
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
