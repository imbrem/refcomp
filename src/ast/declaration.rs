use super::table::{Variable, Function, Procedure, SymbolTable, Symbol, Scoped};
use super::types::{parse_type};
use super::{parse_bare_scope};
use crate::parser::{Rule};
use pest::iterators::Pair;
use std::rc::Rc;

pub enum Declaration {
    Variable(Vec<Rc<Variable>>),
    Function(Rc<Function>),
    Procedure(Rc<Procedure>)
}

impl Scoped for Declaration {
    fn enter_scope(&self, sym: &mut SymbolTable) {match self {
            Declaration::Variable(vars) => for var in vars.iter() {
                sym.define(Symbol::Variable(var.clone()))},
            Declaration::Function(f) => sym.define(Symbol::Function(f.clone())),
            Declaration::Procedure(p) => sym.define(Symbol::Procedure(p.clone()))
    }}
    fn leave_scope(&self, sym : &mut SymbolTable) {match self {
            Declaration::Variable(vars) => for var in vars.iter() {sym.undef(var.get_name());},
            Declaration::Function(f) => {sym.undef(f.get_name());},
            Declaration::Procedure(p) => {sym.undef(p.get_name());}
    }}
}

impl Scoped for Vec<Declaration> {
    fn enter_scope(&self, sym : &mut SymbolTable) {
        for decl in self.iter() {decl.enter_scope(sym);}
    }
    fn leave_scope(&self, sym : &mut SymbolTable) {
        for decl in self.iter().rev() {decl.leave_scope(sym);}
    }
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

fn parse_procedure_declaration(pair : Pair<Rule>, sym : &mut SymbolTable) -> Option<Declaration> {
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
        Some(t) => {
            let mut func = Function::new(name, parameters, t);
            func.enter_scope(sym);
            let scope = parse_bare_scope(scope.into_inner().next().unwrap(), sym).unwrap();
            func.implement(scope);
            func.leave_scope(sym);
            Some(Declaration::Function(Rc::new(func)))
        },
        None => {
            let mut proc = Procedure::new(name, parameters);
            proc.enter_scope(sym);
            let scope = parse_bare_scope(scope.into_inner().next().unwrap(), sym).unwrap();
            proc.implement(scope);
            proc.leave_scope(sym);
            Some(Declaration::Procedure(Rc::new(proc)))
        },
    }
}

pub fn parse_declaration(pair : Pair<Rule>, sym : &mut SymbolTable) -> Option<Declaration> {
    match pair.as_rule() {
        Rule::variable_declaration => parse_variable_declaration(pair),
        Rule::procedure_declaration => parse_procedure_declaration(pair, sym),
        _ => None
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::statement::{Statement, Scope, Assignment, OutputElement};
    use crate::ast::expression::{Expression, Constant};
    use crate::parser::{Rule, CSC488Parser};
    use crate::ast::types::{ArrayType, ScalarType, Type};
    use pest::Parser;
    use std::rc::Rc;

    #[test]
    fn variable_declarations_parse_properly() {
        let vtype = Type::ArrayType(Rc::new(ArrayType::new(ScalarType::Integer, vec![3])));
        let mut sym = SymbolTable::new();
        let decl = parse_declaration(
            CSC488Parser::parse(Rule::declaration, "var u, v [3] integer")
            .unwrap().next().unwrap(),
            &mut sym
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
        let mut target_procedure = Procedure::new(
            "my_procedure".to_string(),
            vec![
                Rc::new(Variable::integer("x".to_string())),
                Rc::new(Variable::integer("y".to_string())),
                Rc::new(Variable::boolean("flag".to_string()))]
        );
        let mut target_function = Function::new(
            "a_nested_function".to_string(),
            vec![
                Rc::new(Variable::boolean("x".to_string())),
                Rc::new(Variable::boolean("y".to_string())),
                Rc::new(Variable::integer("n".to_string()))],
            Type::integer()
        );
        let atype =
            Type::ArrayType(Rc::new(ArrayType::new(ScalarType::Boolean, vec![3])));
        let nested_variables = vec![
            Symbol::Variable(Rc::new(Variable::new(
                "an_array_variable".to_string(),
                atype.clone()
            ))),
            Symbol::Variable(Rc::new(Variable::new(
                "another_array_variable".to_string(),
                atype.clone()
            )))];
        target_function.implement(
            Scope::new_from_symbols(Vec::new(), nested_variables));

        let inner_variable = Rc::new(Variable::integer("an_integer".to_string()));
        let target_constant_assignment = Assignment::to_variable(
            inner_variable.clone(),
            Expression::Constant(Constant::Integer(75))
        );
        let target_variable_assignment = Assignment::to_variable(
            inner_variable.clone(),
            Expression::Variable(inner_variable.clone())
        );

        let target_print = vec![
            OutputElement::Text("hello".to_string()),
            OutputElement::Expression(Expression::Variable(inner_variable.clone()))
        ];

        target_procedure.implement(Scope::new_from_symbols(
            vec![
                Statement::Assignment(target_constant_assignment),
                Statement::Assignment(target_variable_assignment),
                Statement::Print(target_print)
                ],
            vec![
                Symbol::Function(Rc::new(target_function)),
                Symbol::Variable(inner_variable)
                ]));

        let mut sym = SymbolTable::new();
        let decl = parse_declaration(
            CSC488Parser::parse(Rule::declaration,
                "func my_procedure(x, y integer, flag boolean) {
                    func a_nested_function(x, y boolean, n integer) integer {
                        var an_array_variable, another_array_variable [3] boolean
                    }
                    var an_integer integer
                    // Assign a constant value to the integer
                    an_integer = 15 * 5
                    // Assign the integer to itself
                    an_integer = an_integer
                    // Print out the value of the integer
                    print \"hello\", an_integer
                 }")
            .unwrap().next().unwrap(),
            &mut sym
        ).unwrap();
        match decl {
            Declaration::Procedure(p) => assert_eq!(*p, target_procedure),
            _ => panic!("Didn't parse a procedure")
        }
    }
}
