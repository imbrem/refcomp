use super::table::{Variable, Function, SymbolTable, Symbol, Scoped, Callable};
use super::types::{parse_type};
use super::{parse_bare_scope};
use crate::parser::{Rule};
use pest::iterators::Pair;
use std::rc::Rc;

#[derive(Debug)]
pub enum Declaration<'a> {
    Variable(Vec<Rc<Variable>>),
    Function(Option<Pair<'a, Rule>>, Rc<Function>),
    Procedure(Option<Pair<'a, Rule>>, Rc<Function>)
}

impl<'a> Scoped for Declaration<'a> {
    fn enter_scope(&self, sym: &mut SymbolTable) {match self {
            Declaration::Variable(vars) => for var in vars.iter() {
                sym.define(Symbol::Variable(var.clone()))},
            Declaration::Function(_, f) => sym.define(Symbol::Function(f.clone())),
            Declaration::Procedure(_, p) => sym.define(Symbol::Procedure(p.clone()))
    }}
    fn leave_scope(&self, sym : &mut SymbolTable) {match self {
            Declaration::Variable(vars) => for var in vars.iter() {sym.undef(var.get_name());},
            Declaration::Function(_, f) => {sym.undef(f.get_name());},
            Declaration::Procedure(_, p) => {sym.undef(p.get_name());}
    }}
}

impl<'a> Scoped for Vec<Declaration<'a>> {
    fn enter_scope(&self, sym : &mut SymbolTable) {
        for decl in self.iter() {decl.enter_scope(sym);}
    }
    fn leave_scope(&self, sym : &mut SymbolTable) {
        for decl in self.iter().rev() {decl.leave_scope(sym);}
    }
}

pub fn implement_declarations<'a>(declarations : &mut Vec<Declaration<'a>>, sym : &mut SymbolTable)
-> Result<(), &'static str> {
    for decl in declarations {
        match decl {
            Declaration::Function(ref mut s, f) => {
                sym.enter_level();
                let mut scope = None;
                std::mem::swap(&mut scope, s);
                if let Some(scope) = scope {
                    f.enter_scope(sym);
                    f.implement(parse_bare_scope(scope, sym)?);
                    f.leave_scope(sym);
                } else {return Err("Tried to implement function without scope!");}
                sym.leave_level();
            },
            Declaration::Procedure(ref mut s, p) => {
                sym.enter_level();
                let mut scope = None;
                std::mem::swap(&mut scope, s);
                if let Some(scope) = scope {
                    p.enter_scope(sym);
                    p.implement(parse_bare_scope(scope, sym)?);
                    p.leave_scope(sym);
                }
                else {return Err("Tried to implement function without scope!");}
                sym.leave_level();
            },
            _ => {}
        }
    }
    Ok(())
}

fn parse_variable_declaration(pair : Pair<Rule>) -> Result<Declaration, &'static str> {
    if pair.as_rule() != Rule::variable_declaration {
        return Err("Error parsing variable declaration: wrong rule!");
    }
    let mut pairs = pair.into_inner();
    let names = pairs.next().unwrap();
    let vtype = parse_type(pairs.next().unwrap()).unwrap();
    Ok(Declaration::Variable(
        names.into_inner().map(
            |name| Rc::new(Variable::new(name.as_str().to_string(), vtype.clone()))
        ).collect()
    ))
}

fn parse_procedure_declaration<'a>(pair : Pair<'a, Rule>, _sym : &mut SymbolTable)
-> Result<Declaration<'a>, &'static str> {
    if pair.as_rule() != Rule::procedure_declaration {
        return Err("Error parsing procedure declaration: wrong rule");
    }
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
            let func = Rc::new(Function::new(name, parameters, t));
            let scope = scope.into_inner().next().unwrap();
            Ok(Declaration::Function(Some(scope), func))
        },
        None => {
            let proc = Rc::new(Function::procedure(name, parameters));
            let scope = scope.into_inner().next().unwrap();
            Ok(Declaration::Procedure(Some(scope), proc))
        },
    }
}

pub fn parse_declaration<'a>(pair : Pair<'a, Rule>, sym : &mut SymbolTable)
    -> Result<Declaration<'a>, &'static str> {
    match pair.as_rule() {
        Rule::variable_declaration => parse_variable_declaration(pair),
        Rule::procedure_declaration => parse_procedure_declaration(pair, sym),
        _ => Err("Error parsing declaration: not a declaration")
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::statement::
        {Statement, Scope, Assignment, OutputElement, Conditional, ConditionalBranch};
    use crate::ast::expression::{
        Expression, Arithmetic, ArithmeticOp, Logical, LogicalOp, Constant};
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
        let target_procedure = Function::procedure(
            "my_procedure".to_string(),
            vec![
                Rc::new(Variable::integer("x".to_string()).to_level(1)),
                Rc::new(Variable::integer("y".to_string()).to_level(1)),
                Rc::new(Variable::boolean("flag".to_string()).to_level(1))]
        );
        target_procedure.update_level(0);
        let param_x = Rc::new(Variable::boolean("x".to_string()).to_level(2));
        let param_y = Rc::new(Variable::boolean("y".to_string()).to_level(2));
        let param_n = Rc::new(Variable::integer("n".to_string()).to_level(2));
        let target_function = Function::new(
            "a_nested_function".to_string(),
            vec![
                param_x.clone(),
                param_y.clone(),
                param_n.clone()],
            Type::integer()
        );
        target_function.update_level(1);
        let atype =
            Type::ArrayType(Rc::new(ArrayType::new(ScalarType::Boolean, vec![3])));
        let nested_variables = vec![
            Symbol::Variable(Rc::new(Variable::new(
                "an_array_variable".to_string(),
                atype.clone()
            ).to_level(2))),
            Symbol::Variable(Rc::new(Variable::new(
                "another_array_variable".to_string(),
                atype.clone()
            ).to_level(2)))];
        let nested_if_condition = Logical::new(
            Expression::Variable(param_x.clone()),
            Expression::Variable(param_y.clone()),
            LogicalOp::And
        ).unwrap();
        let nested_if_return = Statement::Return(
            Some(Arithmetic::new(
                Expression::Variable(param_n.clone()),
                Expression::Constant(Constant::Integer(1)),
                ArithmeticOp::Add
            ).unwrap())
        );
        let nested_else_return = Statement::Return(
            Some(Arithmetic::new(
                Expression::Variable(param_n.clone()),
                Expression::Constant(Constant::Integer(2)),
                ArithmeticOp::Mul
            ).unwrap())
        );
        let nested_unreachable_return = Statement::Return(
            Some(Expression::Variable(param_n.clone()))
        );
        let nested_if = Conditional{
            conditional_branches : vec![ConditionalBranch{
                condition : nested_if_condition,
                scope : Scope::new_from_symbols(
                    vec![nested_if_return], Vec::new()
                )
            }],
            else_branch : Some(Scope::new_from_symbols(
                vec![nested_else_return], Vec::new()
            ))
        };


        target_function.implement(
            Scope::new_from_symbols(vec![
                Statement::Conditional(nested_if),
                nested_unreachable_return
                ], nested_variables));

        let inner_variable = Rc::new(Variable::integer("an_integer".to_string()).to_level(1));

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
        let outer_scope = parse_bare_scope(
            CSC488Parser::parse(Rule::bare_scope,
                "func my_procedure(x, y integer, flag boolean) {
                    func a_nested_function(x, y boolean, n integer) integer {
                        var an_array_variable, another_array_variable [3] boolean
                        if x and y { /* Multiline
                            comment
                            */return (n + 1)
                        } else {
                            return (n * 2)
                        }
                        return (n)
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
        assert_eq!(outer_scope.get_variables().len(), 0);
        assert_eq!(outer_scope.get_functions().len(), 0);
        assert_eq!(outer_scope.get_statements().len(), 0);
        let mut decls = outer_scope.get_procedures().iter();
        let decl = decls.next().unwrap().clone();
        assert_eq!(Rc::new(target_procedure), decl);
        assert_eq!(decls.next(), None);
    }
}
