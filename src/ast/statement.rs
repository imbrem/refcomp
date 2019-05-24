use super::table::{Function, Variable, Scoped, Symbol, SymbolTable, Callable, DependencyVisitor};
use super::expression::{Expression, ArrayIndex, parse_arguments};
use super::declaration::Declaration;
use super::parse_bare_scope;
use super::types::Typed;
use crate::parser::Rule;
use pest::iterators::Pair;
use std::rc::Rc;

type SPResult = Result<Statement, &'static str>;

#[derive(Clone, Debug, PartialEq)]
pub enum AssignmentDestination {
    Variable(Rc<Variable>),
    ArrayIndex(ArrayIndex)
}

impl DependencyVisitor for AssignmentDestination {
    fn visit_dependencies<T: FnMut(Rc<Variable>)>(&self, visitor : T) -> T {
        match self {
            AssignmentDestination::Variable(v) => v.visit_dependencies(visitor),
            AssignmentDestination::ArrayIndex(a) => a.visit_dependencies(visitor)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub destination : AssignmentDestination,
    pub value : Expression
}

impl Assignment {
    pub fn to_variable(var : Rc<Variable>, val : Expression) -> Assignment {
        Assignment{destination : AssignmentDestination::Variable(var), value : val}
    }
    pub fn to_index(arr : ArrayIndex, val : Expression) -> Assignment {
        Assignment{destination : AssignmentDestination::ArrayIndex(arr), value : val}
    }
}

impl DependencyVisitor for Assignment {
    fn visit_dependencies<T: FnMut(Rc<Variable>)>(&self, visitor : T) -> T {
        self.value.visit_dependencies(self.destination.visit_dependencies(visitor))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConditionalBranch {
    pub condition : Expression,
    pub scope : Scope
}

impl DependencyVisitor for ConditionalBranch {
    fn visit_dependencies<T: FnMut(Rc<Variable>)>(&self, visitor : T) -> T {
        self.condition.visit_dependencies(self.scope.visit_dependencies(visitor))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Conditional {
    pub conditional_branches : Vec<ConditionalBranch>,
    pub else_branch : Option<Scope>
}

impl DependencyVisitor for Conditional {
    fn visit_dependencies<T: FnMut(Rc<Variable>)>(&self, mut visitor : T) -> T {
        for branch in &self.conditional_branches {visitor = branch.visit_dependencies(visitor)}
        match &self.else_branch {
            Some(scope) => scope.visit_dependencies(visitor),
            None => visitor
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct While {
    pub condition : Expression,
    pub scope : Scope
}

impl DependencyVisitor for While {
    fn visit_dependencies<T: FnMut(Rc<Variable>)>(&self, visitor : T) -> T {
        self.condition.visit_dependencies(self.scope.visit_dependencies(visitor))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Repeat {
    pub condition : Expression,
    pub scope : Scope
}

impl DependencyVisitor for Repeat {
    fn visit_dependencies<T: FnMut(Rc<Variable>)>(&self, visitor : T) -> T {
        self.condition.visit_dependencies(self.scope.visit_dependencies(visitor))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ProcedureCall {
    procedure : Rc<Function>,
    arguments : Vec<Expression>
}

impl ProcedureCall {
    pub fn new(procedure : Rc<Function>, arguments : Vec<Expression>) -> SPResult {
        if procedure.get_arity() == arguments.len() {
            for (arg, typ) in arguments.iter()
                .zip(procedure.get_params().iter().map(|p| p.get_type())) {
                if arg.get_type() != typ {return Err("Argument type mismatch");}
            }
            Ok(Statement::ProcedureCall(
                ProcedureCall{procedure : procedure, arguments : arguments})
        )} else {Err("Invalid number of arguments to function call")}
    }
    pub fn get_proc(&self) -> Rc<Function> {
        self.procedure.clone()
    }
    pub fn get_args(&self) -> &[Expression]{
        &self.arguments
    }
}

impl DependencyVisitor for ProcedureCall {
    fn visit_dependencies<T: FnMut(Rc<Variable>)>(&self, mut visitor : T) -> T {
        for arg in &self.arguments {visitor = arg.visit_dependencies(visitor)}
        self.procedure.visit_dependencies(visitor)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    variables : Vec<Rc<Variable>>,
    functions : Vec<Rc<Function>>,
    procedures : Vec<Rc<Function>>,
    statements : Vec<Statement>
}

impl Scope {
    pub fn new(statements : Vec<Statement>, declarations : Vec<Declaration>) -> Scope {
        let mut result = {
            Scope{
                variables : vec![],
                functions : vec![],
                procedures : vec![],
                statements : statements
            }
        };
        for decl in declarations {
            match decl {
                Declaration::Variable(vars) => for var in vars {result.variables.push(var)},
                Declaration::Function(_, f) => result.functions.push(f),
                Declaration::Procedure(_, p) => result.procedures.push(p)
            }
        }
        result
    }
    pub fn new_from_symbols(statements : Vec<Statement>, symbols : Vec<Symbol>) -> Scope {
        let mut result = {
            Scope{
                variables : vec![],
                functions : vec![],
                procedures : vec![],
                statements : statements
            }
        };
        for sym in symbols {
            match sym {
                Symbol::Variable(v) => result.variables.push(v),
                Symbol::Function(f) => result.functions.push(f),
                Symbol::Procedure(p) => result.procedures.push(p)
            }
        }
        result
    }
    pub fn empty() -> Scope {
        Scope{
            variables : vec![],
            functions : vec![],
            procedures : vec![],
            statements : vec![]
        }
    }

    pub fn get_variables(&self) -> &Vec<Rc<Variable>> {&self.variables}
    pub fn get_functions(&self) -> &Vec<Rc<Function>> {&self.functions}
    pub fn get_procedures(&self) -> &Vec<Rc<Function>> {&self.procedures}
    pub fn get_statements(&self) -> &Vec<Statement> {&self.statements}

    pub fn into_main(mut self, name : String) -> Function {
        let res = Function::procedure(name, vec![]);
        // Now, delete all variables from the locals table (they're globals) and delete all
        // functions (they're already defined!)
        self.variables.clear(); self.functions.clear(); self.procedures.clear();
        // Set level to zero
        res.update_level(0);
        // Implement, with everything as a global!
        res.implement(self);
        res
    }
}

impl Scoped for Scope {
    fn enter_scope(&self, sym : &mut SymbolTable) {
        for var in &self.variables {
            sym.define(Symbol::Variable(var.clone()))
        }
        for func in &self.functions {
            sym.define(Symbol::Function(func.clone()))
        }
        for pr in &self.procedures {
            sym.define(Symbol::Procedure(pr.clone()))
        }
    }
    fn leave_scope(&self, sym : &mut SymbolTable) {
        for var in &self.variables {
            sym.undef(var.get_name());
        }
        for func in &self.functions {
            sym.undef(func.get_name());
        }
        for pr in &self.procedures {
            sym.undef(pr.get_name());
        }
    }
}

impl DependencyVisitor for Scope {
    fn visit_dependencies<T: FnMut(Rc<Variable>)>(&self, mut visitor : T) -> T {
        for statement in &self.statements {visitor = statement.visit_dependencies(visitor)}
        visitor
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Assignment(Assignment),
    Conditional(Conditional),
    While(While),
    Repeat(Repeat),
    Break(u32),
    Return(Option<Expression>),
    Print(Vec<OutputElement>),
    Input(Vec<Rc<Variable>>),
    ProcedureCall(ProcedureCall),
    Scope(Scope)
}

impl DependencyVisitor for Statement {
    fn visit_dependencies<T: FnMut(Rc<Variable>)>(&self, visitor : T) -> T {
        match self {
            Statement::Assignment(a) => a.visit_dependencies(visitor),
            Statement::Conditional(c) => c.visit_dependencies(visitor),
            Statement::While(w) => w.visit_dependencies(visitor),
            Statement::Repeat(r) => r.visit_dependencies(visitor),
            Statement::Break(_) => visitor,
            Statement::Return(Some(e)) => e.visit_dependencies(visitor),
            Statement::Return(None) => visitor,
            Statement::Print(v) => {
                let mut visitor = visitor;
                for oe in v {visitor = oe.visit_dependencies(visitor)}
                visitor
            },
            Statement::Input(i) => {
                let mut visitor = visitor;
                for var in i {visitor = var.visit_dependencies(visitor)}
                visitor
            },
            Statement::ProcedureCall(p) => p.visit_dependencies(visitor),
            Statement::Scope(s) => s.visit_dependencies(visitor)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum OutputElement {
    Expression(Expression),
    Text(String),
    Newline
}

impl DependencyVisitor for OutputElement {
    fn visit_dependencies<T: FnMut(Rc<Variable>)>(&self, visitor : T) -> T {
        match self {
            OutputElement::Expression(e) => e.visit_dependencies(visitor),
            _ => visitor
        }
    }
}

fn parse_assignment(pair : Pair<Rule>, sym : &SymbolTable) -> Result<Statement, &'static str> {
    let mut pairs = pair.into_inner();
    let variable = pairs.next().unwrap();
    let expression = Expression::from_pair(pairs.next().unwrap(), sym)?;
    match variable.as_rule() {
        Rule::array_index => {
            let mut pairs = variable.into_inner();
            let var = if let Some(Symbol::Variable(v)) =
                sym.dereference(pairs.next().unwrap().as_str()) {v}
                else {return Err("Cannot find array variable");};
            let mut indices = Vec::new();
            for pair in pairs {
                indices.push(Expression::from_pair(pair, sym)?);
            }
            let idx = if let Expression::ArrayIndex(idx) = ArrayIndex::new(var, indices)? {idx}
                else {unreachable!()};
            Ok(Statement::Assignment(Assignment::to_index(idx, expression)))
        },
        Rule::identifier => match sym.dereference(variable.as_str()) {
            Some(Symbol::Variable(v)) =>
                Ok(Statement::Assignment(Assignment::to_variable(v, expression))),
            _ => Err("Cannot find variable")
        },
        _ => Err("Invalid assignment statement")
    }
}

pub fn parse_statement(pair : Pair<Rule>, sym : &mut SymbolTable) -> Result<Statement, &'static str> {
    match pair.as_rule() {
        Rule::assignment => parse_assignment(pair, sym),
        Rule::conditional => {
            let mut pairs = pair.into_inner();
            let conditional_branches = pairs.next().unwrap().into_inner()
                .map(|b| {
                    let mut pairs = b.into_inner();
                    let expression = Expression::from_pair(pairs.next().unwrap(), sym).unwrap();
                    let scope =
                        parse_bare_scope(pairs.next().unwrap().into_inner().next().unwrap(), sym)
                        .unwrap();
                    ConditionalBranch{condition : expression, scope : scope}
                }).collect();
            let else_branch = match pairs.next() {
                Some(e) => Some(parse_bare_scope(
                    e.into_inner().next().unwrap().into_inner().next().unwrap(), sym
                )?),
                None => None
            };
            Ok(Statement::Conditional(Conditional{
                        conditional_branches : conditional_branches, else_branch : else_branch}))
        },
        Rule::while_loop => {
            let mut pairs = pair.into_inner();
            let cond = Expression::from_pair(pairs.next().unwrap(), sym)?;
            let scope = parse_bare_scope(pairs.next().unwrap().into_inner().next().unwrap(), sym)?;
            Ok(Statement::While(While{ condition : cond, scope : scope }))
        },
        Rule::repeat_loop => {
            let mut pairs = pair.into_inner();
            let scope = parse_bare_scope(pairs.next().unwrap().into_inner().next().unwrap(), sym)?;
            let cond = Expression::from_pair(pairs.next().unwrap(), sym)?;
            Ok(Statement::Repeat(Repeat{ condition : cond, scope : scope }))
        },
        Rule::break_loop => {
            let mut pairs = pair.into_inner();
            Ok(Statement::Break(
                if let Some(pair) = pairs.next() {
                    match str::parse::<u32>(pair.as_str()) {
                        Ok(int) => int,
                        Err(_) => {return Err("Could not parse integer!");}
                    }
                } else {
                    1
                }
            ))
        }
        Rule::return_statement => match pair.into_inner().next() {
            Some(e) => Ok(Statement::Return(Some(Expression::from_pair(e, sym).unwrap()))),
            None => Ok(Statement::Return(None))
        },
        Rule::print_statement => {
            let outputs = pair.into_inner().next().unwrap().into_inner().map(
                |o| -> Result<OutputElement, &'static str>{
                    let o = o.into_inner().next();
                    if let Some(o) = o {
                        match o.as_rule() {
                            Rule::expression =>
                                Ok(OutputElement::Expression(Expression::from_pair(o, sym)?)),
                            Rule::text =>
                                Ok(OutputElement::Text(
                                    o.into_inner().next().unwrap().as_str().to_string()
                                )),
                            _ => unreachable!()
                        }
                    } else {
                        Ok(OutputElement::Newline)
                    }
                });
            let outputs : Result<Vec<OutputElement>, &'static str> = outputs.collect();
            Ok(Statement::Print(outputs?))
        },
        Rule::input_statement => {
            let inputs = pair.into_inner().next().unwrap().into_inner().map(|i| {
                match sym.dereference(i.as_str()) {
                    Some(sym) => match sym {
                        Symbol::Variable(v) => Ok(v),
                        Symbol::Function(_) =>
                            Err("Expected variable in input statement, got function"),
                        Symbol::Procedure(_) =>
                            Err("Expected variable in input statement, got procedure")
                    },
                    None => Err("Could not dereference variable in input statement")
                }
            });
            let inputs : Result<Vec<Rc<Variable>>, &'static str> = inputs.collect();
            Ok(Statement::Input(inputs?))
        },
        Rule::procedure_call => {
            let mut pairs = pair.into_inner();
            let name = pairs.next().unwrap().as_str();
            let procedure = match sym.dereference(name) {
                Some(Symbol::Procedure(p)) => p,
                Some(_) => {return Err("Error building procedure call: symbol not a procedure")},
                _ => {return Err("Error building procedure call: symbol undefined")}
            };
            let args = match pairs.next() {
                Some(args) => parse_arguments(args, sym)?,
                None => Vec::new()
            };
            ProcedureCall::new(procedure, args)
        },
        Rule::scope => Ok(
            Statement::Scope(parse_bare_scope(pair.into_inner().next().unwrap(), sym).unwrap())
        ),
        _ => Err("Invalid statement")
    }
}
