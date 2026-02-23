use crate::parser::parser_ast::*;
use std::collections::HashMap;

pub struct UniqueCounter {
    temporary_count: i32,
    label_count : i32
}

#[derive(Debug, PartialEq, Clone)]
pub enum UniqueType {
    Temporary,
    Label
}

pub fn semantic_analysis(ast: &Program, counter: &mut UniqueCounter) -> Program {
    let mut variable_map: HashMap<String, String> = HashMap::new();
    let transformed_ast = resolve_program(ast, &mut variable_map, counter);

    dbg!(&transformed_ast);

    transformed_ast
}

pub fn create_counter() -> UniqueCounter {
    UniqueCounter { temporary_count: 0, label_count: 0 }
}

fn resolve_program(ast: &Program, variable_map: &mut HashMap<String, String>, counter: &mut UniqueCounter) -> Program {
    match ast {
        Program::Program(func) => Program::Program(resolve_function(func, variable_map, counter))
    }
}

fn resolve_function(function: &FunctionDefinition, variable_map: &mut HashMap<String, String>, counter: &mut UniqueCounter) -> FunctionDefinition {
    match function {
        FunctionDefinition::Function(name, body) => 
            FunctionDefinition::Function(name.clone(), resolve_block_item(body, variable_map, counter))
    }
}

fn resolve_block_item(block_items: &Vec<BlockItem>, variable_map: &mut HashMap<String, String>, counter: &mut UniqueCounter) -> Vec<BlockItem> {
    let mut new_items: Vec<BlockItem> = Vec::new();
    for item in block_items {
        match item {
            BlockItem::D(decl) => new_items.push(BlockItem::D(resolve_declaration(decl, variable_map, counter))),
            BlockItem::S(stat) => new_items.push(BlockItem::S(resolve_statement(stat, variable_map)))
        }
    }
    new_items
}

fn resolve_declaration(decl: &Declaration, variable_map: &mut HashMap<String, String>, counter: &mut UniqueCounter) -> Declaration {
    let (name, init) = match decl {
        Declaration::Declaration(n, i) => (n, i)
    };
    if variable_map.contains_key(name) {
        panic!("Duplicate variable declarations!")
    }
    let unique_name = make_unique(name, UniqueType::Temporary, counter);
    variable_map.insert(name.clone(), unique_name.clone());
    if init.is_some() {
        let init = resolve_exp(&init.clone().unwrap(), variable_map);
        Declaration::Declaration(unique_name, Some(init.clone()))
    } else {
        Declaration::Declaration(unique_name, init.clone())
    }
}

fn resolve_exp(exp: &Exp, variable_map: &mut HashMap<String, String>) -> Exp {
    match exp {
        Exp::Assignment(left, right) => {
            if is_not_a_var_node(left) {
                panic!("Invalid lvalue!")
            }
            Exp::Assignment(Box::new(resolve_exp(left, variable_map)), Box::new(resolve_exp(right, variable_map)))
        },
        Exp::Var(var) => {
            if variable_map.contains_key(var) {
                Exp::Var(variable_map.get(var).unwrap().clone())
            } else {
                panic!("Undeclared variable!: {var}")
            }
        },
        Exp::Unary(op, exp) => Exp::Unary(op.clone(), Box::new(resolve_exp(exp, variable_map))),
        Exp::Binary(op, src, dst) => 
            Exp::Binary(op.clone(), Box::new(resolve_exp(src, variable_map)), Box::new(resolve_exp(dst, variable_map))),
        Exp::Constant(num) => Exp::Constant(*num)
    }
}

fn resolve_statement(statement: &Statement, variable_map: &mut HashMap<String, String>) -> Statement {
    match statement {
        Statement::Return(exp) => Statement::Return(resolve_exp(exp, variable_map)),
        Statement::Expression(exp) => Statement::Expression(resolve_exp(exp, variable_map)),
        Statement::Null => Statement::Null
    }
}

fn is_not_a_var_node(exp: &Box<Exp>) -> bool {
    match exp.as_ref() {
        Exp::Var(_) => false,
        _ => true
    }
}


pub fn make_unique(name: &String, utype: UniqueType, counter: &mut UniqueCounter) -> String {
    let mut unique = String::new();
    unique += name.clone().as_str();
    unique += ".";
    if utype == UniqueType::Temporary {
        unique += make_temporary(counter).as_str()
    } else {
        unique += make_label(counter).as_str()
    }
    unique
}

fn make_temporary(counter: &mut UniqueCounter) -> String {
    let mut temporary = String::new();
    temporary += counter.temporary_count.to_string().as_str();
    counter.temporary_count += 1;
    temporary
}

fn make_label(counter: &mut UniqueCounter) -> String {
    let mut label = String::new();
    label += counter.label_count.to_string().as_str();
    counter.label_count += 1;
    label
}