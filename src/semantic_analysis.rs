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

struct VarData {
    new_name: String,
    from_current_block: bool
}

/// Invokes the semantic analysis phase and returns an updated AST.
pub fn semantic_analysis(ast: &Program, counter: &mut UniqueCounter) -> Program {
    let mut variable_map: HashMap<String, VarData> = HashMap::new();
    let transformed_ast = resolve_program(ast, &mut variable_map, counter);

    //dbg!(&transformed_ast);

    let loop_ast = label_program(&transformed_ast, counter, &None);

    //dbg!(&loop_ast);

    loop_ast
}

/// Creates an UniqueCounter which has two counter fields for creating unique temporary
/// and label names for semantic analysis.
pub fn create_counter() -> UniqueCounter {
    UniqueCounter { temporary_count: 0, label_count: 0 }
}

fn create_vardata(unique_name: &String, from_current_block: bool) -> VarData {
    VarData { new_name: unique_name.clone(), from_current_block: from_current_block }
}

fn resolve_program(ast: &Program, variable_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter) -> Program {
    match ast {
        Program::Program(func) => Program::Program(resolve_function(func, variable_map, counter))
    }
}

/// Resolves function by resolving function block body
fn resolve_function(function: &FunctionDefinition, variable_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter) -> FunctionDefinition {
    match function {
        FunctionDefinition::Function(name, body) => 
            FunctionDefinition::Function(name.clone(), resolve_block(body, variable_map, counter))
    }
}

/// Resolves block items inside block
fn resolve_block(block: &Block, variable_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter) -> Block {
    match block {
        Block::Block(items) => Block::Block(resolve_block_item(items, variable_map, counter))
    }
}

///
fn resolve_block_item(block_items: &Vec<BlockItem>, variable_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter) -> Vec<BlockItem> {
    let mut new_items: Vec<BlockItem> = Vec::new();
    for item in block_items {
        match item {
            BlockItem::D(decl) => new_items.push(BlockItem::D(resolve_declaration(decl, variable_map, counter))),
            BlockItem::S(stat) => new_items.push(BlockItem::S(resolve_statement(stat, variable_map, counter)))
        }
    }
    new_items
}

/// Checks if declaration name has been used before in this block, and if so, rejects.
/// Otherwise, adds new name to map and resolves init exp if present.
fn resolve_declaration(decl: &Declaration, variable_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter) -> Declaration {
    let (name, init) = match decl {
        Declaration::Declaration(n, i) => (n, i)
    };

    if variable_map.contains_key(name) && variable_map.get(name).unwrap().from_current_block {
        panic!("Duplicate variable declarations!")
    }

    let unique_name = make_unique(name, UniqueType::Temporary, counter);
    variable_map.insert(name.clone(), create_vardata(&unique_name, true));

    if init.is_some() {
        let init = resolve_exp(&init.clone().unwrap(), variable_map);
        Declaration::Declaration(unique_name, Some(init.clone()))
    } else {
        Declaration::Declaration(unique_name, init.clone())
    }
}

fn resolve_exp(exp: &Exp, variable_map: &mut HashMap<String, VarData>) -> Exp {
    match exp {
        Exp::Assignment(left, right) => {
            if is_not_a_var_node(left) {
                panic!("Invalid lvalue!")
            }
            Exp::Assignment(Box::new(resolve_exp(left, variable_map)), Box::new(resolve_exp(right, variable_map)))
        },
        Exp::Var(var) => {
            if variable_map.contains_key(var) {
                Exp::Var(variable_map.get(var).unwrap().new_name.clone())
            } else {
                panic!("Undeclared variable!: {var}")
            }
        },
        Exp::Unary(op, exp) => Exp::Unary(op.clone(), Box::new(resolve_exp(exp, variable_map))),
        Exp::Binary(op, src, dst) => 
            Exp::Binary(op.clone(), Box::new(resolve_exp(src, variable_map)), Box::new(resolve_exp(dst, variable_map))),
        Exp::Constant(num) => Exp::Constant(*num),
        Exp::Conditional(condition, then, els) =>
            Exp::Conditional(Box::new(resolve_exp(condition, variable_map)), Box::new(resolve_exp(then, variable_map)), Box::new(resolve_exp(els, variable_map)))
    }
}

fn resolve_statement(statement: &Statement, variable_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter) -> Statement {
    match statement {
        Statement::Return(exp) => Statement::Return(resolve_exp(exp, variable_map)),
        Statement::Expression(exp) => Statement::Expression(resolve_exp(exp, variable_map)),
        Statement::Null => Statement::Null,
        Statement::If(condition, then, els) => {
            Statement::If(resolve_exp(condition, variable_map), 
                          Box::new(resolve_statement(then, variable_map, counter)), 
                          resolve_optional_box_statement(els, variable_map, counter))
        }
        Statement::Compound(block) => {
            let mut new_variable_map = copy_variable_map(variable_map);
            Statement::Compound(resolve_block(block, &mut new_variable_map, counter))
        },
        Statement::Break(label) => Statement::Break(label.clone()),
        Statement::Continue(label) => Statement::Continue(label.clone()),
        Statement::While(condition, body, label) => {
            let mut new_variable_map = copy_variable_map(variable_map);
            let condition = resolve_exp(condition, &mut new_variable_map);
            let body = Box::new(resolve_statement(body, &mut new_variable_map, counter));
            Statement::While(condition, body, label.clone())
        },
        Statement::DoWhile(body, condition, label) => {
            let mut new_variable_map = copy_variable_map(variable_map);
            let body = Box::new(resolve_statement(body, &mut new_variable_map, counter));
            let condition = resolve_exp(condition, &mut new_variable_map);
            Statement::DoWhile(body, condition, label.clone())
        },
        Statement::For(init, condition, post, body, label)  => {
            let mut new_variable_map = copy_variable_map(variable_map);
            let init = resolve_for_init(init, &mut new_variable_map, counter);
            let condition = resolve_optional_exp(condition, &mut new_variable_map);
            let post = resolve_optional_exp(post, &mut new_variable_map);
            let body = Box::new(resolve_statement(body, &mut new_variable_map, counter));
            Statement::For(init, condition, post, body, label.clone())
        }
    }
}

fn resolve_optional_exp(exp: &Option<Exp>, variable_map: &mut HashMap<String, VarData>) -> Option<Exp> {
    if exp.is_some() {
        Some(resolve_exp(&exp.clone().unwrap(), variable_map))
    } else {
        None
    }
}

fn resolve_optional_box_statement(statement: &Option<Box<Statement>>, variable_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter) -> Option<Box<Statement>> {
    match statement {
        Some(statement) => Some(Box::new(resolve_statement(statement, variable_map, counter))),
        None => None
    }
}

fn resolve_for_init(init: &ForInit, variable_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter) -> ForInit {
    match init {
        ForInit::InitExp(e)  => ForInit::InitExp(resolve_optional_exp(e, variable_map)),
        ForInit::InitDecl(d) => ForInit::InitDecl(resolve_declaration(d, variable_map, counter))
    }
}

fn is_not_a_var_node(exp: &Box<Exp>) -> bool {
    match exp.as_ref() {
        Exp::Var(_) => false,
        _ => true
    }
}

/// Makes a unique name by combining the given name with . and a number from counter. 
/// Ex: main => main.1
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

/// Helper for make_unique
fn make_temporary(counter: &mut UniqueCounter) -> String {
    let mut temporary = String::new();
    temporary += counter.temporary_count.to_string().as_str();
    counter.temporary_count += 1;
    temporary
}

/// Helper for make_unique
fn make_label(counter: &mut UniqueCounter) -> String {
    let mut label = String::new();
    label += counter.label_count.to_string().as_str();
    counter.label_count += 1;
    label
}

fn copy_variable_map(variable_map: &mut HashMap<String, VarData>) -> HashMap<String, VarData> {
    let mut new_map: HashMap<String, VarData> = HashMap::new();

    for entry in variable_map {
        new_map.insert(entry.0.clone(), create_vardata(&entry.1.new_name.clone(), false));
    }

    new_map
}

/// Starts the process of handling loop statements like break and continue
/// by deciding what loop they're associated with.
fn label_program(transformed_ast: &Program, counter: &mut UniqueCounter, label: &Option<String>) -> Program {
    match transformed_ast {
        Program::Program(function) => Program::Program(label_function(function, counter, label))
    }
}

fn label_function(function: &FunctionDefinition, counter: &mut UniqueCounter, label: &Option<String>) -> FunctionDefinition {
    match function {
        FunctionDefinition::Function(name, body) => FunctionDefinition::Function(name.clone(), label_block(body, counter, label))
    }
}

fn label_block(block: &Block, counter: &mut UniqueCounter, label: &Option<String>) -> Block {
    match block {
        Block::Block(items) => Block::Block(label_block_item(items, counter, label))
    }
}

fn label_block_item(items: &Vec<BlockItem>, counter: &mut UniqueCounter, label: &Option<String>) -> Vec<BlockItem> {
    let mut new_items: Vec<BlockItem> = Vec::new();
    for item in items {
        match item {
            BlockItem::S(statement)  => new_items.push(BlockItem::S(label_statement(statement, counter, label))),
            BlockItem::D(_)                      => new_items.push(item.clone()),
        }
    }

    new_items
}

fn label_statement(statement: &Statement, counter: &mut UniqueCounter, label: &Option<String>) -> Statement {
    match statement {
        Statement::Return(exp)      => Statement::Return(exp.clone()),
        Statement::Expression(exp)  => Statement::Expression(exp.clone()),
        Statement::If(condition, then, els) => {
            let then = Box::new(label_statement(then, counter, label));
            let els = label_optional_box_statement(els, counter, label);
            Statement::If(condition.clone(), then, els)
        },
        Statement::Compound(block) => Statement::Compound(label_block(block, counter, label)),
        Statement::Break(_)  => {
            if label.is_none() {
                panic!("break statement outside of loop")
            }
            Statement::Break(label.clone().unwrap())
        },
        Statement::Continue(_) => {
            if label.is_none() {
                panic!("continue statement outside of loop")
            }
            Statement::Continue(label.clone().unwrap())
        },
        Statement::While(condition, body, _) => {
            let while_name = String::from("while");
            let new_label = make_unique(&while_name, UniqueType::Label, counter);
            let label_body = Box::new(label_statement(body, counter, &Some(new_label.clone())));
            Statement::While(condition.clone(), label_body, new_label)
        },
        Statement::DoWhile(body, condition, _) => {
            let do_while_name = String::from("dowhile");
            let new_label = make_unique(&do_while_name, UniqueType::Label, counter);
            let label_body = Box::new(label_statement(body, counter, &Some(new_label.clone())));
            Statement::DoWhile(label_body, condition.clone(), new_label)
        },
        Statement::For(init, condition, post, body, _) => {
            let for_name = String::from("for");
            let new_label = make_unique(&for_name, UniqueType::Label, counter);
            let label_body = Box::new(label_statement(body, counter, &Some(new_label.clone())));
            Statement::For(init.clone(), condition.clone(), post.clone(), label_body, new_label)
        },
        Statement::Null => Statement::Null
    }
}

fn label_optional_box_statement(statement: &Option<Box<Statement>>, counter: &mut UniqueCounter, label: &Option<String>) -> Option<Box<Statement>> {
    match statement {
        Some(statement) => Some(Box::new(label_statement(statement, counter, label))),
        None                             => None
    }
}