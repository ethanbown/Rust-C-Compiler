use crate::parser::parser_ast::*;
use std::{collections::HashMap};

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
    from_current_scope: bool,
    has_linkage: bool
}

#[derive(Debug)]
pub struct TypeData {
    pub dtype: Type,
    pub defined: bool,
    pub param_count: usize
}
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    FunType(usize)
}

/// Invokes the semantic analysis phase and returns an updated AST.
pub fn semantic_analysis(ast: &Program, counter: &mut UniqueCounter) -> (Program, HashMap<String, TypeData>) {
    let mut identifier_map: HashMap<String, VarData> = HashMap::new();
    let mut symbols: HashMap<String, TypeData> = HashMap::new();
    let transformed_ast = resolve_program(ast, &mut identifier_map, counter);

    //dbg!(&transformed_ast);

    typecheck_program(&transformed_ast, &mut symbols);

    let loop_ast = label_program(&transformed_ast, counter, &None);

    //dbg!(&loop_ast);

    (loop_ast, symbols)
}

/// Creates an UniqueCounter which has two counter fields for creating unique temporary
/// and label names for semantic analysis.
pub fn create_counter() -> UniqueCounter {
    UniqueCounter { temporary_count: 0, label_count: 0 }
}

fn create_vardata(unique_name: &String, from_current_scope: bool, has_linkage: bool) -> VarData {
    VarData { new_name: unique_name.clone(), from_current_scope: from_current_scope, has_linkage: has_linkage }
}

fn resolve_program(ast: &Program, identifier_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter) -> Program {
    match ast {
        Program::Program(func) => Program::Program(resolve_function(func, identifier_map, counter))
    }
}

/// Resolves function by resolving function block body
fn resolve_function(functions: &Vec<FunctionDeclaration>, identifier_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter) -> Vec<FunctionDeclaration> {
    let mut return_val = Vec::new();
    
    for function in functions {
        return_val.push(resolve_function_declaration(function, identifier_map, counter));
    }

    return_val
}

/// Resolves block items inside block
fn resolve_block(block: &Option<Block>, identifier_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter) -> Option<Block> {
    match block {
        Some(Block::Block(items)) => Some(Block::Block(resolve_block_item(items, identifier_map, counter))),
        None                                       => None
    }
}

/// Resolves block items that can be statements or declarations
fn resolve_block_item(block_items: &Vec<BlockItem>, identifier_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter) -> Vec<BlockItem> {
    let mut new_items: Vec<BlockItem> = Vec::new();
    for item in block_items {
        match item {
            BlockItem::D(decl) => new_items.push(BlockItem::D(resolve_declaration(decl, identifier_map, counter))),
            BlockItem::S(stat) => new_items.push(BlockItem::S(resolve_statement(stat, identifier_map, counter)))
        }
    }
    new_items
}

fn resolve_declaration(decl: &Declaration, identifier_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter) -> Declaration {
    match decl {
        Declaration::VarDecl(var_decl) => Declaration::VarDecl(resolve_variable_declaration(var_decl, identifier_map, counter)),
        Declaration::FunDecl(fun_decl) => {
            match fun_decl {
                FunctionDeclaration::Function(_, _, body) => {
                    if body.is_some() {
                        panic!("Local function declaration has a body");
                    }
                },
            }

            Declaration::FunDecl(resolve_function_declaration(fun_decl, identifier_map, counter))
        }
    }
}
/// Checks if declaration name has been used before in this block, and if so, rejects.
/// Otherwise, adds new name to map and resolves init exp if present.
fn resolve_variable_declaration(decl: &VariableDeclaration, identifier_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter) -> VariableDeclaration {
    let (name, init) = match decl {
        VariableDeclaration::Variable(n, i) => (n, i)
    };

    let unique_name = resolve_name(&name, identifier_map, counter, false);

    if init.is_some() {
        let init = resolve_exp(&init.clone().unwrap(), identifier_map);
        VariableDeclaration::Variable(unique_name, Some(init.clone()))
    } else {
        VariableDeclaration::Variable(unique_name, init.clone())
    }
}

fn resolve_name(name: &String, identifier_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter, has_linkage: bool) -> String {
    if identifier_map.contains_key(name) && identifier_map.get(name).unwrap().from_current_scope {
        panic!("Duplicate name declarations!")
    }

    let unique_name = make_unique(name, UniqueType::Temporary, counter);
    identifier_map.insert(name.clone(), create_vardata(&unique_name, true, has_linkage));

    unique_name
}

/// Resolves function declarations
fn resolve_function_declaration(decl: &FunctionDeclaration, identifier_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter) -> FunctionDeclaration {
    let (name, params, body) =
        match decl {
            FunctionDeclaration::Function(name, params, body) => (name, params, body)
        };
    
    if identifier_map.contains_key(name) {
        let prev_entry = identifier_map.get(name).unwrap();
        if prev_entry.from_current_scope && !prev_entry.has_linkage {
            panic!("Duplicate declaration");
        }
    }

    identifier_map.insert(name.clone(), create_vardata(&name.clone(), true, true));
    let mut inner_map = copy_identifier_map(identifier_map);
    let mut new_params: Vec<String> = Vec::new();

    for param in params {
        new_params.push(resolve_param(param, &mut inner_map, counter));
    }

    let mut new_body = None;
    
    if body.is_some() {
        new_body = resolve_block(body, &mut inner_map, counter);
    }

    FunctionDeclaration::Function(name.clone(), new_params, new_body)
}

fn resolve_param(name: &String, identifier_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter) -> String {
    resolve_name(name, identifier_map, counter, false)
}

fn resolve_exp(exp: &Exp, identifier_map: &mut HashMap<String, VarData>) -> Exp {
    match exp {
        Exp::Assignment(left, right) => {
            if is_not_a_var_node(left) {
                panic!("Invalid lvalue!")
            }
            Exp::Assignment(Box::new(resolve_exp(left, identifier_map)), Box::new(resolve_exp(right, identifier_map)))
        },
        Exp::Var(var) => {
            if identifier_map.contains_key(var) {
                Exp::Var(identifier_map.get(var).unwrap().new_name.clone())
            } else {
                panic!("Undeclared variable!: {var}")
            }
        },
        Exp::Unary(op, exp) => Exp::Unary(op.clone(), Box::new(resolve_exp(exp, identifier_map))),
        Exp::Binary(op, src, dst) => 
            Exp::Binary(op.clone(), Box::new(resolve_exp(src, identifier_map)), Box::new(resolve_exp(dst, identifier_map))),
        Exp::Constant(num) => Exp::Constant(*num),
        Exp::Conditional(condition, then, els) =>
            Exp::Conditional(Box::new(resolve_exp(condition, identifier_map)), Box::new(resolve_exp(then, identifier_map)), Box::new(resolve_exp(els, identifier_map))),
        Exp::FunctionCall(fun_name, args) => {
            if identifier_map.contains_key(fun_name) {
                let new_fun_name = identifier_map.get(fun_name).unwrap().new_name.clone();
                let mut new_args: Vec<Exp> = Vec::new();

                for arg in args {
                    new_args.push(resolve_exp(arg, identifier_map))
                }

                Exp::FunctionCall(new_fun_name, new_args)
            } else {
                panic!("Undeclared function!: {fun_name}")
            }
        }
    }
}

fn resolve_statement(statement: &Statement, identifier_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter) -> Statement {
    match statement {
        Statement::Return(exp) => Statement::Return(resolve_exp(exp, identifier_map)),
        Statement::Expression(exp) => Statement::Expression(resolve_exp(exp, identifier_map)),
        Statement::Null => Statement::Null,
        Statement::If(condition, then, els) => {
            Statement::If(resolve_exp(condition, identifier_map), 
                          Box::new(resolve_statement(then, identifier_map, counter)), 
                          resolve_optional_box_statement(els, identifier_map, counter))
        }
        Statement::Compound(block) => {
            let mut new_identifier_map = copy_identifier_map(identifier_map);
            let block = Some(block.clone());
            Statement::Compound(resolve_block(&block, &mut new_identifier_map, counter).unwrap())
        },
        Statement::Break(label) => Statement::Break(label.clone()),
        Statement::Continue(label) => Statement::Continue(label.clone()),
        Statement::While(condition, body, label) => {
            let mut new_identifier_map = copy_identifier_map(identifier_map);
            let condition = resolve_exp(condition, &mut new_identifier_map);
            let body = Box::new(resolve_statement(body, &mut new_identifier_map, counter));
            Statement::While(condition, body, label.clone())
        },
        Statement::DoWhile(body, condition, label) => {
            let mut new_identifier_map = copy_identifier_map(identifier_map);
            let body = Box::new(resolve_statement(body, &mut new_identifier_map, counter));
            let condition = resolve_exp(condition, &mut new_identifier_map);
            Statement::DoWhile(body, condition, label.clone())
        },
        Statement::For(init, condition, post, body, label)  => {
            let mut new_identifier_map = copy_identifier_map(identifier_map);
            let init = resolve_for_init(init, &mut new_identifier_map, counter);
            let condition = resolve_optional_exp(condition, &mut new_identifier_map);
            let post = resolve_optional_exp(post, &mut new_identifier_map);
            let body = Box::new(resolve_statement(body, &mut new_identifier_map, counter));
            Statement::For(init, condition, post, body, label.clone())
        }
    }
}

fn resolve_optional_exp(exp: &Option<Exp>, identifier_map: &mut HashMap<String, VarData>) -> Option<Exp> {
    if exp.is_some() {
        Some(resolve_exp(&exp.clone().unwrap(), identifier_map))
    } else {
        None
    }
}

fn resolve_optional_box_statement(statement: &Option<Box<Statement>>, identifier_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter) -> Option<Box<Statement>> {
    match statement {
        Some(statement) => Some(Box::new(resolve_statement(statement, identifier_map, counter))),
        None => None
    }
}

fn resolve_for_init(init: &ForInit, identifier_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter) -> ForInit {
    match init {
        ForInit::InitExp(e)  => ForInit::InitExp(resolve_optional_exp(e, identifier_map)),
        ForInit::InitDecl(d) => ForInit::InitDecl(resolve_variable_declaration(d, identifier_map, counter))
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

fn copy_identifier_map(identifier_map: &mut HashMap<String, VarData>) -> HashMap<String, VarData> {
    let mut new_map: HashMap<String, VarData> = HashMap::new();

    for entry in identifier_map {
        new_map.insert(entry.0.clone(), create_vardata(&entry.1.new_name.clone(), false, entry.1.has_linkage.clone()));
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

fn label_function(functions: &Vec<FunctionDeclaration>, counter: &mut UniqueCounter, label: &Option<String>) -> Vec<FunctionDeclaration> {
    let mut return_val: Vec<FunctionDeclaration> = Vec::new();

    for function in functions {
        match function {
            FunctionDeclaration::Function(name, params, body) => 
                return_val.push(FunctionDeclaration::Function(name.clone(), params.clone(), label_block(body, counter, label)))
        }
    }

    return_val
}

fn label_block(block: &Option<Block>, counter: &mut UniqueCounter, label: &Option<String>) -> Option<Block> {
    match block {
        Some(Block::Block(items)) => Some(Block::Block(label_block_item(items, counter, label))),
        None                                       => None
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
        Statement::Compound(block) => {
            let block = Some(block.clone());
            Statement::Compound(label_block(&block, counter, label).unwrap())
        },
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

/// Creates a new typedata struct
fn create_typedata(dtype: Type, defined: bool, param_count: usize) -> TypeData {
    TypeData {dtype: dtype, defined: defined, param_count: param_count}
}

/// Start typechecking
fn typecheck_program(program: &Program, symbols: &mut HashMap<String, TypeData>) {
    match program {
        Program::Program(functions) => {
            for function in functions {
                typecheck_function_declaration(function, symbols);
            }
        },
    }
}

///
fn typecheck_function_declaration(decl: &FunctionDeclaration, symbols: &mut HashMap<String, TypeData>) {
    let (name, param_list, body) = {
        match decl {
            FunctionDeclaration::Function(name, param_list, body) => (name, param_list, body)
        }
    };

    let fun_type = Type::FunType(param_list.len());
    let has_body = body.is_some();
    let mut already_defined = false;

    if symbols.contains_key(name) {
        let old_decl = symbols.get(name).unwrap();
        
        if old_decl.dtype != fun_type {
            panic!("Incompatible function declaration");
        }
        
        already_defined = old_decl.defined;
        if already_defined && has_body {
            panic!("Function is defined more than once")
        }
    }

    symbols.insert(name.clone(), create_typedata(fun_type, already_defined || has_body, param_list.len()));

    if has_body {
        for param in param_list {
            symbols.insert(param.clone(), create_typedata(Type::Int, true, 0));
        }
        typecheck_block(body, symbols);
    }
}

/// Checks the type of a variable
fn typecheck_variable_declaration(decl: &VariableDeclaration, symbols: &mut HashMap<String, TypeData>) {
    match decl {
        VariableDeclaration::Variable(name, init) => {
            symbols.insert(name.clone(), create_typedata(Type::Int, true, 0));

            if init.is_some() {
                typecheck_exp(init, symbols);
            }
        }
    }
}

///
fn typecheck_exp(init: &Option<Exp>, symbols: &mut HashMap<String, TypeData>) {
    if init.is_none() {
        return;
    }

    let init = init.clone().unwrap();
    match init {
        Exp::FunctionCall(name, args) => {
            let fun_type = symbols.get(&name).unwrap();
            
            if fun_type.dtype == Type::Int {
                panic!("Variable used as a function name");
            }
            
            if fun_type.param_count != args.len() {
                panic!("Function called with the wrong number of arguments");
            }
            
            for arg in args {
                typecheck_exp(&Some(arg), symbols);
            }
        },
        Exp::Var(v) => {
            if symbols.get(&v).unwrap().dtype != Type::Int {
                panic!("Function name used as variable");
            }
        },
        Exp::Assignment(exp1, exp2) => {
            typecheck_exp(&Some(*exp1), symbols);
            typecheck_exp(&Some(*exp2), symbols);
        },
        Exp::Unary(_, exp) => {
            typecheck_exp(&Some(*exp), symbols);
        },
        Exp::Binary(_, exp1, exp2) => {
            typecheck_exp(&Some(*exp1), symbols);
            typecheck_exp(&Some(*exp2), symbols);
        },
        Exp::Conditional(cond, exp1, exp2) => {
            typecheck_exp(&Some(*cond), symbols);
            typecheck_exp(&Some(*exp1), symbols);
            typecheck_exp(&Some(*exp2), symbols);
        },
        Exp::Constant(_) => ()
    }
}

fn typecheck_block(body: &Option<Block>, symbols: &mut HashMap<String, TypeData>) {
    if body.is_none() {
        return;
    }

    let body = body.clone().unwrap();
    match body {
        Block::Block(items) => typecheck_block_item(&items, symbols),
    }
}

fn typecheck_block_item(items: &Vec<BlockItem>, symbols: &mut HashMap<String, TypeData>) {
    for item in items {
        match item {
            BlockItem::D(decl)      => typecheck_declaration(decl, symbols),
            BlockItem::S(statement)   => typecheck_statement(statement, symbols)
        }
    }
}

fn typecheck_statement(statement: &Statement, symbols: &mut HashMap<String, TypeData>) {
    match statement {
        Statement::While(condition, body, _) => {
            typecheck_exp(&Some(condition.clone()), symbols);
            typecheck_statement(body, symbols);
        },
        Statement::Compound(body) => typecheck_block(&Some(body.clone()), symbols),
        Statement::DoWhile(body, condition, _) => {
            typecheck_statement(body, symbols);
            typecheck_exp(&Some(condition.clone()), symbols);
        },
        Statement::Expression(exp) => {
            typecheck_exp(&Some(exp.clone()), symbols);
        },
        Statement::If(condition, then, els) => {
            typecheck_exp(&Some(condition.clone()), symbols);
            typecheck_statement(then, symbols);
            if els.is_some() {
                typecheck_statement(&els.clone().unwrap(), symbols);
            }
        },
        Statement::For(for_init, condition, post, body, _) => {
            typecheck_for_init(for_init, symbols);
            typecheck_exp(&condition, symbols);
            typecheck_exp(&post, symbols);
            typecheck_statement(body, symbols);
        },
        Statement::Return(exp) => typecheck_exp(&Some(exp.clone()), symbols),
        Statement::Break(_) => (),
        Statement::Continue(_) => (),
        Statement::Null => ()
    }
}

fn typecheck_for_init(for_init: &ForInit, symbols: &mut HashMap<String, TypeData>) {
    match for_init {
        ForInit::InitExp(exp) => typecheck_exp(exp, symbols),
        ForInit::InitDecl(decl) => typecheck_variable_declaration(decl, symbols),
    }
}

fn typecheck_declaration(decl: &Declaration, symbols: &mut HashMap<String, TypeData>) {
    match decl {
        Declaration::FunDecl(fun_decl) => typecheck_function_declaration(fun_decl, symbols),
        Declaration::VarDecl(var_decl) => typecheck_variable_declaration(var_decl, symbols),
    }
}