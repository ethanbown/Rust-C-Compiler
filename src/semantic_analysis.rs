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
    pub attrs: IdentifierAttr
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    FunType(usize)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Scope {
    File,
    Block
}

#[derive(Debug, PartialEq, Clone)]
pub enum IdentifierAttr {
    //   defined, global, param_count
    FunAttr(bool, bool, usize),
    //    initial_value,   global
    StaticAttr(InitialValue, bool),
    LocalAttr
}

#[derive(Debug, PartialEq, Clone)]
pub enum InitialValue {
    Tentative,
    NoInitializer,
    Initial(i32)
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

fn create_vardata(unique_name: &String, from_current_scope: bool, has_linkage: bool) -> VarData {
    VarData { new_name: unique_name.clone(), from_current_scope: from_current_scope, has_linkage: has_linkage }
}

fn resolve_program(ast: &Program, identifier_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter) -> Program {
    let mut new_declarations: Vec<Declaration> = Vec::new();

    match ast {
        Program::Program(declarations) => {
            for decl in declarations {
                new_declarations.push(resolve_declaration(decl, identifier_map, counter, &Scope::File))
            }
        }
    }

    Program::Program(new_declarations)
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
            BlockItem::D(decl) => new_items.push(BlockItem::D(resolve_declaration(decl, identifier_map, counter, &Scope::Block))),
            BlockItem::S(stat) => new_items.push(BlockItem::S(resolve_statement(stat, identifier_map, counter)))
        }
    }
    new_items
}

fn resolve_declaration(decl: &Declaration, identifier_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter, scope: &Scope) -> Declaration {
    match decl {
        Declaration::VarDecl(var_decl) => {
            if *scope == Scope::File {
                Declaration::VarDecl(resolve_file_scope_variable_declaration(var_decl, identifier_map))
            } else {
                Declaration::VarDecl(resolve_local_variable_declaration(var_decl, identifier_map, counter))
            }
        },
        Declaration::FunDecl(fun_decl) => {
            match fun_decl {
                FunctionDeclaration::Function(name, _, body, _) => {
                    if *scope == Scope::Block && body.is_some() {
                        panic!("Local function declaration {name} has a body");
                    }
                },
            }

            Declaration::FunDecl(resolve_function_declaration(fun_decl, identifier_map, counter, scope))
        }
    }
}

/// Resolves file level variable declarations
fn resolve_file_scope_variable_declaration(decl: &VariableDeclaration, identifier_map: &mut HashMap<String, VarData>) -> VariableDeclaration {
    let name = match decl {
        VariableDeclaration::Variable(n, _, _) => n
     };

    identifier_map.insert(name.clone(), create_vardata(&name.clone(), true, true));
    decl.clone()
}

/// Checks if declaration name has been used before in this block, and if so, rejects.
/// Otherwise, adds new name to map and resolves init exp if present.
fn resolve_local_variable_declaration(decl: &VariableDeclaration, identifier_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter) -> VariableDeclaration {
    let (name, init, storage_class) = match decl {
        VariableDeclaration::Variable(n, i, s) => (n, i, s)
    };

    if identifier_map.contains_key(name) {
        let prev_entry = identifier_map.get(name).unwrap();
        if prev_entry.from_current_scope {
            if !(prev_entry.has_linkage && *storage_class.as_ref().unwrap() == StorageClass::Extern) {
                panic!("Conflicting local declarations of {name}");
            }
        }
    }
    
    if storage_class.is_some() && *storage_class.as_ref().unwrap() == StorageClass::Extern {
        identifier_map.insert(name.clone(), create_vardata(&name.clone(), true, true));
        VariableDeclaration::Variable(name.clone(), init.clone(), storage_class.clone())
    } else {
        let unique_name = resolve_name(&name, identifier_map, counter, false);

        if init.is_some() {
            let init = resolve_exp(&init.clone().unwrap(), identifier_map);
            VariableDeclaration::Variable(unique_name, Some(init.clone()), storage_class.clone())
        } else {
            VariableDeclaration::Variable(unique_name, init.clone(), storage_class.clone())
        }   
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
fn resolve_function_declaration(decl: &FunctionDeclaration, identifier_map: &mut HashMap<String, VarData>, counter: &mut UniqueCounter, scope: &Scope) -> FunctionDeclaration {
    let (name, params, body, storage_class) =
        match decl {
            FunctionDeclaration::Function(name, params, body, storage_class) => (name, params, body, storage_class)
        };

    if (storage_class.is_some() && *storage_class.as_ref().unwrap() == StorageClass::Static) && *scope == Scope::Block {
        panic!("Block scope function has static specifier");
    }
    
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

    FunctionDeclaration::Function(name.clone(), new_params, new_body, storage_class.clone())
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
        ForInit::InitDecl(d) => ForInit::InitDecl(resolve_local_variable_declaration(d, identifier_map, counter))
    }
}

fn is_not_a_var_node(exp: &Box<Exp>) -> bool {
    match exp.as_ref() {
        Exp::Var(_) => false,
        _ => true
    }
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
        Program::Program(declarations) => Program::Program(label_declaration(declarations, counter, label))
    }
}

fn label_declaration(declarations: &Vec<Declaration>, counter: &mut UniqueCounter, label: &Option<String>) -> Vec<Declaration> {
    let mut return_val: Vec<Declaration> = Vec::new();

    for decl in declarations {
        match decl {
            Declaration::FunDecl(function) => return_val.push(Declaration::FunDecl(label_function(function, counter, label))),
            Declaration::VarDecl(var_decl) => return_val.push(Declaration::VarDecl(var_decl.clone()))
        }
    }

    return_val
}

fn label_function(function: &FunctionDeclaration, counter: &mut UniqueCounter, label: &Option<String>) -> FunctionDeclaration {
    match function {
        FunctionDeclaration::Function(name, params, body, storage_class) => 
            FunctionDeclaration::Function(name.clone(), params.clone(), label_block(&body, counter, label), storage_class.clone())
    }
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
fn create_typedata(dtype: Type, attr: IdentifierAttr) -> TypeData {
    TypeData {dtype: dtype, attrs: attr}
}

/// Start typechecking
fn typecheck_program(program: &Program, symbols: &mut HashMap<String, TypeData>) {
    match program {
        Program::Program(declarations) => {
            for decl in declarations {
                typecheck_declaration(decl, symbols, &Scope::File);
            }
        },
    }
}

///
fn typecheck_function_declaration(decl: &FunctionDeclaration, symbols: &mut HashMap<String, TypeData>) {
    let (name, param_list, body, storage_class) = {
        match decl {
            FunctionDeclaration::Function(name, param_list, body, storage_class) => (name, param_list, body, storage_class)
        }
    };

    let fun_type = Type::FunType(param_list.len());
    let has_body = body.is_some();
    let mut already_defined = false;
    let mut global = 
        if storage_class.is_none() {
            true
        } else {
            *storage_class.as_ref().unwrap() != StorageClass::Static
        };

    if symbols.contains_key(name) {
        let old_decl = symbols.get(name).unwrap();
        
        if old_decl.dtype != fun_type {
            panic!("Incompatible function declaration");
        }
        
        (already_defined, global) = match old_decl.attrs {
            IdentifierAttr::FunAttr(defined, global, _) => (defined, global),
            _                                            => panic!("not a function in typecheck function")
        };

        if already_defined && has_body {
            panic!("Function is defined more than once")
        }

        if global && (storage_class.is_some() && *storage_class.as_ref().unwrap() == StorageClass::Static) {
            panic!("Static function declaration follows non-static");
        }

    }

    let attrs = IdentifierAttr::FunAttr(already_defined || has_body, global, param_list.len());
    symbols.insert(name.clone(), create_typedata(fun_type, attrs));

    if has_body {
        for param in param_list {
            symbols.insert(param.clone(), create_typedata(Type::Int, IdentifierAttr::LocalAttr));
        }
        typecheck_block(body, symbols);
    }
}

/// Checks the type of a variable
fn typecheck_file_scope_variable_declaration(decl: &VariableDeclaration, symbols: &mut HashMap<String, TypeData>) {
    match decl {
        VariableDeclaration::Variable(name, init, storage_class) => {
            let mut initial_value;
            if init.is_none() {
                if storage_class.is_some() && *storage_class.as_ref().unwrap() == StorageClass::Extern {
                    initial_value = InitialValue::NoInitializer;
                } else {
                    initial_value = InitialValue::Tentative;
                }
            } else {
                initial_value = match init.as_ref().unwrap() {
                    Exp::Constant(int) => InitialValue::Initial(*int),
                    _                       => panic!("Non-constant initializer")
                };
            }

            let mut global = 
                if storage_class.is_none() {
                    true
                } else {
                    *storage_class.as_ref().unwrap() != StorageClass::Static
                };

            if symbols.contains_key(name) {
                let old_decl = symbols.get(name).unwrap();
                let (old_decl_attr_init, old_decl_attr_global) = match &old_decl.attrs {
                    IdentifierAttr::StaticAttr(init, global) => (init.clone(), global.clone()),
                    _                                           => panic!("Not a variable with static storage duration")
                };
                
                if old_decl.dtype != Type::Int {
                    panic!("Function redeclared as variable");
                }

                if storage_class.is_some() && *storage_class.as_ref().unwrap() == StorageClass::Extern {
                    global = old_decl_attr_global;
                } else if global != old_decl_attr_global {
                    panic!("Conflicting variable linkage of {name}");
                }

                if initial_value_is_constant(&old_decl_attr_init) {
                    if initial_value_is_constant(&initial_value) {
                        panic!("Conflicting file scope variable definition");
                    } else {
                        initial_value = old_decl_attr_init;
                    }
                } else if !initial_value_is_constant(&initial_value) && initial_value_is_tentative(&old_decl_attr_init) {
                    initial_value = InitialValue::Tentative;
                }
            }

            let attrs = IdentifierAttr::StaticAttr(initial_value, global);
            symbols.insert(name.clone(), create_typedata(Type::Int, attrs));
        }
    }
}

fn typecheck_local_variable_declaration(decl: &VariableDeclaration, symbols: &mut HashMap<String, TypeData>) {
    match decl {
        VariableDeclaration::Variable(name, init, storage_class) => {
            let initial_value;
            if storage_class.is_some() && *storage_class.as_ref().unwrap() == StorageClass::Extern {
                if init.is_some() {
                    panic!("Initializer on local extern variable declaration");
                }
                if symbols.contains_key(name) {
                    let old_decl = symbols.get(name).unwrap();
                    if old_decl.dtype != Type::Int {
                        panic!("Function redeclared as variable");
                    }
                } else {
                    symbols.insert(name.clone(), create_typedata(Type::Int, IdentifierAttr::StaticAttr(InitialValue::NoInitializer, true)));
                }
            } else if storage_class.is_some() && *storage_class.as_ref().unwrap() == StorageClass::Static {
                if init.is_none() {
                    initial_value = InitialValue::Initial(0);
                } else {
                    match init.as_ref().unwrap() {
                        Exp::Constant(int) => initial_value = InitialValue::Initial(*int),
                        _                       => panic!("Non-constant initializer on local static variable")
                    }
                }

                symbols.insert(name.clone(), create_typedata(Type::Int, IdentifierAttr::StaticAttr(initial_value, false)));
            } else {
                symbols.insert(name.clone(), create_typedata(Type::Int, IdentifierAttr::LocalAttr));
                if init.is_some() {
                    typecheck_exp(init, symbols);
                }
            }
        }
    }
}

fn initial_value_is_constant(val: &InitialValue) -> bool {
    match val {
        InitialValue::Initial(_) => true,
        _                        => false,
    }
}

fn initial_value_is_tentative(val: &InitialValue) -> bool {
    match val {
        InitialValue::Tentative => true,
        _                       => false,
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

            let param_count = match fun_type.attrs {
                IdentifierAttr::FunAttr(_, _, param_count) => param_count,
                _                                                  => panic!("Function call of non-function")
            };
            
            if param_count != args.len() {
                panic!("Function called with the wrong number of arguments");
            }
            
            for arg in args {
                typecheck_exp(&Some(arg), symbols);
            }
        },
        Exp::Var(v) => {
            if symbols.contains_key(&v) && symbols.get(&v).unwrap().dtype != Type::Int {
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
            BlockItem::D(decl)      => typecheck_declaration(decl, symbols, &Scope::Block),
            BlockItem::S(statement)   => typecheck_statement(statement, symbols, &Scope::Block)
        }
    }
}

fn typecheck_statement(statement: &Statement, symbols: &mut HashMap<String, TypeData>, scope: &Scope) {
    match statement {
        Statement::While(condition, body, _) => {
            typecheck_exp(&Some(condition.clone()), symbols);
            typecheck_statement(body, symbols, scope);
        },
        Statement::Compound(body) => typecheck_block(&Some(body.clone()), symbols),
        Statement::DoWhile(body, condition, _) => {
            typecheck_statement(body, symbols, scope);
            typecheck_exp(&Some(condition.clone()), symbols);
        },
        Statement::Expression(exp) => {
            typecheck_exp(&Some(exp.clone()), symbols);
        },
        Statement::If(condition, then, els) => {
            typecheck_exp(&Some(condition.clone()), symbols);
            typecheck_statement(then, symbols, scope);
            if els.is_some() {
                typecheck_statement(&els.clone().unwrap(), symbols, scope);
            }
        },
        Statement::For(for_init, condition, post, body, _) => {
            typecheck_for_init(for_init, symbols);
            typecheck_exp(&condition, symbols);
            typecheck_exp(&post, symbols);
            typecheck_statement(body, symbols, scope);
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
        ForInit::InitDecl(decl) => {
            let (_, _, storage_class) = match decl {
                VariableDeclaration::Variable(n, i, s) => (n, i, s)
            };

            if storage_class.is_some() {
                panic!("For loop declaration has static/extern storage class");
            }

            typecheck_local_variable_declaration(decl, symbols)
        },
    }
}

fn typecheck_declaration(decl: &Declaration, symbols: &mut HashMap<String, TypeData>, scope: &Scope) {
    match decl {
        Declaration::FunDecl(fun_decl) => typecheck_function_declaration(fun_decl, symbols),
        Declaration::VarDecl(var_decl) => {
            if *scope == Scope::File {
                typecheck_file_scope_variable_declaration(var_decl, symbols)
            } else {
                typecheck_local_variable_declaration(var_decl, symbols);
            }
        },
    }
}