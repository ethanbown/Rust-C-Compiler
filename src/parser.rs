use std::mem::discriminant;
use crate::lexer::Tokens;
use crate::lexer::Span;
use crate::parser::parser_ast::BinaryOperator;
use crate::parser::parser_ast::Block;
use crate::parser::parser_ast::StorageClass;

pub mod parser_ast {
    use String as Name;
    use String as Identifier;
    use String as Label;
    use Exp as Init;
    use Exp as Condition;
    use Exp as Post;
    use Statement as Body;
    use Statement as Then;
    use Statement as Else;

    #[derive(Debug)]
    pub enum Program {
        Program(Vec<Declaration>)
    }

    #[derive(Debug, Clone)]
    pub enum Declaration {
        FunDecl(FunctionDeclaration),
        VarDecl(VariableDeclaration)
    }

    #[derive(Debug, Clone)]
    pub enum FunctionDeclaration {
        Function(Name, Vec<Identifier>, Option<Block>, Option<StorageClass>)
    }

    #[derive(Debug, Clone)]
    pub enum Statement {
        Return(Exp),
        Expression(Exp),
        If(Condition, Box<Then>, Option<Box<Else>>),
        Compound(Block),
        Break(Label),
        Continue(Label),
        While(Condition, Box<Body>, Label),
        DoWhile(Box<Body>, Condition, Label),
        For(ForInit, Option<Condition>, Option<Post>, Box<Body>, Label),
        Null
    }

    #[derive(Debug, Clone)]
    pub enum VariableDeclaration {
        Variable(Name, Option<Init>, Option<StorageClass>)
    }

    #[derive(Debug, Clone)]
    pub enum Exp {
        Constant(i32),
        Var(Identifier),
        Unary(UnaryOperator, Box<Exp>),
        Binary(BinaryOperator, Box<Exp>, Box<Exp>),
        Assignment(Box<Exp>, Box<Exp>),
        Conditional(Box<Condition>, Box<Exp>, Box<Exp>),
        FunctionCall(Identifier, Vec<Exp>)
    }

    #[derive(Debug, Clone)]
    pub enum UnaryOperator {
        Complement,
        Negate,
        LogicalNot
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum StorageClass {
        Static,
        Extern
    }

    #[derive(Debug, Clone)]
    pub enum BinaryOperator {
        Add,
        Subtract,
        Multiply,
        Divide,
        Remainder,
        BitwiseAND,
        BitwiseOR,
        BitwiseXOR,
        LeftShift,
        RightShift,
        LogicalAND,
        LogicalOR,
        EqualTo,
        NotEqualTo,
        LessThan,
        GreaterThan,
        LessOrEqual,
        GreaterOrEqual
    }

    #[derive(Debug, Clone)]
    pub enum ForInit {
        InitDecl(VariableDeclaration),
        InitExp(Option<Exp>)
    }

    // Only for function body and block statements
    #[derive(Debug, Clone)]
    pub enum Block {
        Block(Vec<BlockItem>)
    }

    #[derive(Debug, Clone)]
    pub enum BlockItem {
        S(Statement),
        D(Declaration)
    }
}

use parser_ast::Program as Program;
use parser_ast::FunctionDeclaration as FunctionDeclaration;
use parser_ast::VariableDeclaration as VariableDeclaration;
use parser_ast::Statement as Statement;
use parser_ast::Exp as Exp;
use parser_ast::UnaryOperator as UnaryOperator;
use parser_ast::BlockItem as BlockItem;
use parser_ast::Declaration as Declaration;
use parser_ast::ForInit as ForInit;

#[cfg(test)]
mod tests {
    //use super::Tokens::*;
}

/// Invokes parser and returns AST of program.
pub fn parser(tokens: &mut Vec<Tokens>) -> Program {
    tokens.reverse();
    let ast = parse_program(tokens);
    if !tokens.is_empty() {
        dbg!(tokens);
        panic!("Did not parse all tokens");
    }

    //dbg!(&ast);

    ast
}

/// Parse entire program.
fn parse_program(tokens: &mut Vec<Tokens>) -> Program {
    let mut declarations: Vec<Declaration> = Vec::new();

    while tokens.len() > 0 && is_specifier(&peek(tokens)) {
        declarations.push(parse_declaration(tokens));
    }

    Program::Program(declarations)
}

/// Parses declarations
fn parse_declaration(tokens: &mut Vec<Tokens>) -> Declaration {
    let mut specifier_list = take_specifier_list(tokens);
    let identifier_token = take_token(tokens);
    let check_if_function = peek(tokens);
    return_token(tokens, identifier_token);
    return_specifier_list(tokens, &mut specifier_list);

    if matches!(check_if_function, Tokens::OpenParenthesis(..)) {
        Declaration::FunDecl(parse_function_declaration(tokens))
    } else {
        Declaration::VarDecl(parse_variable_declaration(tokens))
    }
}

/// Parse function declarations and function definitions
fn parse_function_declaration(tokens: &mut Vec<Tokens>) -> FunctionDeclaration {
    // Checks for function elements surrounding body
    let storage_class = parse_specifiers(tokens);
    let name = parse_identifier(tokens);
    expected_token(Tokens::OpenParenthesis(Span::default()), tokens);
    let param_list = parse_param_list(tokens);
    expected_token(Tokens::ClosedParenthesis(Span::default()), tokens);
    let next_token = peek(tokens);
    
    if matches!(next_token, Tokens::Semicolon(..)) {
        expected_token(Tokens::Semicolon(Span::default()), tokens);
        return FunctionDeclaration::Function(name, param_list, None, storage_class);
    }

    FunctionDeclaration::Function(name, param_list, Some(parse_block(tokens)), storage_class)
}

/// Goes through specifiers by removing them from list
fn take_specifier_list(tokens: &mut Vec<Tokens>) -> Vec<Tokens> {
    let mut specifier_list: Vec<Tokens> = Vec::new();
    
    while is_specifier(&peek(tokens)) {
        specifier_list.push(take_token(tokens));
    }

    specifier_list
}

/// Returns specifiers to tokens
fn return_specifier_list(tokens: &mut Vec<Tokens>, specifier_list: &mut Vec<Tokens>) {
    while !specifier_list.is_empty() {
        tokens.push(specifier_list.pop().unwrap());
    }
}

/// Parses variable declaration; can optionally include initialization expression.
fn parse_variable_declaration(tokens: &mut Vec<Tokens>) -> VariableDeclaration {
    let storage_class = parse_specifiers(tokens);
    let identifier = parse_identifier(tokens);

    // Either a declaration is initialized or not
    if matches!(peek(tokens), Tokens::Assignment(..)) {
        take_token(tokens);
        let exp = parse_exp(tokens, 0);
        expected_token(Tokens::Semicolon(Span::default()), tokens);
        VariableDeclaration::Variable(identifier, Some(exp), storage_class)
    } else {
        expected_token(Tokens::Semicolon(Span::default()), tokens);
        VariableDeclaration::Variable(identifier, None, storage_class)
    }
}

/// Parses specifiers like int and static that appear at beginning of declaration
fn parse_specifiers(tokens: &mut Vec<Tokens>) -> Option<StorageClass> {
    let specifier_list= take_specifier_list(tokens);

    let (_type_specifier, storage_class) = parse_type_and_storage_class(&specifier_list);

    storage_class
}

/// Parses types like int and storage classes like static
fn parse_type_and_storage_class(specifier_list: &Vec<Tokens>) -> (Tokens, Option<StorageClass>) {
    let mut types: Vec<Tokens> = Vec::new();
    let mut storage_classes: Vec<Tokens> = Vec::new();

    for specifier in specifier_list {
        if matches!(*specifier, Tokens::Int(..)) {
            types.push(specifier.clone());
        } else {
            storage_classes.push(specifier.clone())
        }
    }

    if types.len() != 1 {
        panic!("Invalid type specifier");
    }
    if storage_classes.len() > 1 {
        panic!("Invalid storage class");
    }

    let type_specifier = Tokens::Int(Span::default());
    let mut storage_class = None;

    if storage_classes.len() == 1 {
        let token = storage_classes.iter().nth(0).unwrap().clone();
        storage_class = parse_storage_class(&token);
    } 

    (type_specifier, storage_class)
}

/// Parses storage class based on input token
fn parse_storage_class(token: &Tokens) -> Option<StorageClass> {
    match token {
        Tokens::Static(_)    => Some(StorageClass::Static),
        Tokens::Extern(_)    => Some(StorageClass::Extern),
        _                 => panic!("Invalid storage class passed to parse_storage_class")
    }
}

/// Checks if next token is a specifier
fn is_specifier(token: &Tokens) -> bool {
    match token {
        Tokens::Int(_)
        | Tokens::Static(_)
        | Tokens::Extern(_) => true,
        _                => false
    }
}

fn parse_param_list(tokens: &mut Vec<Tokens>) -> Vec<String> {
    let mut return_val: Vec<String> = Vec::new();
    let mut next_token = peek(tokens);

    if matches!(next_token, Tokens::Void(..)) {
        expected_token(Tokens::Void(Span::default()), tokens);
        return return_val;
    } else if !matches!(next_token, Tokens::Int(..)) {
        panic!("Empty parameter list without void keyword.")
    } 

    while matches!(next_token, Tokens::Int(..)) {
        expected_token(Tokens::Int(Span::default()), tokens);
        let identifier = parse_identifier(tokens);
        return_val.push(identifier);
        next_token = peek(tokens);
        
        if matches!(next_token, Tokens::Comma(..)) {
            expected_token(Tokens::Comma(Span::default()), tokens);
            next_token = peek(tokens);
            if !matches!(next_token, Tokens::Int(..)) {
                panic!("trailing comma in function parameter list.");
            }
        }
    } 

    return_val
}

/// Handles a list of statements/declarations in compound statements and function bodies
fn parse_block(tokens: &mut Vec<Tokens>) -> Block {
    expected_token(Tokens::OpenCurlyBrace(Span::default()), tokens);
    let mut block_body: Vec<BlockItem> = Vec::new();
    
    // Goes through every line inside function body
    while !matches!(peek(tokens), Tokens::ClosedCurlyBrace(..)) {
        let next_block_item = parse_block_item(tokens);
        block_body.push(next_block_item);
    }
    take_token(tokens);
    Block::Block(block_body)
}

/// Parse identifier and return string name.
fn parse_identifier(tokens: &mut Vec<Tokens>) -> String {
    let val = take_token(tokens);
    let val = match val {
        Tokens::Identifier(str, _) => str,
        _ => panic!("Not a valid identifier: {:?}", val)
    };
    val
}

/// Decides whether to parse declaration or statement based on if the next token is an "int" keyword.
/// 
/// Treated as a declaration if next token is so, otherwise as a statement.
fn parse_block_item(tokens: &mut Vec<Tokens>) -> BlockItem {
    if is_specifier(&peek(tokens))  {
        let decl = parse_declaration(tokens);
        BlockItem::D(decl)
    } else {
        let stat = parse_statement(tokens);
        BlockItem::S(stat)
    }
}



/// Parses return, expression, and null statements.
fn parse_statement(tokens: &mut Vec<Tokens>) -> Statement {
    let next_token = peek(tokens);
    if matches!(next_token, Tokens::Return(..)) {
        // Handles returns
        take_token(tokens);
        let return_val = parse_exp(tokens, 0);
        expected_token(Tokens::Semicolon(Span::default()), tokens);
        Statement::Return(return_val)
    } else if matches!(next_token, Tokens::Semicolon(..)) {
        // Handles empty statements
        take_token(tokens);
        Statement::Null
    } else if matches!(next_token, Tokens::If(..)) {
        take_token(tokens);
        expected_token(Tokens::OpenParenthesis(Span::default()), tokens);
        let condition = parse_exp(tokens, 0);
        expected_token(Tokens::ClosedParenthesis(Span::default()), tokens);
        let if_statement = parse_statement(tokens);
        let next_token = peek(tokens);
        
        // Need to peek to check for optional else
        if matches!(next_token, Tokens::Else(..)) {
            take_token(tokens);
            let else_statement = parse_statement(tokens);
            Statement::If(condition, Box::new(if_statement), Some(Box::new(else_statement)))
        } else {
            Statement::If(condition, Box::new(if_statement), None)
        }
    } else if matches!(next_token, Tokens::OpenCurlyBrace(..)) {
        // Start of compound block
        Statement::Compound(parse_block(tokens))
    } else if matches!(next_token, Tokens::Break(..)) {
        take_token(tokens);
        expected_token(Tokens::Semicolon(Span::default()), tokens);
        Statement::Break("TEMP".to_string())
    } else if matches!(next_token, Tokens::Continue(..)) {
        take_token(tokens);
        expected_token(Tokens::Semicolon(Span::default()), tokens);
        Statement::Continue("TEMP".to_string())
    } else if matches!(next_token, Tokens::While(..)) {
        take_token(tokens);
        expected_token(Tokens::OpenParenthesis(Span::default()), tokens);
        let condition = parse_exp(tokens, 0);
        expected_token(Tokens::ClosedParenthesis(Span::default()), tokens);
        let inner_stat = parse_statement(tokens);
        Statement::While(condition, Box::new(inner_stat), "TEMP".to_string())
    } else if matches!(next_token, Tokens::Do(..)) {
        take_token(tokens);
        let inner_stat = parse_statement(tokens);
        expected_token(Tokens::While(Span::default()), tokens);
        expected_token(Tokens::OpenParenthesis(Span::default()), tokens);
        let condition = parse_exp(tokens, 0);
        expected_token(Tokens::ClosedParenthesis(Span::default()), tokens);
        expected_token(Tokens::Semicolon(Span::default()), tokens);
        Statement::DoWhile(Box::new(inner_stat), condition, "TEMP".to_string())
    } else if matches!(next_token, Tokens::For(..)) {
        take_token(tokens);
        expected_token(Tokens::OpenParenthesis(Span::default()), tokens);
        let for_init = parse_for_init(tokens);
        let condition = parse_optional_exp(tokens, Tokens::Semicolon(Span::default()));
        expected_token(Tokens::Semicolon(Span::default()), tokens);
        let last_exp = parse_optional_exp(tokens, Tokens::ClosedParenthesis(Span::default()));
        expected_token(Tokens::ClosedParenthesis(Span::default()), tokens);
        let inner_stat = parse_statement(tokens);
        Statement::For(for_init, condition, last_exp, Box::new(inner_stat), "TEMP".to_string())

    } else {
        // Expression statement
        let exp = parse_exp(tokens, 0);
        expected_token(Tokens::Semicolon(Span::default()), tokens);
        Statement::Expression(exp)
    }
}

/// Either returns a declaration or expression for initialization
fn parse_for_init(tokens: &mut Vec<Tokens>) -> ForInit {
    let next_token = peek(tokens);
    if is_specifier(&next_token) {
        ForInit::InitDecl(parse_variable_declaration(tokens))
    } else {
        let return_val = ForInit::InitExp(parse_optional_exp(tokens, Tokens::Semicolon(Span::default())));
        expected_token(Tokens::Semicolon(Span::default()), tokens);
        return_val
    }
}

/// Parses precedence and associativity via precedence climbing
/// 
/// min_prec must be zero on the first call, > 0 on recursive calls
fn parse_exp(tokens: &mut Vec<Tokens>, min_prec: i32) -> Exp {
    let mut left = parse_factor(tokens);
    let mut next_token = peek(tokens);
    while is_binop(&next_token) && precedence(&next_token) >= min_prec {
        if matches!(next_token, Tokens::Assignment(..)) {
            take_token(tokens);
            let right = parse_exp(tokens, precedence(&next_token));
            left = Exp::Assignment(Box::new(left), Box::new(right));
        } else if is_compound_assignment(&next_token) {
            let operator = parse_binop(tokens);
            let right = parse_exp(tokens, precedence(&next_token));
            left = Exp::Assignment(Box::new(left.clone()), Box::new(Exp::Binary(operator, Box::new(left), Box::new(right))));
        } else if matches!(next_token, Tokens::QuestionMark(..)) {
            let middle = parse_conditional_middle(tokens);
            let right = parse_exp(tokens, precedence(&next_token));
            left = Exp::Conditional(Box::new(left), Box::new(middle), Box::new(right))
        } else {
            let operator = parse_binop(tokens);
            let right = parse_exp(tokens, precedence(&next_token) + 1);
            left = Exp::Binary(operator, Box::new(left), Box::new(right));
        }
        next_token = peek(tokens);
    }
    return left;
}

// Peeks at next token to determine if there is an optional token by
// comparing next token to end token.
fn parse_optional_exp(tokens: &mut Vec<Tokens>, end_token: Tokens) -> Option<Exp> {
    let next_token = peek(tokens);
    if next_token == end_token {
        None
    } else {
        Some(parse_exp(tokens, 0))
    }
}

fn parse_factor(tokens: &mut Vec<Tokens>) -> Exp {
    let next_token = peek(tokens);
    if is_int(&next_token) {
        let return_val = parse_int(tokens);
        Exp::Constant(return_val)
    }
    else if is_identifier(&next_token) {
        let identifier = parse_identifier(tokens);
        let next_token = check_for_postfix_operator(tokens);
        if is_increment_or_decrement(&next_token) {
            if matches!(next_token, Tokens::Increment(..)) {
                let assign = Exp::Assignment(Box::new(Exp::Var(identifier.clone())), Box::new(Exp::Binary(BinaryOperator::Add, Box::new(Exp::Var(identifier.clone())), Box::new(Exp::Constant(1)))));
                Exp::Binary(BinaryOperator::Subtract, Box::new(assign), Box::new(Exp::Constant(1)))
                
            } else {
                let assign = Exp::Assignment(Box::new(Exp::Var(identifier.clone())), Box::new(Exp::Binary(BinaryOperator::Subtract, Box::new(Exp::Var(identifier.clone())), Box::new(Exp::Constant(1)))));
                Exp::Binary(BinaryOperator::Add, Box::new(assign), Box::new(Exp::Constant(1)))
            }
        } else if matches!(peek(tokens), Tokens::OpenParenthesis(..)) {
            expected_token(Tokens::OpenParenthesis(Span::default()), tokens);
            let args = parse_argument_list(tokens);
            expected_token(Tokens::ClosedParenthesis(Span::default()), tokens);
            Exp::FunctionCall(identifier, args)
        } else {
            Exp::Var(identifier)
        }
    }
    else if is_unop(&next_token) {
        if is_increment_or_decrement(&next_token) {
            take_token(tokens);
            let inner_exp = parse_factor(tokens);
            if matches!(next_token, Tokens::Increment(..)) {
                Exp::Assignment(Box::new(inner_exp.clone()), Box::new(Exp::Binary(BinaryOperator::Add, Box::new(inner_exp), Box::new(Exp::Constant(1)))))
            } else {
                Exp::Assignment(Box::new(inner_exp.clone()), Box::new(Exp::Binary(BinaryOperator::Subtract, Box::new(inner_exp), Box::new(Exp::Constant(1)))))
            }
        } else {
            let operator = parse_unop(tokens);
            let inner_exp = parse_factor(tokens);
            Exp::Unary(operator, Box::new(inner_exp))
        }
    } else if matches!(next_token, Tokens::OpenParenthesis(..)) {
        take_token(tokens);
        let inner_exp = parse_exp(tokens, 0);
        expected_token(Tokens::ClosedParenthesis(Span::default()), tokens);
        return inner_exp
    } else {
        panic!("Malformed expression: {next_token:?}")
    }
}

/// Parses argument list and returns a list of expressions
fn parse_argument_list(tokens: &mut Vec<Tokens>) -> Vec<Exp> {
    let mut return_val: Vec<Exp> = Vec::new();
    let mut next_token = peek(tokens);

    while !matches!(next_token, Tokens::ClosedParenthesis(..)) {
        return_val.push(parse_exp(tokens, 0));
        next_token = peek(tokens);
        
        if matches!(next_token, Tokens::Comma(..)) {
            expected_token(Tokens::Comma(Span::default()), tokens);
        }
    }

    return_val
}

fn parse_int(tokens: &mut Vec<Tokens>) -> i32 {
    let val = take_token(tokens);
    let val = match val {
        Tokens::IntegerConstant(i32_num, _) => i32_num,
        _ => panic!("Not a valid i32 integer: '{:?}'", val)
    };
    val
}

fn parse_unop(tokens: &mut Vec<Tokens>) -> UnaryOperator {
    let unop = take_token(tokens);
    let unop = match unop {
        Tokens::Negation(_)    => UnaryOperator::Negate,
        Tokens::Complement(_)  => UnaryOperator::Complement,
        Tokens::LogicalNOT(_)  => UnaryOperator::LogicalNot, 
        _ => panic!("Failure in parse_unop.")
    };
    unop
}

/// Returns parser version of binary operator.
fn parse_binop(tokens: &mut Vec<Tokens>) -> BinaryOperator {
    let token = take_token(tokens);
    match token {
        Tokens::Add(_)            
        | Tokens::AddAssign(_)           => BinaryOperator::Add,
        Tokens::Negation(_) 
        | Tokens::SubtractAssign(_)      => BinaryOperator::Subtract,
        Tokens::Multiply(_)
        | Tokens::MultiplyAssign(_)      => BinaryOperator::Multiply,
        Tokens::Divide(_)         
        | Tokens::DivideAssign(_)        => BinaryOperator::Divide,
        Tokens::Remainder(_)     
        | Tokens::RemainderAssign(_)     => BinaryOperator::Remainder,
        Tokens::BitwiseAND(_)     
        | Tokens::BitwiseANDAssign(_)    => BinaryOperator::BitwiseAND,
        Tokens::BitwiseOR(_)      
        | Tokens::BitwiseORAssign(_)     => BinaryOperator::BitwiseOR,
        Tokens::BitwiseXOR(_)     
        | Tokens::BitwiseXORAssign(_)    => BinaryOperator::BitwiseXOR,
        Tokens::LeftShift(_)      
        | Tokens::LeftShiftAssign(_)     => BinaryOperator::LeftShift,
        Tokens::RightShift(_)     
        | Tokens::RightShiftAssign(_)    => BinaryOperator::RightShift,
        Tokens::LogicalAND(_)     => BinaryOperator::LogicalAND,
        Tokens::LogicalOR(_)      => BinaryOperator::LogicalOR,
        Tokens::EqualTo(_)        => BinaryOperator::EqualTo,
        Tokens::NotEqualTo(_)     => BinaryOperator::NotEqualTo,
        Tokens::LessThan(_)       => BinaryOperator::LessThan,
        Tokens::GreaterThan(_)    => BinaryOperator::GreaterThan,
        Tokens::LessOrEqual(_)    => BinaryOperator::LessOrEqual,
        Tokens::GreaterOrEqual(_) => BinaryOperator::GreaterOrEqual,
        _ => panic!("Issue in parse_binop")
    }
}

/// Deals with "? e1 :" of "cond ? e1 : e2" expressions.
fn parse_conditional_middle(tokens: &mut Vec<Tokens>) -> Exp {
    expected_token(Tokens::QuestionMark(Span::default()), tokens);
    let return_val = parse_exp(tokens, 0);
    expected_token(Tokens::Colon(Span::default()), tokens);

    return_val
}

fn is_compound_assignment(token: &Tokens) -> bool {
    match token {
        Tokens::AddAssign(_)
        | Tokens::SubtractAssign(_)
        | Tokens::MultiplyAssign(_)
        | Tokens::DivideAssign(_)
        | Tokens::RemainderAssign(_)
        | Tokens::BitwiseANDAssign(_)
        | Tokens::BitwiseORAssign(_)
        | Tokens::BitwiseXORAssign(_)
        | Tokens::LeftShiftAssign(_)
        | Tokens::RightShiftAssign(_) => true,
        _                 => false
    }
}

/// Removes token and checks if removed value is the expected token.
fn expected_token(expected: Tokens, tokens: &mut Vec<Tokens>) {
    let actual = take_token(tokens);
    if !matches!((&actual, &expected), (a, b) if discriminant(a) == discriminant(b)) {
        panic!("Actual token '{:?}' does not match expected token '{:?}'", actual, expected);
    }
}

/// Removes and returns token at the end of tokens.
fn take_token(tokens: &mut Vec<Tokens>) -> Tokens {
    let token = tokens.pop();
    let token = match token {
        Some(token) => token,
        None => panic!("No more tokens to take in take_token.")
    };
    token
}

/// Adds token back to list
fn return_token(tokens: &mut Vec<Tokens>, token: Tokens) {
    tokens.push(token);
}

/// Returns token at end of tokens without removal.
fn peek(tokens: &mut Vec<Tokens>) -> Tokens {
    let token  = take_token(tokens);
    tokens.push(token.clone());
    token
}

fn is_identifier(token: &Tokens) -> bool {
    match token {
        Tokens::Identifier(..) => true,
        _ => false
    }
}

fn is_int(token: &Tokens) -> bool {
    match token {
        Tokens::IntegerConstant(..) => true,
        _ => false
    }
}

fn is_unop(token: &Tokens) -> bool {
    match token {
        Tokens::Negation(_)
        | Tokens::Complement(_)
        | Tokens::LogicalNOT(_)
        | Tokens::Increment(_)
        | Tokens::Decrement(_) => true,
        _ => false
    }
}

fn is_increment_or_decrement(token: &Tokens) -> bool {
    match token {
        Tokens::Increment(_)
        | Tokens::Decrement(_) => true,
        _ => false
    }
}

/// Checks if next non-closed parenthesis token is increment or decrement, does not modify tokens.
fn check_for_postfix_operator(tokens: &mut Vec<Tokens>) -> Tokens {
    let mut next_token = take_token(tokens);
    let mut number_of_closed_parenthesis = 0;

    while matches!(next_token, Tokens::ClosedParenthesis(..)) {
        number_of_closed_parenthesis += 1;
        next_token = take_token(tokens);
    }

    let return_val = next_token.clone();

    if !matches!(next_token, Tokens::Increment(..)) && !matches!(next_token, Tokens::Decrement(..)) {
        tokens.push(next_token.clone());
    }

    while number_of_closed_parenthesis > 0 {
        tokens.push(Tokens::ClosedParenthesis(Span::default()));
        number_of_closed_parenthesis -= 1;
    }

    return_val
}

fn is_binop(token: &Tokens) -> bool {
    match token {
        Tokens::Add(_)       
        | Tokens::Negation(_) 
        | Tokens::Divide(_)   
        | Tokens::Multiply(_) 
        | Tokens::Remainder(_)
        | Tokens::BitwiseAND(_)
        | Tokens::BitwiseOR(_)
        | Tokens::BitwiseXOR(_)
        | Tokens::LeftShift(_)
        | Tokens::RightShift(_) 
        | Tokens::LogicalAND(_)
        | Tokens::LogicalOR(_) 
        | Tokens::EqualTo(_) 
        | Tokens::NotEqualTo(_)
        | Tokens::LessThan(_)
        | Tokens::GreaterThan(_)
        | Tokens::LessOrEqual(_)
        | Tokens::GreaterOrEqual(_) 
        | Tokens::Assignment(_)
        | Tokens::AddAssign(_)   
        | Tokens::SubtractAssign(_)
        | Tokens::MultiplyAssign(_)
        | Tokens::DivideAssign(_)
        | Tokens::RemainderAssign(_)
        | Tokens::BitwiseANDAssign(_)
        | Tokens::BitwiseORAssign(_)
        | Tokens::BitwiseXORAssign(_)
        | Tokens::LeftShiftAssign(_)
        | Tokens::RightShiftAssign(_)
        | Tokens::QuestionMark(_)     => (),
        _ => return false
    }
    true
}

/// Returns assigned precedence of binary tokens.
fn precedence(next_token: &Tokens) -> i32 {
    match next_token {
        Tokens::Multiply(_) | Tokens::Divide(_) | Tokens::Remainder(_) => 60,
        Tokens::Add(_) | Tokens::Negation(_)                     => 55,
        Tokens::LeftShift(_) | Tokens::RightShift(_)             => 50,
        Tokens::LessThan(_) | Tokens::LessOrEqual(_)
        | Tokens::GreaterThan(_) | Tokens::GreaterOrEqual(_)     => 45,
        Tokens::EqualTo(_) | Tokens::NotEqualTo(_)               => 42,
        Tokens::BitwiseAND(_)                                    => 40,
        Tokens::BitwiseXOR(_)                                    => 35,
        Tokens::BitwiseOR(_)                                     => 30,
        Tokens::LogicalAND(_)                                    => 25,
        Tokens::LogicalOR(_)                                     => 20,
        Tokens::QuestionMark(_)                                  => 10,
        Tokens::Assignment(_) | Tokens::AddAssign(_) 
        | Tokens::SubtractAssign(_) | Tokens::MultiplyAssign(_)
        | Tokens::DivideAssign(_) | Tokens::RemainderAssign(_)
        | Tokens::BitwiseANDAssign(_) | Tokens::BitwiseORAssign(_)
        | Tokens::BitwiseXORAssign(_) | Tokens::LeftShiftAssign(_)
        | Tokens::RightShiftAssign(_)                            => 1,
        _ => panic!("Non-valid token in precedence: {next_token:?}")
    }
}