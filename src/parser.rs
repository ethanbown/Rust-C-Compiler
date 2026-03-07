use crate::lexer::Tokens;
use crate::parser::parser_ast::BinaryOperator;
use crate::parser::parser_ast::Block;

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
        Program(FunctionDefinition)
    }

    #[derive(Debug)]
    pub enum FunctionDefinition {
        Function(Name, Block)
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
    pub enum Declaration {
        Declaration(Name, Option<Init>)
    }

    #[derive(Debug, Clone)]
    pub enum Exp {
        Constant(i32),
        Var(Identifier),
        Unary(UnaryOperator, Box<Exp>),
        Binary(BinaryOperator, Box<Exp>, Box<Exp>),
        Assignment(Box<Exp>, Box<Exp>),
        Conditional(Box<Condition>, Box<Exp>, Box<Exp>)
    }

    #[derive(Debug, Clone)]
    pub enum UnaryOperator {
        Complement,
        Negate,
        LogicalNot
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
        InitDecl(Declaration),
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
use parser_ast::FunctionDefinition as FunctionDefinition;
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
        panic!("more than just a main function");
    }

    //dbg!(&ast);

    ast
}

/// Parse entire program.
fn parse_program(tokens: &mut Vec<Tokens>) -> Program {
    Program::Program(parse_function(tokens))
}

/// Parse function and function body.
fn parse_function(tokens: &mut Vec<Tokens>) -> FunctionDefinition {
    // Checks for function elements surrounding body
    expected_token(Tokens::Int, tokens);
    let name = parse_identifier(tokens);
    expected_token(Tokens::OpenParenthesis, tokens);
    expected_token(Tokens::Void, tokens);
    expected_token(Tokens::ClosedParenthesis, tokens);
    FunctionDefinition::Function(name, parse_block(tokens))
}

/// Handles a list of statements/declarations in compound statements and function bodies
fn parse_block(tokens: &mut Vec<Tokens>) -> Block {
    expected_token(Tokens::OpenCurlyBrace, tokens);
    let mut block_body: Vec<BlockItem> = Vec::new();
    
    // Goes through every line inside function body
    while peek(tokens) != Tokens::ClosedCurlyBrace {
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
        Tokens::Identifier(str) => str,
        _ => panic!("Not a valid identifier: {:?}", val)
    };
    val
}

/// Decides whether to parse declaration or statement based on if the next token is an "int" keyword.
/// 
/// Treated as a declaration if next token is so, otherwise as a statement.
fn parse_block_item(tokens: &mut Vec<Tokens>) -> BlockItem {
    if peek(tokens) == Tokens::Int {
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
    if next_token == Tokens::Return {
        // Handles returns
        take_token(tokens);
        let return_val = parse_exp(tokens, 0);
        expected_token(Tokens::Semicolon, tokens);
        Statement::Return(return_val)
    } else if next_token == Tokens::Semicolon {
        // Handles empty statements
        take_token(tokens);
        Statement::Null
    } else if next_token == Tokens::If {
        take_token(tokens);
        expected_token(Tokens::OpenParenthesis, tokens);
        let condition = parse_exp(tokens, 0);
        expected_token(Tokens::ClosedParenthesis, tokens);
        let if_statement = parse_statement(tokens);
        let next_token = peek(tokens);
        
        // Need to peek to check for optional else
        if next_token == Tokens::Else {
            take_token(tokens);
            let else_statement = parse_statement(tokens);
            Statement::If(condition, Box::new(if_statement), Some(Box::new(else_statement)))
        } else {
            Statement::If(condition, Box::new(if_statement), None)
        }
    } else if next_token == Tokens::OpenCurlyBrace {
        // Start of compound block
        Statement::Compound(parse_block(tokens))
    } else if next_token == Tokens::Break {
        take_token(tokens);
        expected_token(Tokens::Semicolon, tokens);
        Statement::Break("TEMP".to_string())
    } else if next_token == Tokens::Continue {
        take_token(tokens);
        expected_token(Tokens::Semicolon, tokens);
        Statement::Continue("TEMP".to_string())
    } else if next_token == Tokens::While {
        take_token(tokens);
        expected_token(Tokens::OpenParenthesis, tokens);
        let condition = parse_exp(tokens, 0);
        expected_token(Tokens::ClosedParenthesis, tokens);
        let inner_stat = parse_statement(tokens);
        Statement::While(condition, Box::new(inner_stat), "TEMP".to_string())
    } else if next_token == Tokens::Do {
        take_token(tokens);
        let inner_stat = parse_statement(tokens);
        expected_token(Tokens::While, tokens);
        expected_token(Tokens::OpenParenthesis, tokens);
        let condition = parse_exp(tokens, 0);
        expected_token(Tokens::ClosedParenthesis, tokens);
        expected_token(Tokens::Semicolon, tokens);
        Statement::DoWhile(Box::new(inner_stat), condition, "TEMP".to_string())
    } else if next_token == Tokens::For {
        take_token(tokens);
        expected_token(Tokens::OpenParenthesis, tokens);
        let for_init = parse_for_init(tokens);
        let condition = parse_optional_exp(tokens, Tokens::Semicolon);
        expected_token(Tokens::Semicolon, tokens);
        let last_exp = parse_optional_exp(tokens, Tokens::ClosedParenthesis);
        expected_token(Tokens::ClosedParenthesis, tokens);
        let inner_stat = parse_statement(tokens);
        Statement::For(for_init, condition, last_exp, Box::new(inner_stat), "TEMP".to_string())

    } else {
        // Expression statement
        let exp = parse_exp(tokens, 0);
        expected_token(Tokens::Semicolon, tokens);
        Statement::Expression(exp)
    }
}

/// Either returns a declaration or expression for initialization
fn parse_for_init(tokens: &mut Vec<Tokens>) -> ForInit {
    let next_token = peek(tokens);
    if next_token == Tokens::Int {
        ForInit::InitDecl(parse_declaration(tokens))
    } else {
        let return_val = ForInit::InitExp(parse_optional_exp(tokens, Tokens::Semicolon));
        expected_token(Tokens::Semicolon, tokens);
        return_val
    }
}

/// Parses declaration; can optionally include initialization expression.
fn parse_declaration(tokens: &mut Vec<Tokens>) -> Declaration {
    expected_token(Tokens::Int, tokens);
    let identifier = parse_identifier(tokens);

    // Either a declaration is initialized or not
    if peek(tokens) == Tokens::Assignment {
        take_token(tokens);
        let exp = parse_exp(tokens, 0);
        expected_token(Tokens::Semicolon, tokens);
        Declaration::Declaration(identifier, Some(exp))
    } else {
        expected_token(Tokens::Semicolon, tokens);
        Declaration::Declaration(identifier, None)
    }
}

/// Parses precedence and associativity via precedence climbing
/// 
/// min_prec must be zero on the first call, > 0 on recursive calls
fn parse_exp(tokens: &mut Vec<Tokens>, min_prec: i32) -> Exp {
    let mut left = parse_factor(tokens);
    let mut next_token = peek(tokens);
    while is_binop(&next_token) && precedence(&next_token) >= min_prec {
        if next_token == Tokens::Assignment {
            take_token(tokens);
            let right = parse_exp(tokens, precedence(&next_token));
            left = Exp::Assignment(Box::new(left), Box::new(right));
        } else if is_compound_assignment(&next_token) {
            let operator = parse_binop(tokens);
            let right = parse_exp(tokens, precedence(&next_token));
            left = Exp::Assignment(Box::new(left.clone()), Box::new(Exp::Binary(operator, Box::new(left), Box::new(right))));
        } else if next_token == Tokens::QuestionMark {
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
            if next_token == Tokens::Increment {
                let assign = Exp::Assignment(Box::new(Exp::Var(identifier.clone())), Box::new(Exp::Binary(BinaryOperator::Add, Box::new(Exp::Var(identifier.clone())), Box::new(Exp::Constant(1)))));
                Exp::Binary(BinaryOperator::Subtract, Box::new(assign), Box::new(Exp::Constant(1)))
                
            } else {
                let assign = Exp::Assignment(Box::new(Exp::Var(identifier.clone())), Box::new(Exp::Binary(BinaryOperator::Subtract, Box::new(Exp::Var(identifier.clone())), Box::new(Exp::Constant(1)))));
                Exp::Binary(BinaryOperator::Add, Box::new(assign), Box::new(Exp::Constant(1)))
            }
        } else {
            Exp::Var(identifier)
        }
    }
    else if is_unop(&next_token) {
        if is_increment_or_decrement(&next_token) {
            take_token(tokens);
            let inner_exp = parse_factor(tokens);
            if next_token == Tokens::Increment {
                Exp::Assignment(Box::new(inner_exp.clone()), Box::new(Exp::Binary(BinaryOperator::Add, Box::new(inner_exp), Box::new(Exp::Constant(1)))))
            } else {
                Exp::Assignment(Box::new(inner_exp.clone()), Box::new(Exp::Binary(BinaryOperator::Subtract, Box::new(inner_exp), Box::new(Exp::Constant(1)))))
            }
        } else {
            let operator = parse_unop(tokens);
            let inner_exp = parse_factor(tokens);
            Exp::Unary(operator, Box::new(inner_exp))
        }
    } else if next_token == Tokens::OpenParenthesis {
        take_token(tokens);
        let inner_exp = parse_exp(tokens, 0);
        expected_token(Tokens::ClosedParenthesis, tokens);
        return inner_exp
    } else {
        panic!("Malformed expression: {next_token:?}")
    }
}

fn parse_int(tokens: &mut Vec<Tokens>) -> i32 {
    let val = take_token(tokens);
    let val = match val {
        Tokens::IntegerConstant(i32_num) => i32_num,
        _ => panic!("Not a valid i32 integer: '{:?}'", val)
    };
    val
}

fn parse_unop(tokens: &mut Vec<Tokens>) -> UnaryOperator {
    let unop = take_token(tokens);
    let unop = match unop {
        Tokens::Negation    => UnaryOperator::Negate,
        Tokens::Complement  => UnaryOperator::Complement,
        Tokens::LogicalNOT  => UnaryOperator::LogicalNot, 
        _ => panic!("Failure in parse_unop.")
    };
    unop
}

/// Returns parser version of binary operator.
fn parse_binop(tokens: &mut Vec<Tokens>) -> BinaryOperator {
    let token = take_token(tokens);
    match token {
        Tokens::Add            
        | Tokens::AddAssign           => BinaryOperator::Add,
        Tokens::Negation 
        | Tokens::SubtractAssign      => BinaryOperator::Subtract,
        Tokens::Multiply
        | Tokens::MultiplyAssign      => BinaryOperator::Multiply,
        Tokens::Divide         
        | Tokens::DivideAssign        => BinaryOperator::Divide,
        Tokens::Remainder     
        | Tokens::RemainderAssign     => BinaryOperator::Remainder,
        Tokens::BitwiseAND     
        | Tokens::BitwiseANDAssign    => BinaryOperator::BitwiseAND,
        Tokens::BitwiseOR      
        | Tokens::BitwiseORAssign     => BinaryOperator::BitwiseOR,
        Tokens::BitwiseXOR     
        | Tokens::BitwiseXORAssign    => BinaryOperator::BitwiseXOR,
        Tokens::LeftShift      
        | Tokens::LeftShiftAssign     => BinaryOperator::LeftShift,
        Tokens::RightShift     
        | Tokens::RightShiftAssign    => BinaryOperator::RightShift,
        Tokens::LogicalAND     => BinaryOperator::LogicalAND,
        Tokens::LogicalOR      => BinaryOperator::LogicalOR,
        Tokens::EqualTo        => BinaryOperator::EqualTo,
        Tokens::NotEqualTo     => BinaryOperator::NotEqualTo,
        Tokens::LessThan       => BinaryOperator::LessThan,
        Tokens::GreaterThan    => BinaryOperator::GreaterThan,
        Tokens::LessOrEqual    => BinaryOperator::LessOrEqual,
        Tokens::GreaterOrEqual => BinaryOperator::GreaterOrEqual,
        _ => panic!("Issue in parse_binop")
    }
}

/// Deals with "? e1 :" of "cond ? e1 : e2" expressions.
fn parse_conditional_middle(tokens: &mut Vec<Tokens>) -> Exp {
    expected_token(Tokens::QuestionMark, tokens);
    let return_val = parse_exp(tokens, 0);
    expected_token(Tokens::Colon, tokens);

    return_val
}

fn is_compound_assignment(token: &Tokens) -> bool {
    match token {
        Tokens::AddAssign
        | Tokens::SubtractAssign
        | Tokens::MultiplyAssign
        | Tokens::DivideAssign
        | Tokens::RemainderAssign
        | Tokens::BitwiseANDAssign
        | Tokens::BitwiseORAssign
        | Tokens::BitwiseXORAssign
        | Tokens::LeftShiftAssign
        | Tokens::RightShiftAssign => true,
        _                 => false
    }
}

/// Removes token and checks if removed value is the expected token.
fn expected_token(expected: Tokens, tokens: &mut Vec<Tokens>) {
    let actual = take_token(tokens);
    if actual != expected {
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

/// Returns token at end of tokens without removal.
fn peek(tokens: &mut Vec<Tokens>) -> Tokens {
    let token  = take_token(tokens);
    tokens.push(token.clone());
    token
}

fn is_identifier(token: &Tokens) -> bool {
    match token {
        Tokens::Identifier(_) => true,
        _ => false
    }
}

fn is_int(token: &Tokens) -> bool {
    match token {
        Tokens::IntegerConstant(_) => true,
        _ => false
    }
}

fn is_unop(token: &Tokens) -> bool {
    match token {
        Tokens::Negation
        | Tokens::Complement
        | Tokens::LogicalNOT
        | Tokens::Increment
        | Tokens::Decrement => true,
        _ => false
    }
}

fn is_increment_or_decrement(token: &Tokens) -> bool {
    match token {
        Tokens::Increment
        | Tokens::Decrement => true,
        _ => false
    }
}

/// Checks if next non-closed parenthesis token is increment or decrement, does not modify tokens.
fn check_for_postfix_operator(tokens: &mut Vec<Tokens>) -> Tokens {
    let mut next_token = take_token(tokens);
    let mut number_of_closed_parenthesis = 0;

    while next_token == Tokens::ClosedParenthesis {
        number_of_closed_parenthesis += 1;
        next_token = take_token(tokens);
    }

    let return_val = next_token.clone();

    if next_token != Tokens::Increment && next_token != Tokens::Decrement {
        tokens.push(next_token.clone());
    }

    while number_of_closed_parenthesis > 0 {
        tokens.push(Tokens::ClosedParenthesis);
        number_of_closed_parenthesis -= 1;
    }

    return_val
}

fn is_binop(token: &Tokens) -> bool {
    match token {
        Tokens::Add       
        | Tokens::Negation 
        | Tokens::Divide   
        | Tokens::Multiply 
        | Tokens::Remainder
        | Tokens::BitwiseAND
        | Tokens::BitwiseOR
        | Tokens::BitwiseXOR
        | Tokens::LeftShift
        | Tokens::RightShift 
        | Tokens::LogicalAND
        | Tokens::LogicalOR 
        | Tokens::EqualTo 
        | Tokens::NotEqualTo
        | Tokens::LessThan
        | Tokens::GreaterThan
        | Tokens::LessOrEqual
        | Tokens::GreaterOrEqual 
        | Tokens::Assignment
        | Tokens::AddAssign   
        | Tokens::SubtractAssign
        | Tokens::MultiplyAssign
        | Tokens::DivideAssign
        | Tokens::RemainderAssign
        | Tokens::BitwiseANDAssign
        | Tokens::BitwiseORAssign
        | Tokens::BitwiseXORAssign
        | Tokens::LeftShiftAssign
        | Tokens::RightShiftAssign
        | Tokens::QuestionMark     => (),
        _ => return false
    }
    true
}

/// Returns assigned precedence of binary tokens.
fn precedence(next_token: &Tokens) -> i32 {
    match next_token {
        Tokens::Multiply | Tokens::Divide | Tokens::Remainder => 60,
        Tokens::Add | Tokens::Negation                        => 55,
        Tokens::LeftShift | Tokens::RightShift                => 50,
        Tokens::LessThan | Tokens::LessOrEqual
        | Tokens::GreaterThan | Tokens::GreaterOrEqual        => 45,
        Tokens::EqualTo | Tokens::NotEqualTo                  => 42,
        Tokens::BitwiseAND                                    => 40,
        Tokens::BitwiseXOR                                    => 35,
        Tokens::BitwiseOR                                     => 30,
        Tokens::LogicalAND                                    => 25,
        Tokens::LogicalOR                                     => 20,
        Tokens::QuestionMark                                  => 10,
        Tokens::Assignment | Tokens::AddAssign 
        | Tokens::SubtractAssign | Tokens::MultiplyAssign
        | Tokens::DivideAssign | Tokens::RemainderAssign
        | Tokens::BitwiseANDAssign | Tokens::BitwiseORAssign
        | Tokens::BitwiseXORAssign | Tokens::LeftShiftAssign
        | Tokens::RightShiftAssign                            => 1,
        _ => panic!("Non-valid token in precedence: {next_token:?}")
    }
}