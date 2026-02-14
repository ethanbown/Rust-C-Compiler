use crate::lexer::Tokens;

pub mod parser_ast {
    #[derive(Debug)]
    pub enum Program {
        Program(FunctionDefinition)
    }

    #[derive(Debug)]
    pub enum FunctionDefinition {
        //       name,   body
        Function(String, Statement)
    }

    #[derive(Debug)]
    pub enum Statement {
        Return(Exp)
    }

    #[derive(Debug)]
    pub enum Exp {
        Constant(i32)
    }
}

use parser_ast::Program as Program;
use parser_ast::FunctionDefinition as FunctionDefinition;
use parser_ast::Statement as Statement;
use parser_ast::Exp as Exp;

pub fn parser(tokens: &mut Vec<Tokens>) -> Program {
    tokens.reverse();
    let ast = parse_program(tokens);
    if !tokens.is_empty() {
        panic!("more than just a main function");
    }
    ast
}

fn expected_token(expected: Tokens, tokens: &mut Vec<Tokens>) {
    let actual = take_token(tokens);
    if actual != expected {
        panic!("Actual token '{:?}' does not match expected token '{:?}'", actual, expected);
    }
}

fn take_token(tokens: &mut Vec<Tokens>) -> Tokens {
    let token = tokens.pop();
    let token = match token {
        Some(token) => token,
        None => panic!("No more tokens to take.")
    };
    token
}

fn parse_program(tokens: &mut Vec<Tokens>) -> Program {
    Program::Program(parse_function(tokens))
}

fn parse_function(tokens: &mut Vec<Tokens>) -> FunctionDefinition {
    expected_token(Tokens::Int, tokens);
    let name = parse_identifier(tokens);
    expected_token(Tokens::OpenParenthesis, tokens);
    expected_token(Tokens::Void, tokens);
    expected_token(Tokens::ClosedParenthesis, tokens);
    expected_token(Tokens::OpenCurlyBrace, tokens);
    let body = parse_statement(tokens);
    expected_token(Tokens::ClosedCurlyBrace, tokens);
    FunctionDefinition::Function(name, body)
}

fn parse_identifier(tokens: &mut Vec<Tokens>) -> String {
    let val = take_token(tokens);
    let val = match val {
        Tokens::Identifier(str) => str,
        _ => panic!("Not a valid identifier: {:?}", val)
    };
    val
}

fn parse_statement(tokens: &mut Vec<Tokens>) -> Statement {
    expected_token(Tokens::Return, tokens);
    let return_val = parse_exp(tokens);
    expected_token(Tokens::Semicolon, tokens);
    Statement::Return(return_val)
}

fn parse_exp(tokens: &mut Vec<Tokens>) -> Exp {
    let return_val = parse_int(tokens);
    Exp::Constant(return_val)
}

fn parse_int(tokens: &mut Vec<Tokens>) -> i32 {
    let val = take_token(tokens);
    let val = match val {
        Tokens::IntegerConstant(i32_num) => i32_num,
        _ => panic!("Not a valid i32 integer: '{:?}'", val)
    };
    val
}