use std::{io::{Read},
          fs::{File}};
use regex::{Regex, RegexSet};

#[cfg(test)]
mod tests {
    //use crate::compiler_driver::*;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Tokens {
    // Tokens with values
    Identifier(String),
    IntegerConstant(i32),

    // Characters
    OpenParenthesis,
    ClosedParenthesis,
    OpenCurlyBrace,
    ClosedCurlyBrace,
    QuestionMark,
    Colon,
    Semicolon,

    // Keywords
    Int,
    Return,
    Void,
    If,
    Else,
    Do,
    While,
    For,
    Break,
    Continue,

    // Unary Operators
    Negation,
    Complement,
    LogicalNOT,
    Increment,
    Decrement,

    // Binary Operators
    Add,
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
    GreaterOrEqual,
    Assignment,

    // Compound Assignment Operators
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    RemainderAssign,
    BitwiseANDAssign,
    BitwiseORAssign,
    BitwiseXORAssign,
    LeftShiftAssign,
    RightShiftAssign,

    // Other
    Invalid
}

pub struct PathData {
    pub file_stem: String,
    pub file_parent: String,
    pub file_path: String
}

/// Invokes lexer and returns a vector of tokens to parse.
pub fn lexer(path: &String) -> Vec<Tokens> {
    let mut code_file =
        match File::open(&path) {
            Ok(file) => file,
            Err(why) => panic!("rcc: failed to open '{}' in lexer: {}", path, why)
        };

    let mut code_data = String::new();
    match code_file.read_to_string(&mut code_data) {
        Ok(_) => (),
        Err(why) => panic!("Failed to read data from '{}' into string in lexer: {}", path, why)
    }

    let token_patterns = [
        r"^[a-zA-Z_]\w*\b",
        r"^[0-9]+\b",
        r"^int\b",
        r"^void\b",
        r"^return\b",
        r"^if",
        r"^else",
        r"^do",
        r"^while",
        r"^for",
        r"^break",
        r"^continue",
        r"^\(",
        r"^\)",
        r"^\{",
        r"^\}",
        r"^;",
        r"^-",
        r"^~",
        r"^\+",
        r"^\*",
        r"^/",
        r"^%",
        r"^&",
        r"^\|",
        r"^\^",
        r"^<<",
        r"^>>",
        r"^!",
        r"^&&",
        r"^\|\|",
        r"^==",
        r"^!=",
        r"^<",
        r"^>",
        r"^<=",
        r"^>=",
        r"^=",
        r"^--",
        r"^\+\+",
        r"^\+=",
        r"^-=",
        r"^\*=",
        r"^/=",
        r"^%=",
        r"^&=",
        r"^\|=",
        r"^\^=",
        r"^<<=",
        r"^>>=",
        r"^\?",
        r"^:"
    ];

    let token_set = RegexSet::new(token_patterns).unwrap();

    // Recommended to compile patterns independently
    let regexes : Vec<_> = token_set
        .patterns()
        .iter()
        .map(|pattern| Regex::new(pattern).unwrap())
        .collect();

    // Collect tokens to return
    let mut tokens: Vec<Tokens> = Vec::new();

    code_data = code_data.trim_start().to_string();

    // Always grab token at beginning of string that has longest match,
    // make sure there is no leading whitespace
    while !code_data.is_empty() {
        //dbg!(&code_data);

        let (token, longest_match) = match_tokens(&code_data, &token_set, &regexes);
        tokens.push(token);
        code_data = String::from(code_data.strip_prefix(longest_match.as_str()).unwrap());
        code_data = code_data.trim_start().to_string();
    }

    //dbg!(&tokens);

    tokens
}

/// Returns the longest token matched.
fn match_tokens(data: &String, token_set: &RegexSet, regexes: &Vec<Regex>) -> (Tokens, String)  {
    let data_str = data.as_str();

    // Scan again to collect matches
    let matches: Vec<&str> = token_set
        .matches(data_str)
        .into_iter()
        .map(|index| &regexes[index])
        .map(|regex| regex.find(data_str).unwrap().as_str())
        .collect();

    let mut longest_match = "";
    for mat in matches {
        if mat.len() > longest_match.len() {
            longest_match = mat;
        }
    }
    
    let token = match longest_match {
            "int"         => Tokens::Int,
            "void"        => Tokens::Void,
            "return"      => Tokens::Return,
            "if"          => Tokens::If,
            "else"        => Tokens::Else,
            "do"          => Tokens::Do,
            "while"       => Tokens::While,
            "for"         => Tokens::For,
            "break"       => Tokens::Break,
            "continue"    => Tokens::Continue,    
            "("           => Tokens::OpenParenthesis,
            ")"           => Tokens::ClosedParenthesis,
            "{"           => Tokens::OpenCurlyBrace,
            "}"           => Tokens::ClosedCurlyBrace,
            ":"           => Tokens::Colon,
            ";"           => Tokens::Semicolon,
            "?"           => Tokens::QuestionMark,
            "-"           => Tokens::Negation,
            "~"           => Tokens::Complement,
            "+"           => Tokens::Add,
            "*"           => Tokens::Multiply,
            "/"           => Tokens::Divide,
            "%"           => Tokens::Remainder,
            "&"           => Tokens::BitwiseAND,
            "|"           => Tokens::BitwiseOR,
            "^"           => Tokens::BitwiseXOR,
            "<<"          => Tokens::LeftShift,
            ">>"          => Tokens::RightShift,
            "!"           => Tokens::LogicalNOT,
            "&&"          => Tokens::LogicalAND,
            "||"          => Tokens::LogicalOR,
            "=="          => Tokens::EqualTo,
            "!="          => Tokens::NotEqualTo,
            "<"           => Tokens::LessThan,
            ">"           => Tokens::GreaterThan,
            "<="          => Tokens::LessOrEqual,
            ">="          => Tokens::GreaterOrEqual,
            "="           => Tokens::Assignment,
            "--"          => Tokens::Decrement,
            "++"          => Tokens::Increment,
            "+="          => Tokens::AddAssign,
            "-="          => Tokens::SubtractAssign,
            "*="          => Tokens::MultiplyAssign,
            "/="          => Tokens::DivideAssign,
            "%="          => Tokens::RemainderAssign,
            "&="          => Tokens::BitwiseANDAssign,
            "|="          => Tokens::BitwiseORAssign,
            "^="          => Tokens::BitwiseXORAssign,
            "<<="         => Tokens::LeftShiftAssign,
            ">>="         => Tokens::RightShiftAssign,
            _             => match_identifier_or_constant(longest_match)
        };
    
    match token {
        Tokens::Invalid => panic!("Invalid token."),
        _               => (token, longest_match.to_string())
    }
}

/// Matches either an identifier or constant, or returns invalid.
fn match_identifier_or_constant(longest_match: &str) -> Tokens {
    if is_identifier(longest_match) {
        Tokens::Identifier(String::from(longest_match))
    }
    else if is_constant(longest_match) {
        Tokens::IntegerConstant(longest_match.parse().expect("rcc: failed to transform constant into i32."))
    }
    else {
        Tokens::Invalid
    }
}

fn is_identifier(longest_match: &str) -> bool {
    let identifier = Regex::new(r"[a-zA-Z_]\w*").unwrap();
    match identifier.find(longest_match) {
        Some(id) => !id.is_empty(),
        None => false
    }
}

fn is_constant(longest_match: &str) -> bool {
    let constant = Regex::new(r"[0-9]+").unwrap();
    match constant.find(longest_match) {
        Some(cons) => !cons.is_empty(),
        None => false
    }
}