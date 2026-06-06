use std::{
    io::{Read},
    fs::{File}
};

use regex::{Regex, RegexSet};

#[derive(Debug, PartialEq, Clone)]
pub struct Span {
    line_number: usize,
    col_number: usize,
    starting_offset: usize,
    ending_offset: usize,
}

impl Span {
    fn new(line_number: usize, col_number: usize, starting_offset: usize, ending_offset: usize) -> Self {
        Self {
            line_number: line_number,
            col_number: col_number,
            starting_offset: starting_offset,
            ending_offset: ending_offset
        }
    }
}


#[derive(Debug, PartialEq, Clone)]
pub enum Tokens {
    // Tokens with values
    Identifier(String, Span),
    IntegerConstant(i32, Span),

    // Characters
    OpenParenthesis(Span),
    ClosedParenthesis(Span),
    OpenCurlyBrace(Span),
    ClosedCurlyBrace(Span),
    QuestionMark(Span),
    Colon(Span),
    Semicolon(Span),

    // Keywords
    Int(Span),
    Return(Span),
    Void(Span),
    If(Span),
    Else(Span),
    Do(Span),
    While(Span),
    For(Span),
    Break(Span),
    Continue(Span),
    Static(Span),
    Extern(Span),

    // Unary Operators
    Negation(Span),
    Complement(Span),
    LogicalNOT(Span),
    Increment(Span),
    Decrement(Span),
    
    // Binary Operators
    Add(Span),
    Multiply(Span),
    Divide(Span),
    Remainder(Span),
    BitwiseAND(Span),
    BitwiseOR(Span),
    BitwiseXOR(Span),
    LeftShift(Span),
    RightShift(Span),
    LogicalAND(Span),
    LogicalOR(Span),
    EqualTo(Span),
    NotEqualTo(Span),
    LessThan(Span),
    GreaterThan(Span),
    LessOrEqual(Span),
    GreaterOrEqual(Span),
    Assignment(Span),
    Comma(Span),

    // Compound Assignment Operators
    AddAssign(Span),
    SubtractAssign(Span),
    MultiplyAssign(Span),
    DivideAssign(Span),
    RemainderAssign(Span),
    BitwiseANDAssign(Span),
    BitwiseORAssign(Span),
    BitwiseXORAssign(Span),
    LeftShiftAssign(Span),
    RightShiftAssign(Span),

    // Other
    Invalid
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
        r"^static",
        r"^extern",
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
        r"^:",
        r"^,",
    ];

    let token_set = RegexSet::new(token_patterns).expect("RegexSet should be generated without error");

    // Recommended to compile patterns independently
    let regexes = token_set
        .patterns()
        .iter()
        .map(|pattern| Regex::new(pattern).unwrap())
        .collect();

    // Collect tokens to return
    let mut tokens: Vec<Tokens> = Vec::new();

    code_data = code_data.trim_start().to_string();

    // Always grab token at beginning of string that has longest match,
    // make sure there is no leading whitespace
    let mut starting_byte_offset: usize = 0;
    let mut line_number: usize = 1;
    let mut col_number: usize = 1;
    while !code_data.is_empty() {
        //dbg!(&code_data);
        dbg!(&starting_byte_offset);
        dbg!(&line_number);
        dbg!(&col_number);
        println!();
        let (token, longest_match) = match_tokens(&code_data, &token_set, &regexes, Span::new(0,0,0,0));
        tokens.push(token);
        starting_byte_offset += longest_match.len();
        col_number += longest_match.len();
        code_data = String::from(code_data.strip_prefix(longest_match.as_str()).unwrap());
        if check_start(&mut code_data) {
            line_number += 1;
            col_number = 1;
        }
        let temp_code_data = code_data.trim_start().to_string();
        // guaranteed to be non-negative
        starting_byte_offset += code_data.len() - temp_code_data.len();
        col_number += code_data.len() - temp_code_data.len();
        code_data = temp_code_data;
    }

    //dbg!(&tokens);

    tokens
}

fn check_start(code_data: &mut String) -> bool {
    for byte in code_data.bytes() {
        if !byte.is_ascii_whitespace() {
            break;
        }
        if byte == b'\n' {
            return true
        }
    }
    false
}

/// Returns the longest token matched.
fn match_tokens(data: &String, token_set: &RegexSet, regexes: &Vec<Regex>, span: Span) -> (Tokens, String)  {
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
            "int"         => Tokens::Int(span),
            "void"        => Tokens::Void(span),
            "return"      => Tokens::Return(span),
            "if"          => Tokens::If(span),
            "else"        => Tokens::Else(span),
            "do"          => Tokens::Do(span),
            "while"       => Tokens::While(span),
            "for"         => Tokens::For(span),
            "break"       => Tokens::Break(span),
            "continue"    => Tokens::Continue(span),    
            "static"      => Tokens::Static(span),
            "extern"      => Tokens::Extern(span),
            "("           => Tokens::OpenParenthesis(span),
            ")"           => Tokens::ClosedParenthesis(span),
            "{"           => Tokens::OpenCurlyBrace(span),
            "}"           => Tokens::ClosedCurlyBrace(span),
            ":"           => Tokens::Colon(span),
            ";"           => Tokens::Semicolon(span),
            "?"           => Tokens::QuestionMark(span),
            "-"           => Tokens::Negation(span),
            "~"           => Tokens::Complement(span),
            "+"           => Tokens::Add(span),
            "*"           => Tokens::Multiply(span),
            "/"           => Tokens::Divide(span),
            "%"           => Tokens::Remainder(span),
            "&"           => Tokens::BitwiseAND(span),
            "|"           => Tokens::BitwiseOR(span),
            "^"           => Tokens::BitwiseXOR(span),
            "<<"          => Tokens::LeftShift(span),
            ">>"          => Tokens::RightShift(span),
            "!"           => Tokens::LogicalNOT(span),
            "&&"          => Tokens::LogicalAND(span),
            "||"          => Tokens::LogicalOR(span),
            "=="          => Tokens::EqualTo(span),
            "!="          => Tokens::NotEqualTo(span),
            "<"           => Tokens::LessThan(span),
            ">"           => Tokens::GreaterThan(span),
            "<="          => Tokens::LessOrEqual(span),
            ">="          => Tokens::GreaterOrEqual(span),
            "="           => Tokens::Assignment(span),
            ","           => Tokens::Comma(span),
            "--"          => Tokens::Decrement(span),
            "++"          => Tokens::Increment(span),
            "+="          => Tokens::AddAssign(span),
            "-="          => Tokens::SubtractAssign(span),
            "*="          => Tokens::MultiplyAssign(span),
            "/="          => Tokens::DivideAssign(span),
            "%="          => Tokens::RemainderAssign(span),
            "&="          => Tokens::BitwiseANDAssign(span),
            "|="          => Tokens::BitwiseORAssign(span),
            "^="          => Tokens::BitwiseXORAssign(span),
            "<<="         => Tokens::LeftShiftAssign(span),
            ">>="         => Tokens::RightShiftAssign(span),
            _             => match_identifier_or_constant(longest_match, span)
        };
    
    match token {
        Tokens::Invalid => panic!("Invalid token."),
        _               => (token, longest_match.to_string())
    }
}

/// Matches either an identifier or constant, or returns invalid.
fn match_identifier_or_constant(longest_match: &str, span: Span) -> Tokens {
    if is_identifier(longest_match) {
        Tokens::Identifier(String::from(longest_match), span)
    }
    else if is_constant(longest_match) {
        Tokens::IntegerConstant(longest_match.parse().expect("rcc: failed to transform constant into i32."), span)
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