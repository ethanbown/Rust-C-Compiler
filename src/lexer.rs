use std::{io::{Read, Write},
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
    Semicolon,

    // Keywords
    Int,
    Return,
    Void,

    // Operators
    Complement,
    Negation,
    Decrement,
    LogicalNOT,
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

    // Other
    Invalid
}

pub struct PathData {
    pub file_stem: String,
    pub file_parent: String,
    pub file_path: String
}

pub fn lexer(path: &String, pd: &PathData) -> Vec<Tokens> {
    let mut code_file =
        match File::open(&path) {
            Ok(file) => file,
            Err(why) => panic!("Failed to open '{}' in lexer: {}", path, why)
        };

    let mut data = String::new();
    match code_file.read_to_string(&mut data) {
        Ok(_) => (),
        Err(why) => panic!("Failed to read data from '{}' into string in lexer: {}", path, why)
    }

    let token_patterns = [
        r"^[a-zA-Z_]\w*\b",
        r"^[0-9]+\b",
        r"^int\b",
        r"^void\b",
        r"^return\b",
        r"^\(",
        r"^\)",
        r"^\{",
        r"^\}",
        r"^;",
        r"^-",
        r"^--",
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
        r"^>="
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

    data = data.trim_start().to_string();
    while !data.is_empty() {
        //dbg!(&data);
        let (token, longest_match) = match_tokens(&data, &token_set, &regexes);
        tokens.push(token);
        data = String::from(data.strip_prefix(longest_match.as_str()).unwrap());
        data = data.trim_start().to_string();
    }

    write_lexer_output(pd, &tokens);
    tokens
}

fn match_tokens(data: &String, token_set: &RegexSet, regexes: &Vec<Regex>) -> (Tokens, String)  {
    let data_str = data.as_str();

    // And then scan again to collect matches
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
    
    let token = 
        match longest_match {
            "int"    => Tokens::Int,
            "void"   => Tokens::Void,
            "return" => Tokens::Return,
            "("      => Tokens::OpenParenthesis,
            ")"      => Tokens::ClosedParenthesis,
            "{"      => Tokens::OpenCurlyBrace,
            "}"      => Tokens::ClosedCurlyBrace,
            ";"      => Tokens::Semicolon,
            "-"      => Tokens::Negation,
            "--"     => Tokens::Decrement,
            "~"      => Tokens::Complement,
            "+"      => Tokens::Add,
            "*"      => Tokens::Multiply,
            "/"      => Tokens::Divide,
            "%"      => Tokens::Remainder,
            "&"      => Tokens::BitwiseAND,
            "|"      => Tokens::BitwiseOR,
            "^"      => Tokens::BitwiseXOR,
            "<<"     => Tokens::LeftShift,
            ">>"     => Tokens::RightShift,
            "!"      => Tokens::LogicalNOT,
            "&&"     => Tokens::LogicalAND,
            "||"     => Tokens::LogicalOR,
            "=="     => Tokens::EqualTo,
            "!="     => Tokens::NotEqualTo,
            "<"      => Tokens::LessThan,
            ">"      => Tokens::GreaterThan,
            "<="     => Tokens::LessOrEqual,
            ">="     => Tokens::GreaterOrEqual,
            _ => {
                let identifier = Regex::new(r"[a-zA-Z_]\w*").unwrap();
                let constant = Regex::new(r"[0-9]+").unwrap();
                let t;

                let is_identifier = 
                    match identifier.find(longest_match) {
                        Some(id) => !id.is_empty(),
                        None => false
                    };

                let is_constant = 
                    match constant.find(longest_match) {
                        Some(cons) => !cons.is_empty(),
                        None => false
                    };
                
                if is_identifier {
                    t = Tokens::Identifier(String::from(longest_match));
                }
                else if is_constant {
                    t = Tokens::IntegerConstant(longest_match.parse().expect("Failed to transform constant into i32."))
                }
                else {
                    t = Tokens::Invalid
                }
                t
            }
        };
    

    match token {
        Tokens::Invalid => panic!("Invalid token."),
        _ => ()
    }

    (token, longest_match.to_string())
}

fn write_lexer_output(pd: &PathData, tokens: &Vec<Tokens>) {
    let lexer_output_path = "/home/werea/c_compiler/lexer_outputs/".to_string() + pd.file_stem.as_str() + ".lex";

    let mut lexer_output_file = File::create(lexer_output_path).expect("Failed to create lexer file.");

    for token in tokens {
        let mut data;
        match token {
            Tokens::Int                        => data = String::from("Int"),
            Tokens::Void                       => data = String::from("Void"),
            Tokens::Return                     => data = String::from("Return"),
            Tokens::Semicolon                  => data = String::from("Semicolon"),
            Tokens::OpenCurlyBrace             => data = String::from("OpenCurlyBrace"),
            Tokens::ClosedCurlyBrace           => data = String::from("ClosedCurlyBrace"),
            Tokens::OpenParenthesis            => data = String::from("OpenParenthesis"),
            Tokens::ClosedParenthesis          => data = String::from("ClosedParenthesis"),
            Tokens::Decrement                  => data = String::from("Decrement"),
            Tokens::Negation                   => data = String::from("Negation"),
            Tokens::Complement                 => data = String::from("Complement"),
            Tokens::Identifier(id)    => data = String::from("Identifier(\n    \"") + id + "\",\n)",
            Tokens::IntegerConstant(num) => data = String::from("IntegerConstant(\n    ") + num.to_string().as_str() + ",\n)",
            Tokens::Add                        => data = String::from("Add"),
            Tokens::Multiply                   => data = String::from("Multiply"),
            Tokens::Divide                     => data = String::from("Divide"),
            Tokens::Remainder                  => data = String::from("Remainder"),
            Tokens::BitwiseAND                 => data = String::from("BitwiseAND"),
            Tokens::BitwiseOR                  => data = String::from("BitwiseOR"),
            Tokens::BitwiseXOR                 => data = String::from("BitwiseXOR"),
            Tokens::LeftShift                  => data = String::from("LeftShift"),
            Tokens::RightShift                 => data = String::from("RightShift"),
            Tokens::LogicalNOT                 => data = String::from("LogicalNOT"),
            Tokens::LogicalAND                 => data = String::from("LogicalAND"),
            Tokens::LogicalOR                  => data = String::from("LogicalOR"),
            Tokens::EqualTo                    => data = String::from("EqualTo"),
            Tokens::NotEqualTo                 => data = String::from("NotEqualTo"),
            Tokens::LessThan                   => data = String::from("LessThan"),
            Tokens::GreaterThan                => data = String::from("GreaterThan"),
            Tokens::LessOrEqual                => data = String::from("LessOrEqual"),
            Tokens::GreaterOrEqual             => data = String::from("GreaterOrEqual"),
            Tokens::Invalid                    => data = String::from("Invalid Token")
        }
        data += ",\n";
        lexer_output_file.write(data.as_bytes()).expect("Failed to write to lexer output file");
    }
}