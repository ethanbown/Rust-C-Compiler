use std::{env::{self},
          fs::{self, File},
          io::{Write},
          path::Path,
          process::Command};

use crate::{assembly::{assembly as assembly, assembly_ast::AssemblyProgram}, 
            code_emission::code_emission as code_emission, 
            lexer::{PathData as PathData, Tokens as Tokens, lexer as lexer}, 
            parser::{parser as parser, parser_ast::Program}, 
            semantic_analysis::{UniqueCounter, create_counter, semantic_analysis}, 
            tacky::{tacky as tacky, tacky_ast::IRProgram}};

enum CompilerFlags {
    StopAtLex,
    StopAtParse,
    StopAtSematic,
    StopAtTacky,
    StopAtCodeGen,
    StopAtAssembly,

    InvalidFlag
}

/// Starts the compiler based on command-line arguments.
pub fn compiler_driver() {
    let args : Vec<String> = env::args().collect();
    match args.len() {
        1 => panic!("rcc: no input files"),
        2 => create_executable(Path::new(&args[1])),
        3 => stop_early(Path::new(&args[2]), get_compiler_flag(&args[1].as_str())),
        _ => panic!("rcc: more than two arguments were passed")
    }
}

/// Returns a PathData struct based on given path.
pub fn create_pathdata(path: &Path) -> PathData {
    let file_stem = path
        .file_stem()
        .unwrap()
        .to_str()
        .unwrap()
        .to_string();

    let file_parent = path
        .parent()
        .unwrap()
        .to_str()
        .unwrap()
        .to_string();

    let file_path = path
        .to_str()
        .unwrap()
        .to_string();

    PathData {
        file_path,
        file_stem,
        file_parent,
    }
}

/// Creates an executable based on the code given at path.
fn create_executable(path: &Path) {
    let path_data = create_pathdata(path);
    run_preprocessor(&path_data);
    compile_preprocessed_file(&path_data, path);
    assemble_and_link_file(&path_data);
}

/// Reads flag given as first argument which determines where to stop in the compilation process.
fn get_compiler_flag(flag: &str) -> CompilerFlags {
    match flag {
        "--lex"      => CompilerFlags::StopAtLex,
        "--parse"    => CompilerFlags::StopAtParse,
        "--validate" => CompilerFlags::StopAtSematic,
        "--tacky"    => CompilerFlags::StopAtTacky,
        "--codegen"  => CompilerFlags::StopAtCodeGen,
        "-S"         => CompilerFlags::StopAtAssembly,
        _            => {
            eprintln!("rcc: invalid flag passed: '{}'\nList of valid flags to stop compilation after:
                       \n--lex:\t\tLexing the input.
                       \n--parse:\tPrevious and parsing.
                       \n--validate:\tPrevious and doing semantic analysis.
                       \n--tacky:\tPrevious and generating the IR: TACKY.
                       \n--codegen:\tPrevious and generating assembly AST.                       
                       \n-S:\t\tFully generating a .s assembly file", 
                       flag);
            CompilerFlags::InvalidFlag
        }
    }
}

/// Stops at compiler stage based on flag and prints completion message to stdout.
fn stop_early(path: &Path, flag: CompilerFlags) {
    match flag {
        CompilerFlags::InvalidFlag     => (),
        CompilerFlags::StopAtLex       => {
            stop_at_lex(path);
            println!("Stopped after lexing!");
        },
        CompilerFlags::StopAtParse     => {
            stop_at_parse(path);
            println!("Stopped after parsing!");
        },
        CompilerFlags::StopAtSematic   => {
            stop_at_semantic(path);
            println!("Stopped after semantic analysis!");
        },
        CompilerFlags::StopAtTacky     => {
            stop_at_tacky(path);
            println!("Stopped after generating TACKY/IR!");
        },
        CompilerFlags::StopAtCodeGen   => {
            stop_at_codegen(path);
            println!("Stopped after code generation!");
        },
        CompilerFlags::StopAtAssembly  => {
            stop_at_assembly(path);
            println!("Stopped after assembly generation!");
        },
    }
}

/// Returns a vector of tokens to parse
pub fn stop_at_lex(path: &Path) -> Vec<Tokens> {
    let pd = create_pathdata(path);
    run_preprocessor(&pd);
    let file_path_i = get_preprocessed_file_path(&pd);
    lexer(&file_path_i,)
}

/// Returns an AST to semantically analyze.
pub fn stop_at_parse(path: &Path) -> Program {
    let mut tokens = stop_at_lex(path);
    parser(&mut tokens)
}

/// Returns a transformed AST to convert to TACKY IR and UniqueCounter for creating unique temporary/label names.
pub fn stop_at_semantic(path: &Path) -> (Program, UniqueCounter) {
    let ast = stop_at_parse(path);
    let mut counter = create_counter();
    (semantic_analysis(&ast, &mut counter), counter)
}

/// Returns an IR of the program to turn into an assembly AST.
pub fn stop_at_tacky(path: &Path) -> IRProgram {
    let (transformed_ast, mut counter) = stop_at_semantic(path);
    tacky(&transformed_ast, &mut counter)
}

/// Returns an assembly AST to turn into assembly and write to a .s file.
pub fn stop_at_codegen(path: &Path) -> AssemblyProgram {
    let tacky_ast = stop_at_tacky(path);
    assembly(&tacky_ast)
}

/// Generates assembly and writes to a .s file.
pub fn stop_at_assembly(path: &Path) {
    let path_data = create_pathdata(path);
    run_preprocessor(&path_data);
    compile_preprocessed_file(&path_data, path);
}

/// Invokes gcc to run preprocessor and produce .i file to use during compilation.
pub fn run_preprocessor(pd : &PathData) {
    let preprocessed_file_path = get_preprocessed_file_path(pd);

    Command::new("gcc")
        .args(["-E", "-P", pd.file_path.as_str(), "-o", &preprocessed_file_path])
        .output()
        .expect("rcc: failed to create {file_stem}.i");
}

/// Runs custom c compiler and writes assembly to temporary .s file.
pub fn compile_preprocessed_file(pd: &PathData, path: &Path) {
    let file_path_i = get_preprocessed_file_path(&pd);
    let file_path_s = get_assembly_file_path(&pd);

    // Start of compilation
    let binary_ast = stop_at_codegen(path);
    let assembly_content = code_emission(&binary_ast);

    let mut assembly_file = File::create(file_path_s).expect("rcc: failed to create assembly file");
    assembly_file.write_all(assembly_content.as_bytes()).expect("rcc: failed to write assembly in compile_preprocessed_file");

    fs::remove_file(&file_path_i).expect("rcc: failed to remove {file_path_i}");
}

/// Assembles and links .s file into executable.
pub fn assemble_and_link_file(pd: &PathData) {
    let file_path_s = pd.file_parent.clone() + "/" + pd.file_stem.as_str() + ".s";
    let executable_path = pd.file_parent.clone() + "/" + pd.file_stem.as_str();
    Command::new("gcc")
        .args([&file_path_s, "-o", &executable_path])
        .output()
        .expect("Failure to create {file_stem}.exe");
    
    fs::remove_file(&file_path_s).expect("Failed to remove {file_path_s}");
}

/// Returns preprocessed file path.
fn get_preprocessed_file_path(pd: &PathData) -> String {
   let path = get_file_path(&pd);
   path + ".i"
}

/// Returns assembly file path.
fn get_assembly_file_path(pd: &PathData) -> String {
    let path = get_file_path(&pd);
    path + ".s"
}

/// Gets file path without extension.
fn get_file_path(pd: &PathData) -> String {
    if pd.file_parent.is_empty() {
        pd.file_stem.clone()
    } else {
        pd.file_parent.clone() + "/" + pd.file_stem.as_str()
    }
}