use std::{env::{self},
          fs::{self, File},
          io::{Write},
          path::{Path},
          process::{Command},
          ffi::OsStr::{self}};

use crate::{lexer::{PathData as PathData, 
                    lexer as lexer}, 
            parser::{parser as parser}, 
            assembly::{assembly as assembly}, 
            code_emission::{code_emission as code_emission}};

enum CompilerFlags {
    StopAtLex,
    StopAtParse,
    StopAtCodeGen,
    StopAtAssembly
}

pub fn compiler_driver() {
    let args : Vec<String> = env::args().collect();
    match args.len() {
        1 => panic!("rcc: fatal error: no input files"),
        2 => create_executable(Path::new(&args[1])),
        3 => stop_early(Path::new(&args[2]), get_compiler_flag(&args[1].as_str())),
        _ => panic!("rcc: fatal error: more than two arguments were passed")
    }
}

fn create_executable(path: &Path) {
    let path_data = create_pathdata(path);
    run_preprocessor(&path_data);
    compile_preprocessed_file(&path_data);
    assemble_and_link_file(&path_data);
}

fn stop_early(path: &Path, flag: CompilerFlags) {
    match flag {
        CompilerFlags::StopAtLex => stop_at_lex(path),
        CompilerFlags::StopAtParse => stop_at_parse(path),
        CompilerFlags::StopAtCodeGen => stop_at_codegen(path),
        CompilerFlags::StopAtAssembly => stop_at_assembly(path),
    }
}

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
    
    let file_extension = path.extension();
    match file_extension {
        Some(osstr) => osstr == OsStr::new("c"),
        None => panic!("Issue: failed to get extension of file")
    };

    PathData {
        file_path,
        file_stem,
        file_parent,
    }
}

fn get_compiler_flag(flag: &str) -> CompilerFlags {
    match flag {
        "--lex" => CompilerFlags::StopAtLex,
        "--parse" => CompilerFlags::StopAtParse,
        "--codegen" => CompilerFlags::StopAtCodeGen,
        "-S" => CompilerFlags::StopAtAssembly,
        _ => panic!("Invalid flag passed: '{}'", flag)
    }
}

pub fn stop_at_lex(path: &Path) {
    let path_data = create_pathdata(path);
    run_preprocessor(&path_data);
    let file_path_i = path_data.file_parent.clone() + "/" + path_data.file_stem.as_str() + ".i";
    let mut _tokens = lexer(&file_path_i, &path_data);
    println!("Stop at Lex!");
}

pub fn stop_at_parse(path: &Path) {
    let path_data = create_pathdata(path);
    run_preprocessor(&path_data);
    let file_path_i = path_data.file_parent.clone() + "/" + path_data.file_stem.as_str() + ".i";
    let mut tokens = lexer(&file_path_i, &path_data);
    parser(&mut tokens);
    println!("Stop at Parse!");
}

pub fn stop_at_codegen(path: &Path) {
    let path_data = create_pathdata(path);
    run_preprocessor(&path_data);
    let file_path_i = path_data.file_parent.clone() + "/" + path_data.file_stem.as_str() + ".i";
    let mut tokens = lexer(&file_path_i, &path_data);
    let ast = parser(&mut tokens);
    assembly(&ast);
    println!("Stop at CodeGen!");
}

pub fn stop_at_assembly(path: &Path) {
    let path_data = create_pathdata(path);
    run_preprocessor(&path_data);
    compile_preprocessed_file(&path_data);
    println!("Stop at Assembly!");
}

pub fn run_preprocessor(pd : &PathData) {
    let preprocessed_file_path = 
        if pd.file_parent.is_empty() {
            pd.file_stem.clone() + ".i"
        } else {
            pd.file_parent.clone() + "/" + pd.file_stem.as_str() + ".i"
        };

    let output =
        Command::new("gcc")
        .args(["-E", "-P", pd.file_path.as_str(), "-o", &preprocessed_file_path])
        .output()
        .expect("Failure to create {file_stem}.i");

    println!("Output: {}", String::from_utf8(output.stdout.to_vec()).expect("Failure to print output from gcc"));
}

pub fn compile_preprocessed_file(pd: &PathData) {
    let file_path_i = pd.file_parent.clone() + "/" + pd.file_stem.as_str() + ".i";
    let file_path_s = pd.file_parent.clone() + "/" + pd.file_stem.as_str() + ".s";


    let mut tokens = lexer(&file_path_i, pd);
    let ast = parser(&mut tokens);
    let binary_ast = assembly(&ast);
    let assembly_content = code_emission(&binary_ast);
    let mut assembly_file = File::create(file_path_s).expect("Failed to create assembly file");
    assembly_file.write_all(assembly_content.as_bytes()).expect("Failed to write assembly in compile_preprocessed_file");

    fs::remove_file(&file_path_i).expect("Failed to remove {file_path_i}");
}

pub fn assemble_and_link_file(pd: &PathData) {
    let file_path_s = pd.file_parent.clone() + "/" + pd.file_stem.as_str() + ".s";
    let executable_path = pd.file_parent.clone() + "/" + pd.file_stem.as_str();
    let output =
        Command::new("gcc")
        .args([&file_path_s, "-o", &executable_path])
        .output()
        .expect("Failure to create {file_stem}.exe");
    
    fs::remove_file(&file_path_s).expect("Failed to remove {file_path_s}");
    
    println!("Output: {}", String::from_utf8(output.stdout.to_vec()).expect("Failure to print output from gcc"));
}