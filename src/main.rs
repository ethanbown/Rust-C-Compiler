mod compiler_driver;
mod lexer;
mod parser;
mod semantic_analysis;
mod tacky;
mod assembly;
mod code_emission;

use compiler_driver::compiler_driver;

fn main() {
    compiler_driver();
}