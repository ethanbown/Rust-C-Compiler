mod compiler_driver;
mod lexer;
mod parser;
mod tacky;
mod assembly;
mod code_emission;

use compiler_driver::compiler_driver as comp_driver;

fn main() {
    comp_driver()
}