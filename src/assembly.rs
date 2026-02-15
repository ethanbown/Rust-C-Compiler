use crate::parser::parser_ast::*;

pub mod assembly_ast {
    #[derive(Debug)]
    pub enum AssemblyProgram {
        Program(AssemblyFunctionDefinition)
    }

    #[derive(Debug)]
    pub enum AssemblyFunctionDefinition {
        //               name,   instructions
        AssemblyFunction(String, Vec<Instructions>)
    }

    #[derive(Debug)]
    pub enum Instructions {
        //  src,     dst
        Mov(Operand, Operand),
        Ret
    }

    #[derive(Debug)]
    pub enum Operand {
        Imm(i32),
        Register
    }
}

use assembly_ast::AssemblyProgram as AssemblyProgram;
use assembly_ast::AssemblyFunctionDefinition as AssemblyFunctionDefinition;
use assembly_ast::Instructions as Instructions;
use assembly_ast::Operand as Operand;

pub fn assembly(ast: &Program) -> AssemblyProgram {
    let binary_ast = assembly_parse_program(&ast);
    binary_ast
}

fn assembly_parse_program(ast: &Program) -> AssemblyProgram {
    let return_val = match ast {
        Program::Program(inner) =>  assembly_parse_function(&inner),
    };
    AssemblyProgram::Program(return_val)
}

fn assembly_parse_function(ast: &FunctionDefinition) -> AssemblyFunctionDefinition {
    let (name, instructions) = match ast {
        FunctionDefinition::Function(name, body) => (name.to_string(), assembly_parse_instructions(&body)),
    };
    AssemblyFunctionDefinition::AssemblyFunction(name, instructions)
}

fn assembly_parse_instructions(ast: &Statement) -> Vec<Instructions> {
    let return_val: Vec<Instructions> = match ast {
        Statement::Return(exp) => {
            let src = assembly_parse_exp(&exp);
            let dst = Operand::Register;
            vec![Instructions::Mov(src, dst), Instructions::Ret]
        }
    };
    return_val
}

fn assembly_parse_exp(ast: &Exp) -> Operand {
    let return_val = match ast {
        Exp::Constant(num) => Operand::Imm(*num),
        Exp::Unary(operator, exp) => panic!("NOT IMPLEMENTED YET")
    };
    return_val
}