use crate::tacky::tacky_ast::*;
use std::collections::HashMap;

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

    #[derive(Debug, Clone)]
    pub enum Instructions {
        //  src,     dst
        Mov(Operand, Operand),
        Unary(AssemblyUnaryOperator, Operand),
        AllocateStack(i32),
        Ret
    }

    #[derive(Debug, Clone)]
    pub enum AssemblyUnaryOperator {
        Neg,
        Not
    }

    #[derive(Debug, Clone)]
    pub enum Operand {
        Imm(i32),
        //     identifier
        Pseudo(String),
        Stack(i32),
        Reg(Reg)
    }

    #[derive(Debug, Clone)]
    pub enum Reg {
        AX,
        R10
    }
}

use assembly_ast::AssemblyProgram as AssemblyProgram;
use assembly_ast::AssemblyFunctionDefinition as AssemblyFunctionDefinition;
use assembly_ast::AssemblyUnaryOperator as AssemblyUnaryOperator;
use assembly_ast::Instructions as Instructions;
use assembly_ast::Operand as Operand;
use assembly_ast::Reg as Reg;

pub fn assembly(tacky_ast: &IRProgram) -> AssemblyProgram {
    let mut binary_ast = assembly_parse_program(&tacky_ast);

    // dbg!(&binary_ast);

    let stack_offset = replace_pseudo_operands(&mut binary_ast);

    // dbg!(&binary_ast);

    fixing_instructions(&mut binary_ast, stack_offset);

    // dbg!(&binary_ast);

    binary_ast
}

fn assembly_parse_program(tacky_ast: &IRProgram) -> AssemblyProgram {
    let return_val = match tacky_ast {
        IRProgram::IRProgram(inner) =>  assembly_parse_function(&inner),
    };
    AssemblyProgram::Program(return_val)
}

fn assembly_parse_function(tacky_ast: &IRFunctionDefinition) -> AssemblyFunctionDefinition {
    let (name, instructions) = match tacky_ast {
        IRFunctionDefinition::IRFunction(name, body) => (name.to_string(), assembly_parse_instructions(&body)),
    };
    AssemblyFunctionDefinition::AssemblyFunction(name, instructions)
}

fn assembly_parse_instructions(tacky_ast: &Vec<IRInstructions>) -> Vec<Instructions> {
    let mut return_val: Vec<Instructions> = Vec::new();
    for instructions in tacky_ast {
        match instructions {
            IRInstructions::Return(val) => {
                let val = assembly_parse_val(val);
                return_val.push(Instructions::Mov(val, Operand::Reg(Reg::AX)));
                return_val.push(Instructions::Ret);
            }
            IRInstructions::Unary(unary_operator, src, dst) => {
                let src = assembly_parse_val(src);
                let dst = assembly_parse_val(dst);
                let unary_operator = assembly_parse_unary_operator(unary_operator);
                let dst_copy = dst.clone();
                return_val.push(Instructions::Mov(src, dst));
                return_val.push(Instructions::Unary(unary_operator, dst_copy));
            }
        }
    }

    return_val
}

fn assembly_parse_val(tacky_ast: &Val) -> Operand {
    let return_val = match tacky_ast {
        Val::Constant(int) => Operand::Imm(*int),
        Val::Var(identifier) => Operand::Pseudo(identifier.to_string())
    };
    return_val
}

fn assembly_parse_unary_operator(tacky_ast: &IRUnaryOperator) -> AssemblyUnaryOperator {
    let return_value = match tacky_ast {
        IRUnaryOperator::Complement => AssemblyUnaryOperator::Not,
        IRUnaryOperator::Negate => AssemblyUnaryOperator::Neg
    };
    return_value
}

fn replace_pseudo_operands(binary_ast: &mut AssemblyProgram) -> i32{
    let mut identifiers_to_offsets: HashMap<String, i32> = HashMap::new();
    let mut stack_offset = 0;
    pseudo_parse_program(binary_ast, &mut identifiers_to_offsets, &mut stack_offset);
    stack_offset
}

fn pseudo_parse_program(binary_ast: &mut AssemblyProgram, identifiers_to_offsets: &mut HashMap<String, i32>, stack_offset: &mut i32) {
    match binary_ast {
        AssemblyProgram::Program(inner) => pseudo_parse_function(inner, identifiers_to_offsets, stack_offset)
    }
}

fn pseudo_parse_function(binary_ast: &mut AssemblyFunctionDefinition, identifiers_to_offsets: &mut HashMap<String, i32>, stack_offset: &mut i32) {
    match binary_ast {
        AssemblyFunctionDefinition::AssemblyFunction(_, body) => pseudo_parse_instructions(body, identifiers_to_offsets, stack_offset),
    }
}

fn pseudo_parse_instructions(binary_ast: &mut Vec<Instructions>, identifiers_to_offsets: &mut HashMap<String, i32>, stack_offset: &mut i32) {
    for instruction in &mut *binary_ast {
        match instruction {
            Instructions::Unary(_, operand) => pesudo_parse_operand(operand, identifiers_to_offsets, stack_offset),
            Instructions::Mov(src,dst ) => {
                pesudo_parse_operand(src, identifiers_to_offsets, stack_offset);
                pesudo_parse_operand(dst, identifiers_to_offsets, stack_offset);
            }
            _ => ()
        }
    }
}

fn pesudo_parse_operand(binary_ast: &mut Operand, identifiers_to_offsets: &mut HashMap<String, i32>, stack_offset: &mut i32) {
    match binary_ast {
        Operand::Pseudo(label) => {
            match identifiers_to_offsets.get(label) {
                Some(offset) => *binary_ast = Operand::Stack(*offset),
                None => {
                    *stack_offset -= 4;
                    identifiers_to_offsets.insert(label.to_string(), *stack_offset);
                    *binary_ast = Operand::Stack(*stack_offset)
                }
            }
        },
        _ => ()
    }
}

fn fixing_instructions(binary_ast: &mut AssemblyProgram, stack_offset: i32) {
    fixing_parse_program(binary_ast, stack_offset);
}

fn fixing_parse_program(binary_ast: &mut AssemblyProgram, stack_offset: i32){
    match binary_ast {
        AssemblyProgram::Program(inner) => fixing_parse_function(inner, stack_offset)
    }
}

fn fixing_parse_function(binary_ast: &mut AssemblyFunctionDefinition, stack_offset: i32) {
    match binary_ast {
        AssemblyFunctionDefinition::AssemblyFunction(_, body) => fixing_parse_instructions(body, stack_offset)
    }
}

fn fixing_parse_instructions(binary_ast: &mut Vec<Instructions>, stack_offset: i32) {
    let mut new_instructions: Vec<Instructions> = Vec::new();
    new_instructions.push(Instructions::AllocateStack(-stack_offset));
    for instruction in &mut *binary_ast {
        match instruction {
            Instructions::Mov(src, dst) => {
                if are_both_memory_addresses(src, dst) {
                    new_instructions.push(Instructions::Mov(src.clone(), Operand::Reg(Reg::R10)));
                    new_instructions.push(Instructions::Mov(Operand::Reg(Reg::R10), dst.clone()))
                } else {
                    new_instructions.push(instruction.clone())
                }
            },
            _ => new_instructions.push(instruction.clone())
        }
    }
    *binary_ast = new_instructions
}

fn are_both_memory_addresses(src: &Operand, dst: &Operand) -> bool {
    match src {
        Operand::Stack(_) => match dst {
                Operand::Stack(_) => true,
                _ => false         
            },
        _ => false
    }
}