use crate::assembly::assembly_ast::AssemblyBinaryOperator;
use crate::assembly::assembly_ast::ConditionCode;
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
        Binary(AssemblyBinaryOperator, Operand, Operand),
        Cmp(Operand, Operand),
        Idiv(Operand),
        Cdq,
        Jmp(String),
        JmpCC(ConditionCode, String),
        SetCC(ConditionCode, Operand),
        Label(String),
        AllocateStack(i32),
        Ret
    }

    #[derive(Debug, Clone)]
    pub enum AssemblyUnaryOperator {
        Neg,
        Not
    }

    #[derive(Debug, Clone)]
    pub enum AssemblyBinaryOperator {
        Add,
        Sub,
        Mult,
        BitwiseAND,
        BitwiseOR,
        BitwiseXOR,
        LeftShift,
        RightShift
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
        DX,
        CL,
        R10,
        R11
    }

    #[derive(Debug, Clone)]
    pub enum ConditionCode {
        E,
        NE,
        G,
        GE,
        L,
        LE
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

    //dbg!(&binary_ast);

    let stack_offset = replace_pseudo_operands(&mut binary_ast);

    //dbg!(&binary_ast);

    fixing_instructions(&mut binary_ast, stack_offset);

    //dbg!(&binary_ast);

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
                if is_logical_not(unary_operator) {
                    assembly_relational_instructions(ConditionCode::E, &mut return_val, src, Operand::Imm(0), dst);
                } else {
                    let unary_operator = assembly_parse_unary_operator(unary_operator);
                    let dst_copy = dst.clone();
                    return_val.push(Instructions::Mov(src, dst));
                    return_val.push(Instructions::Unary(unary_operator, dst_copy));
                }
            }
            IRInstructions::Binary(binary_operator,src1 ,src2 ,dst ) => {
                let src1 = assembly_parse_val(src1);
                let src2 = assembly_parse_val(src2);
                let dst = assembly_parse_val(dst);
                match binary_operator {
                    IRBinaryOperator::Divide => {
                        return_val.push(Instructions::Mov(src1, Operand::Reg(Reg::AX)));
                        return_val.push(Instructions::Cdq);
                        return_val.push(Instructions::Idiv(src2));
                        return_val.push(Instructions::Mov(Operand::Reg(Reg::AX), dst));
                    },
                    IRBinaryOperator::Remainder => {
                        return_val.push(Instructions::Mov(src1, Operand::Reg(Reg::AX)));
                        return_val.push(Instructions::Cdq);
                        return_val.push(Instructions::Idiv(src2));
                        return_val.push(Instructions::Mov(Operand::Reg(Reg::DX), dst));
                    },
                    IRBinaryOperator::GreaterThan    => assembly_relational_instructions(ConditionCode::G, &mut return_val, src1, src2, dst),
                    IRBinaryOperator::GreaterOrEqual => assembly_relational_instructions(ConditionCode::GE, &mut return_val, src1, src2, dst),
                    IRBinaryOperator::LessThan       => assembly_relational_instructions(ConditionCode::L, &mut return_val, src1, src2, dst),
                    IRBinaryOperator::LessOrEqual    => assembly_relational_instructions(ConditionCode::LE, &mut return_val, src1, src2, dst),
                    IRBinaryOperator::EqualTo        => assembly_relational_instructions(ConditionCode::E, &mut return_val, src1, src2, dst),
                    IRBinaryOperator::NotEqualTo     => assembly_relational_instructions(ConditionCode::NE, &mut return_val, src1, src2, dst),
                    _ => {
                        let binary_operator = assembly_parse_binary_operator(binary_operator);
                        let dst_copy = dst.clone();
                        return_val.push(Instructions::Mov(src1, dst));
                        return_val.push(Instructions::Binary(binary_operator, src2, dst_copy));
                    }
                }
            }
            IRInstructions::Copy(src, dst) => {
                let src = assembly_parse_val(src);
                let dst = assembly_parse_val(dst);
                return_val.push(Instructions::Mov(src, dst));
            },
            IRInstructions::Jump(identifier) => return_val.push(Instructions::Jmp(identifier.clone())),
            IRInstructions::JumpIfZero(val, target) => {
                let val = assembly_parse_val(val);
                return_val.push(Instructions::Cmp(Operand::Imm(0), val));
                return_val.push(Instructions::JmpCC(ConditionCode::E, target.clone()));
            },
            IRInstructions::JumpIfNotZero(val, target) => {
                let val = assembly_parse_val(val);
                return_val.push(Instructions::Cmp(Operand::Imm(0), val));
                return_val.push(Instructions::JmpCC(ConditionCode::NE, target.clone()));
            },
            IRInstructions::Label(identifier) => return_val.push(Instructions::Label(identifier.clone()))
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
    match tacky_ast {
        IRUnaryOperator::Complement => AssemblyUnaryOperator::Not,
        IRUnaryOperator::Negate => AssemblyUnaryOperator::Neg,
        IRUnaryOperator::LogicalNot => panic!("should not parse in assembly a '!' token")
    }
}

fn is_logical_not(tacky_ast: &IRUnaryOperator) -> bool {
    match tacky_ast {
        IRUnaryOperator::LogicalNot => true,
        _ => false
    }
}

fn assembly_parse_binary_operator(tacky_ast: &IRBinaryOperator) -> AssemblyBinaryOperator {
    match tacky_ast {
        IRBinaryOperator::Add           => AssemblyBinaryOperator::Add,
        IRBinaryOperator::Subtract      => AssemblyBinaryOperator::Sub,
        IRBinaryOperator::Multiply      => AssemblyBinaryOperator::Mult,
        IRBinaryOperator::BitwiseAND    => AssemblyBinaryOperator::BitwiseAND,
        IRBinaryOperator::BitwiseXOR    => AssemblyBinaryOperator::BitwiseXOR,
        IRBinaryOperator::BitwiseOR     => AssemblyBinaryOperator::BitwiseOR,
        IRBinaryOperator::LeftShift     => AssemblyBinaryOperator::LeftShift,
        IRBinaryOperator::RightShift    => AssemblyBinaryOperator::RightShift,
        _ => panic!("Invalid binary operator")
    }
}

fn assembly_relational_instructions(cond: ConditionCode, return_val: &mut Vec<Instructions>, src1: Operand, src2: Operand, dst: Operand) {
    return_val.push(Instructions::Cmp(src2.clone(), src1.clone()));
    return_val.push(Instructions::Mov(Operand::Imm(0), dst.clone()));
    return_val.push(Instructions::SetCC(cond, dst.clone()));
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
            Instructions::Mov(src,dst ) => {
                pesudo_parse_operand(src, identifiers_to_offsets, stack_offset);
                pesudo_parse_operand(dst, identifiers_to_offsets, stack_offset);
            },
            Instructions::Unary(_, operand) => pesudo_parse_operand(operand, identifiers_to_offsets, stack_offset),
            Instructions::Binary(_, op1, op2 ) => {
                pesudo_parse_operand(op1, identifiers_to_offsets, stack_offset);
                pesudo_parse_operand(op2, identifiers_to_offsets, stack_offset);
            },
            Instructions::Cmp(op1, op2) => {
                pesudo_parse_operand(op1, identifiers_to_offsets, stack_offset);
                pesudo_parse_operand(op2, identifiers_to_offsets, stack_offset);
            },
            Instructions::Idiv(operand) => pesudo_parse_operand(operand, identifiers_to_offsets, stack_offset),
            Instructions::Cdq => (),
            Instructions::Jmp(_) => (),
            Instructions::JmpCC(_, _) => (),
            Instructions::SetCC(_, op) => pesudo_parse_operand(op, identifiers_to_offsets, stack_offset),
            Instructions::Label(_) => (),
            Instructions::AllocateStack(_) => (),
            Instructions::Ret => (),
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
                    new_instructions.push(Instructions::Mov(Operand::Reg(Reg::R10), dst.clone()));
                } else {
                    new_instructions.push(instruction.clone());
                }
            },
            Instructions::Idiv(operand) => {
                // Need to replace if operand is constant
                // Ex: idivl $3 
                if is_constant(operand) {
                    new_instructions.push(Instructions::Mov(operand.clone(), Operand::Reg(Reg::R10)));
                    new_instructions.push(Instructions::Idiv(Operand::Reg(Reg::R10)));
                } else {
                    new_instructions.push(instruction.clone());
                }
            },
            Instructions::Binary(binary_operand, op1,op2) => {
                match binary_operand {
                    AssemblyBinaryOperator::Add 
                    | AssemblyBinaryOperator::Sub 
                    | AssemblyBinaryOperator::BitwiseAND 
                    | AssemblyBinaryOperator::BitwiseXOR 
                    | AssemblyBinaryOperator::BitwiseOR => {
                        if are_both_memory_addresses(op1, op2) {
                            new_instructions.push(Instructions::Mov(op1.clone(), Operand::Reg(Reg::R10)));
                            new_instructions.push(Instructions::Binary(binary_operand.clone(), Operand::Reg(Reg::R10), op2.clone()));
                        } else {
                            new_instructions.push(instruction.clone());
                        }
                    },
                    AssemblyBinaryOperator::Mult => {
                        if is_destination_memory_address(op2) {
                            new_instructions.push(Instructions::Mov(op2.clone(), Operand::Reg(Reg::R11)));
                            new_instructions.push(Instructions::Binary(binary_operand.clone(), op1.clone(), Operand::Reg(Reg::R11)));
                            new_instructions.push(Instructions::Mov(Operand::Reg(Reg::R11), op2.clone()));
                        } else {
                            new_instructions.push(instruction.clone());
                        }
                    },
                     AssemblyBinaryOperator::LeftShift 
                    | AssemblyBinaryOperator::RightShift => {
                        if !is_constant(op1) {
                            new_instructions.push(Instructions::Mov(op1.clone(), Operand::Reg(Reg::CL)));
                            new_instructions.push(Instructions::Binary(binary_operand.clone(), Operand::Reg(Reg::CL), op2.clone()));
                        } else {
                            new_instructions.push(instruction.clone());
                        }
                    }
                }
            },
            Instructions::Cmp(src, dst) => {
                if are_both_memory_addresses(src, dst) {
                    new_instructions.push(Instructions::Mov(src.clone(), Operand::Reg(Reg::R10)));
                    new_instructions.push(Instructions::Cmp(Operand::Reg(Reg::R10), dst.clone()));
                } else if is_constant(dst) {
                    new_instructions.push(Instructions::Mov(dst.clone(), Operand::Reg(Reg::R11)));
                    new_instructions.push(Instructions::Cmp(src.clone(), Operand::Reg(Reg::R11)));
                } else {
                    new_instructions.push(instruction.clone());
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

fn is_destination_memory_address(dst: &Operand) -> bool {
    match dst {
        Operand::Stack(_) => true,
        _ => false
    }
}

fn is_constant(operand: &Operand) -> bool {
    match operand {
        Operand::Imm(_) => true,
        _ => false
    }
}