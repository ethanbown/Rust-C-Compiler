use std::{collections::HashMap};

use crate::{assembly::assembly_ast::*, semantic_analysis::IdentifierAttr};
use super::semantic_analysis::TypeData as TypeData;

enum RegType {
    // Double Word
    DW,
    // Word
    W,
    // Byte
    B,
}

pub fn code_emission(binary_ast: &AssemblyProgram, symbols: &HashMap<String, TypeData>) -> String {
    emission_program(&binary_ast, symbols)
}

fn emission_program(binary_ast: &AssemblyProgram, symbols: &HashMap<String, TypeData>) -> String {
    let mut data = String::new();
    
    match binary_ast {
        AssemblyProgram::Program(inner) => {
            for top_level in inner {
                data += &emission_top_level(top_level, symbols);
                data += "\n";
            }
        }
    };

    data + "\n.section .note.GNU-stack,\"\",@progbits\n"
}

fn emission_top_level(binary_ast: &AssemblyTopLevel, symbols: &HashMap<String, TypeData>) -> String {
    let data = match binary_ast {
        AssemblyTopLevel::AssemblyFunction(name, global, inst) => {
            let mut str = String::from("\t");
            if *global {
                str += ".globl "; 
                str += name;
                str += "\n";
            }
            str += "\t.text\n";
            str += name;
            str += ":\n";
            str += "\tpushq\t%rbp\n";
            str += "\tmovq\t%rsp, %rbp\n\n";
            let inst = emission_instructions(&inst, symbols);
            str += &inst;
            str
        },
        AssemblyTopLevel::AssemblyStaticVariable(identifier, global, init) => {
            let mut str = String::from("");
            if *global {
                str += "\t.globl ";
                str += identifier;
                str += "\n";
                if *init != 0 {
                    str += "\t.data\n\t.align 4\n";
                    str += identifier;
                    str += ":\n";
                    str += "\t.long ";
                    str += init.to_string().as_str();
                    str += "\n";
                } else {
                    str += "\t.bss\n\t.align 4\n";
                    str += identifier;
                    str += ":\n";
                    str += "\t.zero 4\n";
                }
            } else {
                if *init != 0 {
                    str += "\t.data\n\t.align 4\n";
                    str += identifier;
                    str += ":\n";
                    str += "\t.long ";
                    str += init.to_string().as_str();
                    str += "\n";
                } else {
                    str += "\t.bss\n\t.align 4\n";
                    str += identifier;
                    str += ":\n";
                    str += "\t.zero 4\n";
                }
            }

            str
        }
    };
    data
}

fn emission_instructions(binary_ast: &Vec<Instructions>, symbols: &HashMap<String, TypeData>) -> String {
    let mut data = String::new();
    
    for inst in binary_ast {
        match inst {
            Instructions::Mov(src, dst) => {
                let src = emission_operand(&src, &RegType::W);
                let dst = emission_operand(&dst, &RegType::W);
                data += "\tmovl\t";
                data += &src;
                data += ", ";
                data += &dst;
                data += "\n";
            },
            Instructions::Ret => {
                data += "\n\tmovq\t%rbp, %rsp\n";
                data += "\tpopq\t%rbp\n";
                data += "\tret\n";
            },
            Instructions::Unary(unary_operator, operand) => {
                emission_unary_operators(unary_operator, &mut data);
                let operand = emission_operand(operand, &RegType::W);
                data += &operand;
                data += "\n";
            },
            Instructions::AllocateStack(int) => {
                data += "\tsubq\t$";
                data += int.to_string().as_str();
                data += ", %rsp\n";
            },
            Instructions::DeallocateStack(int) => {
                data += "\taddq\t$";
                data += int.to_string().as_str();
                data += ", %rsp\n";
            },
            Instructions::Push(op) => {
                let op = emission_operand(op, &RegType::DW);
                data += "\tpushq\t";
                data += op.to_string().as_str();
                data += "\n";
            }
            Instructions::Binary(binary_operator, src, dst) => {
                emission_binary_operators(binary_operator, &mut data);
                let reg_type = match binary_operator {
                    AssemblyBinaryOperator::LeftShift
                    | AssemblyBinaryOperator::RightShift => RegType::B,
                    _ => RegType::W
                };
                let src = emission_operand(src, &reg_type);
                let dst = emission_operand(dst, &RegType::W);
                data += &src;
                data += ", ";
                data += &dst;
                data += "\n";
            },
            Instructions::Idiv(operand) => {
                let operand = emission_operand(operand, &RegType::W);
                data += "\tidivl\t";
                data += &operand;
                data += "\n";
            },
            Instructions::Cdq => data += "\tcdq\t\n",
            Instructions::Cmp(op1, op2) => {
                let op1 = emission_operand(op1, &RegType::W);
                let op2 = emission_operand(op2, &RegType::W);
                data += "\tcmpl\t";
                data += &op1;
                data += ", ";
                data += &op2;
                data += "\n";
            }
            Instructions::Jmp(label) => {
                data += "\tjmp\t.L";
                data += &label;
                data += "\n";
            },
            Instructions::JmpCC(cond_code, label) => {
                let cond_code = emission_condition_code(cond_code);
                data += "\tj";
                data += &cond_code;
                data += "\t.L";
                data += &label;
                data += "\n";
            }
            Instructions::SetCC(cond_code, operand) => {
                let operand = emission_operand(operand, &RegType::B);
                let cond_code = emission_condition_code(cond_code);
                data += "\tset";
                data += &cond_code;
                data += "\t";
                data += &operand;
                data += "\n";
            },
            Instructions::Label(label) => {
                data += ".L";
                data += &label;
                data += ":\n";
            },
            Instructions::Call(label) => {
                data += "\tcall\t";
                data += &label;
                
                let vardata = match symbols.get(label) {
                    Some(data) => data,
                    None                 => panic!("Issue calling {label}")
                };

                let defined = match vardata.attrs {
                    IdentifierAttr::FunAttr(defined, _, _) => defined,
                    _                                            => panic!("Calling non-function")
                };

                if !defined {
                    data += "@PLT";
                }

                data += "\n";
            },

        }
    }

    data
}

fn emission_unary_operators(unary_operator: &AssemblyUnaryOperator, data: &mut String) {
    match unary_operator {
        AssemblyUnaryOperator::Neg => *data += "\tnegl\t",
        AssemblyUnaryOperator::Not => *data += "\tnotl\t"
    }
}

fn emission_binary_operators(binary_operator: &AssemblyBinaryOperator, data: &mut String) {
    match binary_operator {
        AssemblyBinaryOperator::Add             => *data += "\taddl\t",
        AssemblyBinaryOperator::Mult            => *data += "\timull\t",
        AssemblyBinaryOperator::Sub             => *data += "\tsubl\t",
        AssemblyBinaryOperator::BitwiseAND      => *data += "\tandl\t",
        AssemblyBinaryOperator::BitwiseXOR      => *data += "\txorl\t",
        AssemblyBinaryOperator::BitwiseOR       => *data += "\torl\t\t",
        AssemblyBinaryOperator::LeftShift       => *data += "\tsall\t",
        AssemblyBinaryOperator::RightShift      => *data += "\tsarl\t"
    }
}

fn emission_operand(binary_ast: &Operand, reg_type: &RegType) -> String {
    let data = match binary_ast {
        Operand::Imm(num) => {
            "$".to_string() + num.to_string().as_str()
        },
        Operand::Reg(register) => match reg_type {
                RegType::DW => emission_doubleword_registers(register),
                RegType::W  => emission_word_registers(register),
                RegType::B  => emission_byte_registers(register),
            },
        Operand::Stack(int) => {
            int.to_string() + "(%rbp)"
        },
        Operand::Data(name)   => {
            name.clone() + "(%rip)".to_string().as_str()
        },
        Operand::Pseudo(_) => panic!("emission_operand shouldn't have pseudo operands"),
    };

    data
}

fn emission_doubleword_registers(register: &Reg) -> String {
    match register {
        Reg::AX   => "%rax".to_string(),
        Reg::CX   => "%rcx".to_string(),
        Reg::DX   => "%rdx".to_string(),
        Reg::DI   => "%rdi".to_string(),
        Reg::SI   => "%rsi".to_string(),
        Reg::R8   => "%r8".to_string(),
        Reg::R9   => "%r9".to_string(),
        Reg::R10  => "%r10".to_string(),
        Reg::R11  => "%r11".to_string(),
    }
}

fn emission_word_registers(register: &Reg) -> String {
     match register {
        Reg::AX   => "%eax".to_string(),
        Reg::CX   => "%ecx".to_string(),
        Reg::DX   => "%edx".to_string(),
        Reg::DI   => "%edi".to_string(),
        Reg::SI   => "%esi".to_string(),
        Reg::R8   => "%r8d".to_string(),
        Reg::R9   => "%r9d".to_string(),
        Reg::R10  => "%r10d".to_string(),
        Reg::R11  => "%r11d".to_string(),
    }
}

fn emission_byte_registers(register: &Reg) -> String {
    match register {
        Reg::AX   => "%al".to_string(),
        Reg::CX   => "%cl".to_string(),
        Reg::DX   => "%dl".to_string(),
        Reg::DI   => "%dil".to_string(),
        Reg::SI   => "%sil".to_string(),
        Reg::R8   => "%r8b".to_string(),
        Reg::R9   => "%r9b".to_string(),
        Reg::R10  => "%r10b".to_string(),
        Reg::R11  => "%r11b".to_string(),
    }
}

fn emission_condition_code(cond_code: &ConditionCode) -> String {
    match cond_code {
        ConditionCode::E   => "e".to_string(),
        ConditionCode::NE  => "ne".to_string(),
        ConditionCode::G   => "g".to_string(),
        ConditionCode::GE  => "ge".to_string(),
        ConditionCode::L   => "l".to_string(),
        ConditionCode::LE  => "le".to_string(),
    }
}