use crate::assembly::assembly_ast::*;

pub fn code_emission(binary_ast: &AssemblyProgram) -> String {
    emission_program(&binary_ast)
}

fn emission_program(binary_ast: &AssemblyProgram) -> String {
    let data = match binary_ast {
        AssemblyProgram::Program(inner) => emission_function(inner),
    };
    data + "\n.section .note.GNU-stack,\"\",@progbits\n"
}

fn emission_function(binary_ast: &AssemblyFunctionDefinition) -> String {
    let data = match binary_ast {
        AssemblyFunctionDefinition::AssemblyFunction(name, inst) => {
            let mut str = String::from("\t.globl ") + name + "\n" + name + ":\n";
            str += "\tpushq\t%rbp\n";
            str += "\tmovq\t%rsp, %rbp\n\n";
            let inst = emission_instructions(&inst);
            str += &inst;
            str
        }
    };
    data
}

fn emission_instructions(binary_ast: &Vec<Instructions>) -> String {
    let mut data = String::new();
    
    for inst in binary_ast {
        match inst {
            Instructions::Mov(src, dst) => {
                let src = emission_operand(&src, false);
                let dst = emission_operand(&dst, false);
                //data += "\n\n\t# returning ";
                //data += &src;
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
                let operand = emission_operand(operand, false);
                data += &operand;
                data += "\n";
            },
            Instructions::AllocateStack(int) => {
                data += "\tsubq\t$";
                data += int.to_string().as_str();
                data += ", %rsp\n";
            },
            Instructions::Binary(binary_operator, src, dst) => {
                emission_binary_operators(binary_operator, &mut data);
                let src = emission_operand(src, false);
                let dst = emission_operand(dst, false);
                data += &src;
                data += ", ";
                data += &dst;
                data += "\n";
            },
            Instructions::Idiv(operand) => {
                let operand = emission_operand(operand, false);
                data += "\tidivl\t";
                data += &operand;
                data += "\n";
            },
            Instructions::Cdq => data += "\tcdq\t\n",
            Instructions::Cmp(op1, op2) => {
                let op1 = emission_operand(op1, false);
                let op2 = emission_operand(op2, false);
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
                let operand = emission_operand(operand, true);
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
            }
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

fn emission_operand(binary_ast: &Operand, is_setcc: bool) -> String {
    let data = match binary_ast {
        Operand::Imm(num) => {
            "$".to_string() + num.to_string().as_str()
        },
        Operand::Reg(register) => {
            if is_setcc {
                emission_lower_registers(register)
            }           
            else {
                emission_registers(register)
            }
        },
        Operand::Stack(int) => {
            int.to_string() + "(%rbp)"
        },
        Operand::Pseudo(_) => panic!("emission_operand shouldn't have pseudo operands")
    };

    data
}

fn emission_registers(register: &Reg) -> String {
    match register {
        Reg::AX   => "%eax".to_string(),
        Reg::DX   => "%edx".to_string(),
        Reg::R10  => "%r10d".to_string(),
        Reg::R11  => "%r11d".to_string(),
        Reg::CL   => "%cl".to_string()
    }
}

fn emission_lower_registers(register: &Reg) -> String {
    match register {
        Reg::AX   => "%al".to_string(),
        Reg::DX   => "%dl".to_string(),
        Reg::R10  => "%r10b".to_string(),
        Reg::R11  => "%r11b".to_string(),
        Reg::CL   => "%cl".to_string()
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