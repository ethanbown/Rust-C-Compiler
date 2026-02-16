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
            str += "\tmovq\t%rsp, %rbp\n";
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
                let src = emission_operand(&src);
                let dst = emission_operand(&dst);
                //data += "\n\n\t# returning ";
                //data += &src;
                data += "\tmovl\t";
                data += &src;
                data += ", ";
                data += &dst;
                data += "\n";
            },
            Instructions::Ret => {
                data += "\tmovq\t%rbp, %rsp\n";
                data += "\tpopq\t%rbp\n";
                data += "\tret\n";
            },
            Instructions::Unary(unary_operator, operand) => {
                emission_unary_operators(unary_operator, &mut data);
                let operand = emission_operand(operand);
                data += &operand;
                data += "\n";
            },
            Instructions::AllocateStack(int) => {
                data += "\tsubq\t$";
                data += int.to_string().as_str();
                data += ", %rsp\n";
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

fn emission_operand(binary_ast: &Operand) -> String {
    let data = match binary_ast {
        Operand::Imm(num) => {
            "$".to_string() + num.to_string().as_str()
        },
        Operand::Reg(register) => {
            emission_registers(register)
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
        Reg::AX => "%eax".to_string(),
        Reg::R10 => "%r10d".to_string()
    }
}