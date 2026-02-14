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
                data += "\t# returning ";
                data += &src;
                data += "\n\tmovl\t";
                data += &src;
                data += ", ";
                data += &dst;
                data += "\n";
            },
            Instructions::Ret => data += "\tret\n",
        }
    }

    data
}

fn emission_operand(binary_ast: &Operand) -> String {
    let data = match binary_ast {
        Operand::Imm(num) => {
            "$".to_string() + num.to_string().as_str()
        },
        Operand::Register => "%eax".to_string(),
    };

    data
}