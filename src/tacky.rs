use crate::parser::parser_ast::*;

pub mod tacky_ast {
    #[derive(Debug)]
    pub enum IRProgram {
        IRProgram(IRFunctionDefinition)
    }

    #[derive(Debug)]
    pub enum IRFunctionDefinition {
        //          name,   instructions
        IRFunction(String, Vec<IRInstructions>)
    }

    #[derive(Debug)]
    pub enum IRInstructions {
        Return(Val),
        //     unary_operator, src, dst
        Unary(IRUnaryOperator, Val, Val)
    }

    #[derive(Debug, Clone)]
    pub enum Val {
        Constant(i32),
        //  identifier
        Var(String)
    }

    #[derive(Debug)]
    pub enum IRUnaryOperator {
        Complement,
        Negate
    }
}

use tacky_ast::IRProgram as IRProgram;
use tacky_ast::IRFunctionDefinition as IRFunctionDefinition;
use tacky_ast::IRInstructions as IRInstructions;
use tacky_ast::IRUnaryOperator as IRUnaryOperator;
use tacky_ast::Val as Val;

pub fn tacky(ast: &Program) -> IRProgram {
    let tacky_ast = ir_parse_program(&ast);

    // dbg!(&tacky_ast);

    tacky_ast
}

fn ir_parse_program(ast: &Program) -> IRProgram {
    let return_val = match ast {
        Program::Program(inner) => ir_parse_function(inner)
    };
    IRProgram::IRProgram(return_val)
}

fn ir_parse_function(ast: &FunctionDefinition) -> IRFunctionDefinition {
    let (name, instructions) = match ast {
        FunctionDefinition::Function(name, body) => {
            let mut temporary_count: i32 = 0;
            (name.to_string(), ir_parse_instructions(&body, &mut temporary_count, name.to_string()))
        }
    };
    IRFunctionDefinition::IRFunction(name, instructions)
}

fn ir_parse_instructions(ast: &Statement, temporary_count: &mut i32, name: String) -> Vec<IRInstructions> {
    let return_val: Vec<IRInstructions> = match ast {
        Statement::Return(exp) => {
            let mut instructions: Vec<IRInstructions> = Vec::new();
            let return_value = emit_tacky(exp, &mut instructions, temporary_count, &name);
            instructions.push(IRInstructions::Return(return_value));
            instructions
        }
    };
    return_val
}

fn emit_tacky(ast: &Exp, instructions: &mut Vec<IRInstructions>, temporary_count: &mut i32, name: &String) -> Val{
    match ast {
        Exp::Constant(c) => Val::Constant(*c),
        Exp::Unary(op, inner) => {
            let src = emit_tacky(inner, instructions, &mut *temporary_count, name);
            let dst_name = make_temporary(name, temporary_count);
            let dst = Val::Var(dst_name);
            let tacky_op = ir_parse_unary_operation(op);
            instructions.push(IRInstructions::Unary(tacky_op, src, dst.clone()));
            dst
        }
    }
}

fn ir_parse_unary_operation(ast: &UnaryOperator) -> IRUnaryOperator {
    let return_val = match ast {
        UnaryOperator::Complement => IRUnaryOperator::Complement,
        UnaryOperator::Negate => IRUnaryOperator::Negate
    };
    return_val
}

fn make_temporary(name: &String, temporary_count: &mut i32) -> String {
    let mut temporary = String::new();
    temporary += name.clone().as_str();
    temporary += ".";
    temporary += temporary_count.to_string().as_str();
    *temporary_count += 1;

    // dbg!(&temporary_count);

    temporary
}