
use crate::parser::parser_ast::*;
use crate::semantic_analysis::UniqueCounter;
use crate::semantic_analysis::UniqueType;
use crate::semantic_analysis::make_unique;

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
        Unary(IRUnaryOperator, Val, Val),
        //     binary_operator, src1, src2, dst
        Binary(IRBinaryOperator, Val, Val, Val),
        //  src,  dst
        Copy(Val, Val),
        //   target
        Jump(String),
        //      condition, target
        JumpIfZero(Val, String),
        //       condition, target
        JumpIfNotZero(Val, String),
        //   identifier
        Label(String)
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
        Negate,
        LogicalNot
    }

    #[derive(Debug)]
    pub enum IRBinaryOperator {
        Add,
        Subtract,
        Multiply,
        Divide,
        Remainder,
        BitwiseAND,
        BitwiseOR,
        BitwiseXOR,
        LeftShift,
        RightShift,
        EqualTo,
        NotEqualTo,
        LessThan,
        LessOrEqual,
        GreaterThan,
        GreaterOrEqual,
    }
}

use tacky_ast::IRProgram as IRProgram;
use tacky_ast::IRFunctionDefinition as IRFunctionDefinition;
use tacky_ast::IRInstructions as IRInstructions;
use tacky_ast::IRUnaryOperator as IRUnaryOperator;
use tacky_ast::IRBinaryOperator as IRBinaryOperator;
use tacky_ast::Val as Val;

pub fn tacky(ast: &Program, counter: &mut UniqueCounter) -> IRProgram {
    let tacky_ast = ir_parse_program(&ast, counter);

    dbg!(&tacky_ast);

    tacky_ast
}

fn ir_parse_program(ast: &Program, counter: &mut UniqueCounter) -> IRProgram {
    let return_val = match ast {
        Program::Program(inner) => ir_parse_function(inner, counter)
    };
    IRProgram::IRProgram(return_val)
}

fn ir_parse_function(ast: &FunctionDefinition, counter: &mut UniqueCounter) -> IRFunctionDefinition {
    let (name, instructions) = match ast {
        FunctionDefinition::Function(name, body) => (name.to_string(), ir_parse_block_items(&body, counter, name.to_string()))
    };
    IRFunctionDefinition::IRFunction(name, instructions)
}

fn ir_parse_block_items(body: &Vec<BlockItem>, counter: &mut UniqueCounter, name: String) -> Vec<IRInstructions> {
    let mut return_val: Vec<IRInstructions> = Vec::new();
    for item in body {
        match item {
            BlockItem::D(decl) => ir_parse_declaration(decl, &mut return_val, counter),
            BlockItem::S(stat)   => {
                let mut inst = ir_parse_instructions(stat, counter, &name);
                return_val.append(&mut inst);
            } 
        }
    }
    return_val.push(IRInstructions::Return(Val::Constant(0)));
    return_val
}

fn ir_parse_declaration(decl: &Declaration, instructions: &mut Vec<IRInstructions>, counter: &mut UniqueCounter) {
    match decl {
        Declaration::Declaration(name, init) => {
            if init.is_some() {
                let copy = init.clone();
                let copy2 = init.clone();

                dbg!(&copy2.unwrap());

                let return_val = emit_tacky(&copy.unwrap(), instructions, counter, name);
                instructions.push(IRInstructions::Copy(return_val, Val::Var(name.clone())));
            } else {
                ()
            }
        }
    }
}

fn ir_parse_instructions(ast: &Statement, counter: &mut UniqueCounter, name: &String) -> Vec<IRInstructions> {
    let return_val: Vec<IRInstructions> = match ast {
        Statement::Return(exp) => {
            let mut instructions: Vec<IRInstructions> = Vec::new();
            let return_value = emit_tacky(exp, &mut instructions, counter, &name);
            instructions.push(IRInstructions::Return(return_value));
            instructions
        },
        Statement::Expression(exp) => {
            let mut instructions: Vec<IRInstructions> = Vec::new();
            emit_tacky(exp, &mut instructions, counter, &name);
            instructions
        },
        Statement::Null => Vec::new()
    };
    return_val
}

fn emit_tacky(ast: &Exp, instructions: &mut Vec<IRInstructions>, counter: &mut UniqueCounter, name: &String) -> Val {
    match ast {
        Exp::Constant(c) => Val::Constant(*c),
        Exp::Unary(op, inner) => {
            let src = emit_tacky(inner, instructions, counter, name);
            let dst_name = make_unique(name, UniqueType::Temporary, counter);
            let dst = Val::Var(dst_name);
            let tacky_op = ir_parse_unary_op(op);
            instructions.push(IRInstructions::Unary(tacky_op, src, dst.clone()));
            dst
        },
        Exp::Binary(op, e1,e2) => {
            
            let dst_name = make_unique(name, UniqueType::Temporary, counter);
            let dst = Val::Var(dst_name);
            if is_logical_and(op) {
                let and_label = String::from("and_label");
                let end = String::from("end");
                let false_label = make_unique(&and_label, UniqueType::Label, counter);
                let end_label = make_unique(&end, UniqueType::Label, counter);
                let v1 = emit_tacky(e1, instructions, counter, name);
                instructions.push(IRInstructions::JumpIfZero(v1, false_label.clone()));
                let v2 = emit_tacky(e2, instructions, counter, name);
                instructions.push(IRInstructions::JumpIfZero(v2, false_label.clone()));
                instructions.push(IRInstructions::Copy(Val::Constant(1), dst.clone()));
                instructions.push(IRInstructions::Jump(end_label.clone()));
                instructions.push(IRInstructions::Label(false_label));
                instructions.push(IRInstructions::Copy(Val::Constant(0), dst.clone()));
                instructions.push(IRInstructions::Label(end_label));
                dst
            } else if is_logical_or(op) { 
                let or_label = String::from("or_label");
                let end = String::from("end");
                let true_label = make_unique(&or_label, UniqueType::Label, counter);
                let end_label = make_unique(&end, UniqueType::Label, counter);
                let v1 = emit_tacky(e1, instructions, counter, name);
                instructions.push(IRInstructions::JumpIfNotZero(v1, true_label.clone()));
                let v2 = emit_tacky(e2, instructions, counter, name);
                instructions.push(IRInstructions::JumpIfNotZero(v2, true_label.clone()));
                instructions.push(IRInstructions::Copy(Val::Constant(0), dst.clone()));
                instructions.push(IRInstructions::Jump(end_label.clone()));
                instructions.push(IRInstructions::Label(true_label));
                instructions.push(IRInstructions::Copy(Val::Constant(1), dst.clone()));
                instructions.push(IRInstructions::Label(end_label));
                dst
            } else {
                let v1 = emit_tacky(e1, instructions, counter, name);
                let v2 = emit_tacky(e2, instructions, counter, name);
                let tacky_op = ir_parse_binary_op(op);
                instructions.push(IRInstructions::Binary(tacky_op, v1, v2, dst.clone()));
                dst
            }
        },
        Exp::Var(var) => Val::Var(var.clone()),
        Exp::Assignment(lhs, rhs) => {
            let result = emit_tacky(rhs, instructions, counter, name);
            let var = emit_tacky(lhs, instructions, counter, name);
            instructions.push(IRInstructions::Copy(result, var.clone()));
            var
        },

    }
}

fn ir_parse_unary_op(ast: &UnaryOperator) -> IRUnaryOperator {
    match ast {
        UnaryOperator::Complement  => IRUnaryOperator::Complement,
        UnaryOperator::Negate      => IRUnaryOperator::Negate,
        UnaryOperator::LogicalNot  => IRUnaryOperator::LogicalNot
    }
}

fn ir_parse_binary_op(op: &BinaryOperator) -> IRBinaryOperator {
    match op {
        BinaryOperator::Add               => IRBinaryOperator::Add,
        BinaryOperator::Subtract          => IRBinaryOperator::Subtract,
        BinaryOperator::Multiply          => IRBinaryOperator::Multiply,
        BinaryOperator::Divide            => IRBinaryOperator::Divide,
        BinaryOperator::Remainder         => IRBinaryOperator::Remainder,
        BinaryOperator::BitwiseAND        => IRBinaryOperator::BitwiseAND,
        BinaryOperator::BitwiseXOR        => IRBinaryOperator::BitwiseXOR,
        BinaryOperator::BitwiseOR         => IRBinaryOperator::BitwiseOR,
        BinaryOperator::LeftShift         => IRBinaryOperator::LeftShift,
        BinaryOperator::RightShift        => IRBinaryOperator::RightShift,
        BinaryOperator::EqualTo           => IRBinaryOperator::EqualTo,
        BinaryOperator::NotEqualTo        => IRBinaryOperator::NotEqualTo,
        BinaryOperator::GreaterThan       => IRBinaryOperator::GreaterThan,
        BinaryOperator::LessThan          => IRBinaryOperator::LessThan,
        BinaryOperator::GreaterOrEqual    => IRBinaryOperator::GreaterOrEqual,
        BinaryOperator::LessOrEqual       => IRBinaryOperator::LessOrEqual,
        BinaryOperator::LogicalAND        => panic!("should not be passed a LogicalAND"),
        BinaryOperator::LogicalOR         => panic!("should not be passed a LogicalOR")
    }
}

fn is_logical_and(op: &BinaryOperator) -> bool {
    match op {
        BinaryOperator::LogicalAND => true,
        _ => false
    }
}

fn is_logical_or(op: &BinaryOperator) -> bool {
    match op {
        BinaryOperator::LogicalOR => true,
        _ => false
    }
} 