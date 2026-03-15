
use crate::parser::parser_ast::*;
use crate::semantic_analysis::UniqueCounter;
use crate::semantic_analysis::UniqueType;
use crate::semantic_analysis::make_unique;

pub mod tacky_ast {
    #[derive(Debug)]
    pub enum IRProgram {
        IRProgram(Vec<IRFunctionDefinition>)
    }

    #[derive(Debug)]
    pub enum IRFunctionDefinition {
        //          name,   params      instructions
        IRFunction(String, Vec<String>, Vec<IRInstructions>)
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
        Label(String),
        // fun_name,    args,   dst
        FunCall(String, Vec<Val>, Val)
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

/// Invokes the TACKY/IR generation phase and returns an IR version of the program.
pub fn tacky(ast: &Program, counter: &mut UniqueCounter) -> IRProgram {
    let tacky_ast = ir_parse_program(&ast, counter);

    //dbg!(&tacky_ast);

    tacky_ast
}

/// Parses the main function in the program.
fn ir_parse_program(ast: &Program, counter: &mut UniqueCounter) -> IRProgram {
    let mut return_val: Vec<IRFunctionDefinition> = Vec::new(); 
    
    match ast {
        Program::Program(functions) => {
            for function in functions {
                if ir_check_body_is_some(function) {
                    return_val.push(ir_parse_function(function, counter));
                }
            }
        }
    }
    IRProgram::IRProgram(return_val)
}

fn ir_check_body_is_some(ast: &FunctionDeclaration) -> bool {
    match ast {
        FunctionDeclaration::Function(_, _, body) => body.is_some()
    }
}

/// Parses function and function body, returning an IR function.
fn ir_parse_function(ast: &FunctionDeclaration, counter: &mut UniqueCounter) -> IRFunctionDefinition {
    let (name, param_list, mut instructions) = match ast {
        FunctionDeclaration::Function(name, param_list, body) => {
            let new_body = ir_parse_block(&body.clone().unwrap(), counter, name.to_string());
            (name.clone(), param_list.clone(), new_body)
        }
    };

    instructions.push(IRInstructions::Return(Val::Constant(0)));
    IRFunctionDefinition::IRFunction(name, param_list, instructions)
}

/// Passes vector of instruction on
fn ir_parse_block(body: &Block, counter: &mut UniqueCounter, name: String) -> Vec<IRInstructions> {
    match body {
        Block::Block(items) => ir_parse_block_items(&items, counter, name.to_string())
    }
}

/// Parses each declaration and statement in the block, returning the IR instructions.
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

    return_val
}

fn ir_parse_declaration(decl: &Declaration, instructions: &mut Vec<IRInstructions>, counter: &mut UniqueCounter) {
    match decl {
        Declaration::VarDecl(var_decl) => ir_parse_variable_declaration(var_decl, instructions, counter),
        // Can be skipped since only function declarations can appear inside blocks
        Declaration::FunDecl(_) => ()
    }   
}

fn ir_parse_variable_declaration(decl: &VariableDeclaration, instructions: &mut Vec<IRInstructions>, counter: &mut UniqueCounter) {
    match decl {
        VariableDeclaration::Variable(name, init) => {
            if init.is_some() {
                let copy = init.clone();

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
        Statement::Null => Vec::new(),
        Statement::If(condition, then, els) => {
            let mut instructions: Vec<IRInstructions> = Vec::new();
            let return_val = emit_tacky(condition, &mut instructions, counter, name);
            let mut statement = ir_parse_instructions(then, counter, name);
            let if_end = String::from("if_end");
            let if_end_label = make_unique(&if_end, UniqueType::Label, counter); 

            if els.is_some() {
                let else_string = String::from("else_branch");
                let else_label = make_unique(&else_string, UniqueType::Label, counter);
                instructions.push(IRInstructions::JumpIfZero(return_val, else_label.clone()));
                instructions.append(&mut statement);
                instructions.push(IRInstructions::Jump(if_end_label.clone()));
                instructions.push(IRInstructions::Label(else_label));
                let els = els.clone().unwrap();
                let mut statement_2 = ir_parse_instructions(&els, counter, name);
                instructions.append(&mut statement_2);
                instructions.push(IRInstructions::Label(if_end_label));

            } else {
                instructions.push(IRInstructions::JumpIfZero(return_val, if_end_label.clone()));
                instructions.append(&mut statement);
                instructions.push(IRInstructions::Label(if_end_label));
            }
            
            instructions
        },
        Statement::Compound(block) => ir_parse_block(block, counter, name.clone()),
        Statement::Break(label)   => {
            let mut instructions: Vec<IRInstructions> = Vec::new();
            let break_label = create_break_label(label);
            instructions.push(IRInstructions::Jump(break_label));

            instructions
        },
        Statement::Continue(label) => {
            let mut instructions: Vec<IRInstructions> = Vec::new();
            let continue_label = create_continue_label(label);
            instructions.push(IRInstructions::Jump(continue_label));

            instructions
        },
        Statement::DoWhile(body, condition, label) => {
            let mut instructions: Vec<IRInstructions> = Vec::new();
            let break_label = create_break_label(&label.clone());
            let continue_label = create_continue_label(&label.clone());

            instructions.push(IRInstructions::Label(label.clone()));
            let mut body = ir_parse_instructions(body, counter, name);
            instructions.append(&mut body);
            instructions.push(IRInstructions::Label(continue_label));
            let condition = emit_tacky(condition, &mut instructions, counter, name);
            instructions.push(IRInstructions::JumpIfNotZero(condition, label.clone()));
            instructions.push(IRInstructions::Label(break_label));

            instructions
        },
        Statement::While(condition, body, label) => {
            let mut instructions: Vec<IRInstructions> = Vec::new();
            let break_label = create_break_label(&label.clone());
            let continue_label = create_continue_label(&label.clone());

            instructions.push(IRInstructions::Label(continue_label.clone()));
            let condition = emit_tacky(condition, &mut instructions, counter, name);
            instructions.push(IRInstructions::JumpIfZero(condition, break_label.clone()));
            let mut body = ir_parse_instructions(body, counter, name);
            instructions.append(&mut body);
            instructions.push(IRInstructions::Jump(continue_label));
            instructions.push(IRInstructions::Label(break_label));

            instructions
        },
        Statement::For(init, condition, post, body, label) => {
            let mut instructions: Vec<IRInstructions> = Vec::new();
            let break_label = create_break_label(&label.clone());
            let continue_label = create_continue_label(&label.clone());

            ir_parse_for_init(init, &mut instructions, counter, name);
            instructions.push(IRInstructions::Label(label.clone()));
            let condition = ir_parse_optional_exp(condition, &mut instructions, counter, name);
            if condition.is_some() {
                let condition = condition.unwrap();
                instructions.push(IRInstructions::JumpIfZero(condition, break_label.clone()));
            }
            let mut body = ir_parse_instructions(body, counter, name);
            instructions.append(&mut body);
            instructions.push(IRInstructions::Label(continue_label));
            ir_parse_optional_exp(post, &mut instructions, counter, name);
            instructions.push(IRInstructions::Jump(label.clone()));
            instructions.push(IRInstructions::Label(break_label));

            instructions
        }
    };
    return_val
}

fn ir_parse_for_init(ast: &ForInit, instructions: &mut Vec<IRInstructions>, counter: &mut UniqueCounter, name: &String) {
    match ast {
        ForInit::InitDecl(decl) => ir_parse_variable_declaration(decl, instructions, counter),
        ForInit::InitExp(exp)   => {
            if exp.is_some() {
                let exp = exp.clone().unwrap();
                emit_tacky(&exp, instructions, counter, name);
            }
        }
    }
}

fn ir_parse_optional_exp(ast: &Option<Exp>, instructions: &mut Vec<IRInstructions>, counter: &mut UniqueCounter, name: &String) -> Option<Val> {
    match ast {
        Some(exp) =>  Some(emit_tacky(exp, instructions, counter, name)),
        None            =>  None
    }
}
/// Handles expression and generates the instructions to compute them.
/// 
/// Handles complex instructions.
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
        Exp::Conditional(condition, e1, e2) => {
            let e2_string = String::from("cond_e2");
            let e2_label = make_unique(&e2_string, UniqueType::Label, counter);
            let end = String::from("cond_end");
            let end_label = make_unique(&end, UniqueType::Label, counter);
            let result_name = make_unique(name, UniqueType::Temporary, counter);
            
            let condition = emit_tacky(condition, instructions, counter, name);
            instructions.push(IRInstructions::JumpIfZero(condition.clone(), e2_label.clone()));
            let v1 = emit_tacky(e1, instructions, counter, name);
            instructions.push(IRInstructions::Copy(v1, Val::Var(result_name.clone())));
            instructions.push(IRInstructions::Jump(end_label.clone()));
            instructions.push(IRInstructions::Label(e2_label));
            let v2 = emit_tacky(e2, instructions, counter, name);
            instructions.push(IRInstructions::Copy(v2, Val::Var(result_name.clone())));
            instructions.push(IRInstructions::Label(end_label));

            Val::Var(result_name)
        },
        Exp::FunctionCall(name, args) => {
            let mut new_args: Vec<Val> = Vec::new();
            let dst_name = make_unique(name, UniqueType::Temporary, counter);
            let dst = Val::Var(dst_name);

            for arg in args {
                new_args.push(emit_tacky(arg, instructions, counter, name));
            }

            instructions.push(IRInstructions::FunCall(name.clone(), new_args, dst.clone()));
            dst
        }

    }
}

/// Converts AST unary op to IR unary op.
fn ir_parse_unary_op(ast: &UnaryOperator) -> IRUnaryOperator {
    match ast {
        UnaryOperator::Complement  => IRUnaryOperator::Complement,
        UnaryOperator::Negate      => IRUnaryOperator::Negate,
        UnaryOperator::LogicalNot  => IRUnaryOperator::LogicalNot
    }
}

/// Converts AST binary op IR binary op.
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

fn create_break_label(label: &String) -> String {
    "break_".to_string() + &label
}

fn create_continue_label(label: &String) -> String {
    "continue_".to_string() + &label
}