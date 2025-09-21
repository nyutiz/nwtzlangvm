use std::collections::HashMap;
use nwtzlang::ast::*;
use crate::bytecode::{Chunk, Instruction};

pub struct Compiler {
    chunk: Chunk,
    locals: HashMap<String, usize>,
    local_count: usize,
    function_table: HashMap<String, (Vec<String>, usize, usize)>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            chunk: Chunk::new(),
            locals: HashMap::new(),
            local_count: 0,
            function_table: HashMap::new(),
        }
    }

    pub fn compile(&mut self, program: Program) -> Chunk {
        for stmt in &program.body {
            if let Some(func_decl) = stmt.as_any().downcast_ref::<FunctionDeclaration>() {
                let start_addr = self.chunk.instructions.len();
                self.function_table.insert(
                    func_decl.name.clone(),
                    (func_decl.parameters.clone(), start_addr, 0)
                );
            }
        }

        for stmt in program.body {
            self.compile_stmt(stmt);
        }

        self.chunk.add_instruction(Instruction::Halt);

        self.chunk.clone()
    }

    fn compile_stmt(&mut self, stmt: Box<dyn Stmt>) {
        match stmt.kind() {
            NodeType::NumericLiteral => {
                let literal = stmt.as_any().downcast_ref::<LiteralExpr>().unwrap();
                if literal.value.fract() == 0.0 {
                    self.chunk.add_instruction(Instruction::LoadInt(literal.value as i64));
                } else {
                    self.chunk.add_instruction(Instruction::LoadFloat(literal.value));
                }
            }

            NodeType::StringLiteral => {
                let str_val = stmt.as_any().downcast_ref::<StringVal>().unwrap();
                self.chunk.add_instruction(Instruction::LoadString(str_val.value.clone()));
            }

            NodeType::BooleanLiteral => {
                let bool_val = stmt.as_any().downcast_ref::<BooleanLiteral>().unwrap();
                self.chunk.add_instruction(Instruction::LoadBool(bool_val.value));
            }

            NodeType::NullLiteral => {
                self.chunk.add_instruction(Instruction::LoadNull);
            }

            NodeType::Identifier => {
                let ident = stmt.as_any().downcast_ref::<IdentifierExpr>().unwrap();


                // temporary error solver for no resolve for null
                if ident.name == "null" {
                    self.chunk.add_instruction(Instruction::LoadNull);
                } else {
                    self.chunk.add_instruction(Instruction::LoadVar(ident.name.clone()));
                }
            }


            NodeType::BinaryExpression => {
                let binary = stmt.as_any().downcast_ref::<BinaryExpr>().unwrap();

                self.compile_stmt(binary.left.clone());
                self.compile_stmt(binary.right.clone());

                match binary.operator.as_str() {
                    "+" => self.chunk.add_instruction(Instruction::Add),
                    "-" => self.chunk.add_instruction(Instruction::Sub),
                    "*" => self.chunk.add_instruction(Instruction::Mul),
                    "/" => self.chunk.add_instruction(Instruction::Div),
                    "%" => self.chunk.add_instruction(Instruction::Mod),
                    "==" => self.chunk.add_instruction(Instruction::Equal),
                    "!=" => self.chunk.add_instruction(Instruction::NotEqual),
                    ">" => self.chunk.add_instruction(Instruction::Greater),
                    "<" => self.chunk.add_instruction(Instruction::Less),
                    ">=" => self.chunk.add_instruction(Instruction::GreaterEqual),
                    "<=" => self.chunk.add_instruction(Instruction::LessEqual),
                    _ => panic!("Unknown binary operator: {}", binary.operator),
                };
            }

            NodeType::VariableDeclaration => {
                let var_decl = stmt.as_any().downcast_ref::<VariableDeclaration>().unwrap();

                if let Some(value) = &var_decl.value {
                    self.compile_stmt(value.clone());
                } else {
                    self.chunk.add_instruction(Instruction::LoadNull);
                }

                self.chunk.add_instruction(Instruction::DeclareVar(
                    var_decl.name.clone(),
                    var_decl.r#type.clone()
                ));

                self.locals.insert(var_decl.name.clone(), self.local_count);
                self.local_count += 1;
            }

            NodeType::AssignmentExpr => {
                let assignment = stmt.as_any().downcast_ref::<AssignmentExpr>().unwrap();

                if let Some(member) = assignment.assigne.as_any().downcast_ref::<MemberExpr>() {
                    self.compile_stmt(member.object.clone());

                    if let Some(ident) = member.property.as_any().downcast_ref::<IdentifierExpr>() {
                        if let Some(value) = &assignment.value {
                            self.compile_stmt(value.clone());
                        } else {
                            self.chunk.add_instruction(Instruction::LoadNull);
                        }
                        self.chunk.add_instruction(Instruction::SetProperty(ident.name.clone()));
                        self.chunk.add_instruction(Instruction::Pop);
                    } else {
                        self.compile_stmt(member.property.clone());
                        if let Some(value) = &assignment.value {
                            self.compile_stmt(value.clone());
                        } else {
                            self.chunk.add_instruction(Instruction::LoadNull);
                        }
                        self.chunk.add_instruction(Instruction::SetIndex);
                        self.chunk.add_instruction(Instruction::Pop);
                    }
                    return;
                }

                if let Some(value) = &assignment.value {
                    self.compile_stmt(value.clone());
                } else {
                    self.chunk.add_instruction(Instruction::LoadNull);
                }

                if let Some(ident) = assignment.assigne.as_any().downcast_ref::<IdentifierExpr>() {
                    self.chunk.add_instruction(Instruction::StoreVar(ident.name.clone()));
                } else {

                }
            }

            NodeType::CallExpr => {
                let call = stmt.as_any().downcast_ref::<CallExpr>().unwrap();

                //if let Some(callee_ident) = call.caller.as_any().downcast_ref::<IdentifierExpr>() {
                //    if callee_ident.name == "log" {
                //        for arg in &call.args {
                //            self.compile_stmt(arg.clone());
                //            self.chunk.add_instruction(Instruction::Print);
                //        }
                //        return;
                //    }
                //}

                for arg in &call.args {
                    self.compile_stmt(arg.clone());
                }
                self.compile_stmt(call.caller.clone());
                self.chunk.add_instruction(Instruction::Call(call.args.len()));
            }


            NodeType::FunctionDeclaration => {
                let func_decl = stmt.as_any().downcast_ref::<FunctionDeclaration>().unwrap();

                let start = self.chunk.instructions.len() + 2;

                let def_idx = self.chunk.add_instruction(Instruction::DefineFunction(
                    func_decl.name.clone(),
                    func_decl.parameters.clone(),
                    start,
                    0,
                ));

                let jump_to_end_idx = self.chunk.add_instruction(Instruction::Jump(0)); // patch plus bas

                debug_assert_eq!(self.chunk.instructions.len(), start);
                for s in &func_decl.body {
                    self.compile_stmt(s.clone());
                }

                self.chunk.add_instruction(Instruction::Return);

                let end = self.chunk.instructions.len();

                if let Instruction::DefineFunction(_, _, _, ref mut end_slot) =
                    self.chunk.instructions[def_idx]
                {
                    *end_slot = end;
                } else {
                    unreachable!("def_idx doit pointer sur DefineFunction");
                }

                self.chunk.patch_jump(jump_to_end_idx, end);

                self.function_table.insert(
                    func_decl.name.clone(),
                    (func_decl.parameters.clone(), start, end),
                );
            }


            NodeType::IfStatement => {
                let if_stmt = stmt.as_any().downcast_ref::<IfStatement>().unwrap();

                self.compile_stmt(if_stmt.condition.clone());

                let jump_to_else = self.chunk.add_instruction(Instruction::JumpIfFalse(0));

                for stmt in &if_stmt.then_branch {
                    self.compile_stmt(stmt.clone());
                }

                let jump_to_end = self.chunk.add_instruction(Instruction::Jump(0));

                let else_start = self.chunk.instructions.len();
                self.chunk.patch_jump(jump_to_else, else_start);

                if let Some(else_branch) = &if_stmt.else_branch {
                    for stmt in else_branch {
                        self.compile_stmt(stmt.clone());
                    }
                }

                let end_addr = self.chunk.instructions.len();
                self.chunk.patch_jump(jump_to_end, end_addr);
            }

            NodeType::ForStatement => {
                let for_stmt = stmt.as_any().downcast_ref::<ForStatement>().unwrap();

                if let Some(init) = &for_stmt.initializer {
                    self.compile_stmt(init.clone());
                }

                let loop_start = self.chunk.instructions.len();

                if let Some(condition) = &for_stmt.condition {
                    self.compile_stmt(condition.clone());
                    let jump_to_end = self.chunk.add_instruction(Instruction::JumpIfFalse(0));

                    for stmt in &for_stmt.body {
                        self.compile_stmt(stmt.clone());
                    }

                    if let Some(increment) = &for_stmt.increment {
                        self.compile_stmt(increment.clone());
                    }

                    self.chunk.add_instruction(Instruction::Jump(loop_start));

                    let end_addr = self.chunk.instructions.len();
                    self.chunk.patch_jump(jump_to_end, end_addr);
                } else {
                    for stmt in &for_stmt.body {
                        self.compile_stmt(stmt.clone());
                    }

                    if let Some(increment) = &for_stmt.increment {
                        self.compile_stmt(increment.clone());
                    }

                    self.chunk.add_instruction(Instruction::Jump(loop_start));
                }
            }

            NodeType::ArrayLiteral => {
                let array = stmt.as_any().downcast_ref::<ArrayLiteral>().unwrap();

                for element in &array.elements {
                    self.compile_stmt(element.clone());
                }

                self.chunk.add_instruction(Instruction::CreateArray(array.elements.len()));
            }

            NodeType::ObjectLiteral => {
                let obj = stmt.as_any().downcast_ref::<ObjectLiteral>().unwrap();

                self.chunk.add_instruction(Instruction::CreateObject);

                for property in &obj.properties {
                    if let Some(value) = &property.value {
                        self.compile_stmt(value.clone());
                        self.chunk.add_instruction(Instruction::SetProperty(property.key.clone()));
                    }
                }
            }

            NodeType::MemberExpr => {
                let member = stmt.as_any().downcast_ref::<MemberExpr>().unwrap();

                self.compile_stmt(member.object.clone());

                if let Some(ident) = member.property.as_any().downcast_ref::<IdentifierExpr>() {
                    self.chunk.add_instruction(Instruction::GetProperty(ident.name.clone()));
                } else {
                    self.compile_stmt(member.property.clone());
                    self.chunk.add_instruction(Instruction::GetIndex);
                }
            }

            _ => {
                println!("Warning: Unhandled node type: {:?}", stmt.kind());
            }
        }
    }
}