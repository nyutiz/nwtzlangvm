#![allow(dead_code)]

use nwtzlang::types::ValueType;

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // Literals
    LoadNull,
    LoadInt(i64),
    LoadFloat(f64),
    LoadBool(bool),
    LoadString(String),

    // Variables
    LoadVar(String),
    StoreVar(String),
    DeclareVar(String, Option<ValueType>),

    // Arithmetic
    Add,
    Substract,
    Multiply,
    Divide,
    Mod,

    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,

    Jump(usize),
    JumpIfFalse(usize),
    JumpIfTrue(usize),

    Call(usize),
    Return,
    DefineFunction(String, Vec<String>, usize, usize),

    CreateObject,
    CreateArray(usize),
    GetProperty(String),
    SetProperty(String),
    GetIndex,
    SetIndex,

    Pop,
    Duplicate,

    Print,

    Halt,
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<String>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn add_instruction(&mut self, instruction: Instruction) -> usize {
        let index = self.instructions.len();
        self.instructions.push(instruction);
        index
    }

    pub fn add_constant(&mut self, value: String) -> usize {
        let index = self.constants.len();
        self.constants.push(value);
        index
    }

    pub fn patch_jump(&mut self, jump_index: usize, target: usize) {
        match &mut self.instructions[jump_index] {
            Instruction::Jump(ref mut addr) |
            Instruction::JumpIfFalse(ref mut addr) |
            Instruction::JumpIfTrue(ref mut addr) => {
                *addr = target;
            }
            _ => panic!("Trying to patch non-jump instruction"),
        }
    }
}