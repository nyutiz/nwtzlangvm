#![allow(dead_code)]

use serde::{Deserialize, Serialize};
use nwtzlang::types::ValueType;
use crate::{Chunk, Instruction};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SerializableInstruction {
    // Literals
    LoadNull,
    LoadInt(i64),
    LoadFloat(f64),
    LoadBool(bool),
    LoadString(String),

    // Variables
    LoadVar(String),
    StoreVar(String),
    DeclareVar(String, Option<SerializableValueType>),

    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Comparison
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,

    // Control flow
    Jump(usize),
    JumpIfFalse(usize),
    JumpIfTrue(usize),

    // Functions
    Call(usize),
    Return,
    DefineFunction(String, Vec<String>, usize, usize),

    // Objects/Arrays
    CreateObject,
    CreateArray(usize),
    GetProperty(String),
    SetProperty(String),
    GetIndex,
    SetIndex,

    // Stack operations
    Pop,
    Duplicate,

    // Debug
    Print,

    // Program control
    Halt,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum SerializableValueType {
    Null,
    Integer,
    Boolean,
    Object,
    Array,
    NativeFn,
    Function,
    String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerializableChunk {
    pub instructions: Vec<SerializableInstruction>,
    pub constants: Vec<String>,
    pub metadata: ChunkMetadata,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChunkMetadata {
    pub version: String,
    //pub created_at: String,
    pub source_file: Option<String>,
    pub optimization_level: u8,
}

impl From<ValueType> for SerializableValueType {
    fn from(vt: ValueType) -> Self {
        match vt {
            ValueType::Null => SerializableValueType::Null,
            ValueType::Integer => SerializableValueType::Integer,
            ValueType::Boolean => SerializableValueType::Boolean,
            ValueType::Object => SerializableValueType::Object,
            ValueType::Array => SerializableValueType::Array,
            ValueType::NativeFn => SerializableValueType::NativeFn,
            ValueType::Function => SerializableValueType::Function,
            ValueType::String => SerializableValueType::String,
        }
    }
}

impl From<SerializableValueType> for ValueType {
    fn from(svt: SerializableValueType) -> Self {
        match svt {
            SerializableValueType::Null => ValueType::Null,
            SerializableValueType::Integer => ValueType::Integer,
            SerializableValueType::Boolean => ValueType::Boolean,
            SerializableValueType::Object => ValueType::Object,
            SerializableValueType::Array => ValueType::Array,
            SerializableValueType::NativeFn => ValueType::NativeFn,
            SerializableValueType::Function => ValueType::Function,
            SerializableValueType::String => ValueType::String,
        }
    }
}

impl From<Instruction> for SerializableInstruction {
    fn from(inst: Instruction) -> Self {
        match inst {
            Instruction::LoadNull => SerializableInstruction::LoadNull,
            Instruction::LoadInt(i) => SerializableInstruction::LoadInt(i),
            Instruction::LoadFloat(f) => SerializableInstruction::LoadFloat(f),
            Instruction::LoadBool(b) => SerializableInstruction::LoadBool(b),
            Instruction::LoadString(s) => SerializableInstruction::LoadString(s),
            Instruction::LoadVar(s) => SerializableInstruction::LoadVar(s),
            Instruction::StoreVar(s) => SerializableInstruction::StoreVar(s),
            Instruction::DeclareVar(s, vt) => SerializableInstruction::DeclareVar(s, vt.map(|v| v.into())),
            Instruction::Add => SerializableInstruction::Add,
            Instruction::Substract => SerializableInstruction::Sub,
            Instruction::Multiply => SerializableInstruction::Mul,
            Instruction::Divide => SerializableInstruction::Div,
            Instruction::Mod => SerializableInstruction::Mod,
            Instruction::Equal => SerializableInstruction::Equal,
            Instruction::NotEqual => SerializableInstruction::NotEqual,
            Instruction::Greater => SerializableInstruction::Greater,
            Instruction::Less => SerializableInstruction::Less,
            Instruction::GreaterEqual => SerializableInstruction::GreaterEqual,
            Instruction::LessEqual => SerializableInstruction::LessEqual,
            Instruction::Jump(addr) => SerializableInstruction::Jump(addr),
            Instruction::JumpIfFalse(addr) => SerializableInstruction::JumpIfFalse(addr),
            Instruction::JumpIfTrue(addr) => SerializableInstruction::JumpIfTrue(addr),
            Instruction::Call(n) => SerializableInstruction::Call(n),
            Instruction::Return => SerializableInstruction::Return,
            Instruction::CreateObject => SerializableInstruction::CreateObject,
            Instruction::CreateArray(n) => SerializableInstruction::CreateArray(n),
            Instruction::GetProperty(s) => SerializableInstruction::GetProperty(s),
            Instruction::SetProperty(s) => SerializableInstruction::SetProperty(s),
            Instruction::GetIndex => SerializableInstruction::GetIndex,
            Instruction::SetIndex => SerializableInstruction::SetIndex,
            Instruction::Pop => SerializableInstruction::Pop,
            Instruction::Duplicate => SerializableInstruction::Duplicate,
            Instruction::Print => SerializableInstruction::Print,
            Instruction::Halt => SerializableInstruction::Halt,
            Instruction::DefineFunction(name, params, start, end) =>
                SerializableInstruction::DefineFunction(name, params, start, end),
        }
    }
}

impl From<SerializableInstruction> for Instruction {
    fn from(inst: SerializableInstruction) -> Self {
        match inst {
            SerializableInstruction::LoadNull => Instruction::LoadNull,
            SerializableInstruction::LoadInt(i) => Instruction::LoadInt(i),
            SerializableInstruction::LoadFloat(f) => Instruction::LoadFloat(f),
            SerializableInstruction::LoadBool(b) => Instruction::LoadBool(b),
            SerializableInstruction::LoadString(s) => Instruction::LoadString(s),
            SerializableInstruction::LoadVar(s) => Instruction::LoadVar(s),
            SerializableInstruction::StoreVar(s) => Instruction::StoreVar(s),
            SerializableInstruction::DeclareVar(s, vt) => Instruction::DeclareVar(s, vt.map(|v| v.into())),
            SerializableInstruction::Add => Instruction::Add,
            SerializableInstruction::Sub => Instruction::Substract,
            SerializableInstruction::Mul => Instruction::Multiply,
            SerializableInstruction::Div => Instruction::Divide,
            SerializableInstruction::Mod => Instruction::Mod,
            SerializableInstruction::Equal => Instruction::Equal,
            SerializableInstruction::NotEqual => Instruction::NotEqual,
            SerializableInstruction::Greater => Instruction::Greater,
            SerializableInstruction::Less => Instruction::Less,
            SerializableInstruction::GreaterEqual => Instruction::GreaterEqual,
            SerializableInstruction::LessEqual => Instruction::LessEqual,
            SerializableInstruction::Jump(addr) => Instruction::Jump(addr),
            SerializableInstruction::JumpIfFalse(addr) => Instruction::JumpIfFalse(addr),
            SerializableInstruction::JumpIfTrue(addr) => Instruction::JumpIfTrue(addr),
            SerializableInstruction::Call(n) => Instruction::Call(n),
            SerializableInstruction::Return => Instruction::Return,
            SerializableInstruction::DefineFunction(name, params, start, end) => Instruction::DefineFunction(name, params, start, end),
            SerializableInstruction::CreateObject => Instruction::CreateObject,
            SerializableInstruction::CreateArray(n) => Instruction::CreateArray(n),
            SerializableInstruction::GetProperty(s) => Instruction::GetProperty(s),
            SerializableInstruction::SetProperty(s) => Instruction::SetProperty(s),
            SerializableInstruction::GetIndex => Instruction::GetIndex,
            SerializableInstruction::SetIndex => Instruction::SetIndex,
            SerializableInstruction::Pop => Instruction::Pop,
            SerializableInstruction::Duplicate => Instruction::Duplicate,
            SerializableInstruction::Print => Instruction::Print,
            SerializableInstruction::Halt => Instruction::Halt,
        }
    }
}

impl From<Chunk> for SerializableChunk {
    fn from(chunk: Chunk) -> Self {
        SerializableChunk {
            instructions: chunk.instructions.into_iter().map(|i| i.into()).collect(),
            constants: chunk.constants,
            metadata: ChunkMetadata {
                version: env!("CARGO_PKG_VERSION").to_string(),
                //created_at: chrono::Utc::now().to_rfc3339(),
                source_file: None,
                optimization_level: 0,
            },
        }
    }
}

impl From<SerializableChunk> for Chunk {
    fn from(chunk: SerializableChunk) -> Self {
        Chunk {
            instructions: chunk.instructions.into_iter().map(|i| i.into()).collect(),
            constants: chunk.constants,
        }
    }
}

pub fn save_bytecode(chunk: &Chunk, filename: &str) -> Result<(), Box<dyn std::error::Error>> {
    let serializable: SerializableChunk = chunk.clone().into();
    let data = bincode::serialize(&serializable)?;
    std::fs::write(filename, data)?;
    Ok(())
}

pub fn load_bytecode(filename: &str) -> Result<Chunk, Box<dyn std::error::Error>> {
    let data = std::fs::read(filename)?;
    let serializable: SerializableChunk = bincode::deserialize(&data)?;
    Ok(serializable.into())
}

pub fn save_bytecode_json(chunk: &Chunk, filename: &str) -> Result<(), Box<dyn std::error::Error>> {
    let serializable: SerializableChunk = chunk.clone().into();
    let json = serde_json::to_string_pretty(&serializable)?;
    std::fs::write(filename, json)?;
    Ok(())
}

pub fn load_bytecode_json(filename: &str) -> Result<Chunk, Box<dyn std::error::Error>> {
    let json = std::fs::read_to_string(filename)?;
    let serializable: SerializableChunk = serde_json::from_str(&json)?;
    Ok(serializable.into())
}