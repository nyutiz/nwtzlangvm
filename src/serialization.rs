#![allow(dead_code)]

use serde::{Deserialize, Serialize};
use nwtzlang::types::ValueType;

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

impl From<crate::bytecode::Instruction> for SerializableInstruction {
    fn from(inst: crate::bytecode::Instruction) -> Self {
        match inst {
            crate::bytecode::Instruction::LoadNull => SerializableInstruction::LoadNull,
            crate::bytecode::Instruction::LoadInt(i) => SerializableInstruction::LoadInt(i),
            crate::bytecode::Instruction::LoadFloat(f) => SerializableInstruction::LoadFloat(f),
            crate::bytecode::Instruction::LoadBool(b) => SerializableInstruction::LoadBool(b),
            crate::bytecode::Instruction::LoadString(s) => SerializableInstruction::LoadString(s),
            crate::bytecode::Instruction::LoadVar(s) => SerializableInstruction::LoadVar(s),
            crate::bytecode::Instruction::StoreVar(s) => SerializableInstruction::StoreVar(s),
            crate::bytecode::Instruction::DeclareVar(s, vt) => SerializableInstruction::DeclareVar(s, vt.map(|v| v.into())),
            crate::bytecode::Instruction::Add => SerializableInstruction::Add,
            crate::bytecode::Instruction::Sub => SerializableInstruction::Sub,
            crate::bytecode::Instruction::Mul => SerializableInstruction::Mul,
            crate::bytecode::Instruction::Div => SerializableInstruction::Div,
            crate::bytecode::Instruction::Mod => SerializableInstruction::Mod,
            crate::bytecode::Instruction::Equal => SerializableInstruction::Equal,
            crate::bytecode::Instruction::NotEqual => SerializableInstruction::NotEqual,
            crate::bytecode::Instruction::Greater => SerializableInstruction::Greater,
            crate::bytecode::Instruction::Less => SerializableInstruction::Less,
            crate::bytecode::Instruction::GreaterEqual => SerializableInstruction::GreaterEqual,
            crate::bytecode::Instruction::LessEqual => SerializableInstruction::LessEqual,
            crate::bytecode::Instruction::Jump(addr) => SerializableInstruction::Jump(addr),
            crate::bytecode::Instruction::JumpIfFalse(addr) => SerializableInstruction::JumpIfFalse(addr),
            crate::bytecode::Instruction::JumpIfTrue(addr) => SerializableInstruction::JumpIfTrue(addr),
            crate::bytecode::Instruction::Call(n) => SerializableInstruction::Call(n),
            crate::bytecode::Instruction::Return => SerializableInstruction::Return,
            crate::bytecode::Instruction::DefineFunction(name, params, start, end) =>
                SerializableInstruction::DefineFunction(name, params, start, end),
            crate::bytecode::Instruction::CreateObject => SerializableInstruction::CreateObject,
            crate::bytecode::Instruction::CreateArray(n) => SerializableInstruction::CreateArray(n),
            crate::bytecode::Instruction::GetProperty(s) => SerializableInstruction::GetProperty(s),
            crate::bytecode::Instruction::SetProperty(s) => SerializableInstruction::SetProperty(s),
            crate::bytecode::Instruction::GetIndex => SerializableInstruction::GetIndex,
            crate::bytecode::Instruction::SetIndex => SerializableInstruction::SetIndex,
            crate::bytecode::Instruction::Pop => SerializableInstruction::Pop,
            crate::bytecode::Instruction::Duplicate => SerializableInstruction::Duplicate,
            crate::bytecode::Instruction::Print => SerializableInstruction::Print,
            crate::bytecode::Instruction::Halt => SerializableInstruction::Halt,
        }
    }
}

impl From<SerializableInstruction> for crate::bytecode::Instruction {
    fn from(inst: SerializableInstruction) -> Self {
        match inst {
            SerializableInstruction::LoadNull => crate::bytecode::Instruction::LoadNull,
            SerializableInstruction::LoadInt(i) => crate::bytecode::Instruction::LoadInt(i),
            SerializableInstruction::LoadFloat(f) => crate::bytecode::Instruction::LoadFloat(f),
            SerializableInstruction::LoadBool(b) => crate::bytecode::Instruction::LoadBool(b),
            SerializableInstruction::LoadString(s) => crate::bytecode::Instruction::LoadString(s),
            SerializableInstruction::LoadVar(s) => crate::bytecode::Instruction::LoadVar(s),
            SerializableInstruction::StoreVar(s) => crate::bytecode::Instruction::StoreVar(s),
            SerializableInstruction::DeclareVar(s, vt) => crate::bytecode::Instruction::DeclareVar(s, vt.map(|v| v.into())),
            SerializableInstruction::Add => crate::bytecode::Instruction::Add,
            SerializableInstruction::Sub => crate::bytecode::Instruction::Sub,
            SerializableInstruction::Mul => crate::bytecode::Instruction::Mul,
            SerializableInstruction::Div => crate::bytecode::Instruction::Div,
            SerializableInstruction::Mod => crate::bytecode::Instruction::Mod,
            SerializableInstruction::Equal => crate::bytecode::Instruction::Equal,
            SerializableInstruction::NotEqual => crate::bytecode::Instruction::NotEqual,
            SerializableInstruction::Greater => crate::bytecode::Instruction::Greater,
            SerializableInstruction::Less => crate::bytecode::Instruction::Less,
            SerializableInstruction::GreaterEqual => crate::bytecode::Instruction::GreaterEqual,
            SerializableInstruction::LessEqual => crate::bytecode::Instruction::LessEqual,
            SerializableInstruction::Jump(addr) => crate::bytecode::Instruction::Jump(addr),
            SerializableInstruction::JumpIfFalse(addr) => crate::bytecode::Instruction::JumpIfFalse(addr),
            SerializableInstruction::JumpIfTrue(addr) => crate::bytecode::Instruction::JumpIfTrue(addr),
            SerializableInstruction::Call(n) => crate::bytecode::Instruction::Call(n),
            SerializableInstruction::Return => crate::bytecode::Instruction::Return,
            SerializableInstruction::DefineFunction(name, params, start, end) =>
                crate::bytecode::Instruction::DefineFunction(name, params, start, end),
            SerializableInstruction::CreateObject => crate::bytecode::Instruction::CreateObject,
            SerializableInstruction::CreateArray(n) => crate::bytecode::Instruction::CreateArray(n),
            SerializableInstruction::GetProperty(s) => crate::bytecode::Instruction::GetProperty(s),
            SerializableInstruction::SetProperty(s) => crate::bytecode::Instruction::SetProperty(s),
            SerializableInstruction::GetIndex => crate::bytecode::Instruction::GetIndex,
            SerializableInstruction::SetIndex => crate::bytecode::Instruction::SetIndex,
            SerializableInstruction::Pop => crate::bytecode::Instruction::Pop,
            SerializableInstruction::Duplicate => crate::bytecode::Instruction::Duplicate,
            SerializableInstruction::Print => crate::bytecode::Instruction::Print,
            SerializableInstruction::Halt => crate::bytecode::Instruction::Halt,
        }
    }
}

impl From<crate::bytecode::Chunk> for SerializableChunk {
    fn from(chunk: crate::bytecode::Chunk) -> Self {
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

impl From<SerializableChunk> for crate::bytecode::Chunk {
    fn from(chunk: SerializableChunk) -> Self {
        crate::bytecode::Chunk {
            instructions: chunk.instructions.into_iter().map(|i| i.into()).collect(),
            constants: chunk.constants,
        }
    }
}

pub fn save_bytecode(chunk: &crate::bytecode::Chunk, filename: &str) -> Result<(), Box<dyn std::error::Error>> {
    let serializable: SerializableChunk = chunk.clone().into();
    let data = bincode::serialize(&serializable)?;
    std::fs::write(filename, data)?;
    Ok(())
}

pub fn load_bytecode(filename: &str) -> Result<crate::bytecode::Chunk, Box<dyn std::error::Error>> {
    let data = std::fs::read(filename)?;
    let serializable: SerializableChunk = bincode::deserialize(&data)?;
    Ok(serializable.into())
}

pub fn save_bytecode_json(chunk: &crate::bytecode::Chunk, filename: &str) -> Result<(), Box<dyn std::error::Error>> {
    let serializable: SerializableChunk = chunk.clone().into();
    let json = serde_json::to_string_pretty(&serializable)?;
    std::fs::write(filename, json)?;
    Ok(())
}

pub fn load_bytecode_json(filename: &str) -> Result<crate::bytecode::Chunk, Box<dyn std::error::Error>> {
    let json = std::fs::read_to_string(filename)?;
    let serializable: SerializableChunk = serde_json::from_str(&json)?;
    Ok(serializable.into())
}