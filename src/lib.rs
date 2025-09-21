pub mod bytecode;
pub mod compiler;
pub mod vm;
pub mod serialization;

pub use nwtzlang::{ast, lexer, parser, types, runtime, environment};

pub use bytecode::{Instruction, Chunk};
pub use compiler::Compiler;
pub use vm::{VM, Value};
pub use serialization::{save_bytecode, load_bytecode, save_bytecode_json, load_bytecode_json};

pub fn compile_source(source: String) -> Result<Chunk, String> {
    let tokens = lexer::tokenize(source);
    let mut parser = parser::Parser::new(tokens);
    let ast = parser.produce_ast();

    let mut compiler = Compiler::new();
    let chunk = compiler.compile(ast);

    Ok(chunk)
}

pub fn execute_source(source: String) -> Result<Value, String> {
    let chunk = compile_source(source)?;
    let mut vm = VM::new();
    vm.execute(&chunk)
}