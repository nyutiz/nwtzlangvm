use std::env;
use std::fs;

use nwtzlang::lexer::tokenize;
use nwtzlang::parser::Parser;
use crate::compiler::Compiler;
use crate::vm::VM;

mod bytecode;
mod compiler;
mod vm;
mod serialization;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <file.nwtz>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];

    let source = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file {}: {}", filename, e);
            std::process::exit(1);
        }
    };

    let tokens = tokenize(source);

    let mut parser = Parser::new(tokens);
    let ast = parser.produce_ast();

    let mut compiler = Compiler::new();
    let chunk = compiler.compile(ast);

    if env::var("DEBUG_BYTECODE").is_ok() {
        for (i, instruction) in chunk.instructions.iter().enumerate() {
            println!("{:04}: {:?}", i, instruction);
        }
        println!();
    }

    let mut vm = VM::new();

    vm.make_global_env();

    match vm.execute(&chunk) {
        Ok(result) => {
            match result {
                vm::Value::Null => {},
                _ => println!("Result: {}", vm_value_to_string(&result)),
            }
        }
        Err(e) => {
            eprintln!("[X] Runtime error: {}", e);
            std::process::exit(1);
        }
    }
}

fn vm_value_to_string(value: &crate::vm::Value) -> String {
    match value {
        vm::Value::Null => "null".to_string(),
        vm::Value::Integer(i) => i.to_string(),
        vm::Value::Float(f) => f.to_string(),
        vm::Value::Boolean(b) => b.to_string(),
        vm::Value::String(s) => s.clone(),
        vm::Value::Array(arr) => {
            let elements: Vec<String> = arr.iter()
                .map(vm_value_to_string)
                .collect();
            format!("[{}]", elements.join(", "))
        }
        vm::Value::Object(obj) => {
            let props: Vec<String> = obj.iter()
                .map(|(k, v)| format!("{}: {}", k, vm_value_to_string(v)))
                .collect();
            format!("{{{}}}", props.join(", "))
        }
        vm::Value::Function(name, _, _, _) => format!("<function {}>", name),
        vm::Value::NativeFunction{name, func:_} => format!("<nativeFunction {}>", name),
    }
}