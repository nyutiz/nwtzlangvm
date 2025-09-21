use std::env;

use nwtzlangvm::serialization::load_bytecode;
use nwtzlangvm::vm::VM;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <bytecode.nwbc>", args[0]);
        std::process::exit(1);
    }

    let bytecode_file = &args[1];

    // Load
    let chunk = match load_bytecode(bytecode_file) {
        Ok(chunk) => chunk,
        Err(e) => {
            eprintln!("Error loading bytecode from {}: {}", bytecode_file, e);
            std::process::exit(1);
        }
    };


    if env::var("DEBUG_BYTECODE").is_ok() {
        println!("\nBytecode instructions:");
        for (i, instruction) in chunk.instructions.iter().enumerate() {
            println!("{:04}: {:?}", i, instruction);
        }
        println!();
    }

    let mut vm = VM::new();
    match vm.execute(&chunk) {
        Ok(result) => {

            match result {
                nwtzlangvm::vm::Value::Null => {},
                _ => {
                    println!("Result: {}", vm_value_to_string(&result));
                }
            }
        }
        Err(e) => {
            eprintln!("Runtime error: {}", e);
            std::process::exit(1);
        }
    }
}

fn vm_value_to_string(value: &nwtzlangvm::vm::Value) -> String {
    match value {
        nwtzlangvm::vm::Value::Null => "null".to_string(),
        nwtzlangvm::vm::Value::Integer(i) => i.to_string(),
        nwtzlangvm::vm::Value::Float(f) => f.to_string(),
        nwtzlangvm::vm::Value::Boolean(b) => b.to_string(),
        nwtzlangvm::vm::Value::String(s) => format!("\"{}\"", s),
        nwtzlangvm::vm::Value::Array(arr) => {
            let elements: Vec<String> = arr.iter()
                .map(vm_value_to_string)
                .collect();
            format!("[{}]", elements.join(", "))
        }
        nwtzlangvm::vm::Value::Object(obj) => {
            let props: Vec<String> = obj.iter()
                .map(|(k, v)| format!("{}: {}", k, vm_value_to_string(v)))
                .collect();
            format!("{{{}}}", props.join(", "))
        }
        nwtzlangvm::vm::Value::Function(name, _, _, _) => format!("<function {}>", name),
    }
}