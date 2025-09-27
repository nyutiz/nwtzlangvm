use std::env;
use std::fs;
use nwtzlang::lexer::tokenize;
use nwtzlang::parser::Parser;
use nwtzlangvm::{make_global_env, vm};
use nwtzlangvm::compiler::Compiler;
use nwtzlangvm::serialization::{load_bytecode, save_bytecode};
use nwtzlangvm::vm::VM;


fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 || !&args[1].contains("nwtz") {
        eprintln!("Usage : ");
        eprintln!("         nwtz <file.nwtz>");
        eprintln!("         nwtz-compile <file.nwtz>");
        eprintln!("         nwtz-vm <file.nwtzc>");
        std::process::exit(1);
    }

    let mut vm = VM::new();

    make_global_env(&mut vm);

    let filename = &args[2];

    let source = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file {}: {}", filename, e);
            std::process::exit(1);
        }
    };

    match args[1].as_str() {
        "nwtz" => {

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
        },
        "nwtz-compile" => {
            //let input_file = &args[2];
            let output_file = if args.len() == 4 {
                args[3].clone()
            } else {
                filename.replace(".nwtz", ".nwtzc")
            };

            let tokens = tokenize(source);
            let mut parser = Parser::new(tokens);
            let ast = parser.produce_ast();

            let mut compiler = Compiler::new();
            let chunk = compiler.compile(ast);

            match save_bytecode(&chunk, &output_file) {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("Error saving bytecode: {}", e);
                    std::process::exit(1);
                }
            }
        },
        "nwtz-vm" => {
            let bytecode_file = &args[2];

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

            match vm.execute(&chunk) {
                Ok(result) => {

                    match result {
                        vm::Value::Null => {},
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
        _ => {
            eprintln!("There is no {} , only:", &args[1]);
            eprintln!("         nwtz <file.nwtz>");
            eprintln!("         nwtz-compile <file.nwtz> <out.nwtzc>");
            eprintln!("         nwtz-vm <file.nwtzc>");
        }
    }



}

fn vm_value_to_string(value: &vm::Value) -> String {
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