use std::env;
use std::fs;

use nwtzlang::lexer::tokenize;
use nwtzlang::parser::Parser;
use nwtzlangvm::compiler::Compiler;
use nwtzlangvm::serialization::save_bytecode;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 || args.len() > 3 {
        eprintln!("Usage: {} <input.nwtz> [output.nwtzc]", args[0]);
        eprintln!("If no output file is specified, uses input filename with .nwtzc extension");
        std::process::exit(1);
    }

    let input_file = &args[1];
    let output_file = if args.len() == 3 {
        args[2].clone()
    } else {
        input_file.replace(".nwtz", ".nwbc")
    };

    let source = match fs::read_to_string(input_file) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file {}: {}", input_file, e);
            std::process::exit(1);
        }
    };

    let tokens = tokenize(source);
    let mut parser = Parser::new(tokens);
    let ast = parser.produce_ast();

    let mut compiler = Compiler::new();
    let chunk = compiler.compile(ast);

    match save_bytecode(&chunk, &output_file) {
        Ok(_) => {

        }
        Err(e) => {
            eprintln!("Error saving bytecode: {}", e);
            std::process::exit(1);
        }
    }
}