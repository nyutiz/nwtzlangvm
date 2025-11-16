pub mod bytecode;
pub mod compiler;
pub mod vm;
pub mod serialization;

use std::collections::HashMap;
use std::{io, };
use std::sync::{Arc};
pub use nwtzlang::{ast, lexer, parser, types, runtime, environment};
pub use crate::bytecode::{Instruction, Chunk};
pub use crate::compiler::Compiler;
pub use crate::vm::{VM, Value};
pub use crate::serialization::{save_bytecode, load_bytecode, save_bytecode_json, load_bytecode_json};

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

pub fn make_global_env(env: & mut VM) {

    //let thread_manager = ThreadManager::new();


    env.set_var("null", Value::Null);
    env.set_var("true", Value::Boolean(true));
    env.set_var("false", Value::Boolean(false));

    env.set_var("log", mk_native_fn(
        "log".to_string(),
        |vm: &mut VM, args: Vec<Value>| {
            for v in args {
                println!("{}", vm.value_to_string(&v));
            }
            Ok(mk_null())
        }
    ));

    env.set_var("time", mk_native_fn(
        "time".to_string(),
        |_vm: &mut VM, _args: Vec<Value>| {
            use std::time::{SystemTime, UNIX_EPOCH};
            let secs = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_millis() as f64 / 1000.0;
            Ok(mk_number_float(secs))
        }
    ));

    env.set_var("sleep", mk_native_fn(
        "sleep".to_string(),
        |_vm: &mut VM, args: Vec<Value>| {
            let secs = match args.get(0) {
                Some(Value::Integer(i)) => *i as f64,
                Some(Value::Float(f)) => *f,
                _ => return Err("sleep: expected number".into()),
            };
            std::thread::sleep(std::time::Duration::from_secs_f64(secs));
            Ok(mk_null())
        }
    ));

    env.set_var("input", mk_native_fn(
        "input".to_string(),
        |_vm: &mut VM, _args: Vec<Value>| {
            let mut s = String::new();
            io::stdin().read_line(&mut s).map_err(|e| e.to_string())?;
            Ok(mk_string(s.trim_end().to_string()))
        }
    ));

    env.set_var("system", mk_object({
        let mut h: HashMap<String, Value> = HashMap::new();

        h.insert("value_type".to_string(), mk_native_fn(
            "type".to_string(),
            |_vm: &mut VM, args: Vec<Value>| {
                if let Some(value) = args.first() {
                    let type_name = match value {
                        Value::Null => "Null",
                        Value::Integer(_) => "Integer",
                        Value::Float(_) => "Float",
                        Value::Boolean(_) => "Boolean",
                        Value::String(_) => "String",
                        Value::Array(_) => "Array",
                        Value::Object(_) => "Object",
                        Value::Function(_, _, _, _) => "Function",
                        Value::NativeFunction { .. } => "NativeFunction",
                    };
                    Ok(mk_string(type_name.to_string()))
                } else {
                    Err("type() expects one argument".to_string())
                }
            }
        ));
        h
    }));

    /*
    env.set_var("thread", mk_object({
        let mut props: HashMap<String, Value> = HashMap::new();
        let thread_manager_clone = thread_manager.clone();

        props.insert(
            "start".to_string(),
            mk_native_fn(
                "thread_start".to_string(),
                move |vm: &mut VM, args: Vec<Value>| {
                    if args.is_empty() {
                        return Err("thread.start: fonction attendue comme argument".to_string());
                    }

                    let func_arg = &args[0];

                    match func_arg {
                        Value::Function(name, params, start, end) => {
                            let func_name = name.clone();
                            let func_name_for_result = name.clone();
                            let func_start = *start;
                            let func_end = *end;
                            let func_params = params.clone();
                            let global_vars = vm.globals.clone();

                            let handle = thread::spawn(move || {
                                let mut thread_vm = VM::new();

                                thread_vm.globals = global_vars;

                                if let Some(Value::NativeFunction { func: log_func, .. }) = thread_vm.globals.get("log") {
                                    let log_func = log_func.clone();
                                    match log_func(&mut thread_vm, vec![Value::String(format!("Thread {} is running!", func_name))]) {
                                        Ok(_) => println!("Thread {} log successful", func_name),
                                        Err(e) => eprintln!("Thread {} log failed: {}", func_name, e),
                                    }
                                }

                            });

                            thread_manager_clone.handles.lock().unwrap().push(handle);
                            Ok(mk_string(format!("Thread {} started", func_name_for_result)))
                        }

                        Value::NativeFunction { name, func } => {
                            let func_name = name.clone();
                            let func_name_for_result = name.clone();
                            let func_clone = func.clone();
                            let global_vars = vm.globals.clone();

                            let handle = thread::spawn(move || {
                                let mut thread_vm = VM::new();
                                thread_vm.globals = global_vars;

                                match func_clone(&mut thread_vm, Vec::new()) {
                                    Ok(_) => println!("Native thread {} completed", func_name),
                                    Err(e) => eprintln!("Native thread {} failed: {}", func_name, e),
                                }
                            });

                            thread_manager_clone.handles.lock().unwrap().push(handle);
                            Ok(mk_string(format!("Native thread {} started", func_name_for_result)))
                        }

                        _ => {
                            Err("thread.start: argument doit être une fonction".to_string())
                        }
                    }
                }
            )
        );

        let thread_manager_wait = thread_manager.clone();
        props.insert(
            "wait".to_string(),
            mk_native_fn(
                "thread_wait".to_string(),
                move |_vm: &mut VM, _args: Vec<Value>| {
                    thread_manager_wait.wait_all();
                    Ok(mk_null())
                }
            )
        );

        props
    }));
     */
}



pub fn mk_null() -> Value {
    Value::Null
}

pub fn mk_bool(b: bool) -> Value {
    Value::Boolean(b)
}

pub fn mk_number_int(n: i64) -> Value {
    Value::Integer(n)
}

pub fn mk_number_float(n: f64) -> Value {
    Value::Float(n)
}

pub fn mk_string(value: String) -> Value {
    Value::String(value)
}

pub fn mk_array(elements: Vec<Value>) -> Value {
    Value::Array(elements)
}

pub fn mk_object(properties: HashMap<String, Value>) -> Value {
    Value::Object(properties)
}

pub fn mk_native_fn<F>(name: String, func: F) -> Value
where
    F: Fn(&mut VM, Vec<Value>) -> Result<Value, String> + Send + Sync + 'static,
{
    Value::NativeFunction {
        name,
        func: Arc::new(func),
    }
}

pub fn mk_fn(func: Arc<dyn Fn(&mut VM, Vec<Value>) -> Result<Value, String> + Send + Sync>) -> Value {
    Value::NativeFunction {
        name: "".to_string(),
        func,
    }
}