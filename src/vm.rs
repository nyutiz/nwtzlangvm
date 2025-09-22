#![allow(dead_code)]

use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::{io};
use crate::bytecode::{Chunk, Instruction};
use nwtzlang::types::{ValueType, NullVal, IntegerVal, BooleanVal, ObjectVal, ArrayVal};
use nwtzlang::runtime::RuntimeVal;
use nwtzlang::ast::StringVal;
use std::sync::{Arc, Mutex};

#[derive(Clone)]
pub enum Value {
    Null,
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Object(HashMap<String, Value>),
    Array(Vec<Value>),
    Function(String, Vec<String>, usize, usize),
    NativeFunction {
        name: String,
        func: Arc<dyn Fn(&mut VM, Vec<Value>) -> Result<Value, String> + Send + Sync>
    },
}


impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "Null"),
            Value::Integer(i) => write!(f, "Integer({})", i),
            Value::Float(fl) => write!(f, "Float({})", fl),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::String(s) => write!(f, "String({:?})", s),
            Value::Object(obj) => write!(f, "Object({:?})", obj),
            Value::Array(arr) => write!(f, "Array({:?})", arr),
            Value::Function(name, params, start, end) => {
                write!(f, "Function({}, {:?}, {}, {})", name, params, start, end)
            },
            Value::NativeFunction { name, .. } => {
                write!(f, "NativeFunction {{ name: {} }}", name)
            },

        }
    }
}

impl Value {

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Null => "Null",
            Value::Boolean(_) => "Boolean",
            Value::Integer(_) => "Integer",
            Value::Float(_) => "Float",
            Value::String(_) => "String",
            Value::Array(_) => "Array",
            Value::Object(_) => "Object",
            Value::Function { .. } => "Function",
            Value::NativeFunction { .. } => "NativeFunction",
        }
    }
    pub fn to_runtime_val(&self) -> Box<dyn RuntimeVal + Send + Sync> {
        match self {
            Value::Null => Box::new(NullVal { r#type: Some(ValueType::Null) }),
            Value::Integer(i) => Box::new(IntegerVal {
                r#type: Some(ValueType::Integer),
                value: *i as f64
            }),
            Value::Float(f) => Box::new(IntegerVal {
                r#type: Some(ValueType::Integer),
                value: *f
            }),
            Value::Boolean(b) => Box::new(BooleanVal {
                r#type: Some(ValueType::Boolean),
                value: *b
            }),
            Value::String(s) => Box::new(StringVal {
                r#type: Some(ValueType::String),
                kind: nwtzlang::ast::NodeType::StringLiteral,
                value: s.clone(),
            }),
            Value::Object(props) => {
                let mut runtime_props = HashMap::new();
                for (key, value) in props {
                    runtime_props.insert(key.clone(), value.to_runtime_val());
                }
                Box::new(ObjectVal {
                    r#type: Some(ValueType::Object),
                    properties: Arc::new(Mutex::new(runtime_props)),
                })
            },
            Value::Array(elements) => {
                let runtime_elements: Vec<Box<dyn RuntimeVal + Send + Sync>> =
                    elements.iter().map(|v| v.to_runtime_val()).collect();
                Box::new(ArrayVal {
                    r#type: Some(ValueType::Array),
                    elements: Arc::new(Mutex::new(runtime_elements)),
                })
            },
            Value::Function(_, _, _, _) => {
                Box::new(NullVal { r#type: Some(ValueType::Function) })
            }
            Value::NativeFunction { name: _, func: _ } => {
                Box::new(NullVal { r#type: Some(ValueType::Function) })
            }
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Null => false,
            Value::Boolean(b) => *b,
            Value::Integer(i) => *i != 0,
            Value::Float(f) => *f != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Array(arr) => !arr.is_empty(),
            Value::Object(_) => true,
            Value::Function(_, _, _, _) => true,
            Value::NativeFunction { name: _, func: _ } => true,
        }
    }
}

pub struct VM {
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    locals: Vec<HashMap<String, Value>>,
    ip: usize,
    call_stack: Vec<usize>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            globals: HashMap::new(),
            locals: vec![HashMap::new()],
            ip: 0,
            call_stack: Vec::new(),
        }
    }

    pub fn set_var(&mut self, name: impl Into<String>, value: Value) {
        self.globals.insert(name.into(), value);
    }

    pub fn make_global_env(&mut self) {
        self.set_var("null", Value::Null);
        self.set_var("true", Value::Boolean(true));
        self.set_var("false", Value::Boolean(false));

        self.set_var("log", Value::NativeFunction {
            name: "log".to_string(),
            func: Arc::new(|vm: &mut VM, args: Vec<Value>| {
                for v in args {
                    println!("{}", vm.value_to_string(&v));
                }
                Ok(Value::Null)
            }),
        });

        self.set_var("time", Value::NativeFunction {
            name: "time".to_string(),
            func: Arc::new(|_vm: &mut VM, _args: Vec<Value>| {
                use std::time::{SystemTime, UNIX_EPOCH};
                let secs = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis() as f64 / 1000.0;
                Ok(Value::Float(secs))
            }),
        });

        self.set_var("sleep", Value::NativeFunction {
            name: "sleep".to_string(),
            func: Arc::new(|_vm: &mut VM, args: Vec<Value>| {
                let secs = match args.get(0) {
                    Some(Value::Integer(i)) => *i as f64,
                    Some(Value::Float(f)) => *f,
                    _ => return Err("sleep: expected number".into()),
                };
                std::thread::sleep(std::time::Duration::from_secs_f64(secs));
                Ok(Value::Null)
            }),
        });

        self.set_var("input", Value::NativeFunction {
            name: "input".to_string(),
            func: Arc::new(|_vm: &mut VM, _args: Vec<Value>| {
                let mut s = String::new();
                io::stdin().read_line(&mut s).map_err(|e| e.to_string())?;
                Ok(Value::String(s.trim_end().to_string()))
            }),
        });

        self.set_var(
            "system",
            Value::Object ({
                let mut h:HashMap<String, Value> = HashMap::new();

                h.insert("value_type".to_string(), Value::NativeFunction{
                    name: "type".to_string(),
                    func: Arc::new(|_vm: &mut VM, args: Vec<Value>| {
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
                            Ok(Value::String(type_name.to_string()))
                        } else {
                            Err("type() expects one argument".to_string())
                        }
                    }),
                });

                h
            })
        );
    }

    pub fn execute(&mut self, chunk: &Chunk) -> Result<Value, String> {
        self.ip = 0;

        while self.ip < chunk.instructions.len() {
            let instruction = &chunk.instructions[self.ip];

            match instruction {
                Instruction::LoadNull => {
                    self.stack.push(Value::Null);
                }

                Instruction::LoadInt(value) => {
                    self.stack.push(Value::Integer(*value));
                }

                Instruction::LoadFloat(value) => {
                    self.stack.push(Value::Float(*value));
                }

                Instruction::LoadBool(value) => {
                    self.stack.push(Value::Boolean(*value));
                }

                Instruction::LoadString(value) => {
                    self.stack.push(Value::String(value.clone()));
                }

                Instruction::LoadVar(name) => {
                    if let Some(value) = self.get_variable(name) {
                        self.stack.push(value);
                    } else {
                        return Err(format!("Undefined variable: {}", name));
                    }
                }

                Instruction::Duplicate => {
                    if let Some(value) = self.stack.last() {
                        self.stack.push(value.clone());
                    } else {
                        return Err("Stack underflow in Duplicate".to_string());
                    }
                }


                Instruction::StoreVar(name) => {
                    if let Some(value) = self.stack.pop() {
                        self.set_variable(name.clone(), value);
                    } else {
                        return Err("Stack underflow in StoreVar".to_string());
                    }
                }



                Instruction::Add => {
                    let (b, a) = self.pop_two()?;
                    match (a, b) {
                        (Value::Integer(a), Value::Integer(b)) => {
                            self.stack.push(Value::Integer(a + b));
                        }
                        (Value::Float(a), Value::Float(b)) => {
                            self.stack.push(Value::Float(a + b));
                        }
                        (Value::Integer(a), Value::Float(b)) => {
                            self.stack.push(Value::Float(a as f64 + b));
                        }
                        (Value::Float(a), Value::Integer(b)) => {
                            self.stack.push(Value::Float(a + b as f64));
                        }
                        (Value::String(a), Value::String(b)) => {
                            self.stack.push(Value::String(a + &b));
                        }
                        _ => return Err("Invalid types for addition".to_string()),
                    }
                }

                Instruction::Sub => {
                    let (b, a) = self.pop_two()?;
                    match (a, b) {
                        (Value::Integer(a), Value::Integer(b)) => {
                            self.stack.push(Value::Integer(a - b));
                        }
                        (Value::Float(a), Value::Float(b)) => {
                            self.stack.push(Value::Float(a - b));
                        }
                        (Value::Integer(a), Value::Float(b)) => {
                            self.stack.push(Value::Float(a as f64 - b));
                        }
                        (Value::Float(a), Value::Integer(b)) => {
                            self.stack.push(Value::Float(a - b as f64));
                        }
                        _ => return Err("Invalid types for subtraction".to_string()),
                    }
                }

                Instruction::Mul => {
                    let (b, a) = self.pop_two()?;
                    match (a, b) {
                        (Value::Integer(a), Value::Integer(b)) => {
                            self.stack.push(Value::Integer(a * b));
                        }
                        (Value::Float(a), Value::Float(b)) => {
                            self.stack.push(Value::Float(a * b));
                        }
                        (Value::Integer(a), Value::Float(b)) => {
                            self.stack.push(Value::Float(a as f64 * b));
                        }
                        (Value::Float(a), Value::Integer(b)) => {
                            self.stack.push(Value::Float(a * b as f64));
                        }
                        _ => return Err("Invalid types for multiplication".to_string()),
                    }
                }

                Instruction::Div => {
                    let (b, a) = self.pop_two()?;
                    match (a, b) {
                        (Value::Integer(a), Value::Integer(b)) => {
                            if b == 0 {
                                return Err("Division by zero".to_string());
                            }
                            self.stack.push(Value::Integer(a / b));
                        }
                        (Value::Float(a), Value::Float(b)) => {
                            if b == 0.0 {
                                return Err("Division by zero".to_string());
                            }
                            self.stack.push(Value::Float(a / b));
                        }
                        (Value::Integer(a), Value::Float(b)) => {
                            if b == 0.0 {
                                return Err("Division by zero".to_string());
                            }
                            self.stack.push(Value::Float(a as f64 / b));
                        }
                        (Value::Float(a), Value::Integer(b)) => {
                            if b == 0 {
                                return Err("Division by zero".to_string());
                            }
                            self.stack.push(Value::Float(a / b as f64));
                        }
                        _ => return Err("Invalid types for division".to_string()),
                    }
                }

                Instruction::Equal => {
                    let (b, a) = self.pop_two()?;
                    let result = self.values_equal(&a, &b);
                    self.stack.push(Value::Boolean(result));
                }

                Instruction::Greater => {
                    let (b, a) = self.pop_two()?;
                    let result = self.compare_values(&a, &b)? > 0;
                    self.stack.push(Value::Boolean(result));
                }

                Instruction::Less => {
                    let (b, a) = self.pop_two()?;
                    let result = self.compare_values(&a, &b)? < 0;
                    self.stack.push(Value::Boolean(result));
                }

                Instruction::Jump(addr) => {
                    self.ip = *addr;
                    continue;
                }

                Instruction::JumpIfFalse(addr) => {
                    if let Some(value) = self.stack.pop() {
                        if !value.is_truthy() {
                            self.ip = *addr;
                            continue;
                        }
                    } else {
                        return Err("Stack underflow in JumpIfFalse".to_string());
                    }
                }

                Instruction::JumpIfTrue(addr) => {
                    if let Some(value) = self.stack.pop() {
                        if value.is_truthy() {
                            self.ip = *addr;
                            continue;
                        }
                    } else {
                        return Err("Stack underflow in JumpIfTrue".to_string());
                    }
                }

                Instruction::Call(arg_count) => {
                    let callee = self.stack.pop().ok_or("Stack underflow in Call")?;

                    match callee {
                        Value::Function(name, params, start, _end) => {
                            if params.len() != *arg_count {
                                return Err(format!(
                                    "Function {} expects {} arguments, got {}",
                                    name, params.len(), arg_count
                                ));
                            }
                            let mut new_locals = std::collections::HashMap::new();
                            for param in params.iter().rev() {
                                let arg = self.stack.pop().ok_or("Stack underflow in function call")?;
                                new_locals.insert(param.clone(), arg);
                            }
                            self.locals.push(new_locals);
                            self.call_stack.push(self.ip + 1);
                            self.ip = start;
                            continue;
                        }

                        Value::NativeFunction { name: _, func } => {
                            let mut args = Vec::with_capacity(*arg_count);
                            for _ in 0..*arg_count {
                                args.push(self.stack.pop().ok_or("Stack underflow in native call")?);
                            }
                            args.reverse();

                            let ret = func(self, args)?;
                            self.stack.push(ret);
                        }

                        _ => return Err("Attempted to call non-function value".to_string()),
                    }
                }

                Instruction::Return => {
                    if let Some(return_addr) = self.call_stack.pop() {
                        self.locals.pop();
                        self.ip = return_addr;
                        continue;
                    } else {
                        break;
                    }
                }

                Instruction::DefineFunction(name, params, start, end) => {
                    let func = Value::Function(name.clone(), params.clone(), *start, *end);
                    self.globals.insert(name.clone(), func);
                }

                Instruction::CreateArray(size) => {
                    let mut elements = Vec::with_capacity(*size);
                    for _ in 0..*size {
                        if let Some(value) = self.stack.pop() {
                            elements.insert(0, value);
                        } else {
                            return Err("Stack underflow in CreateArray".to_string());
                        }
                    }
                    self.stack.push(Value::Array(elements));
                }

                Instruction::CreateObject => {
                    self.stack.push(Value::Object(HashMap::new()));
                }

                Instruction::Print => {
                    if let Some(value) = self.stack.pop() {
                        println!("{}", self.value_to_string(&value));
                    }
                }

                Instruction::Halt => {
                    break;
                }

                Instruction::SetProperty(ref key) => {
                    let value = self.stack.pop().ok_or("SetProperty: stack underflow (value)")?;
                    let obj   = self.stack.pop().ok_or("SetProperty: stack underflow (object)")?;

                    match obj {
                        Value::Object(mut map) => {
                            map.insert(key.clone(), value);
                            // Laisser l'objet sur la pile pour chaîner d'autres SetProperty
                            self.stack.push(Value::Object(map));
                        }
                        _ => return Err(format!("SetProperty: target is not an object (key = {})", key)),
                    }
                }

                Instruction::GetProperty(ref key) => {
                    let obj = self.stack.pop().ok_or("GetProperty: stack underflow (object)")?;
                    match obj {
                        Value::Object(map) => {
                            if let Some(v) = map.get(key).cloned() {
                                self.stack.push(v);
                            } else {
                                return Err(format!("Undefined property: {}", key));
                                // ou: self.stack.push(Value::Null);
                            }
                        }
                        _ => return Err(format!("GetProperty: target is not an object (key = {})", key)),
                    }
                }

                Instruction::SetIndex => {
                    let value = self.stack.pop().ok_or("SetIndex: underflow (value)")?;
                    let index = self.stack.pop().ok_or("SetIndex: underflow (index)")?;
                    let obj   = self.stack.pop().ok_or("SetIndex: underflow (object)")?;

                    match (obj, index) {
                        (Value::Array(mut arr), Value::Integer(i)) if i >= 0 => {
                            let i = i as usize;
                            if i < arr.len() { arr[i] = value; } else if i == arr.len() { arr.push(value); }
                            else { return Err("SetIndex: out of bounds".into()); }
                            self.stack.push(Value::Array(arr));
                        }
                        (Value::Object(mut map), Value::String(k)) => {
                            map.insert(k, value);
                            self.stack.push(Value::Object(map));
                        }
                        (o, i) => return Err(format!("SetIndex: unsupported (object={:?}, index={:?})", o, i)),
                    }
                }

                Instruction::DeclareVar(name, _type) => {
                    if let Some(value) = self.stack.pop() {
                        self.locals.last_mut().unwrap().insert(name.clone(), value);
                    } else {
                        return Err("Stack underflow in DeclareVar".to_string());
                    }
                }

                _ => {
                    return Err(format!("Unimplemented instruction: {:?}", instruction));
                }
            }

            self.ip += 1;
        }

        Ok(self.stack.pop().unwrap_or(Value::Null))
    }

    fn pop_two(&mut self) -> Result<(Value, Value), String> {
        let b = self.stack.pop().ok_or("Stack underflow")?;
        let a = self.stack.pop().ok_or("Stack underflow")?;
        Ok((b, a))
    }

    fn get_variable(&self, name: &str) -> Option<Value> {
        for locals in self.locals.iter().rev() {
            if let Some(value) = locals.get(name) {
                return Some(value.clone());
            }
        }

        self.globals.get(name).cloned()
    }

    fn set_variable(&mut self, name: String, value: Value) {
        for locals in self.locals.iter_mut().rev() {
            if locals.contains_key(&name) {
                locals.insert(name, value);
                return;
            }
        }

        self.globals.insert(name, value);
    }

    fn values_equal(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Null, Value::Null) => true,
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Integer(a), Value::Float(b)) => *a as f64 == *b,
            (Value::Float(a), Value::Integer(b)) => *a == *b as f64,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            _ => false,
        }
    }

    fn compare_values(&self, a: &Value, b: &Value) -> Result<i32, String> {
        match (a, b) {
            (Value::Integer(a), Value::Integer(b)) => Ok(a.cmp(b) as i32),
            (Value::Float(a), Value::Float(b)) => Ok(a.partial_cmp(b).unwrap() as i32),
            (Value::Integer(a), Value::Float(b)) => Ok((*a as f64).partial_cmp(b).unwrap() as i32),
            (Value::Float(a), Value::Integer(b)) => Ok(a.partial_cmp(&(*b as f64)).unwrap() as i32),
            (Value::String(a), Value::String(b)) => Ok(a.cmp(b) as i32),
            _ => Err("Cannot compare these types".to_string()),
        }
    }

    fn value_to_string(&self, value: &Value) -> String {
        match value {
            Value::Null => "null".to_string(),
            Value::Integer(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Boolean(b) => b.to_string(),
            Value::String(s) => s.clone(),
            Value::Array(arr) => {
                let elements: Vec<String> = arr.iter()
                    .map(|v| self.value_to_string(v))
                    .collect();
                format!("[{}]", elements.join(", "))
            }
            Value::Object(obj) => {
                let props: Vec<String> = obj.iter()
                    .map(|(k, v)| format!("{}: {}", k, self.value_to_string(v)))
                    .collect();
                format!("{{{}}}", props.join(", "))
            }
            Value::Function(name, _, _, _) => format!("<function {}>", name),
            Value::NativeFunction { name, func: _ } => format!("<native {}>", name),
        }
    }
}