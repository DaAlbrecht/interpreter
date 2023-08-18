use std::collections::HashMap;

use super::object::{Object, TypeName};

pub fn get_builtins() -> HashMap<String, Object> {
    let mut builtins = HashMap::new();
    builtins.insert("len".to_string(), Object::BuiltinFunction(len));
    builtins.insert("first".to_string(), Object::BuiltinFunction(first));
    builtins.insert("last".to_string(), Object::BuiltinFunction(last));
    builtins.insert("rest".to_string(), Object::BuiltinFunction(rest));
    builtins.insert("push".to_string(), Object::BuiltinFunction(push));
    builtins.insert("puts".to_string(), Object::BuiltinFunction(puts));
    builtins
}

pub fn len(args: Option<Vec<Object>>) -> Object {
    match args {
        Some(args) => match args.len() {
            1 => match &args[0] {
                Object::String(s) => Object::Int(s.len() as i64),
                Object::Array(a) => Object::Int(a.len() as i64),
                _ => Object::Error(format!(
                    "argument to `len` not supported, got {}",
                    args[0].type_name()
                )),
            },
            _ => Object::Error(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            )),
        },
        None => Object::Error("wrong number of arguments. got=0, want=1".to_string()),
    }
}

pub fn first(args: Option<Vec<Object>>) -> Object {
    match args {
        Some(args) => match args.len() {
            1 => match args[0].clone() {
                Object::Array(a) => a.first().unwrap_or(&Object::Null).clone(),
                _ => Object::Error(format!(
                    "argument to `first` must be ARRAY, got {}",
                    args[0].type_name()
                )),
            },
            _ => Object::Error(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            )),
        },
        None => Object::Error("wrong number of arguments. got=0, want=1".to_string()),
    }
}

pub fn last(args: Option<Vec<Object>>) -> Object {
    match args {
        Some(args) => match args.len() {
            1 => match args[0].clone() {
                Object::Array(a) => a.last().unwrap_or(&Object::Null).clone(),
                _ => Object::Error(format!(
                    "argument to `last` must be ARRAY, got {}",
                    args[0].type_name()
                )),
            },
            _ => Object::Error(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            )),
        },
        None => Object::Error("wrong number of arguments. got=0, want=1".to_string()),
    }
}

pub fn rest(args: Option<Vec<Object>>) -> Object {
    match args {
        Some(args) => match args.len() {
            1 => match args[0].clone() {
                Object::Array(a) => match a.len() {
                    0 => Object::Null,
                    _ => Object::Array(a[1..].to_vec()),
                },
                _ => Object::Error(format!(
                    "argument to `rest` must be ARRAY, got {}",
                    args[0].type_name()
                )),
            },
            _ => Object::Error(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            )),
        },
        None => Object::Error("wrong number of arguments. got=0, want=1".to_string()),
    }
}

pub fn push(args: Option<Vec<Object>>) -> Object {
    match args {
        Some(args) => match args.len() {
            2 => match args[0].clone() {
                Object::Array(mut a) => {
                    a.push(args[1].clone());
                    Object::Array(a)
                }
                _ => Object::Error(format!(
                    "argument to `push` must be ARRAY, got {}",
                    args[0].type_name()
                )),
            },
            _ => Object::Error(format!(
                "wrong number of arguments. got={}, want=2",
                args.len()
            )),
        },
        None => Object::Error("wrong number of arguments. got=0, want=2".to_string()),
    }
}

pub fn puts(args: Option<Vec<Object>>) -> Object {
    match args {
        Some(args) => {
            for arg in args {
                println!("{}", arg);
            }
            Object::Null
        }
        None => Object::Error("wrong number of arguments. got=0, want=1".to_string()),
    }
}
