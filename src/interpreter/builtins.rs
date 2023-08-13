use std::collections::HashMap;

use super::object::Object;

pub fn get_builtins() -> HashMap<String, Object> {
    let mut builtins = HashMap::new();
    builtins.insert("len".to_string(), Object::BuiltinFunction(len));
    builtins
}

pub fn len(args: Option<Vec<Object>>) -> Object {
    if let Some(args) = args {
        if args.len() != 1 {
            return Object::Error(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            ));
        } else {
            match &args[0] {
                Object::String(s) => return Object::Int(s.len() as i64),
                _ => {
                    return Object::Error(format!(
                        "argument to `len` not supported, got {}",
                        args[0]
                    ))
                }
            }
        }
    }
    Object::Error("wrong number of arguments. got=0, want=1".to_string())
}
