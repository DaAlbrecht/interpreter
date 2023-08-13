use std::{cell::RefCell, fmt::Display, rc::Rc};

use super::{ast::BlockStatement, environment::Environment};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(i64),
    String(String),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
    FunctionLiteral(Function),
    BuiltinFunction(fn(Option<Vec<Object>>) -> Object),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub parameters: Option<Vec<String>>,
    pub body: BlockStatement,
    pub env: Rc<RefCell<Environment>>,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(_) => write!(f, "INTEGER"),
            Object::String(_) => write!(f, "STRING"),
            Object::Boolean(_) => write!(f, "BOOLEAN"),
            Object::Null => write!(f, "NULL"),
            Object::ReturnValue(obj) => write!(f, "{}", obj),
            Object::Error(msg) => write!(f, "ERROR: {}", msg),
            Object::FunctionLiteral(func) => {
                let mut params = String::new();
                if let Some(parameters) = &func.parameters {
                    for param in parameters {
                        params.push_str(&param);
                        params.push_str(", ");
                    }
                }
                write!(f, "fn({}) {{\n{}\n}}", params, func.body)
            }
            Object::BuiltinFunction(_) => write!(f, "BUILTIN FUNCTION"),
        }
    }
}
