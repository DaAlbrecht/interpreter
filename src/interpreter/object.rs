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
    Array(Vec<Object>),
}

pub trait TypeName {
    fn type_name(&self) -> String;
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
            Object::Int(int) => write!(f, "{}", int),
            Object::String(string) => write!(f, "{}", string),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::ReturnValue(obj) => write!(f, "{}", obj),
            Object::Null => write!(f, "null"),
            Object::Error(msg) => write!(f, "error: {}", msg),
            Object::FunctionLiteral(func) => {
                let mut params = String::new();
                if let Some(parameters) = &func.parameters {
                    for i in 0..parameters.len() {
                        params.push_str(&parameters[i]);
                        if i != parameters.len() - 1 {
                            params.push_str(", ");
                        }
                    }
                }
                write!(f, "fn({}) {{\n{}\n}}", params, func.body)
            }
            Object::BuiltinFunction(_) => write!(f, "BUILTIN FUNCTION"),
            Object::Array(elements) => {
                let mut elements_str = String::new();
                for i in 0..elements.len() {
                    elements_str.push_str(&elements[i].to_string());
                    if i != elements.len() - 1 {
                        elements_str.push_str(", ");
                    }
                }
                write!(f, "[{}]", elements_str)
            }
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut params = String::new();
        if let Some(parameters) = &self.parameters {
            for param in parameters {
                params.push_str(&param);
                params.push_str(", ");
            }
        }
        write!(f, "fn({}) {{\n{}\n}}", params, self.body)
    }
}

//----------------------------------type_name----------------------------------//

impl TypeName for Object {
    fn type_name(&self) -> String {
        match self {
            Object::Int(_) => "INTEGER".to_string(),
            Object::String(_) => "STRING".to_string(),
            Object::Boolean(_) => "BOOLEAN".to_string(),
            Object::Null => "NULL".to_string(),
            Object::ReturnValue(obj) => obj.type_name(),
            Object::Error(msg) => format!("ERROR: {}", msg),
            Object::FunctionLiteral(func) => {
                let mut params = String::new();
                if let Some(parameters) = &func.parameters {
                    for i in 0..parameters.len() {
                        params.push_str(&parameters[i]);
                        if i != parameters.len() - 1 {
                            params.push_str(", ");
                        }
                    }
                }
                format!("fn({}) {{\n{}\n}}", params, func.body)
            }
            Object::BuiltinFunction(_) => "BUILTIN FUNCTION".to_string(),
            Object::Array(element) => {
                let mut elements_str = String::new();
                for i in 0..element.len() {
                    elements_str.push_str(&element[i].to_string());
                    if i != element.len() - 1 {
                        elements_str.push_str(", ");
                    }
                }
                format!("[{}]", elements_str)
            }
        }
    }
}
