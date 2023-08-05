use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(_) => write!(f, "INTEGER"),
            Object::Boolean(_) => write!(f, "BOOLEAN"),
            Object::Null => write!(f, "NULL"),
            Object::ReturnValue(obj) => write!(f, "{}", obj),
            Object::Error(msg) => write!(f, "ERROR: {}", msg),
        }
    }
}
