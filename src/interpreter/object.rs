use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(i64),
    Boolean(bool),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(int) => write!(f, "{}", int),
            Object::Boolean(bool) => write!(f, "{}", bool),
            Object::Null => write!(f, "null"),
        }
    }
}
