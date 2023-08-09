use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::object::Object;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed_environment(outer: Rc<RefCell<Environment>>) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(Box::new(outer.borrow().clone())),
        }
    }

    pub fn get(&self, name: &str) -> Option<(Object, bool)> {
        match self.store.get(name) {
            Some(obj) => Some((obj.clone(), true)),
            None => match &self.outer {
                Some(outer) => outer.get(name),
                None => None,
            },
        }
    }

    pub fn set(&mut self, name: &str, value: Object) -> Option<Object> {
        self.store.insert(name.to_string(), value)
    }
}
