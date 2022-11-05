use super::object::Object;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct Env {
    env: HashMap<String, Object>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        self.env.get(name).cloned()
    }

    pub fn set(&mut self, name: String, val: Object) {
        self.env.insert(name, val);
    }
}
