use super::object::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct Env {
    env: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Rc<RefCell<Env>>) -> Self {
        let mut env = Env::new();
        env.outer = Some(outer);
        env
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.env.get(name) {
            Some(value) => Some(value.clone()),
            None => match self.outer {
                Some(ref outer) => outer.borrow().get(name),
                None => None,
            },
        }
    }

    pub fn set(&mut self, name: String, val: Object) {
        self.env.insert(name, val);
    }

    pub fn symbols(&self) -> HashMap<String, Object> {
        let mut symbols = HashMap::new();
        for (name, object) in self.env.clone() {
            // TODO: Public and private components and functions
            match object {
                Object::Function { .. } => drop(symbols.insert(name, object)),
                Object::Component(_) => drop(symbols.insert(name, object)),
                _ => {}
            };
        }
        symbols
    }
}

impl PartialEq for Env {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}
