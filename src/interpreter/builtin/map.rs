use std::cell::RefCell;
use std::rc::Rc;

use crate::interpreter::object::*;

macro_rules! validate_args_len {
    ($args:expr, $expected:expr, $line:expr) => {
        if $args.len() != $expected {
            return Some(Err(RuntimeError::NotEnoughArgs {
                got: $args.len(),
                want: $expected,
                line: $line,
            }));
        }
    };
}

pub fn keys(bound: Object, args: Vec<Object>, line: usize) -> Option<EvalResult> {
    validate_args_len!(args, 0, line);
    if let Object::Map(obj) = bound {
        Some(Ok(Object::Array(Rc::new(RefCell::new(
            obj.borrow().keys().cloned().collect(),
        )))))
    } else {
        None
    }
}

pub fn values(bound: Object, args: Vec<Object>, line: usize) -> Option<EvalResult> {
    validate_args_len!(args, 0, line);
    if let Object::Map(obj) = bound {
        Some(Ok(Object::Array(Rc::new(RefCell::new(
            obj.borrow().values().cloned().collect(),
        )))))
    } else {
        None
    }
}
