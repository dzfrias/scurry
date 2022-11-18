use crate::ast::{AstType, TypeAnnotation};
use crate::interpreter::object::*;
use std::cell::RefCell;
use std::rc::Rc;

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

pub fn remove(bound: Object, args: Vec<Object>, line: usize) -> Option<EvalResult> {
    validate_args_len!(args, 1, line);
    if let Object::Map(obj) = bound {
        obj.borrow_mut().remove(&args[0]);
        Some(Ok(Object::Nil))
    } else {
        None
    }
}

pub fn merge(bound: Object, args: Vec<Object>, line: usize) -> Option<EvalResult> {
    validate_args_len!(args, 1, line);
    let Object::Map(map) = &args[0] else {
        return Some(Err(RuntimeError::WrongArgType { name: "map".to_owned(), expected: TypeAnnotation::from_iter([AstType::Map]), got: args[0].scurry_type().into(), line }));
    };
    if let Object::Map(obj) = bound {
        obj.borrow_mut().extend(map.borrow().clone());
        Some(Ok(Object::Nil))
    } else {
        None
    }
}
