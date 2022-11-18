use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::{AstType, TypeAnnotation};
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

pub fn len(bound: Object, args: Vec<Object>, line: usize) -> Option<EvalResult> {
    validate_args_len!(args, 0, line);
    if let Object::String(obj) = bound {
        Some(Ok(Object::Int(obj.len() as i32)))
    } else {
        None
    }
}

pub fn trim(bound: Object, args: Vec<Object>, line: usize) -> Option<EvalResult> {
    validate_args_len!(args, 0, line);
    if let Object::String(obj) = bound {
        Some(Ok(Object::String(obj.trim().to_owned())))
    } else {
        None
    }
}

pub fn starts_with(bound: Object, args: Vec<Object>, line: usize) -> Option<EvalResult> {
    validate_args_len!(args, 1, line);
    let Object::String(string) = &args[0] else {
        return Some(Err(RuntimeError::WrongArgType { name: "string".to_owned(), expected: TypeAnnotation::from_iter([AstType::String]), got: args[0].scurry_type().into(), line }));
    };
    if let Object::String(obj) = bound {
        Some(Ok(Object::Bool(obj.starts_with(string))))
    } else {
        None
    }
}

pub fn ends_with(bound: Object, args: Vec<Object>, line: usize) -> Option<EvalResult> {
    validate_args_len!(args, 1, line);
    let Object::String(string) = &args[0] else {
        return Some(Err(RuntimeError::WrongArgType { name: "string".to_owned(), expected: TypeAnnotation::from_iter([AstType::String]), got: args[0].scurry_type().into(), line }));
    };
    if let Object::String(obj) = bound {
        Some(Ok(Object::Bool(obj.ends_with(string))))
    } else {
        None
    }
}

pub fn substring(bound: Object, args: Vec<Object>, line: usize) -> Option<EvalResult> {
    validate_args_len!(args, 2, line);
    let Object::Int(start) = args[0] else {
        return Some(Err(RuntimeError::WrongArgType { name: "start".to_owned(), expected: TypeAnnotation::from_iter([AstType::Int]), got: args[0].scurry_type().into(), line }));
    };
    let Object::Int(end) = args[1] else {
        return Some(Err(RuntimeError::WrongArgType { name: "end".to_owned(), expected: TypeAnnotation::from_iter([AstType::Int]), got: args[1].scurry_type().into(), line }));
    };
    if let Object::String(obj) = bound {
        if start >= end {
            return Some(Err(RuntimeError::IndexOutOfRange {
                obj: Object::String(obj),
                index: start,
                line,
            }));
        }
        if start < 0 || end < 0 {
            return Some(Err(RuntimeError::IndexOutOfRange {
                obj: Object::String(obj),
                index: start,
                line,
            }));
        }
        Some(Ok(Object::String(
            obj.chars()
                .skip(start as usize)
                .take(end as usize)
                .collect(),
        )))
    } else {
        None
    }
}

pub fn split(bound: Object, args: Vec<Object>, line: usize) -> Option<EvalResult> {
    validate_args_len!(args, 1, line);
    let Object::String(delim) = &args[0] else {
        return Some(Err(RuntimeError::WrongArgType { name: "delim".to_owned(), expected: TypeAnnotation::from_iter([AstType::String]), got: args[0].scurry_type().into(), line }));
    };
    if let Object::String(obj) = bound {
        Some(Ok(Object::Array(Rc::new(RefCell::new(
            obj.split(delim)
                .map(|s| Object::String(s.to_owned()))
                .collect(),
        )))))
    } else {
        None
    }
}
