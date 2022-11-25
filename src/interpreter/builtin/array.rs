use crate::ast::{AstType, TypeAnnotation};
use crate::interpreter::object::*;
use std::cell::RefCell;

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
    if let Object::Array(obj) = bound {
        Some(Ok(Object::Int(obj.borrow().len() as i32)))
    } else {
        None
    }
}

pub fn push(bound: Object, args: Vec<Object>, line: usize) -> Option<EvalResult> {
    validate_args_len!(args, 1, line);
    if let Object::Array(obj) = bound {
        obj.borrow_mut().push(args[0].clone());
        Some(Ok(Object::Nil))
    } else {
        None
    }
}

pub fn pop(bound: Object, args: Vec<Object>, line: usize) -> Option<EvalResult> {
    validate_args_len!(args, 0, line);
    if let Object::Array(obj) = bound {
        Some(Ok(obj.borrow_mut().pop().unwrap_or(Object::Nil)))
    } else {
        None
    }
}

pub fn contains(bound: Object, args: Vec<Object>, line: usize) -> Option<EvalResult> {
    validate_args_len!(args, 1, line);
    if let Object::Array(obj) = bound {
        Some(Ok(Object::Bool(obj.borrow_mut().contains(&args[0]))))
    } else {
        None
    }
}

pub fn insert(bound: Object, args: Vec<Object>, line: usize) -> Option<EvalResult> {
    validate_args_len!(args, 2, line);
    let Object::Int(idx) = args[1] else {
        return Some(Err(RuntimeError::WrongArgType { name: "index".to_owned(), expected: TypeAnnotation::from_iter([AstType::Int]), got: args[1].scurry_type().into(), line }));
    };
    if let Object::Array(obj) = bound {
        let len = obj.borrow().len() + 1;
        if idx > len as i32 {
            return Some(Err(RuntimeError::IndexOutOfRange {
                obj: Object::Array(obj),
                index: idx,
                line,
            }));
        }
        obj.borrow_mut().insert(
            idx.try_into().unwrap_or(len - idx.unsigned_abs() as usize),
            args[0].clone(),
        );
        Some(Ok(Object::Nil))
    } else {
        None
    }
}

pub fn remove(bound: Object, args: Vec<Object>, line: usize) -> Option<EvalResult> {
    validate_args_len!(args, 1, line);
    let Object::Int(idx) = args[0] else {
        return Some(Err(RuntimeError::WrongArgType { name: "index".to_owned(), expected: TypeAnnotation::from_iter([AstType::Int]), got: args[0].scurry_type().into(), line }));
    };
    if let Object::Array(obj) = bound {
        let len = obj.borrow().len();
        if idx > (len as i32) - 1 {
            return Some(Err(RuntimeError::IndexOutOfRange {
                obj: Object::Array(obj),
                index: idx,
                line,
            }));
        }
        obj.borrow_mut()
            .remove(idx.try_into().unwrap_or(len - idx.unsigned_abs() as usize));
        Some(Ok(Object::Nil))
    } else {
        None
    }
}

pub fn concat(bound: Object, args: Vec<Object>, line: usize) -> Option<EvalResult> {
    validate_args_len!(args, 1, line);
    let Object::Array(arr) = &args[0] else {
        return Some(Err(RuntimeError::WrongArgType { name: "array".to_owned(), expected: TypeAnnotation::from_iter([AstType::Array]), got: args[0].scurry_type().into(), line }));
    };
    if let Object::Array(obj) = bound {
        let mut new_arr = <RefCell<Vec<Object>>>::clone(arr).into_inner();
        obj.borrow_mut().append(&mut new_arr);
        Some(Ok(Object::Nil))
    } else {
        None
    }
}
