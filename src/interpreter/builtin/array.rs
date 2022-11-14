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
