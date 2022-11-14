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

pub fn abs(bound: Object, args: Vec<Object>, line: usize) -> Option<EvalResult> {
    validate_args_len!(args, 0, line);
    if let Object::Float(obj) = bound {
        Some(Ok(Object::Float(obj.abs())))
    } else {
        None
    }
}

pub fn to_int(bound: Object, args: Vec<Object>, line: usize) -> Option<EvalResult> {
    validate_args_len!(args, 0, line);
    if let Object::Float(obj) = bound {
        Some(Ok(Object::Int(obj as i32)))
    } else {
        None
    }
}
