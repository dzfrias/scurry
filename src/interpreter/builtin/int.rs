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
    if let Object::Int(obj) = bound {
        Some(Ok(Object::Int(obj.abs())))
    } else {
        None
    }
}

pub fn to_float(bound: Object, args: Vec<Object>, line: usize) -> Option<EvalResult> {
    validate_args_len!(args, 0, line);
    if let Object::Int(obj) = bound {
        Some(Ok(Object::Float(obj as f32)))
    } else {
        None
    }
}
