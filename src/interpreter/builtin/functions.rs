use crate::interpreter::object::*;

macro_rules! validate_args_len {
    ($args:expr, $expected:expr, $line:expr) => {
        if $args.len() != $expected {
            return Err(RuntimeError::NotEnoughArgs {
                got: $args.len(),
                want: $expected,
                line: $line,
            });
        }
    };
}

pub fn scurry_type(args: Vec<Object>, line: usize) -> EvalResult {
    validate_args_len!(args, 1, line);
    Ok(Object::String(args[0].scurry_type().to_string()))
}

pub fn truthy(args: Vec<Object>, line: usize) -> EvalResult {
    validate_args_len!(args, 1, line);
    Ok(Object::Bool(args[0].is_truthy()))
}
