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

pub fn println(args: Vec<Object>, _line: usize) -> EvalResult {
    println!(
        "{}",
        args.into_iter()
            .map(|arg| {
                if let Object::String(s) = arg {
                    format!("\"{s}\" ")
                } else {
                    arg.to_string() + " "
                }
            })
            .collect::<String>()
    );
    Ok(Object::Nil)
}

pub fn scurry_type(args: Vec<Object>, line: usize) -> EvalResult {
    validate_args_len!(args, 1, line);
    Ok(Object::String(args[0].scurry_type().to_string()))
}
