use super::object::*;

macro_rules! validate_args_len {
    ($args:expr, $expected:expr, $line:expr) => {
        if $args.len() != $expected {
            return Err(RuntimeError::NotEnoughArgs {
                got: $expected,
                want: $args.len(),
                line: $line,
            });
        }
    };
}

pub fn get_builtin(name: &str) -> Option<BuiltinFunc> {
    match name {
        "println" => Some(println),
        "type" => Some(scurry_type),
        _ => None,
    }
}

fn println(args: Vec<Object>, _line: usize) -> EvalResult {
    println!(
        "{}",
        args.into_iter()
            .map(|arg| arg.to_string() + " ")
            .collect::<String>()
    );
    Ok(Object::Nil)
}

fn scurry_type(args: Vec<Object>, line: usize) -> EvalResult {
    validate_args_len!(args, 1, line);
    Ok(Object::String(args[0].scurry_type().to_string()))
}
