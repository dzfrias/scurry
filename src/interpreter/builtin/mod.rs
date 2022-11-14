mod array;
mod functions;
mod map;

use super::object::*;

pub fn get_builtin_func(name: &str) -> Option<BuiltinFunc> {
    match name {
        "println" => Some(functions::println),
        "type" => Some(functions::scurry_type),
        _ => None,
    }
}

pub fn get_array_method(name: &str) -> Option<BuiltinMethod> {
    match name {
        "len" => Some(array::len),
        "push" => Some(array::push),
        "pop" => Some(array::pop),
        _ => None,
    }
}

pub fn get_map_method(name: &str) -> Option<BuiltinMethod> {
    match name {
        "keys" => Some(map::keys),
        "values" => Some(map::values),
        _ => None,
    }
}
