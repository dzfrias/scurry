mod array;
mod float;
mod functions;
mod int;
mod map;
mod string;

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
        "remove" => Some(map::remove),
        _ => None,
    }
}

pub fn get_string_method(name: &str) -> Option<BuiltinMethod> {
    match name {
        "len" => Some(string::len),
        "trim" => Some(string::trim),
        _ => None,
    }
}

pub fn get_int_method(name: &str) -> Option<BuiltinMethod> {
    match name {
        "abs" => Some(int::abs),
        "to_float" => Some(int::to_float),
        _ => None,
    }
}

pub fn get_float_method(name: &str) -> Option<BuiltinMethod> {
    match name {
        "abs" => Some(float::abs),
        "to_int" => Some(float::to_int),
        _ => None,
    }
}
