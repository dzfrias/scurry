mod array;
mod float;
mod functions;
mod int;
mod map;
mod string;

use super::object::*;

pub fn get_builtin_func(name: &str) -> Option<BuiltinFunc> {
    match name {
        "type" => Some(functions::scurry_type),
        "truthy" => Some(functions::truthy),
        _ => None,
    }
}

pub fn get_array_method(name: &str) -> Option<BuiltinMethod> {
    match name {
        "len" => Some(array::len),
        "push" => Some(array::push),
        "pop" => Some(array::pop),
        "contains" => Some(array::contains),
        "insert" => Some(array::insert),
        "remove" => Some(array::remove),
        "concat" => Some(array::concat),
        _ => None,
    }
}

pub fn get_map_method(name: &str) -> Option<BuiltinMethod> {
    match name {
        "keys" => Some(map::keys),
        "values" => Some(map::values),
        "remove" => Some(map::remove),
        "merge" => Some(map::merge),
        _ => None,
    }
}

pub fn get_string_method(name: &str) -> Option<BuiltinMethod> {
    match name {
        "len" => Some(string::len),
        "trim" => Some(string::trim),
        "starts_with" => Some(string::starts_with),
        "ends_with" => Some(string::ends_with),
        "substring" => Some(string::substring),
        "split" => Some(string::split),
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
