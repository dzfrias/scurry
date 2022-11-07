use crate::ast::*;
use std::fmt;
use thiserror::Error;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(i32),
    Float(f32),
    Bool(bool),
    String(String),
    Array(Vec<Object>),
    Nil,
}

impl Object {
    pub fn scurry_type(&self) -> Type {
        match self {
            Self::Int(_) => Type::Int,
            Self::Float(_) => Type::Float,
            Self::Bool(_) => Type::Bool,
            Self::String(_) => Type::String,
            Self::Array(_) => Type::Array,
            Self::Nil => Type::Nil,
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Int(i) => *i != 0,
            Self::Float(f) => *f != 0.0,
            Self::Bool(b) => *b,
            Self::String(s) => !s.is_empty(),
            Self::Array(arr) => !arr.is_empty(),
            Self::Nil => false,
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::Float(n) => write!(f, "{}", n),
            Self::Bool(b) => write!(f, "{}", {
                if *b {
                    "True"
                } else {
                    "False"
                }
            }),
            Self::String(s) => write!(f, "{}", s),
            Self::Array(arr) => {
                let joined = arr
                    .iter()
                    .map(|elem| elem.to_string() + ", ")
                    .collect::<String>();
                write!(f, "[{}]", joined.strip_suffix(", ").unwrap_or_default())
            }
            Self::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Nil,
    Array,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "Int"),
            Self::Float => write!(f, "Float"),
            Self::Bool => write!(f, "Bool"),
            Self::String => write!(f, "String"),
            Self::Array => write!(f, "Array"),
            Self::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Number {
    Int(i32),
    Float(f32),
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(num) => write!(f, "{num}"),
            Self::Float(num) => write!(f, "{num}"),
        }
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum RuntimeError {
    #[error("variable not found: {name} on line {line}")]
    VariableNotFound { name: String, line: usize },
    #[error("cannot perform `{op}` on type `{operand}` on line {line}")]
    InvalidUnaryOperand {
        op: PrefixOp,
        operand: Type,
        line: usize,
    },
    #[error("cannot perform `{op}` between `{left}` and `{right}` on line {line}")]
    InvalidBinaryOperand {
        op: InfixOp,
        left: Type,
        right: Type,
        line: usize,
    },
    #[error("integer overflow occured in the expression: `{left} {op} {right}` on line {line}")]
    IntegerOverflow {
        op: InfixOp,
        left: i32,
        right: i32,
        line: usize,
    },
    #[error("division by zero occured in the expression: `{left} {op} {right}` on line {line}")]
    DivisionByZero {
        op: InfixOp,
        left: Number,
        right: Number,
        line: usize,
    },
    #[error("index of `{index}` out of range in array `{obj}`")]
    IndexOutOfRange { obj: Object, index: i32 },
    #[error("index operator not supported between `{obj}` and `{index_type}`")]
    IndexOperatorOutOfRange { obj: Type, index_type: Type },
}

pub type EvalResult = Result<Object, RuntimeError>;
