use super::env::Env;
use crate::ast::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::rc::Rc;
use thiserror::Error;

#[allow(unused_macros)]
macro_rules! array {
    ($($elem:expr),+ $(,)?) => {
        {
            let mut vector = Vec::new();
            $(vector.push($elem);)*
            Object::Array(Rc::new(RefCell::new(vector)))
        }
    };
}
#[allow(unused_imports)]
pub(crate) use array;

#[derive(Debug, PartialEq)]
pub enum Object {
    Int(i32),
    Float(f32),
    Bool(bool),
    String(String),
    Array(Rc<RefCell<Vec<Object>>>),
    Map(Rc<RefCell<HashMap<Object, Object>>>),
    Function {
        params: Vec<Ident>,
        body: Block,
        env: Rc<RefCell<Env>>,
        bound: Option<Rc<Instance>>,
    },
    Component(Component),
    Instance(Instance),
    ReturnVal(Box<Object>),
    Nil,
}

impl Clone for Object {
    fn clone(&self) -> Self {
        match self {
            Self::Int(i) => Object::Int(*i),
            Self::Float(f) => Object::Float(*f),
            Self::Bool(b) => Object::Bool(*b),
            Self::String(s) => Object::String(s.clone()),
            Self::Function {
                params,
                body,
                env,
                bound,
            } => Object::Function {
                params: params.clone(),
                body: body.clone(),
                env: Rc::clone(env),
                bound: bound.clone(),
            },
            Self::Nil => Object::Nil,
            Self::ReturnVal(obj) => Object::ReturnVal(obj.clone()),
            Self::Instance(instance) => Object::Instance(instance.clone()),
            Self::Component(component) => Object::Component(component.clone()),
            Self::Map(map) => Object::Map(Rc::clone(map)),
            Self::Array(arr) => Object::Array(Rc::clone(arr)),
        }
    }
}

impl Hash for Object {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Int(i) => i.hash(state),
            Self::Bool(b) => b.hash(state),
            Self::String(s) => s.hash(state),
            _ => "".hash(state),
        }
    }
}

// There are no NaNs or Infinites in Scurry
impl Eq for Object {
    fn assert_receiver_is_total_eq(&self) {}
}

impl Object {
    pub fn scurry_type(&self) -> Type {
        match self {
            Self::Int(_) => Type::Int,
            Self::Function { .. } => Type::Function,
            Self::Float(_) => Type::Float,
            Self::Bool(_) => Type::Bool,
            Self::String(_) => Type::String,
            Self::Array(_) => Type::Array,
            Self::Map(_) => Type::Map,
            Self::Instance(Instance { component, .. }) => Type::Instance(component.name.0.clone()),
            Self::Component { .. } => Type::Component,
            Self::Nil => Type::Nil,
            Self::ReturnVal(_) => Type::Nil,
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Int(i) => *i != 0,
            Self::Float(f) => *f != 0.0,
            Self::Bool(b) => *b,
            Self::String(s) => !s.is_empty(),
            Self::Function { .. } => false,
            Self::Array(arr) => !arr.borrow().is_empty(),
            Self::Map(map) => !map.borrow().is_empty(),
            Self::ReturnVal(_) => false,
            Self::Instance { .. } => false,
            Self::Component { .. } => false,
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
                    .borrow()
                    .iter()
                    .map(|elem| elem.to_string() + ", ")
                    .collect::<String>();
                write!(f, "[{}]", joined.strip_suffix(", ").unwrap_or_default())
            }
            Self::Map(map) => {
                let pairs = map
                    .borrow()
                    .iter()
                    .map(|(key, val)| key.to_string() + ": " + &val.to_string() + ", ")
                    .collect::<String>();
                write!(f, "{{{}}}", pairs.strip_suffix(", ").unwrap_or_default())
            }
            Self::ReturnVal(obj) => write!(f, "{}", *obj),
            Self::Function { params, .. } => {
                let params = params
                    .iter()
                    .map(|ident| ident.to_string() + ", ")
                    .collect::<String>();
                write!(
                    f,
                    "fn({}) {{ ... }}",
                    params.strip_suffix(", ").unwrap_or_default()
                )
            }
            Self::Instance(Instance { component, .. }) => {
                let name = &component.name;
                write!(f, "instance of type `{}`", name)
            }
            Self::Component(Component { name, .. }) => {
                write!(f, "decl {name} {{ ... }}")
            }
            Self::Nil => write!(f, "Nil"),
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
    Map,
    Function,
    Instance(String),
    Component,
    Method,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "Int"),
            Self::Float => write!(f, "Float"),
            Self::Bool => write!(f, "Bool"),
            Self::String => write!(f, "String"),
            Self::Array => write!(f, "Array"),
            Self::Nil => write!(f, "Nil"),
            Self::Map => write!(f, "Map"),
            Self::Function => write!(f, "Function"),
            Self::Component => write!(f, "Component"),
            Self::Method => write!(f, "Method"),
            Self::Instance(inst_type) => write!(f, "{}", inst_type),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Component {
    pub name: Ident,
    pub fields: Vec<Ident>,
    pub methods: HashMap<String, Object>,
    pub embeds: Vec<(Component, Vec<Ident>)>,
}

#[derive(Debug, PartialEq)]
pub struct Instance {
    pub component: Rc<Component>,
    pub field_values: Rc<RefCell<HashMap<String, Object>>>,
    pub embeds: Vec<Instance>,
}

impl Clone for Instance {
    fn clone(&self) -> Self {
        Self {
            component: Rc::clone(&self.component),
            field_values: Rc::clone(&self.field_values),
            embeds: self.embeds.clone(),
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
    #[error("index of `{index}` out of range in array `{obj}` on line {line}")]
    IndexOutOfRange {
        obj: Object,
        index: i32,
        line: usize,
    },
    #[error("index operator not supported between `{obj}` and `{index_type}` on line {line}")]
    IndexOperatorNotSupported {
        obj: Type,
        index_type: Type,
        line: usize,
    },
    #[error("key (`{key}`) not in map: `{obj}` on line {line}")]
    KeyNotFound {
        obj: Object,
        key: Object,
        line: usize,
    },
    #[error("cannot iterate through object of type `{obj}`")]
    CannotIterate { obj: Type, line: usize },
    #[error("not enough function arguments, got {got}, want {want}")]
    NotEnoughArgs {
        got: usize,
        want: usize,
        line: usize,
    },
    #[error("type `{obj}` is not callable")]
    NotCallable { obj: Type, line: usize },
    #[error("cannot use dot operator on type `{obj}` on line {line}")]
    DotOperatorNotSupported { obj: Type, line: usize },
    #[error("unrecognized field `{field}` on object of type `{obj}` on line {line}")]
    UnrecognizedField {
        field: String,
        obj: Type,
        line: usize,
    },
    #[error("invalid embed `{name}`, must refer to a component")]
    InvalidEmbed { name: String, line: usize },
    #[error("invalid assigned field `{field}`")]
    InvalidAssignedField { field: String, line: usize },
}

pub type EvalResult = Result<Object, RuntimeError>;
