use super::env::Env;
use crate::ast::*;
use crate::parser::ParserError;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::path::PathBuf;
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

pub type BuiltinFunc = fn(Vec<Object>, usize) -> EvalResult;
pub type BuiltinMethod = fn(Object, Vec<Object>, usize) -> Option<EvalResult>;

#[derive(Debug, PartialEq)]
pub enum Object {
    Int(i32),
    Float(f32),
    Bool(bool),
    String(String),
    Array(Rc<RefCell<Vec<Object>>>),
    Map(Rc<RefCell<HashMap<Object, Object>>>),
    Function {
        params: Vec<(Ident, TypeAnnotation)>,
        body: Block,
        env: Rc<RefCell<Env>>,
        bound: Option<Rc<Instance>>,
        visibility: Option<Visibility>,
    },
    Builtin(BuiltinFunc),
    BuiltinMethod {
        bound: Box<Object>,
        function: BuiltinMethod,
    },
    Component(Component),
    Instance(Instance),
    Module {
        file: PathBuf,
        exports: Rc<HashMap<String, Object>>,
    },
    ControlChange(ControlChange),
    Nil,
    // Only used for statements, that return absolutely nothing
    AbsoluteNil,
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
                visibility,
            } => Object::Function {
                params: params.clone(),
                body: body.clone(),
                env: Rc::clone(env),
                bound: bound.clone(),
                visibility: visibility.clone(),
            },
            Self::Builtin(builtin) => Self::Builtin(*builtin),
            Self::BuiltinMethod { bound, function } => Self::BuiltinMethod {
                bound: bound.clone(),
                function: *function,
            },
            Self::Nil => Object::Nil,
            Self::AbsoluteNil => Self::AbsoluteNil,
            Self::ControlChange(control) => Object::ControlChange(control.clone()),
            Self::Instance(instance) => Object::Instance(instance.clone()),
            Self::Component(component) => Object::Component(component.clone()),
            Self::Module { file, exports } => Object::Module {
                file: file.clone(),
                exports: Rc::clone(exports),
            },
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
            Self::Builtin(_) => Type::Builtin,
            Self::BuiltinMethod { .. } => Type::Builtin,
            Self::Float(_) => Type::Float,
            Self::Bool(_) => Type::Bool,
            Self::String(_) => Type::String,
            Self::Array(_) => Type::Array,
            Self::Map(_) => Type::Map,
            Self::Instance(Instance { component, .. }) => Type::Instance(component.name.0.clone()),
            Self::Component { .. } => Type::Component,
            Self::Module { .. } => Type::Module,
            Self::Nil => Type::Nil,
            Self::AbsoluteNil => Type::Nil,
            Self::ControlChange(_) => Type::Nil,
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Int(i) => *i != 0,
            Self::Float(f) => *f != 0.0,
            Self::Bool(b) => *b,
            Self::String(s) => !s.is_empty(),
            Self::Array(arr) => !arr.borrow().is_empty(),
            Self::Map(map) => !map.borrow().is_empty(),
            Self::Function { .. } => false,
            Self::Builtin(_) => false,
            Self::BuiltinMethod { .. } => false,
            Self::Module { .. } => false,
            Self::ControlChange(_) => false,
            Self::Instance { .. } => false,
            Self::Component { .. } => false,
            Self::Nil => false,
            Self::AbsoluteNil => false,
        }
    }

    pub fn is_absnil(&self) -> bool {
        self == &Self::AbsoluteNil
    }

    pub fn fits_type(&self, type_annotation: TypeAnnotation) -> bool {
        let own_type: AstType = self.scurry_type().into();
        type_annotation
            .0
            .iter()
            .any(|ty| &own_type == ty || ty == &AstType::Any)
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
            Self::String(s) => write!(f, "\"{}\"", s),
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
            Self::ControlChange(_) => write!(f, "<control_breaker>"),
            Self::Function { params, .. } => {
                let params = params
                    .iter()
                    .map(|(ident, _)| ident.to_string() + ", ")
                    .collect::<String>();
                write!(
                    f,
                    "fn({}) {{ ... }}",
                    params.strip_suffix(", ").unwrap_or_default()
                )
            }
            Self::Builtin(_) | Self::BuiltinMethod { .. } => write!(f, "<builtin>"),

            Self::Instance(Instance { component, .. }) => {
                let name = &component.name;
                write!(f, "instance of type `{}`", name)
            }
            Self::Module { file, .. } => write!(f, "module from file {}", file.display()),
            Self::Component(Component { name, .. }) => {
                write!(f, "decl {name} {{ ... }}")
            }
            Self::Nil => write!(f, "Nil"),
            Self::AbsoluteNil => write!(f, ""),
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
    Builtin,
    Instance(String),
    Component,
    Method,
    Module,
}

impl Into<AstType> for Type {
    fn into(self) -> AstType {
        match self {
            Self::Int => AstType::Int,
            Self::Float => AstType::Float,
            Self::Bool => AstType::Bool,
            Self::String => AstType::String,
            Self::Nil => AstType::Nil,
            Self::Array => AstType::Array,
            Self::Map => AstType::Map,
            Self::Function => AstType::Function,
            Self::Builtin => AstType::Function,
            Self::Method => AstType::Function,
            Self::Module => AstType::Module,
            Self::Component => AstType::ComponentDecl,
            Self::Instance(component) => AstType::Component(component),
        }
    }
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
            Self::Builtin => write!(f, "Builtin"),
            Self::Map => write!(f, "Map"),
            Self::Function => write!(f, "Function"),
            Self::Component => write!(f, "Component"),
            Self::Method => write!(f, "Method"),
            Self::Module => write!(f, "Module"),
            Self::Instance(inst_type) => write!(f, "{}", inst_type),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Component {
    pub name: Ident,
    pub fields: Vec<(Ident, TypeAnnotation)>,
    pub methods: HashMap<String, Object>,
    pub embeds: Vec<(Component, Vec<Expr>)>,
    pub exports: Vec<String>,
    pub visibility: Visibility,
}

#[derive(Debug, PartialEq)]
pub struct Instance {
    pub component: Rc<Component>,
    pub field_values: Rc<RefCell<HashMap<String, Object>>>,
    pub embeds: Vec<Instance>,
    pub visibility: Visibility,
}

impl Instance {
    pub fn clone_with_private(&self) -> Self {
        let mut clone = self.clone();
        clone.visibility = Visibility::Private;
        clone
    }

    pub fn has_special(&self, name: SpecialMethod) -> bool {
        self.component.methods.contains_key(name.into())
    }

    pub fn get_special(&self, name: SpecialMethod) -> Option<&Object> {
        self.component.methods.get(name.into())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum SpecialMethod {
    New,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Call,
    Truthy,
}

impl From<SpecialMethod> for &str {
    fn from(method: SpecialMethod) -> Self {
        match method {
            SpecialMethod::New => "$new",
            SpecialMethod::Add => "$add",
            SpecialMethod::Sub => "$sub",
            SpecialMethod::Mul => "$mul",
            SpecialMethod::Div => "$div",
            SpecialMethod::Mod => "$mod",
            SpecialMethod::Call => "$call",
            SpecialMethod::Truthy => "$truthy",
        }
    }
}

impl Clone for Instance {
    fn clone(&self) -> Self {
        Self {
            component: Rc::clone(&self.component),
            field_values: Rc::clone(&self.field_values),
            embeds: self.embeds.clone(),
            visibility: self.visibility.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ControlChange {
    Return(Box<Object>),
    Break,
    Continue,
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
    #[error("index of `{index}` out of range in `{obj}` on line {line}")]
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
    #[error("cannot iterate through object of type `{obj}` on line {line}")]
    CannotIterate { obj: Type, line: usize },
    #[error("not enough function arguments, got {got}, want {want} on line {line}")]
    NotEnoughArgs {
        got: usize,
        want: usize,
        line: usize,
    },
    #[error("type `{obj}` is not callable on line {line}")]
    NotCallable { obj: Type, line: usize },
    #[error("cannot use dot operator on type `{obj}` on line {line}")]
    DotOperatorNotSupported { obj: Type, line: usize },
    #[error("unrecognized field `{field}` on object of type `{obj}` on line {line}")]
    UnrecognizedField {
        field: String,
        obj: Type,
        line: usize,
    },
    #[error("invalid embed `{name}`, must refer to a component on line {line}")]
    InvalidEmbed { name: String, line: usize },
    #[error("cannot assign to expression `{expr}` on line {line}")]
    CannotAssign { expr: Expr, line: usize },
    #[error("could not read file \"{name}\" on line {line}")]
    CouldNotReadFile { name: PathBuf, line: usize },
    #[error("ParserError")]
    ParserErrors {
        contents: String,
        errs: Vec<ParserError>,
    },
    #[error("wrong argument type `{name}`, expected `{expected}`, got `{got}` on line {line}")]
    WrongArgType {
        name: String,
        expected: TypeAnnotation,
        got: AstType,
        line: usize,
    },
    #[error(
        "mismatched assign type on `{name}`, expected `{expected}`, got `{got}` on line {line}"
    )]
    MismatchedAssignType {
        name: String,
        expected: TypeAnnotation,
        got: AstType,
        line: usize,
    },
}

pub type EvalResult = Result<Object, RuntimeError>;
