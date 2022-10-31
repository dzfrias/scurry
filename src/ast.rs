use crate::lexer::Token;
use core::fmt;

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Blank,
    Assign(AssignStmt),
    Return(ReturnStmt),
    Expr(Expr),
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Blank => write!(f, "BLANK"),
            Self::Assign(stmt) => write!(f, "{stmt}"),
            Self::Return(stmt) => write!(f, "{stmt}"),
            Self::Expr(expr) => write!(f, "{expr}"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Blank,
    Ident(Ident),
    Literal(Literal),
    Infix(InfixExpr),
    Prefix(PrefixExpr),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Blank => write!(f, "BLANK"),
            Self::Ident(ident) => write!(f, "{ident}"),
            Self::Literal(literal) => write!(f, "{literal}"),
            Self::Infix(infix) => write!(f, "{infix}"),
            Self::Prefix(prefix) => write!(f, "{prefix}"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct InfixExpr {
    pub left: Box<Expr>,
    pub op: InfixOp,
    pub right: Box<Expr>,
}

impl fmt::Display for InfixExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.op, self.right)
    }
}

#[derive(Debug, PartialEq)]
pub struct PrefixExpr {
    pub left: Box<Expr>,
    pub op: PrefixOp,
}

impl fmt::Display for PrefixExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}{})", self.op, self.left)
    }
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer(i32),
    Boolean(bool),
    String(String),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(i) => write!(f, "{i}"),
            Self::Boolean(b) => write!(f, "{b}"),
            Self::String(s) => write!(f, "{s}"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum InfixOp {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Gt,
    Lt,
    Eq,
    NotEq,
    Ge,
    Le,
    Modulo,
}

impl TryFrom<&Token> for InfixOp {
    type Error = String;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        Ok(match token {
            Token::Plus => Self::Plus,
            Token::Minus => Self::Minus,
            Token::Asterisk => Self::Asterisk,
            Token::Slash => Self::Slash,
            Token::Gt => Self::Gt,
            Token::Lt => Self::Lt,
            Token::Eq => Self::Eq,
            Token::NotEq => Self::NotEq,
            Token::Ge => Self::Ge,
            Token::Le => Self::Le,
            Token::Percent => Self::Modulo,
            _ => return Err(format!("Invalid infix operator token: {:?}", token)),
        })
    }
}

impl fmt::Display for InfixOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InfixOp::Plus => write!(f, "+"),
            InfixOp::Minus => write!(f, "-"),
            InfixOp::Asterisk => write!(f, "*"),
            InfixOp::Slash => write!(f, "/"),
            InfixOp::Gt => write!(f, ">"),
            InfixOp::Lt => write!(f, "<"),
            InfixOp::Eq => write!(f, "=="),
            InfixOp::NotEq => write!(f, "!="),
            InfixOp::Ge => write!(f, ">="),
            InfixOp::Le => write!(f, "<="),
            InfixOp::Modulo => write!(f, "%"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrefixOp {
    Minus,
    Bang,
    Plus,
}

impl TryFrom<&Token> for PrefixOp {
    type Error = String;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        Ok(match token {
            Token::Minus => Self::Minus,
            Token::Bang => Self::Bang,
            Token::Plus => Self::Plus,
            _ => return Err(format!("Invalid prefix operator token: {:?}", token)),
        })
    }
}

impl fmt::Display for PrefixOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrefixOp::Minus => write!(f, "-"),
            PrefixOp::Bang => write!(f, "!"),
            PrefixOp::Plus => write!(f, "+"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct AssignStmt {
    pub name: Ident,
    pub value: Expr,
    pub line: usize,
}

impl fmt::Display for AssignStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {};", self.name, self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct ReturnStmt {
    pub value: Expr,
    pub line: usize,
}

impl fmt::Display for ReturnStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "return {};", self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct Ident(pub String);

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq)]
pub struct Block(pub Vec<Stmt>);

pub type Program = Block;
