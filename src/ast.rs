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
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Blank => write!(f, "BLANK"),
            Self::Ident(ident) => write!(f, "{ident}"),
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
