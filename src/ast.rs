#[derive(Debug, PartialEq)]
pub enum Stmt {
    Blank,
    Assign(AssignStmt),
    Return(ReturnStmt),
    Expr(Expr),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Blank,
    Ident(Ident),
}

#[derive(Debug, PartialEq)]
pub struct AssignStmt {
    pub name: Ident,
    pub value: Expr,
    pub line: usize,
}

#[derive(Debug, PartialEq)]
pub struct ReturnStmt {
    pub value: Expr,
    pub line: usize,
}

#[derive(Debug, PartialEq)]
pub struct Ident(pub String);

#[derive(Debug, PartialEq)]
pub struct Block(pub Vec<Stmt>);

pub type Program = Block;
