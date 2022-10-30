#[derive(Debug, PartialEq)]
pub enum Stmt {
    Blank,
    Let(LetStmt),
    Return(ReturnStmt),
    Expr(Expr),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Blank,
    Ident(Ident),
}

#[derive(Debug, PartialEq)]
pub struct LetStmt {
    pub name: Ident,
    pub value: Expr,
}

#[derive(Debug, PartialEq)]
pub struct ReturnStmt {
    pub value: Expr,
}

#[derive(Debug, PartialEq)]
pub struct Ident(pub String);

#[derive(Debug, PartialEq)]
pub struct Block(pub Vec<Stmt>);

pub type Program = Block;
