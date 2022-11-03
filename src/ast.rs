use crate::lexer::Token;
use core::fmt;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    Infix(InfixExpr),
    Prefix(PrefixExpr),
    Dot(DotExpr),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident(ident) => write!(f, "{ident}"),
            Self::Literal(literal) => write!(f, "{literal}"),
            Self::Infix(infix) => write!(f, "{infix}"),
            Self::Prefix(prefix) => write!(f, "{prefix}"),
            Self::Dot(dotexpr) => write!(f, "{dotexpr}"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct InfixExpr {
    pub left: Box<Expr>,
    pub op: InfixOp,
    pub right: Box<Expr>,
    pub line: usize,
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
    pub line: usize,
}

impl fmt::Display for PrefixExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}{})", self.op, self.left)
    }
}

#[derive(Debug, PartialEq)]
pub struct DotExpr {
    pub left: Box<Expr>,
    pub field: Ident,
}

impl fmt::Display for DotExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.left, self.field)
    }
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer(i32),
    Boolean(bool),
    String(String),
    Float(f32),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(i) => write!(f, "{i}"),
            Self::Float(float) => write!(f, "{float}"),
            Self::Boolean(b) => write!(f, "{}", {
                if *b {
                    "True"
                } else {
                    "False"
                }
            }),
            Self::String(s) => write!(f, "\"{s}\""),
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
    LogicalAnd,
    LogicalOr,
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
            Token::LogicalAnd => Self::LogicalAnd,
            Token::LogicalOr => Self::LogicalOr,
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
            InfixOp::LogicalOr => write!(f, "||"),
            InfixOp::LogicalAnd => write!(f, "&&"),
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
pub struct Ident(pub String);

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Assign(AssignStmt),
    Return(ReturnStmt),
    If(IfStmt),
    For(ForStmt),
    While(WhileStmt),
    Break,
    Continue,
    Expr(Expr),
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Assign(stmt) => write!(f, "{stmt}"),
            Self::Return(stmt) => write!(f, "{stmt}"),
            Self::If(stmt) => write!(f, "{stmt}"),
            Self::For(stmt) => write!(f, "{stmt}"),
            Self::While(stmt) => write!(f, "{stmt}"),
            Self::Break => write!(f, "break;"),
            Self::Continue => write!(f, "continue;"),
            Self::Expr(expr) => write!(f, "{expr}"),
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
pub struct IfStmt {
    pub condition: Expr,
    pub true_block: Block,
    pub else_block: Option<Block>,
    pub elifs: Vec<ElifStmt>,
}

impl fmt::Display for IfStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = String::from(format!("if {} {{ {} }}", self.condition, self.true_block));
        for elif in &self.elifs {
            s.push_str(&format!(" {elif}"));
        }
        if let Some(else_block) = &self.else_block {
            s.push_str(&format!(" else {{ {} }}", else_block))
        }
        write!(f, "{s}")
    }
}

#[derive(Debug, PartialEq)]
pub struct ElifStmt {
    pub condition: Expr,
    pub block: Block,
}

impl fmt::Display for ElifStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "elif {} {{ {} }}", self.condition, self.block)
    }
}

#[derive(Debug, PartialEq)]
pub struct ForStmt {
    pub iter_ident: Ident,
    pub expr: Expr,
    pub block: Block,
}

impl fmt::Display for ForStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "for {} in {} {{ {} }}",
            self.iter_ident, self.expr, self.block
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct WhileStmt {
    pub condition: Expr,
    pub block: Block,
}

impl fmt::Display for WhileStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "while {} {{ {} }}", self.condition, self.block)
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq)]
pub struct Block(pub Vec<Stmt>);

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let joined = self
            .0
            .iter()
            .map(|stmt| stmt.to_string() + "; ")
            .collect::<String>();
        write!(
            f,
            "{}",
            joined.strip_suffix(" ").expect("Should have trailing ' '")
        )
    }
}

pub type Program = Block;

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_to_string {
        ($inputs:expr, $expecteds:expr) => {
            for (input, expected) in $inputs.iter().zip($expecteds) {
                assert_eq!(input.to_string(), expected);
            }
        };
    }

    #[test]
    fn literal_display() {
        let inputs = [
            Literal::Integer(3),
            Literal::Boolean(true),
            Literal::String("test".to_owned()),
        ];
        let expecteds = ["3", "True", "\"test\""];

        test_to_string!(inputs, expecteds)
    }

    #[test]
    fn infix_expr_display() {
        let inputs = [
            InfixExpr {
                left: Box::new(Expr::Literal(Literal::Integer(3))),
                op: InfixOp::Plus,
                right: Box::new(Expr::Literal(Literal::Integer(3))),
                line: 1,
            },
            InfixExpr {
                left: Box::new(Expr::Infix(InfixExpr {
                    left: Box::new(Expr::Literal(Literal::Integer(3))),
                    op: InfixOp::Gt,
                    right: Box::new(Expr::Literal(Literal::Integer(5))),
                    line: 1,
                })),
                op: InfixOp::Plus,
                right: Box::new(Expr::Literal(Literal::Integer(3))),
                line: 1,
            },
        ];
        let expecteds = ["(3 + 3)", "((3 > 5) + 3)"];

        test_to_string!(inputs, expecteds)
    }

    #[test]
    fn prefix_expr_display() {
        let inputs = [
            PrefixExpr {
                left: Box::new(Expr::Literal(Literal::Boolean(true))),
                op: PrefixOp::Bang,
                line: 1,
            },
            PrefixExpr {
                left: Box::new(Expr::Infix(InfixExpr {
                    left: Box::new(Expr::Literal(Literal::Integer(3))),
                    op: InfixOp::LogicalOr,
                    right: Box::new(Expr::Literal(Literal::Integer(4))),
                    line: 1,
                })),
                op: PrefixOp::Minus,
                line: 1,
            },
        ];
        let expecteds = ["(!True)", "(-(3 || 4))"];

        test_to_string!(inputs, expecteds)
    }

    #[test]
    fn assign_stmt_display() {
        let inputs = [AssignStmt {
            name: Ident("x".to_owned()),
            value: Expr::Literal(Literal::Integer(3)),
            line: 1,
        }];

        let expecteds = ["x = 3;"];

        test_to_string!(inputs, expecteds)
    }

    #[test]
    fn if_stmt_display() {
        let inputs = [
            IfStmt {
                condition: Expr::Literal(Literal::Integer(3)),
                true_block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Boolean(true)))]),
                else_block: Some(Block(vec![Stmt::Expr(Expr::Literal(Literal::String(
                    "test".to_owned(),
                )))])),
                elifs: vec![ElifStmt {
                    condition: Expr::Literal(Literal::Boolean(false)),
                    block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(4)))]),
                }],
            },
            IfStmt {
                condition: Expr::Literal(Literal::Integer(3)),
                true_block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Boolean(true)))]),
                else_block: Some(Block(vec![Stmt::Expr(Expr::Literal(Literal::String(
                    "test".to_owned(),
                )))])),
                elifs: Vec::new(),
            },
            IfStmt {
                condition: Expr::Literal(Literal::Integer(3)),
                true_block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Boolean(true)))]),
                else_block: None,
                elifs: Vec::new(),
            },
        ];
        let expecteds = [
            "if 3 { True; } elif False { 4; } else { \"test\"; }",
            "if 3 { True; } else { \"test\"; }",
            "if 3 { True; }",
        ];

        test_to_string!(inputs, expecteds)
    }

    #[test]
    fn return_stmt_display() {
        let inputs = [ReturnStmt {
            value: Expr::Literal(Literal::Integer(3)),
            line: 1,
        }];
        let expecteds = ["return 3;"];

        test_to_string!(inputs, expecteds)
    }

    #[test]
    fn dot_expr_display() {
        let inputs = [DotExpr {
            left: Box::new(Expr::Literal(Literal::Integer(1))),
            field: Ident("test".to_owned()),
        }];
        let expecteds = ["1.test"];

        test_to_string!(inputs, expecteds)
    }

    #[test]
    fn for_stmt_display() {
        let inputs = [ForStmt {
            iter_ident: Ident("i".to_owned()),
            expr: Expr::Literal(Literal::Integer(3)),
            block: Block(vec![Stmt::Expr(Expr::Ident(Ident("i".to_owned())))]),
        }];
        let expecteds = ["for i in 3 { i; }"];

        test_to_string!(inputs, expecteds)
    }

    #[test]
    fn while_stmt_display() {
        let inputs = [WhileStmt {
            condition: Expr::Literal(Literal::Boolean(true)),
            block: Block(vec![Stmt::Expr(Expr::Literal(Literal::String(
                "test".to_owned(),
            )))]),
        }];
        let expecteds = ["while True { \"test\"; }"];

        test_to_string!(inputs, expecteds)
    }
}
