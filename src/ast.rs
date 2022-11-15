use crate::lexer::Token;
use core::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    Infix(InfixExpr),
    Prefix(PrefixExpr),
    Dot(DotExpr),
    Function(FunctionExpr),
    Call(CallExpr),
    Index(IndexExpr),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident(ident) => write!(f, "{ident}"),
            Self::Literal(literal) => write!(f, "{literal}"),
            Self::Infix(infix) => write!(f, "{infix}"),
            Self::Prefix(prefix) => write!(f, "{prefix}"),
            Self::Function(function) => write!(f, "{function}"),
            Self::Dot(dotexpr) => write!(f, "{dotexpr}"),
            Self::Index(indexexpr) => write!(f, "{indexexpr}"),
            Self::Call(call) => write!(f, "{call}"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub struct DotExpr {
    pub left: Box<Expr>,
    pub field: Ident,
    pub line: usize,
}

impl fmt::Display for DotExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.left, self.field)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionExpr {
    pub params: Vec<Ident>,
    pub block: Block,
}

impl fmt::Display for FunctionExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.params.is_empty() {
            return write!(f, "fn() {}", self.block);
        }
        let joined = self
            .params
            .iter()
            .map(|ident| ident.to_string() + ", ")
            .collect::<String>();
        write!(
            f,
            "fn({}) {}",
            joined
                .strip_suffix(", ")
                .expect("Should always have a trailing ', '"),
            self.block,
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpr {
    pub func: Box<Expr>,
    pub args: Vec<Expr>,
    pub line: usize,
}

impl fmt::Display for CallExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let args = self
            .args
            .iter()
            .map(|arg| arg.to_string() + ", ")
            .collect::<String>();
        write!(
            f,
            "{}({})",
            self.func,
            args.strip_suffix(", ").unwrap_or_default()
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IndexExpr {
    pub left: Box<Expr>,
    pub index: Box<Expr>,
    pub line: usize,
}

impl fmt::Display for IndexExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}[{}]", self.left, self.index)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Nil,
    Integer(i32),
    Boolean(bool),
    String(String),
    Float(f32),
    Array(Vec<Expr>),
    Map(Vec<(Expr, Expr)>),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "Nil"),
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
            Self::Array(arr) => {
                let joined = arr
                    .iter()
                    .map(|expr| expr.to_string() + ", ")
                    .collect::<String>();
                write!(f, "[{}]", joined.strip_suffix(", ").unwrap_or_default())
            }
            Self::Map(map) => {
                let joined = map
                    .iter()
                    .map(|(key, val)| key.to_string() + ": " + &val.to_string() + ", ")
                    .collect::<String>();
                write!(f, "{{{}}}", joined.strip_suffix(", ").unwrap_or_default())
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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

impl From<AssignOp> for InfixOp {
    fn from(value: AssignOp) -> Self {
        match value {
            AssignOp::Plus => InfixOp::Plus,
            AssignOp::Minus => InfixOp::Minus,
            AssignOp::Divide => InfixOp::Slash,
            AssignOp::Multiply => InfixOp::Asterisk,
            AssignOp::Modulo => InfixOp::Modulo,
        }
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

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Ident(pub String);

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Assign(AssignStmt),
    If(IfStmt),

    Function(FunctionStmt),
    Return(ReturnStmt),

    Declaration(DeclarationStmt),

    For(ForStmt),
    While(WhileStmt),
    Switch(SwitchStmt),
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
            Self::Function(stmt) => write!(f, "{stmt}"),
            Self::Switch(stmt) => write!(f, "{stmt}"),
            Self::Declaration(stmt) => write!(f, "{stmt}"),
            Self::Break => write!(f, "break;"),
            Self::Continue => write!(f, "continue;"),
            Self::Expr(expr) => write!(f, "{expr}"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AssignStmt {
    pub name: Expr,
    pub value: Expr,
    pub operator: Option<AssignOp>,
    pub line: usize,
}

impl fmt::Display for AssignStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {};", self.name, self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum AssignOp {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStmt {
    pub value: Expr,
}

impl fmt::Display for ReturnStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "return {};", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfStmt {
    pub condition: Expr,
    pub true_block: Block,
    pub else_block: Option<Block>,
    pub elifs: Vec<ElifStmt>,
}

impl fmt::Display for IfStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = format!("if {} {}", self.condition, self.true_block);
        for elif in &self.elifs {
            s.push_str(&format!(" {elif}"));
        }
        if let Some(else_block) = &self.else_block {
            s.push_str(&format!(" else {}", else_block))
        }
        write!(f, "{s}")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ElifStmt {
    pub condition: Expr,
    pub block: Block,
}

impl fmt::Display for ElifStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "elif {} {}", self.condition, self.block)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ForStmt {
    pub iter_ident: Ident,
    pub expr: Expr,
    pub block: Block,
    pub line: usize,
}

impl fmt::Display for ForStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "for {} in {} {}", self.iter_ident, self.expr, self.block)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct WhileStmt {
    pub condition: Expr,
    pub block: Block,
}

impl fmt::Display for WhileStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "while {} {}", self.condition, self.block)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionStmt {
    pub name: Ident,
    pub params: Vec<Ident>,
    pub block: Block,
    pub visibility: Visibility,
}

impl fmt::Display for FunctionStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let joined = self
            .params
            .iter()
            .map(|ident| ident.to_string() + ", ")
            .collect::<String>();
        if self.visibility == Visibility::Public {
            write!(
                f,
                "exp fn {}({}) {}",
                self.name,
                joined.strip_suffix(", ").unwrap_or(""),
                self.block,
            )
        } else {
            write!(
                f,
                "fn {}({}) {}",
                self.name,
                joined.strip_suffix(", ").unwrap_or(""),
                self.block,
            )
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct SwitchStmt {
    pub expr: Expr,
    pub cases: Vec<Case>,
    pub default: Option<Block>,
}

impl fmt::Display for SwitchStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.cases.is_empty() && self.default.is_none() {
            return write!(f, "switch {} {{}}", self.expr);
        }
        let cases = self
            .cases
            .iter()
            .map(|case| case.to_string() + " ")
            .collect::<String>();
        if let Some(block) = &self.default {
            write!(f, "switch {} {{ {cases}default {block} }}", self.expr)
        } else {
            write!(f, "switch {} {{ {cases}}}", self.expr)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Case {
    pub conditions: Vec<Expr>,
    pub block: Block,
}

impl fmt::Display for Case {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let cases = self
            .conditions
            .iter()
            .map(|expr| expr.to_string() + " | ")
            .collect::<String>();
        write!(
            f,
            "case {} {}",
            cases
                .strip_suffix(" | ")
                .expect("Should have trailing ' | '"),
            self.block
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct DeclarationStmt {
    pub name: Ident,
    pub methods: Vec<FunctionStmt>,
    pub fields: Vec<Ident>,
    pub embeds: Vec<Embed>,
}

impl fmt::Display for DeclarationStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.methods.is_empty() && self.fields.is_empty() && self.embeds.is_empty() {
            return write!(f, "decl {} {{}}", self.name);
        }
        let methods = self
            .methods
            .iter()
            .map(|func| func.to_string() + " ")
            .collect::<String>();
        let fields = self
            .fields
            .iter()
            .map(|ident| ident.to_string() + " ")
            .collect::<String>();
        let embeds = self
            .embeds
            .iter()
            .map(|embed| embed.to_string() + " ")
            .collect::<String>();
        write!(f, "decl {} {{ {fields}{embeds}{methods}}}", self.name)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Embed {
    pub name: Ident,
    pub assigned: Vec<Ident>,
    pub line: usize,
}

impl fmt::Display for Embed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.assigned.is_empty() {
            return write!(f, "[{}] {{}}", self.name);
        }
        let assigned = self
            .assigned
            .iter()
            .map(|embed| embed.to_string() + ", ")
            .collect::<String>();
        write!(f, "[{}] {{ {assigned} }}", self.name)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block(pub Vec<Stmt>);

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_empty() {
            return write!(f, "{{}}");
        }
        let joined = self
            .0
            .iter()
            .map(|stmt| stmt.to_string() + "; ")
            .collect::<String>();
        write!(
            f,
            "{{ {} }}",
            joined.strip_suffix(' ').expect("Should have trailing ' '")
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
            Literal::Array(vec![Expr::Literal(Literal::Integer(3))]),
            Literal::Array(vec![
                Expr::Literal(Literal::Integer(3)),
                Expr::Ident(Ident("x".to_owned())),
            ]),
            Literal::Array(Vec::new()),
            Literal::Map(vec![(
                Expr::Literal(Literal::Integer(1)),
                Expr::Literal(Literal::Integer(2)),
            )]),
            Literal::Map(vec![
                (
                    Expr::Literal(Literal::Integer(1)),
                    Expr::Literal(Literal::Integer(2)),
                ),
                (
                    Expr::Literal(Literal::Integer(3)),
                    Expr::Literal(Literal::Integer(3)),
                ),
            ]),
            Literal::Map(Vec::new()),
        ];
        let expecteds = [
            "3",
            "True",
            "\"test\"",
            "[3]",
            "[3, x]",
            "[]",
            "{1: 2}",
            "{1: 2, 3: 3}",
            "{}",
        ];

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
    fn block_display() {
        let inputs = [
            Block(Vec::new()),
            Block(vec![Stmt::Expr(Expr::Literal(Literal::Float(3.3)))]),
        ];
        let expecteds = ["{}", "{ 3.3; }"];

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
            name: Expr::Ident(Ident("x".to_owned())),
            value: Expr::Literal(Literal::Integer(3)),
            line: 1,
            operator: None,
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
        }];
        let expecteds = ["return 3;"];

        test_to_string!(inputs, expecteds)
    }

    #[test]
    fn dot_expr_display() {
        let inputs = [DotExpr {
            left: Box::new(Expr::Literal(Literal::Integer(1))),
            field: Ident("test".to_owned()),
            line: 1,
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
            line: 1,
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

    #[test]
    fn function_stmt_display() {
        let inputs = [
            FunctionStmt {
                name: Ident("x".to_owned()),
                params: vec![Ident("y".to_owned())],
                block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(3)))]),
                visibility: Visibility::Private,
            },
            FunctionStmt {
                name: Ident("x".to_owned()),
                params: vec![Ident("y".to_owned()), Ident("z".to_owned())],
                block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(3)))]),
                visibility: Visibility::Private,
            },
            FunctionStmt {
                name: Ident("x".to_owned()),
                params: Vec::new(),
                block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(3)))]),
                visibility: Visibility::Private,
            },
        ];
        let expecteds = ["fn x(y) { 3; }", "fn x(y, z) { 3; }", "fn x() { 3; }"];

        test_to_string!(inputs, expecteds)
    }

    #[test]
    fn function_expr_display() {
        let inputs = [
            FunctionExpr {
                params: vec![Ident("x".to_owned())],
                block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Boolean(true)))]),
            },
            FunctionExpr {
                params: vec![Ident("x".to_owned()), Ident("y".to_owned())],
                block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Boolean(true)))]),
            },
            FunctionExpr {
                params: Vec::new(),
                block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Boolean(true)))]),
            },
        ];
        let expecteds = ["fn(x) { True; }", "fn(x, y) { True; }", "fn() { True; }"];

        test_to_string!(inputs, expecteds)
    }

    #[test]
    fn decl_stmt_display() {
        let inputs = [
            DeclarationStmt {
                name: Ident("Test".to_owned()),
                methods: Vec::new(),
                fields: Vec::new(),
                embeds: Vec::new(),
            },
            DeclarationStmt {
                name: Ident("Test".to_owned()),
                fields: vec![Ident("test".to_owned()), Ident("test".to_owned())],
                methods: Vec::new(),
                embeds: Vec::new(),
            },
            DeclarationStmt {
                name: Ident("Test".to_owned()),
                fields: vec![Ident("test".to_owned()), Ident("test".to_owned())],
                methods: vec![FunctionStmt {
                    name: Ident("testing".to_owned()),
                    params: Vec::new(),
                    block: Block(Vec::new()),
                    visibility: Visibility::Private,
                }],
                embeds: Vec::new(),
            },
        ];
        let expecteds = [
            "decl Test {}",
            "decl Test { test test }",
            "decl Test { test test fn testing() {} }",
        ];

        test_to_string!(inputs, expecteds)
    }

    #[test]
    fn call_expr_display() {
        let inputs = [
            CallExpr {
                func: Box::new(Expr::Ident(Ident("test".to_owned()))),
                args: vec![
                    Expr::Literal(Literal::Integer(4)),
                    Expr::Ident(Ident("testing".to_owned())),
                ],
                line: 1,
            },
            CallExpr {
                func: Box::new(Expr::Ident(Ident("test".to_owned()))),
                args: vec![Expr::Literal(Literal::Integer(4))],
                line: 1,
            },
            CallExpr {
                func: Box::new(Expr::Ident(Ident("test".to_owned()))),
                args: Vec::new(),
                line: 1,
            },
        ];
        let expecteds = ["test(4, testing)", "test(4)", "test()"];

        test_to_string!(inputs, expecteds)
    }

    #[test]
    fn switch_stmt_display() {
        let inputs = [
            SwitchStmt {
                expr: Expr::Ident(Ident("x".to_owned())),
                cases: vec![Case {
                    conditions: vec![Expr::Ident(Ident("y".to_owned()))],
                    block: Block(Vec::new()),
                }],
                default: None,
            },
            SwitchStmt {
                expr: Expr::Ident(Ident("x".to_owned())),
                cases: vec![
                    Case {
                        conditions: vec![Expr::Ident(Ident("y".to_owned()))],
                        block: Block(Vec::new()),
                    },
                    Case {
                        conditions: vec![Expr::Literal(Literal::Integer(3))],
                        block: Block(Vec::new()),
                    },
                ],
                default: None,
            },
            SwitchStmt {
                expr: Expr::Ident(Ident("x".to_owned())),
                cases: Vec::new(),
                default: None,
            },
            SwitchStmt {
                expr: Expr::Ident(Ident("x".to_owned())),
                cases: vec![Case {
                    conditions: vec![Expr::Ident(Ident("y".to_owned()))],
                    block: Block(Vec::new()),
                }],
                default: Some(Block(Vec::new())),
            },
            SwitchStmt {
                expr: Expr::Ident(Ident("x".to_owned())),
                cases: Vec::new(),
                default: Some(Block(Vec::new())),
            },
            SwitchStmt {
                expr: Expr::Ident(Ident("x".to_owned())),
                cases: vec![Case {
                    conditions: vec![
                        Expr::Ident(Ident("y".to_owned())),
                        Expr::Ident(Ident("z".to_owned())),
                    ],
                    block: Block(Vec::new()),
                }],
                default: None,
            },
        ];
        let expecteds = [
            "switch x { case y {} }",
            "switch x { case y {} case 3 {} }",
            "switch x {}",
            "switch x { case y {} default {} }",
            "switch x { default {} }",
            "switch x { case y | z {} }",
        ];

        test_to_string!(inputs, expecteds)
    }
}
