use crate::ast::*;
use crate::lexer::Token;
use logos::{Lexer, Logos};
use std::fmt;
use std::ops::Range;
use thiserror::Error;

#[derive(Debug, PartialEq, Eq)]
pub struct Position {
    line: usize,
    range: Range<usize>,
}

impl Position {
    pub fn line(&self) -> usize {
        self.line
    }

    pub fn range(&self) -> &Range<usize> {
        &self.range
    }

    pub fn format_on_source(&self, source: &str) -> String {
        let mut res = String::new();
        let line = self.line;
        res.push_str(&format!(
            "{}: {}",
            line,
            source
                .lines()
                .nth(line - 1)
                .expect("Line should be in source")
        ));
        res.push('\n');
        res.push_str(&(" ".repeat(line.to_string().len()) + "  "));
        res.push_str(&" ".repeat(self.range.start));
        res.push_str(&"^".repeat(self.range.len()));
        res
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.range.len() == 1 {
            write!(f, "line: {}, column: {}", self.line, self.range.end)
        } else {
            write!(
                f,
                "line: {}, range: <{}, {}>",
                self.line, self.range.start, self.range.end
            )
        }
    }
}

#[derive(Debug, Error, PartialEq)]
pub enum ParserError {
    #[error("expected `{token}` on {pos}")]
    ExpectedToken { pos: Position, token: Token },

    #[error("expected any of `{:?}` on {pos}", tokens)]
    ExpectedAnyOf { pos: Position, tokens: Vec<Token> },

    #[error("unteriminated string on {pos}")]
    UnteriminatedString { pos: Position },

    #[error("illegal character(s) on {pos}")]
    IllegalCharacter { pos: Position },

    #[error("invalid operator in this context: `{token}` on {pos}")]
    InvalidPrefixOperator { token: Token, pos: Position },

    #[error("illegal ident, cannot have number in front: `{ident}` on {pos}")]
    InvalidIdent { ident: String, pos: Position },

    #[error(
        "invalid token `{token}` at {pos}, must be used in a `{}` scope",
        expect_scope
    )]
    InvalidKeywordInScope {
        token: Token,
        expect_scope: ScopeType,
        pos: Position,
    },
}

impl ParserError {
    pub fn position(&self) -> &Position {
        match self {
            Self::ExpectedToken { pos, .. } => pos,
            Self::InvalidKeywordInScope { pos, .. } => pos,
            Self::InvalidIdent { pos, .. } => pos,
            Self::ExpectedAnyOf { pos, .. } => pos,
            Self::IllegalCharacter { pos } => pos,
            Self::UnteriminatedString { pos } => pos,
            Self::InvalidPrefixOperator { pos, .. } => pos,
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Or,
    And,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    IndexDot,
}

impl From<&Token> for Precedence {
    fn from(token: &Token) -> Self {
        match token {
            Token::Eq | Token::NotEq => Precedence::Equals,
            Token::Lt | Token::Gt | Token::Le | Token::Ge => Precedence::LessGreater,
            Token::LogicalAnd => Precedence::And,
            Token::LogicalOr => Precedence::Or,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk | Token::Percent => Precedence::Product,
            Token::Lparen => Precedence::Call,
            Token::Lbracket | Token::Dot => Precedence::IndexDot,
            _ => Precedence::Lowest,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ScopeType {
    Function,
    Loop,
    Decl,
}

impl fmt::Display for ScopeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Function => write!(f, "function"),
            Self::Loop => write!(f, "loop"),
            Self::Decl => write!(f, "decl"),
        }
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a, Token>,
    errors: Vec<ParserError>,

    current_token: Token,
    peek_token: Token,

    scope_stack: Vec<ScopeType>,

    current_len: usize,
    whitespace_len: usize,
    line: usize,
    column: usize,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        let lexer = Token::lexer(source);
        let mut parser = Self {
            lexer,
            errors: Vec::new(),

            current_token: Token::EOF,
            peek_token: Token::EOF,
            scope_stack: Vec::new(),

            current_len: 0,
            whitespace_len: 0,
            line: 1,
            column: 0,
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn parse(mut self) -> Result<Program, Vec<ParserError>> {
        let mut program = Block(Vec::new()) as Program;
        while self.current_token != Token::EOF {
            if let Some(stmt) = self.parse_stmt() {
                program.0.push(stmt);
            }
            self.next_token();
        }
        if self.errors.is_empty() {
            Ok(program)
        } else {
            Err(self.errors)
        }
    }

    pub fn position(&self) -> Position {
        if self.current_token == Token::EOF {
            let last_line_count = self
                .lexer
                .source()
                .lines()
                .last()
                .expect("Source should have one line")
                .len();
            return Position {
                line: self.lexer.source().lines().count(),
                range: last_line_count - 1..last_line_count,
            };
        }

        Position {
            line: self.line,
            range: self.column - self.current_len - self.whitespace_len
                ..self.column - self.whitespace_len,
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.current_len = self.lexer.span().len();
        self.whitespace_len = 0;
        self.column += self.current_len;
        self.peek_token = self.lexer.next().unwrap_or(Token::EOF);
        // Remove as many future newlines or whitespaces as possible
        while self.peek_token == Token::Newline || self.peek_token == Token::HorizontalWhitespace {
            if self.peek_token == Token::Newline {
                self.line += 1;
                self.column = 0;
            } else {
                let whitespace_len = self.lexer.span().len();
                self.column += whitespace_len;
                // Adds to current whitespace length so when self.position() is called, the
                // whitespace consumed ahead of time does not factor into the position returned.
                self.whitespace_len += whitespace_len;
            }
            self.peek_token = self.lexer.next().unwrap_or(Token::EOF);
        }
    }

    fn expect_peek(&mut self, token: Token) -> Option<()> {
        if self.peek_token == token {
            self.next_token();
            Some(())
        } else {
            self.errors.push(ParserError::ExpectedToken {
                pos: self.position(),
                token,
            });
            None
        }
    }

    fn peek_prec(&self) -> Precedence {
        Precedence::from(&self.peek_token)
    }

    fn current_prec(&self) -> Precedence {
        Precedence::from(&self.current_token)
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        let stmt = match self.current_token {
            Token::Ident(_) if self.peek_token == Token::Assign => {
                Stmt::Assign(self.parse_assign_stmt()?)
            }
            Token::Function if matches!(self.peek_token, Token::Ident(..)) => {
                Stmt::Function(self.parse_function_stmt()?)
            }
            Token::Return => {
                self.scopes_contain(ScopeType::Function)?;
                Stmt::Return(self.parse_return_stmt()?)
            }
            Token::If => Stmt::If(self.parse_if_stmt()?),
            Token::For => Stmt::For(self.parse_for_stmt()?),
            Token::While => Stmt::While(self.parse_while_stmt()?),
            Token::Declaration => Stmt::Declaration(self.parse_decl_stmt()?),
            Token::Switch => Stmt::Switch(self.parse_switch_stmt()?),
            Token::Break => {
                self.validate_current_scope(ScopeType::Loop)?;
                Stmt::Break
            }
            Token::Continue => {
                self.validate_current_scope(ScopeType::Loop)?;
                Stmt::Continue
            }
            Token::Ident(ref name) if self.peek_token == Token::Dot => {
                let cloned_lex = self.lexer.clone();
                let cloned_name = name.clone();
                self.next_token();
                self.next_token();
                if self.peek_token == Token::Assign {
                    Stmt::Assign(self.parse_dot_assign_stmt(Ident(cloned_name))?)
                } else {
                    // Put the lexer back into its old state
                    self.current_token = Token::Ident(cloned_name);
                    self.peek_token = Token::Dot;
                    self.lexer = cloned_lex;
                    Stmt::Expr(self.parse_expr_stmt()?)
                }
            }
            Token::Ident(ref name) if self.peek_token == Token::Lbracket => {
                let cloned_lex = self.lexer.clone();
                let cloned_name = name.clone();
                self.next_token();
                self.next_token();
                let index = self.parse_expr(Precedence::Lowest)?;
                self.expect_peek(Token::Rbracket)?;
                if self.peek_token == Token::Assign {
                    Stmt::Assign(self.parse_index_assign_stmt(Ident(cloned_name), index)?)
                } else {
                    // Put the lexer back into its old state
                    self.current_token = Token::Ident(cloned_name);
                    self.peek_token = Token::Lbracket;
                    self.lexer = cloned_lex;
                    Stmt::Expr(self.parse_expr_stmt()?)
                }
            }

            // Expression on one line
            _ => Stmt::Expr(self.parse_expr_stmt()?),
        };
        if self.current_token == Token::Rbrace && self.peek_token == Token::Semicolon {
            self.next_token();
        } else if self.current_token != Token::Rbrace {
            self.expect_peek(Token::Semicolon);
        }
        Some(stmt)
    }

    fn parse_expr_stmt(&mut self) -> Option<Expr> {
        let expr = self.parse_expr(Precedence::Lowest)?;
        Some(expr)
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Option<Expr> {
        let mut left_exp = match self.current_token {
            Token::Integer(i) => Expr::Literal(Literal::Integer(i)),
            Token::Float(f) => Expr::Literal(Literal::Float(f)),
            Token::True => Expr::Literal(Literal::Boolean(true)),
            Token::False => Expr::Literal(Literal::Boolean(false)),
            Token::Nil => Expr::Literal(Literal::Nil),
            Token::Lbracket => {
                Expr::Literal(Literal::Array(self.parse_expr_list(Token::Rbracket)?))
            }
            Token::String(ref s) => Expr::Literal(Literal::String(s.to_owned())),
            Token::Ident(_) => Expr::Ident(self.parse_ident().expect("Should be on ident")),
            Token::Bang | Token::Minus | Token::Plus => Expr::Prefix(self.parse_prefix_expr()?),
            Token::Function => Expr::Function(self.parse_function_expr()?),
            Token::Lparen => self.parse_grouped_expr()?,
            Token::Lbrace => Expr::Literal(Literal::Map(self.parse_map_literal()?)),
            Token::Error => {
                self.errors.push(ParserError::IllegalCharacter {
                    pos: self.position(),
                });
                return None;
            }
            Token::ErrorUnterminatedString => {
                self.errors.push(ParserError::UnteriminatedString {
                    pos: self.position(),
                });
                return None;
            }
            Token::ErrorInvalidIdent(ref s) => {
                self.errors.push(ParserError::InvalidIdent {
                    ident: s.to_owned(),
                    pos: self.position(),
                });
                return None;
            }
            _ => {
                self.errors.push(ParserError::InvalidPrefixOperator {
                    token: self.current_token.clone(),
                    pos: self.position(),
                });
                return None;
            }
        };

        while self.peek_token != Token::Semicolon && precedence < self.peek_prec() {
            left_exp = match self.peek_token {
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::Eq
                | Token::NotEq
                | Token::Lt
                | Token::Gt
                | Token::Ge
                | Token::Le
                | Token::LogicalAnd
                | Token::LogicalOr
                | Token::Percent => {
                    self.next_token();
                    Expr::Infix(self.parse_infix_expr(left_exp)?)
                }
                Token::Lbracket => {
                    self.next_token();
                    Expr::Index(self.parse_index_expr(left_exp)?)
                }
                Token::Dot => {
                    self.next_token();
                    Expr::Dot(self.parse_dot_expr(left_exp)?)
                }
                Token::Lparen => {
                    self.next_token();
                    Expr::Call(self.parse_call_expr(left_exp)?)
                }
                _ => return Some(left_exp),
            };
        }
        Some(left_exp)
    }

    fn parse_ident(&mut self) -> Option<Ident> {
        if let Token::Ident(ident) = &self.current_token {
            Some(Ident(ident.to_owned()))
        } else {
            self.errors.push(ParserError::ExpectedToken {
                pos: self.position(),
                token: Token::Ident("".to_owned()),
            });
            None
        }
    }

    fn parse_grouped_expr(&mut self) -> Option<Expr> {
        if self.current_token != Token::Lparen {
            return None;
        }
        self.next_token();
        let expr = self.parse_expr(Precedence::Lowest)?;
        self.expect_peek(Token::Rparen)?;
        Some(expr)
    }

    fn parse_infix_expr(&mut self, left: Expr) -> Option<InfixExpr> {
        let infix_op = InfixOp::try_from(&self.current_token).ok()?;
        let precedence = self.current_prec();
        self.next_token();
        let right = self.parse_expr(precedence)?;
        Some(InfixExpr {
            left: Box::new(left),
            op: infix_op,
            right: Box::new(right),
            line: self.line,
        })
    }

    fn parse_dot_expr(&mut self, left_exp: Expr) -> Option<DotExpr> {
        self.next_token();
        let ident = self.parse_ident()?;
        Some(DotExpr {
            left: Box::new(left_exp),
            field: ident,
            line: self.line,
        })
    }

    fn parse_prefix_expr(&mut self) -> Option<PrefixExpr> {
        let prefix_op = PrefixOp::try_from(&self.current_token).ok()?;
        self.next_token();
        let expr = self.parse_expr(Precedence::Prefix)?;
        Some(PrefixExpr {
            op: prefix_op,
            left: Box::new(expr),
            line: self.line,
        })
    }

    fn parse_function_expr(&mut self) -> Option<FunctionExpr> {
        self.expect_peek(Token::Lparen)?;
        let params = self.parse_function_params()?;
        self.expect_peek(Token::Lbrace)?;
        self.scope_stack.push(ScopeType::Function);
        let block = self.parse_block()?;
        self.scope_stack.pop().expect("Should have scope on stack");
        Some(FunctionExpr { params, block })
    }

    fn parse_block(&mut self) -> Option<Block> {
        let mut statements = Block(Vec::new());
        self.next_token();
        while self.current_token != Token::Rbrace && self.current_token != Token::EOF {
            if let Some(stmt) = self.parse_stmt() {
                statements.0.push(stmt);
            }
            self.next_token();
        }
        Some(statements)
    }

    fn validate_current_scope(&mut self, expect_scope: ScopeType) -> Option<ScopeType> {
        if let Some(scope) = self.scope_stack.last() {
            if scope == &expect_scope {
                return Some(expect_scope);
            }
        }
        self.errors.push(ParserError::InvalidKeywordInScope {
            token: self.current_token.clone(),
            expect_scope,
            pos: self.position(),
        });
        None
    }

    fn scopes_contain(&mut self, expect_scope: ScopeType) -> Option<ScopeType> {
        if self.scope_stack.contains(&expect_scope) {
            Some(expect_scope)
        } else {
            self.errors.push(ParserError::InvalidKeywordInScope {
                token: self.current_token.clone(),
                expect_scope,
                pos: self.position(),
            });
            None
        }
    }

    fn parse_return_stmt(&mut self) -> Option<ReturnStmt> {
        self.next_token();
        let return_val = self.parse_expr(Precedence::Lowest)?;
        Some(ReturnStmt { value: return_val })
    }

    fn parse_assign_stmt(&mut self) -> Option<AssignStmt> {
        let ident = self.parse_ident()?;
        self.expect_peek(Token::Assign)?;
        self.next_token();
        let value = self.parse_expr(Precedence::Lowest)?;
        Some(AssignStmt {
            name: ident,
            value,
            field: None,
            index: None,
            line: self.line,
        })
    }

    fn parse_dot_assign_stmt(&mut self, ident: Ident) -> Option<AssignStmt> {
        let field = self.parse_ident()?;
        self.expect_peek(Token::Assign)?;
        self.next_token();
        let value = self.parse_expr(Precedence::Lowest)?;
        Some(AssignStmt {
            name: ident,
            value,
            field: Some(field),
            index: None,
            line: self.line,
        })
    }

    fn parse_index_assign_stmt(&mut self, ident: Ident, index: Expr) -> Option<AssignStmt> {
        self.expect_peek(Token::Assign)?;
        self.next_token();
        let value = self.parse_expr(Precedence::Lowest)?;
        Some(AssignStmt {
            name: ident,
            value,
            field: None,
            index: Some(index),
            line: self.line,
        })
    }

    fn parse_if_stmt(&mut self) -> Option<IfStmt> {
        self.next_token();
        let condition = self.parse_expr(Precedence::Lowest)?;
        self.expect_peek(Token::Lbrace)?;
        let true_block = self.parse_block()?;
        let mut elifs = Vec::new();
        while self.peek_token == Token::Elif {
            // To elif
            self.next_token();
            // To expression
            self.next_token();
            let condition = self.parse_expr(Precedence::Lowest)?;
            self.expect_peek(Token::Lbrace)?;
            let elif_block = self.parse_block()?;
            elifs.push(ElifStmt {
                condition,
                block: elif_block,
            });
        }
        let mut else_block = None;
        if self.peek_token == Token::Else {
            self.next_token();
            self.expect_peek(Token::Lbrace)?;
            else_block = Some(self.parse_block()?);
        }
        Some(IfStmt {
            condition,
            true_block,
            else_block,
            elifs,
        })
    }

    fn parse_for_stmt(&mut self) -> Option<ForStmt> {
        self.next_token();
        let ident = self.parse_ident()?;
        self.expect_peek(Token::In)?;
        self.next_token();
        let expr = self.parse_expr(Precedence::Lowest)?;
        self.expect_peek(Token::Lbrace)?;
        self.scope_stack.push(ScopeType::Loop);
        let block = self.parse_block()?;
        self.scope_stack.pop().expect("Should have scope on stack");
        Some(ForStmt {
            iter_ident: ident,
            expr,
            block,
            line: self.line,
        })
    }

    fn parse_while_stmt(&mut self) -> Option<WhileStmt> {
        self.next_token();
        let condition = self.parse_expr(Precedence::Lowest)?;
        self.expect_peek(Token::Lbrace)?;
        self.scope_stack.push(ScopeType::Loop);
        let block = self.parse_block()?;
        self.scope_stack.pop().expect("Should have scope on stack");
        Some(WhileStmt { condition, block })
    }

    fn parse_function_stmt(&mut self) -> Option<FunctionStmt> {
        let mut special = false;
        if self.peek_token == Token::Dollar {
            self.next_token();
            self.scopes_contain(ScopeType::Decl)?;
            special = true;
        }
        self.next_token();
        let name = {
            let mut name = self.parse_ident()?;
            if special {
                // Flags as special because idents cannot contain '$'
                name.0.insert(0, '$');
            }
            name
        };
        self.expect_peek(Token::Lparen)?;
        let params = self.parse_function_params()?;
        self.expect_peek(Token::Lbrace)?;
        self.scope_stack.push(ScopeType::Function);
        let block = self.parse_block()?;
        self.scope_stack.pop().expect("Should have scope on stack");
        Some(FunctionStmt {
            name,
            params,
            block,
        })
    }

    fn parse_decl_stmt(&mut self) -> Option<DeclarationStmt> {
        self.next_token();
        let name = self.parse_ident()?;

        self.expect_peek(Token::Lbrace)?;
        self.next_token();
        let mut fields = Vec::new();
        let mut methods = Vec::new();
        let mut embeds = Vec::new();
        while self.current_token != Token::Rbrace {
            match &self.current_token {
                Token::Ident(ident) => fields.push(Ident(ident.to_owned())),
                Token::Lbracket => {
                    self.next_token();
                    let embed_name = self.parse_ident()?;
                    self.expect_peek(Token::Rbracket)?;
                    self.expect_peek(Token::Lbrace)?;
                    self.next_token();
                    let mut assigned = Vec::new();
                    if self.current_token != Token::Rbrace {
                        assigned.push(self.parse_ident()?);
                        while self.peek_token == Token::Comma {
                            self.next_token();
                            self.next_token();
                            let field = self.parse_ident()?;
                            assigned.push(field);
                        }
                        self.expect_peek(Token::Rbrace)?;
                    }
                    embeds.push(Embed {
                        name: embed_name,
                        assigned,
                        line: self.line,
                    })
                }
                Token::Function => {
                    self.scope_stack.push(ScopeType::Decl);
                    let function = self.parse_function_stmt()?;
                    self.scope_stack.pop().expect("should have scope on stack");
                    methods.push(function);
                }
                _ => self.errors.push(ParserError::ExpectedAnyOf {
                    pos: self.position(),
                    tokens: vec![Token::Lbrace, Token::Function, Token::Ident("".to_owned())],
                }),
            }
            self.next_token();
        }
        Some(DeclarationStmt {
            name,
            methods,
            fields,
            embeds,
        })
    }

    fn parse_switch_stmt(&mut self) -> Option<SwitchStmt> {
        self.next_token();
        let expr = self.parse_expr(Precedence::Lowest)?;
        self.expect_peek(Token::Lbrace)?;
        self.next_token();
        if self.current_token == Token::Rbrace {
            return Some(SwitchStmt {
                expr,
                cases: Vec::new(),
                default: None,
            });
        }
        let mut cases = Vec::new();
        let mut default_case = None;
        while self.current_token != Token::Rbrace {
            if self.current_token == Token::Default {
                self.expect_peek(Token::Lbrace)?;
                default_case = Some(self.parse_block()?);
                self.next_token();
                continue;
            }
            if self.current_token != Token::Case {
                self.errors.push(ParserError::ExpectedAnyOf {
                    pos: self.position(),
                    tokens: vec![Token::Default, Token::Case],
                });
                return None;
            }
            self.next_token();
            let mut conditions = Vec::new();
            conditions.push(self.parse_expr(Precedence::Lowest)?);
            while self.peek_token == Token::Pipe {
                self.next_token();
                self.next_token();
                conditions.push(self.parse_expr(Precedence::Lowest)?);
            }
            self.expect_peek(Token::Lbrace)?;
            let block = self.parse_block()?;
            self.next_token();
            cases.push(Case { conditions, block })
        }

        Some(SwitchStmt {
            expr,
            cases,
            default: default_case,
        })
    }

    fn parse_function_params(&mut self) -> Option<Vec<Ident>> {
        let mut idents = Vec::new();
        if self.peek_token == Token::Rparen {
            self.next_token();
            return Some(idents);
        }
        self.next_token();
        let ident = self.parse_ident()?;
        idents.push(ident);

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            let ident = self.parse_ident()?;
            idents.push(ident);
        }

        self.expect_peek(Token::Rparen);

        Some(idents)
    }

    fn parse_call_expr(&mut self, left_exp: Expr) -> Option<CallExpr> {
        let args = self.parse_expr_list(Token::Rparen)?;
        Some(CallExpr {
            func: Box::new(left_exp),
            args,
            line: self.line,
        })
    }

    fn parse_expr_list(&mut self, end: Token) -> Option<Vec<Expr>> {
        let mut exprs = Vec::new();
        if self.peek_token == end {
            self.next_token();
            return Some(exprs);
        }

        self.next_token();
        exprs.push(self.parse_expr(Precedence::Lowest)?);

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            exprs.push(self.parse_expr(Precedence::Lowest)?);
        }
        self.expect_peek(end)?;

        Some(exprs)
    }

    fn parse_index_expr(&mut self, left_exp: Expr) -> Option<IndexExpr> {
        self.next_token();
        let index = self.parse_expr(Precedence::Lowest)?;
        self.expect_peek(Token::Rbracket)?;
        Some(IndexExpr {
            left: Box::new(left_exp),
            index: Box::new(index),
            line: self.line,
        })
    }

    fn parse_map_literal(&mut self) -> Option<Vec<(Expr, Expr)>> {
        let mut pairs = Vec::new();
        while self.peek_token != Token::Rbrace {
            self.next_token();
            let key = self.parse_expr(Precedence::Lowest)?;
            self.expect_peek(Token::Colon)?;
            self.next_token();
            let value = self.parse_expr(Precedence::Lowest)?;
            pairs.push((key, value));
            if self.peek_token != Token::Rbrace && self.expect_peek(Token::Comma).is_none() {
                return None;
            }
        }
        self.expect_peek(Token::Rbrace)
            .expect("Should have an rbrace");
        Some(pairs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_parse {
        ($inputs:expr, $expecteds:expr) => {
            for (input, expected) in $inputs.iter().zip($expecteds) {
                let parser = Parser::new(input);
                let program = parser.parse().expect("Should have no errors");
                assert_eq!(expected, program.0[0]);
            }
        };
    }

    macro_rules! test_parse_errs {
        ($inputs:expr, $errs:expr) => {
            for (input, err) in $inputs.iter().zip($errs) {
                let parser = Parser::new(input);
                let errs = parser.parse().unwrap_err();
                assert_eq!(err, errs[0]);
            }
        };
    }

    #[test]
    fn parse_literal_exprs() {
        let inputs = ["123;", "3.3;", "\"test\";", "True;", "Nil;"];
        let expecteds = [
            Stmt::Expr(Expr::Literal(Literal::Integer(123))),
            Stmt::Expr(Expr::Literal(Literal::Float(3.3))),
            Stmt::Expr(Expr::Literal(Literal::String("test".to_owned()))),
            Stmt::Expr(Expr::Literal(Literal::Boolean(true))),
            Stmt::Expr(Expr::Literal(Literal::Nil)),
        ];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn tracks_first_line() {
        let input = "123 456";
        let parser = Parser::new(input);
        assert_eq!(
            Position {
                line: 1,
                range: 0..3,
            },
            parser.position()
        );
    }

    #[test]
    fn tracks_position_horizontally() {
        let input = "123 456";
        let mut parser = Parser::new(input);
        parser.next_token();
        assert_eq!(
            Position {
                line: 1,
                range: 4..7,
            },
            parser.position()
        );
    }

    #[test]
    fn tracks_position_vertically() {
        let input = "123



456";
        let mut parser = Parser::new(input);

        assert_eq!(Token::Integer(123), parser.current_token);
        parser.next_token();
        assert_eq!(Token::Integer(456), parser.current_token);
        assert_eq!(
            Position {
                line: 5,
                range: 0..3,
            },
            parser.position()
        )
    }

    #[test]
    fn parse_let_stmt() {
        let inputs = ["x = 3;"];
        let expecteds = [Stmt::Assign(AssignStmt {
            name: Ident("x".to_owned()),
            value: Expr::Literal(Literal::Integer(3)),
            index: None,
            field: None,
            line: 1,
        })];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_stmt_with_no_significant_whitespace() {
        let inputs = ["x
            = 3;"];
        let expecteds = [Stmt::Assign(AssignStmt {
            name: Ident("x".to_owned()),
            value: Expr::Literal(Literal::Integer(3)),
            index: None,
            field: None,
            line: 2,
        })];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_stmt_on_line_2() {
        let inputs = ["
            x = 3;
            "];
        let expecteds = [Stmt::Assign(AssignStmt {
            name: Ident("x".to_owned()),
            value: Expr::Literal(Literal::Integer(3)),
            field: None,
            index: None,
            line: 2,
        })];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn operator_parsing_doesnt_cause_panic_with_eof() {
        let inputs = ["1+"];
        let errs = [ParserError::InvalidPrefixOperator {
            token: Token::EOF,
            pos: Position {
                line: 1,
                range: 1..2,
            },
        }];

        test_parse_errs!(inputs, errs)
    }

    #[test]
    fn invalid_prefix_operators_recognized() {
        let inputs = ["/1;", "=1;"];
        let errs = [
            ParserError::InvalidPrefixOperator {
                token: Token::Slash,
                pos: Position {
                    line: 1,
                    range: 0..1,
                },
            },
            ParserError::InvalidPrefixOperator {
                token: Token::Assign,
                pos: Position {
                    line: 1,
                    range: 0..1,
                },
            },
        ];

        test_parse_errs!(inputs, errs)
    }

    #[test]
    fn needs_semicolon_at_end_of_statements() {
        let inputs = ["x = 3", "333"];
        let errs = [
            ParserError::ExpectedToken {
                pos: Position {
                    line: 1,
                    range: 4..5,
                },
                token: Token::Semicolon,
            },
            ParserError::ExpectedToken {
                pos: Position {
                    line: 1,
                    range: 0..3,
                },
                token: Token::Semicolon,
            },
        ];

        test_parse_errs!(inputs, errs)
    }

    #[test]
    fn parser_throws_error_at_invalid_tokens() {
        let inputs = ["#", "$", "&"];
        let errs = [
            ParserError::IllegalCharacter {
                pos: Position {
                    line: 1,
                    range: 0..1,
                },
            },
            ParserError::IllegalCharacter {
                pos: Position {
                    line: 1,
                    range: 0..1,
                },
            },
            ParserError::IllegalCharacter {
                pos: Position {
                    line: 1,
                    range: 0..1,
                },
            },
        ];

        test_parse_errs!(inputs, errs)
    }

    #[test]
    fn parser_throws_error_at_unterminated_string() {
        let inputs = ["\"invalid!", "\"two words"];
        let errs = [
            ParserError::UnteriminatedString {
                pos: Position {
                    line: 1,
                    range: 0..9,
                },
            },
            ParserError::UnteriminatedString {
                pos: Position {
                    line: 1,
                    range: 0..10,
                },
            },
        ];

        test_parse_errs!(inputs, errs)
    }

    #[test]
    fn parse_basic_binary_operators() {
        let inputs = [
            "3 + 3;",
            "4 / 5;",
            "2 * 2;",
            "4 - 4;",
            "20 % 2;",
            "3 == 3;",
            "6 >= 3;",
            "8 != 8;",
            "9 && 9;",
            "10 || 10;",
        ];
        let expecteds = [
            Stmt::Expr(Expr::Infix(InfixExpr {
                left: Box::new(Expr::Literal(Literal::Integer(3))),
                op: InfixOp::Plus,
                right: Box::new(Expr::Literal(Literal::Integer(3))),
                line: 1,
            })),
            Stmt::Expr(Expr::Infix(InfixExpr {
                left: Box::new(Expr::Literal(Literal::Integer(4))),
                op: InfixOp::Slash,
                right: Box::new(Expr::Literal(Literal::Integer(5))),
                line: 1,
            })),
            Stmt::Expr(Expr::Infix(InfixExpr {
                left: Box::new(Expr::Literal(Literal::Integer(2))),
                op: InfixOp::Asterisk,
                right: Box::new(Expr::Literal(Literal::Integer(2))),
                line: 1,
            })),
            Stmt::Expr(Expr::Infix(InfixExpr {
                left: Box::new(Expr::Literal(Literal::Integer(4))),
                op: InfixOp::Minus,
                right: Box::new(Expr::Literal(Literal::Integer(4))),
                line: 1,
            })),
            Stmt::Expr(Expr::Infix(InfixExpr {
                left: Box::new(Expr::Literal(Literal::Integer(20))),
                op: InfixOp::Modulo,
                right: Box::new(Expr::Literal(Literal::Integer(2))),
                line: 1,
            })),
            Stmt::Expr(Expr::Infix(InfixExpr {
                left: Box::new(Expr::Literal(Literal::Integer(3))),
                op: InfixOp::Eq,
                right: Box::new(Expr::Literal(Literal::Integer(3))),
                line: 1,
            })),
            Stmt::Expr(Expr::Infix(InfixExpr {
                left: Box::new(Expr::Literal(Literal::Integer(6))),
                op: InfixOp::Ge,
                right: Box::new(Expr::Literal(Literal::Integer(3))),
                line: 1,
            })),
            Stmt::Expr(Expr::Infix(InfixExpr {
                left: Box::new(Expr::Literal(Literal::Integer(8))),
                op: InfixOp::NotEq,
                right: Box::new(Expr::Literal(Literal::Integer(8))),
                line: 1,
            })),
            Stmt::Expr(Expr::Infix(InfixExpr {
                left: Box::new(Expr::Literal(Literal::Integer(9))),
                op: InfixOp::LogicalAnd,
                right: Box::new(Expr::Literal(Literal::Integer(9))),
                line: 1,
            })),
            Stmt::Expr(Expr::Infix(InfixExpr {
                left: Box::new(Expr::Literal(Literal::Integer(10))),
                op: InfixOp::LogicalOr,
                right: Box::new(Expr::Literal(Literal::Integer(10))),
                line: 1,
            })),
        ];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_unary_operators() {
        let inputs = ["-1;", "!True;", "+44;"];
        let expecteds = [
            Stmt::Expr(Expr::Prefix(PrefixExpr {
                left: Box::new(Expr::Literal(Literal::Integer(1))),
                op: PrefixOp::Minus,
                line: 1,
            })),
            Stmt::Expr(Expr::Prefix(PrefixExpr {
                left: Box::new(Expr::Literal(Literal::Boolean(true))),
                op: PrefixOp::Bang,
                line: 1,
            })),
            Stmt::Expr(Expr::Prefix(PrefixExpr {
                left: Box::new(Expr::Literal(Literal::Integer(44))),
                op: PrefixOp::Plus,
                line: 1,
            })),
        ];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn operator_precedence() {
        let inputs = [
            "-1 + 4;",
            "4 - 4 * 3;",
            "(3 + 3) * 4;",
            "-1 + -1;",
            "3 == 3 + 3;",
            "4 == 4 && True;",
            "3 && 3 || 3;",
        ];
        let expecteds = [
            Stmt::Expr(Expr::Infix(InfixExpr {
                left: Box::new(Expr::Prefix(PrefixExpr {
                    left: Box::new(Expr::Literal(Literal::Integer(1))),
                    op: PrefixOp::Minus,
                    line: 1,
                })),
                op: InfixOp::Plus,
                right: Box::new(Expr::Literal(Literal::Integer(4))),
                line: 1,
            })),
            Stmt::Expr(Expr::Infix(InfixExpr {
                left: Box::new(Expr::Literal(Literal::Integer(4))),
                op: InfixOp::Minus,
                right: Box::new(Expr::Infix(InfixExpr {
                    left: Box::new(Expr::Literal(Literal::Integer(4))),
                    op: InfixOp::Asterisk,
                    right: Box::new(Expr::Literal(Literal::Integer(3))),
                    line: 1,
                })),
                line: 1,
            })),
            Stmt::Expr(Expr::Infix(InfixExpr {
                left: Box::new(Expr::Infix(InfixExpr {
                    left: Box::new(Expr::Literal(Literal::Integer(3))),
                    op: InfixOp::Plus,
                    right: Box::new(Expr::Literal(Literal::Integer(3))),
                    line: 1,
                })),
                op: InfixOp::Asterisk,
                right: Box::new(Expr::Literal(Literal::Integer(4))),
                line: 1,
            })),
            Stmt::Expr(Expr::Infix(InfixExpr {
                left: Box::new(Expr::Prefix(PrefixExpr {
                    left: Box::new(Expr::Literal(Literal::Integer(1))),
                    op: PrefixOp::Minus,
                    line: 1,
                })),
                op: InfixOp::Plus,
                right: Box::new(Expr::Prefix(PrefixExpr {
                    left: Box::new(Expr::Literal(Literal::Integer(1))),
                    op: PrefixOp::Minus,
                    line: 1,
                })),
                line: 1,
            })),
            Stmt::Expr(Expr::Infix(InfixExpr {
                left: Box::new(Expr::Literal(Literal::Integer(3))),
                op: InfixOp::Eq,
                right: Box::new(Expr::Infix(InfixExpr {
                    left: Box::new(Expr::Literal(Literal::Integer(3))),
                    op: InfixOp::Plus,
                    right: Box::new(Expr::Literal(Literal::Integer(3))),
                    line: 1,
                })),
                line: 1,
            })),
            Stmt::Expr(Expr::Infix(InfixExpr {
                left: Box::new(Expr::Infix(InfixExpr {
                    left: Box::new(Expr::Literal(Literal::Integer(4))),
                    op: InfixOp::Eq,
                    right: Box::new(Expr::Literal(Literal::Integer(4))),
                    line: 1,
                })),
                op: InfixOp::LogicalAnd,
                right: Box::new(Expr::Literal(Literal::Boolean(true))),
                line: 1,
            })),
            Stmt::Expr(Expr::Infix(InfixExpr {
                left: Box::new(Expr::Infix(InfixExpr {
                    left: Box::new(Expr::Literal(Literal::Integer(3))),
                    op: InfixOp::LogicalAnd,
                    right: Box::new(Expr::Literal(Literal::Integer(3))),
                    line: 1,
                })),
                op: InfixOp::LogicalOr,
                right: Box::new(Expr::Literal(Literal::Integer(3))),
                line: 1,
            })),
        ];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_if_stmt() {
        let inputs = ["if 1 { 3; };"];
        let expecteds = [Stmt::If(IfStmt {
            condition: Expr::Literal(Literal::Integer(1)),
            true_block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(3)))]),
            else_block: None,
            elifs: Vec::new(),
        })];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_if_else_stmt() {
        let inputs = ["if 1 { 3; } else { 4; };"];
        let expecteds = [Stmt::If(IfStmt {
            condition: Expr::Literal(Literal::Integer(1)),
            true_block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(3)))]),
            else_block: Some(Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(4)))])),
            elifs: Vec::new(),
        })];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_if_elif_stmt() {
        let inputs = ["if 1 { 3; } elif True { 4; };"];
        let expecteds = [Stmt::If(IfStmt {
            condition: Expr::Literal(Literal::Integer(1)),
            true_block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(3)))]),
            else_block: None,
            elifs: vec![ElifStmt {
                condition: Expr::Literal(Literal::Boolean(true)),
                block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(4)))]),
            }],
        })];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_if_elif_stmt_with_multiple_elifs() {
        let inputs = ["if 1 { 3; } elif True { 4; } elif False { 5; };"];
        let expecteds = [Stmt::If(IfStmt {
            condition: Expr::Literal(Literal::Integer(1)),
            true_block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(3)))]),
            else_block: None,
            elifs: vec![
                ElifStmt {
                    condition: Expr::Literal(Literal::Boolean(true)),
                    block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(4)))]),
                },
                ElifStmt {
                    condition: Expr::Literal(Literal::Boolean(false)),
                    block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(5)))]),
                },
            ],
        })];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_if_elif_stmt_with_multiple_elifs_and_else() {
        let inputs = ["if 1 { 3; } elif True { 4; } elif False { 5; } else { 4; };"];
        let expecteds = [Stmt::If(IfStmt {
            condition: Expr::Literal(Literal::Integer(1)),
            true_block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(3)))]),
            else_block: Some(Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(4)))])),
            elifs: vec![
                ElifStmt {
                    condition: Expr::Literal(Literal::Boolean(true)),
                    block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(4)))]),
                },
                ElifStmt {
                    condition: Expr::Literal(Literal::Boolean(false)),
                    block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(5)))]),
                },
            ],
        })];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn rbrace_stmts_have_optional_semicolons() {
        let inputs = ["if 1 { 3; }"];
        let expecteds = [Stmt::If(IfStmt {
            condition: Expr::Literal(Literal::Integer(1)),
            true_block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(3)))]),
            else_block: None,
            elifs: Vec::new(),
        })];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn errors_appear_in_expressions_nested_in_statements() {
        let inputs = ["x = \"hello", "x = #;"];
        let errs = [
            ParserError::UnteriminatedString {
                pos: Position {
                    line: 1,
                    range: 4..10,
                },
            },
            ParserError::IllegalCharacter {
                pos: Position {
                    line: 1,
                    range: 4..5,
                },
            },
        ];

        test_parse_errs!(inputs, errs)
    }

    #[test]
    fn parser_tracks_line_numbers_in_expressions() {
        let inputs = ["(1 + 1)
        * 3;"];
        let expecteds = [Stmt::Expr(Expr::Infix(InfixExpr {
            left: Box::new(Expr::Infix(InfixExpr {
                left: Box::new(Expr::Literal(Literal::Integer(1))),
                op: InfixOp::Plus,
                right: Box::new(Expr::Literal(Literal::Integer(1))),
                line: 1,
            })),
            op: InfixOp::Asterisk,
            right: Box::new(Expr::Literal(Literal::Integer(3))),
            line: 2,
        }))];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_dot_expr() {
        let inputs = ["hello.world;", "(1 + 1).test;"];
        let expecteds = [
            Stmt::Expr(Expr::Dot(DotExpr {
                left: Box::new(Expr::Ident(Ident("hello".to_owned()))),
                field: Ident("world".to_owned()),
                line: 1,
            })),
            Stmt::Expr(Expr::Dot(DotExpr {
                left: Box::new(Expr::Infix(InfixExpr {
                    left: Box::new(Expr::Literal(Literal::Integer(1))),
                    op: InfixOp::Plus,
                    right: Box::new(Expr::Literal(Literal::Integer(1))),
                    line: 1,
                })),
                field: Ident("test".to_owned()),
                line: 1,
            })),
        ];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_for_stmt() {
        let inputs = ["for i in 1 { i; }", "for ident in 3 + 3 { ident; }"];
        let expecteds = [
            Stmt::For(ForStmt {
                iter_ident: Ident("i".to_owned()),
                expr: Expr::Literal(Literal::Integer(1)),
                block: Block(vec![Stmt::Expr(Expr::Ident(Ident("i".to_owned())))]),
                line: 1,
            }),
            Stmt::For(ForStmt {
                iter_ident: Ident("ident".to_owned()),
                expr: Expr::Infix(InfixExpr {
                    left: Box::new(Expr::Literal(Literal::Integer(3))),
                    op: InfixOp::Plus,
                    right: Box::new(Expr::Literal(Literal::Integer(3))),
                    line: 1,
                }),
                block: Block(vec![Stmt::Expr(Expr::Ident(Ident("ident".to_owned())))]),
                line: 1,
            }),
        ];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_while_stmt() {
        let inputs = ["while True { 3; }", "while 3 == 3 { x; }"];
        let expecteds = [
            Stmt::While(WhileStmt {
                condition: Expr::Literal(Literal::Boolean(true)),
                block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(3)))]),
            }),
            Stmt::While(WhileStmt {
                condition: Expr::Infix(InfixExpr {
                    left: Box::new(Expr::Literal(Literal::Integer(3))),
                    op: InfixOp::Eq,
                    right: Box::new(Expr::Literal(Literal::Integer(3))),
                    line: 1,
                }),
                block: Block(vec![Stmt::Expr(Expr::Ident(Ident("x".to_owned())))]),
            }),
        ];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_continue_and_break_stmt() {
        let inputs = [
            "for i in x { continue; }",
            "for i in x { break; }",
            "while True { continue; }",
            "while False { break; }",
        ];
        let expecteds = [
            Stmt::For(ForStmt {
                iter_ident: Ident("i".to_owned()),
                expr: Expr::Ident(Ident("x".to_owned())),
                block: Block(vec![Stmt::Continue]),
                line: 1,
            }),
            Stmt::For(ForStmt {
                iter_ident: Ident("i".to_owned()),
                expr: Expr::Ident(Ident("x".to_owned())),
                block: Block(vec![Stmt::Break]),
                line: 1,
            }),
            Stmt::While(WhileStmt {
                condition: Expr::Literal(Literal::Boolean(true)),
                block: Block(vec![Stmt::Continue]),
            }),
            Stmt::While(WhileStmt {
                condition: Expr::Literal(Literal::Boolean(false)),
                block: Block(vec![Stmt::Break]),
            }),
        ];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn continue_and_break_can_only_be_used_in_loops() {
        let inputs = ["continue;", "break;"];
        let errs = [
            ParserError::InvalidKeywordInScope {
                token: Token::Continue,
                expect_scope: ScopeType::Loop,
                pos: Position {
                    line: 1,
                    range: 0..8,
                },
            },
            ParserError::InvalidKeywordInScope {
                token: Token::Break,
                expect_scope: ScopeType::Loop,
                pos: Position {
                    line: 1,
                    range: 0..5,
                },
            },
        ];

        test_parse_errs!(inputs, errs)
    }

    #[test]
    fn parse_function_expr() {
        let inputs = ["fn(x) { x; }", "fn(e, y) { e + y; }"];
        let expecteds = [
            Stmt::Expr(Expr::Function(FunctionExpr {
                params: vec![Ident("x".to_owned())],
                block: Block(vec![Stmt::Expr(Expr::Ident(Ident("x".to_owned())))]),
            })),
            Stmt::Expr(Expr::Function(FunctionExpr {
                params: vec![Ident("e".to_owned()), Ident("y".to_owned())],
                block: Block(vec![Stmt::Expr(Expr::Infix(InfixExpr {
                    left: Box::new(Expr::Ident(Ident("e".to_owned()))),
                    op: InfixOp::Plus,
                    right: Box::new(Expr::Ident(Ident("y".to_owned()))),
                    line: 1,
                }))]),
            })),
        ];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_function_stmt() {
        let inputs = [
            "fn test(i) { i; }",
            "fn test2(x, y) { 1; }",
            "fn test() { 3; }",
        ];
        let expecteds = [
            Stmt::Function(FunctionStmt {
                name: Ident("test".to_owned()),
                params: vec![Ident("i".to_owned())],
                block: Block(vec![Stmt::Expr(Expr::Ident(Ident("i".to_owned())))]),
            }),
            Stmt::Function(FunctionStmt {
                name: Ident("test2".to_owned()),
                params: vec![Ident("x".to_owned()), Ident("y".to_owned())],
                block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(1)))]),
            }),
            Stmt::Function(FunctionStmt {
                name: Ident("test".to_owned()),
                params: Vec::new(),
                block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(3)))]),
            }),
        ];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_return_stmt() {
        let inputs = ["fn test() { return 3; }"];
        let expecteds = [Stmt::Function(FunctionStmt {
            name: Ident("test".to_owned()),
            params: Vec::new(),
            block: Block(vec![Stmt::Return(ReturnStmt {
                value: Expr::Literal(Literal::Integer(3)),
            })]),
        })];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn cant_return_outside_of_function_context() {
        let inputs = ["return 3;"];
        let errs = [ParserError::InvalidKeywordInScope {
            token: Token::Return,
            expect_scope: ScopeType::Function,
            pos: Position {
                line: 1,
                range: 0..6,
            },
        }];

        test_parse_errs!(inputs, errs)
    }

    #[test]
    fn can_return_from_loop_in_function() {
        let inputs = ["fn(x) { for i in x { return i; } }"];
        let expecteds = [Stmt::Expr(Expr::Function(FunctionExpr {
            params: vec![Ident("x".to_owned())],
            block: Block(vec![Stmt::For(ForStmt {
                iter_ident: Ident("i".to_owned()),
                expr: Expr::Ident(Ident("x".to_owned())),
                block: Block(vec![Stmt::Return(ReturnStmt {
                    value: Expr::Ident(Ident("i".to_owned())),
                })]),
                line: 1,
            })]),
        }))];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_decl_stmt() {
        let inputs = [
            "decl Test { field1 field2 }",
            "decl Test { field1 field2 fn xy(x, y) { 1; } }",
            "decl Test { field1 field2 fn xy(x, y) { 1; } [Testing] { field1, field2 } }",
            "decl Test {}",
            "decl Test { field1 field2 fn xy(x, y) { 1; } [Testing] {} }",
        ];
        let expecteds = [
            Stmt::Declaration(DeclarationStmt {
                name: Ident("Test".to_owned()),
                methods: Vec::new(),
                fields: vec![Ident("field1".to_owned()), Ident("field2".to_owned())],
                embeds: Vec::new(),
            }),
            Stmt::Declaration(DeclarationStmt {
                name: Ident("Test".to_owned()),
                methods: vec![FunctionStmt {
                    name: Ident("xy".to_owned()),
                    params: vec![Ident("x".to_owned()), Ident("y".to_owned())],
                    block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(1)))]),
                }],
                fields: vec![Ident("field1".to_owned()), Ident("field2".to_owned())],
                embeds: Vec::new(),
            }),
            Stmt::Declaration(DeclarationStmt {
                name: Ident("Test".to_owned()),
                methods: vec![FunctionStmt {
                    name: Ident("xy".to_owned()),
                    params: vec![Ident("x".to_owned()), Ident("y".to_owned())],
                    block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(1)))]),
                }],
                fields: vec![Ident("field1".to_owned()), Ident("field2".to_owned())],
                embeds: vec![Embed {
                    name: Ident("Testing".to_owned()),
                    assigned: vec![Ident("field1".to_owned()), Ident("field2".to_owned())],
                    line: 1,
                }],
            }),
            Stmt::Declaration(DeclarationStmt {
                name: Ident("Test".to_owned()),
                methods: Vec::new(),
                fields: Vec::new(),
                embeds: Vec::new(),
            }),
            Stmt::Declaration(DeclarationStmt {
                name: Ident("Test".to_owned()),
                methods: vec![FunctionStmt {
                    name: Ident("xy".to_owned()),
                    params: vec![Ident("x".to_owned()), Ident("y".to_owned())],
                    block: Block(vec![Stmt::Expr(Expr::Literal(Literal::Integer(1)))]),
                }],
                fields: vec![Ident("field1".to_owned()), Ident("field2".to_owned())],
                embeds: vec![Embed {
                    name: Ident("Testing".to_owned()),
                    assigned: Vec::new(),
                    line: 1,
                }],
            }),
        ];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_call_expr() {
        let inputs = ["test(x, y);", "test(3);", "test();"];
        let expecteds = [
            Stmt::Expr(Expr::Call(CallExpr {
                func: Box::new(Expr::Ident(Ident("test".to_owned()))),
                args: vec![
                    Expr::Ident(Ident("x".to_owned())),
                    Expr::Ident(Ident("y".to_owned())),
                ],
                line: 1,
            })),
            Stmt::Expr(Expr::Call(CallExpr {
                func: Box::new(Expr::Ident(Ident("test".to_owned()))),
                args: vec![Expr::Literal(Literal::Integer(3))],
                line: 1,
            })),
            Stmt::Expr(Expr::Call(CallExpr {
                func: Box::new(Expr::Ident(Ident("test".to_owned()))),
                args: Vec::new(),
                line: 1,
            })),
        ];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_array_literal() {
        let inputs = ["[1, 2, 3];", "[1];", "[];"];
        let expecteds = [
            Stmt::Expr(Expr::Literal(Literal::Array(vec![
                Expr::Literal(Literal::Integer(1)),
                Expr::Literal(Literal::Integer(2)),
                Expr::Literal(Literal::Integer(3)),
            ]))),
            Stmt::Expr(Expr::Literal(Literal::Array(vec![Expr::Literal(
                Literal::Integer(1),
            )]))),
            Stmt::Expr(Expr::Literal(Literal::Array(Vec::new()))),
        ];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_index_expr() {
        let inputs = ["x[3];"];
        let expecteds = [Stmt::Expr(Expr::Index(IndexExpr {
            left: Box::new(Expr::Ident(Ident("x".to_owned()))),
            index: Box::new(Expr::Literal(Literal::Integer(3))),
            line: 1,
        }))];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_map_literal() {
        let inputs = ["{1: 3};", "{1: 3, 3: 2};", "{}"];
        let expecteds = [
            Stmt::Expr(Expr::Literal(Literal::Map(vec![(
                Expr::Literal(Literal::Integer(1)),
                Expr::Literal(Literal::Integer(3)),
            )]))),
            Stmt::Expr(Expr::Literal(Literal::Map(vec![
                (
                    Expr::Literal(Literal::Integer(1)),
                    Expr::Literal(Literal::Integer(3)),
                ),
                (
                    Expr::Literal(Literal::Integer(3)),
                    Expr::Literal(Literal::Integer(2)),
                ),
            ]))),
            Stmt::Expr(Expr::Literal(Literal::Map(Vec::new()))),
        ];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_switch_stmt() {
        let inputs = [
            "switch x { case y {} }",
            "switch x { case y {} case 3 {} }",
            "switch x {}",
            "switch x { case y {} default {} }",
            "switch x { default {} }",
            "switch x { case y | z {} }",
        ];

        let expecteds = [
            Stmt::Switch(SwitchStmt {
                expr: Expr::Ident(Ident("x".to_owned())),
                cases: vec![Case {
                    conditions: vec![Expr::Ident(Ident("y".to_owned()))],
                    block: Block(Vec::new()),
                }],
                default: None,
            }),
            Stmt::Switch(SwitchStmt {
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
            }),
            Stmt::Switch(SwitchStmt {
                expr: Expr::Ident(Ident("x".to_owned())),
                cases: Vec::new(),
                default: None,
            }),
            Stmt::Switch(SwitchStmt {
                expr: Expr::Ident(Ident("x".to_owned())),
                cases: vec![Case {
                    conditions: vec![Expr::Ident(Ident("y".to_owned()))],
                    block: Block(Vec::new()),
                }],
                default: Some(Block(Vec::new())),
            }),
            Stmt::Switch(SwitchStmt {
                expr: Expr::Ident(Ident("x".to_owned())),
                cases: Vec::new(),
                default: Some(Block(Vec::new())),
            }),
            Stmt::Switch(SwitchStmt {
                expr: Expr::Ident(Ident("x".to_owned())),
                cases: vec![Case {
                    conditions: vec![
                        Expr::Ident(Ident("y".to_owned())),
                        Expr::Ident(Ident("z".to_owned())),
                    ],
                    block: Block(Vec::new()),
                }],
                default: None,
            }),
        ];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_dot_assignment() {
        let inputs = ["hello.world = 3;"];
        let expecteds = [Stmt::Assign(AssignStmt {
            name: Ident("hello".to_owned()),
            value: Expr::Literal(Literal::Integer(3)),
            field: Some(Ident("world".to_owned())),
            index: None,
            line: 1,
        })];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_index_assignment() {
        let inputs = ["test[1] = 3;"];
        let expecteds = [Stmt::Assign(AssignStmt {
            name: Ident("test".to_owned()),
            value: Expr::Literal(Literal::Integer(3)),
            field: None,
            index: Some(Expr::Literal(Literal::Integer(1))),
            line: 1,
        })];

        test_parse!(inputs, expecteds)
    }
}
