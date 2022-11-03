use crate::ast::*;
use crate::lexer::Token;
use logos::{Lexer, Logos};
use std::fmt;
use std::ops::Range;
use thiserror::Error;

#[derive(Debug, PartialEq)]
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
    #[error("expected `{:?}` on {pos}", token)]
    ExpectedToken { pos: Position, token: Token },

    #[error("unteriminated string on {pos}")]
    UnteriminatedString { pos: Position },

    #[error("illegal character(s) on {pos}")]
    IllegalCharacter { pos: Position },

    #[error("invalid operator in this context: `{:?}` on {pos}", token)]
    InvalidPrefixOperator { token: Token, pos: Position },

    #[error("illegal ident, cannot have number in front: `{:?}` on {pos}", ident)]
    InvalidIdent { ident: String, pos: Position },
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

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a, Token>,
    errors: Vec<ParserError>,

    current_token: Token,
    peek_token: Token,

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
            Token::Return => Stmt::Return(self.parse_return_stmt()?),
            Token::If => Stmt::If(self.parse_if_stmt()?),
            Token::For => Stmt::For(self.parse_for_stmt()?),

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
            Token::String(ref s) => Expr::Literal(Literal::String(s.to_owned())),
            Token::Ident(_) => Expr::Ident(self.parse_ident().expect("Should be on ident")),
            Token::Bang | Token::Minus | Token::Plus => Expr::Prefix(self.parse_prefix_expr()?),
            Token::Lparen => self.parse_grouped_expr()?,
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
                Token::Dot => {
                    self.next_token();
                    Expr::Dot(self.parse_dot_expr(left_exp)?)
                }
                _ => return Some(left_exp),
            };
        }
        Some(left_exp)
    }

    fn parse_ident(&self) -> Option<Ident> {
        if let Token::Ident(name) = &self.current_token {
            Some(Ident(name.to_owned()))
        } else {
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
        if !matches!(self.peek_token, Token::Ident(..)) {
            self.errors.push(ParserError::ExpectedToken {
                pos: self.position(),
                token: Token::Ident("".to_string()),
            });
            return None;
        }
        self.next_token();
        let ident = self.parse_ident().expect("Token should be ident");
        Some(DotExpr {
            left: Box::new(left_exp),
            field: ident,
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

    fn parse_return_stmt(&mut self) -> Option<ReturnStmt> {
        self.next_token();
        let return_val = self.parse_expr(Precedence::Lowest)?;
        Some(ReturnStmt {
            value: return_val,
            line: self.line,
        })
    }

    fn parse_assign_stmt(&mut self) -> Option<AssignStmt> {
        let ident = if let Token::Ident(ident) = &self.current_token {
            Ident(ident.to_owned())
        } else {
            self.errors.push(ParserError::ExpectedToken {
                pos: self.position(),
                token: Token::Ident("".to_owned()),
            });
            return None;
        };
        self.expect_peek(Token::Assign)?;
        self.next_token();
        let value = self.parse_expr(Precedence::Lowest)?;
        Some(AssignStmt {
            name: ident,
            value,
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
        let ident = if let Token::Ident(ident) = &self.current_token {
            Ident(ident.to_owned())
        } else {
            self.errors.push(ParserError::ExpectedToken {
                pos: self.position(),
                token: Token::Ident("".to_owned()),
            });
            return None;
        };
        self.expect_peek(Token::In)?;
        self.next_token();
        let expr = self.parse_expr(Precedence::Lowest)?;
        self.expect_peek(Token::Lbrace)?;
        let block = self.parse_block()?;
        Some(ForStmt {
            iter_ident: ident,
            expr,
            block,
        })
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
        let inputs = ["123;", "3.3;", "\"test\";", "True;"];
        let expecteds = [
            Stmt::Expr(Expr::Literal(Literal::Integer(123))),
            Stmt::Expr(Expr::Literal(Literal::Float(3.3))),
            Stmt::Expr(Expr::Literal(Literal::String("test".to_owned()))),
            Stmt::Expr(Expr::Literal(Literal::Boolean(true))),
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
            line: 2,
        })];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_return_stmt() {
        let inputs = ["return 3;"];
        let expecteds = [Stmt::Return(ReturnStmt {
            value: Expr::Literal(Literal::Integer(3)),
            line: 1,
        })];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_stmt_on_line_2() {
        let inputs = ["
            return 3;
            "];
        let expecteds = [Stmt::Return(ReturnStmt {
            value: Expr::Literal(Literal::Integer(3)),
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
        let inputs = ["x = 3", "return 3", "333"];
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
                    range: 7..8,
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
        let inputs = ["return \"hello", "x = #;"];
        let errs = [
            ParserError::UnteriminatedString {
                pos: Position {
                    line: 1,
                    range: 7..13,
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
            })),
            Stmt::Expr(Expr::Dot(DotExpr {
                left: Box::new(Expr::Infix(InfixExpr {
                    left: Box::new(Expr::Literal(Literal::Integer(1))),
                    op: InfixOp::Plus,
                    right: Box::new(Expr::Literal(Literal::Integer(1))),
                    line: 1,
                })),
                field: Ident("test".to_owned()),
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
            }),
        ];

        test_parse!(inputs, expecteds)
    }
}
