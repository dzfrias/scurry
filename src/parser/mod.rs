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

    #[error("illegal character on {pos}")]
    IllegalCharacter { pos: Position },

    #[error("no prefix operator `{:?}` on {pos}", token)]
    InvalidPrefixOperator { token: Token, pos: Position },
}

#[derive(Debug, PartialEq, PartialOrd)]
#[allow(dead_code)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

impl From<&Token> for Precedence {
    fn from(token: &Token) -> Self {
        match token {
            Token::Eq | Token::NotEq => Precedence::Equals,
            Token::Lt | Token::Gt | Token::Le | Token::Ge => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk | Token::Percent => Precedence::Product,
            Token::Lparen => Precedence::Call,
            Token::Lbracket => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a, Token>,
    errors: Vec<ParserError>,

    current_token: Token,
    current_span: Range<usize>,
    peek_token: Token,

    line: usize,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        let lexer = Token::lexer(source);
        let mut parser = Self {
            lexer,
            errors: Vec::new(),
            current_token: Token::EOF,
            current_span: 0..0,
            peek_token: Token::EOF,
            line: 1,
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
        let mut lineno = 1;
        let mut char_index = 0;
        let start = self.current_span.start;
        let column = self
            .lexer
            .source()
            .lines()
            .find_map(|line| {
                let mut col = 0;
                for _ in 0..line.len() {
                    if char_index == start {
                        return Some(col);
                    }
                    col += 1;
                    char_index += 1;
                }

                lineno += 1;
                char_index += 1;
                None
            })
            // WARNING: Panics if whitespace is the error
            .expect("Current char should be in source");
        Position {
            line: lineno,
            range: column..column + self.current_span.len(),
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.current_span = self.lexer.span();
        self.peek_token = self.lexer.next().unwrap_or(Token::EOF);
        if self.current_token == Token::Newline {
            self.line += 1;
            // Get rid of newline and/or following ones
            self.next_token();
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
            Token::If => self.parse_if_stmt(),

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

            // Expression on one line
            _ => Stmt::Expr(self.parse_expr_stmt()?),
        };
        Some(stmt)
    }

    fn parse_expr_stmt(&mut self) -> Option<Expr> {
        let expr = self.parse_expr(Precedence::Lowest)?;
        Some(expr)
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Option<Expr> {
        let mut left_exp = match self.current_token {
            Token::Integer(i) => Expr::Literal(Literal::Integer(i)),
            Token::True => Expr::Literal(Literal::Boolean(true)),
            Token::False => Expr::Literal(Literal::Boolean(false)),
            Token::String(ref s) => Expr::Literal(Literal::String(s.to_owned())),
            Token::Ident(_) => Expr::Ident(self.parse_ident().expect("Should be on ident")),
            Token::Bang | Token::Minus | Token::Plus => Expr::Prefix(self.parse_prefix_expr()?),
            Token::Lparen => self.parse_grouped_expr()?,
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
                | Token::Percent => {
                    self.next_token();
                    Expr::Infix(self.parse_infix_expr(left_exp)?)
                }
                _ => return Some(left_exp),
            }
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
        self.next_token();
        let right = self.parse_expr(self.current_prec())?;
        Some(InfixExpr {
            left: Box::new(left),
            op: infix_op,
            right: Box::new(right),
        })
    }

    fn parse_prefix_expr(&mut self) -> Option<PrefixExpr> {
        let prefix_op = PrefixOp::try_from(&self.current_token).ok()?;
        self.next_token();
        let expr = self.parse_expr(Precedence::Prefix)?;
        Some(PrefixExpr {
            op: prefix_op,
            left: Box::new(expr),
        })
    }

    fn parse_return_stmt(&mut self) -> Option<ReturnStmt> {
        self.next_token();
        let return_val = self.parse_expr(Precedence::Lowest)?;
        if self.peek_token == Token::Semicolon {
            self.next_token();
        }
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
        if self.peek_token == Token::Semicolon {
            self.next_token();
        }
        Some(AssignStmt {
            name: ident,
            value,
            line: self.line,
        })
    }

    // TODO: Parse if statement
    fn parse_if_stmt(&mut self) -> Stmt {
        todo!()
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
}
