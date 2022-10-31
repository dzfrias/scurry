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
}

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a, Token>,
    errors: Vec<ParserError>,

    current_token: Token,
    current_span: Range<usize>,
    peek_token: Token,
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

    fn parse_expr_stmt(&mut self) -> Option<Expr> {
        Some(Expr::Blank)
    }

    fn parse_return_stmt(&mut self) -> Option<ReturnStmt> {
        self.next_token();
        while self.current_token != Token::Semicolon && self.current_token != Token::EOF {
            self.next_token();
        }
        Some(ReturnStmt { value: Expr::Blank })
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
        while self.current_token != Token::Semicolon && self.current_token != Token::EOF {
            self.next_token();
        }
        Some(AssignStmt {
            name: ident,
            value: Expr::Blank,
        })
    }

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
            value: Expr::Blank,
        })];

        test_parse!(inputs, expecteds)
    }

    #[test]
    fn parse_return_stmt() {
        let inputs = ["return 3;"];
        let expecteds = [Stmt::Return(ReturnStmt { value: Expr::Blank })];

        test_parse!(inputs, expecteds)
    }
}
