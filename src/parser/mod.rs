use crate::ast::*;
use crate::lexer::Token;
use logos::{Lexer, Logos};
use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum ParserError {
    #[error("expected `{:?}` on line {line}", token)]
    ExpectedToken { line: usize, token: Token },
}

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a, Token>,
    errors: Vec<ParserError>,

    current_token: Token,
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

    pub fn line(&self) -> usize {
        self.line
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next().unwrap_or(Token::EOF);
        if self.current_token == Token::Newline {
            self.line += 1;
            // Get rid of newline
            self.next_token();
        }
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        let stmt = match self.current_token {
            Token::Ident(_) if self.peek_token == Token::Assign => {
                Stmt::Assign(self.parse_assign_stmt()?)
            }
            Token::Return => Stmt::Return(self.parse_return_stmt()?),
            Token::If => self.parse_if_stmt(),
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
                line: self.line,
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
        while self.current_token != Token::Semicolon {
            self.next_token();
        }
        Some(ReturnStmt { value: Expr::Blank })
    }

    fn parse_assign_stmt(&mut self) -> Option<AssignStmt> {
        let ident = if let Token::Ident(ident) = &self.current_token {
            Ident(ident.to_owned())
        } else {
            self.errors.push(ParserError::ExpectedToken {
                line: self.line,
                token: Token::Ident("".to_owned()),
            });
            return None;
        };
        self.expect_peek(Token::Assign)?;
        while self.current_token != Token::Semicolon {
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
        assert_eq!(1, parser.line());
    }

    #[test]
    fn tracks_line_numbers() {
        let input = "123
        456";
        let mut parser = Parser::new(input);

        assert_eq!(Token::Integer(123), parser.current_token);
        parser.next_token();
        assert_eq!(Token::Integer(456), parser.current_token);
        assert_eq!(2, parser.line())
    }

    #[test]
    fn tracks_line_numbers_with_multiple_newlines() {
        let input = "123



        456";
        let mut parser = Parser::new(input);

        assert_eq!(Token::Integer(123), parser.current_token);
        parser.next_token();
        assert_eq!(Token::Integer(456), parser.current_token);
        assert_eq!(5, parser.line())
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
