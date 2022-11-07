use logos::{self, Logos};
use std::fmt;

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    #[regex(r"[_A-Za-z][_A-Za-z0-9]*", |lex| lex.slice().to_owned(), priority = 2)]
    Ident(String),
    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse())]
    Float(f32),
    #[regex(r"[0-9]+", |lex| lex.slice().parse(), priority = 2)]
    Integer(i32),
    #[regex("\"(?s:[^\"\\\\]|\\\\.)*\"", |lex| lex.slice().strip_prefix('"').expect("Should have leading \"").strip_suffix('"').expect("Should have trailing \"").to_owned())]
    String(String),
    #[token("True")]
    True,
    #[token("False")]
    False,
    #[token("Nil")]
    Nil,

    #[token("=")]
    Assign,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("!")]
    Bang,
    #[token("*")]
    Asterisk,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token(".")]
    Dot,
    #[token("|")]
    Pipe,

    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("==")]
    Eq,
    #[token("!=")]
    NotEq,
    #[token(">=")]
    Ge,
    #[token("<=")]
    Le,
    #[token("&&")]
    LogicalAnd,
    #[token("||")]
    LogicalOr,

    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,

    #[token("(")]
    Lparen,
    #[token(")")]
    Rparen,
    #[token("{")]
    Lbrace,
    #[token("}")]
    Rbrace,
    #[token("[")]
    Lbracket,
    #[token("]")]
    Rbracket,

    #[token("fn")]
    Function,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("elif")]
    Elif,
    #[token("return")]
    Return,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("while")]
    While,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("switch")]
    Switch,
    #[token("case")]
    Case,
    #[token("default")]
    Default,
    #[token("decl")]
    Declaration,

    // Not explicitly tokenized, used in parser
    EOF,

    #[regex(r"\n")]
    Newline,
    #[regex(r"[ \t\f]+")]
    HorizontalWhitespace,

    #[error]
    Error,
    #[regex("\"(?s:[^\"\\\\]|\\\\.)*")]
    ErrorUnterminatedString,
    #[regex("[_A-Za-z0-9][_A-Za-z0-9]*", |lex| lex.slice().to_owned())]
    ErrorInvalidIdent(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Ident(_) => write!(f, "IDENT"),
            Token::Float(_) => write!(f, "FLOAT"),
            Token::Integer(_) => write!(f, "INTEGER"),
            Token::String(_) => write!(f, "STRING"),
            Token::True => write!(f, "BOOLEAN"),
            Token::False => write!(f, "BOOLEAN"),
            Token::Nil => write!(f, "NIL"),
            Token::Assign => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Bang => write!(f, "!"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::Dot => write!(f, "."),
            Token::Pipe => write!(f, "|"),
            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),
            Token::Eq => write!(f, "=="),
            Token::NotEq => write!(f, "!="),
            Token::Ge => write!(f, ">="),
            Token::Le => write!(f, "<="),
            Token::LogicalAnd => write!(f, "&&"),
            Token::LogicalOr => write!(f, "||"),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::Semicolon => write!(f, ";"),
            Token::Lparen => write!(f, "("),
            Token::Rparen => write!(f, ")"),
            Token::Lbrace => write!(f, "{{"),
            Token::Rbrace => write!(f, "}}"),
            Token::Lbracket => write!(f, "["),
            Token::Rbracket => write!(f, "]"),
            Token::Function => write!(f, "fn"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Elif => write!(f, "elif"),
            Token::Return => write!(f, "return"),
            Token::For => write!(f, "for"),
            Token::In => write!(f, "in"),
            Token::While => write!(f, "while"),
            Token::Break => write!(f, "break"),
            Token::Continue => write!(f, "continue"),
            Token::Switch => write!(f, "switch"),
            Token::Case => write!(f, "case"),
            Token::Default => write!(f, "default"),
            Token::Declaration => write!(f, "decl"),
            Token::EOF => write!(f, "EOF"),
            Token::Newline => write!(f, "NEWLINE"),
            Token::HorizontalWhitespace => write!(f, "WHITESPACE"),
            Token::Error => write!(f, "ERROR"),
            Token::ErrorUnterminatedString => write!(f, "ERRORUNTERMINATEDSTRING"),
            Token::ErrorInvalidIdent(_) => write!(f, "ERRORINVALIDIDENT"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenizes_alphabetic_idents() {
        let mut lexer = Token::lexer("abcd");
        assert_eq!(
            Token::Ident("abcd".to_owned()),
            lexer.next().expect("Should lex something")
        )
    }

    #[test]
    fn tokenizes_alphanumeric_idents() {
        let mut lexer = Token::lexer("abcd1");
        assert_eq!(
            Token::Ident("abcd1".to_owned()),
            lexer.next().expect("Should lex something")
        )
    }

    #[test]
    fn does_not_recognized_ident_with_number_in_front() {
        let mut lexer = Token::lexer("1abcd");
        assert_eq!(
            Token::ErrorInvalidIdent("1abcd".to_owned()),
            lexer.next().expect("Should lex something")
        )
    }

    #[test]
    fn tokenizes_floats() {
        let mut lexer = Token::lexer("3.1");
        assert_eq!(
            Token::Float(3.1),
            lexer.next().expect("Should lex something")
        )
    }

    #[test]
    fn tokenizes_integers() {
        let mut lexer = Token::lexer("1");
        assert_eq!(
            Token::Integer(1),
            lexer.next().expect("Should lex something")
        )
    }

    #[test]
    fn does_not_panic_on_integer_overflow() {
        let mut lexer = Token::lexer("2147483648");
        assert_eq!(Token::Error, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_strings() {
        let mut lexer = Token::lexer("\"hello world\"");
        assert_eq!(
            Token::String("hello world".to_owned()),
            lexer.next().expect("Should lex something")
        )
    }

    #[test]
    fn errors_on_unteriminated_string() {
        let mut lexer = Token::lexer("\"hello world");
        assert_eq!(
            Token::ErrorUnterminatedString,
            lexer.next().expect("Should lex something")
        )
    }

    #[test]
    fn tokenizes_assigns() {
        let mut lexer = Token::lexer("=");
        assert_eq!(Token::Assign, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_plus() {
        let mut lexer = Token::lexer("+");
        assert_eq!(Token::Plus, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_minus() {
        let mut lexer = Token::lexer("-");
        assert_eq!(Token::Minus, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_slash() {
        let mut lexer = Token::lexer("/");
        assert_eq!(Token::Slash, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_asterisk() {
        let mut lexer = Token::lexer("*");
        assert_eq!(Token::Asterisk, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_percent() {
        let mut lexer = Token::lexer("%");
        assert_eq!(Token::Percent, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_bang() {
        let mut lexer = Token::lexer("!");
        assert_eq!(Token::Bang, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_dot() {
        let mut lexer = Token::lexer(".");
        assert_eq!(Token::Dot, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_lt() {
        let mut lexer = Token::lexer("<");
        assert_eq!(Token::Lt, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_gt() {
        let mut lexer = Token::lexer(">");
        assert_eq!(Token::Gt, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_noteq() {
        let mut lexer = Token::lexer("!=");
        assert_eq!(Token::NotEq, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_ge() {
        let mut lexer = Token::lexer(">=");
        assert_eq!(Token::Ge, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_le() {
        let mut lexer = Token::lexer("<=");
        assert_eq!(Token::Le, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_comma() {
        let mut lexer = Token::lexer(",");
        assert_eq!(Token::Comma, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_colon() {
        let mut lexer = Token::lexer(":");
        assert_eq!(Token::Colon, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_semicolon() {
        let mut lexer = Token::lexer(";");
        assert_eq!(
            Token::Semicolon,
            lexer.next().expect("Should lex something")
        )
    }

    #[test]
    fn tokenizes_lparen() {
        let mut lexer = Token::lexer("(");
        assert_eq!(Token::Lparen, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_rparen() {
        let mut lexer = Token::lexer(")");
        assert_eq!(Token::Rparen, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_lbrace() {
        let mut lexer = Token::lexer("{");
        assert_eq!(Token::Lbrace, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_rbrace() {
        let mut lexer = Token::lexer("}");
        assert_eq!(Token::Rbrace, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_lbracket() {
        let mut lexer = Token::lexer("[");
        assert_eq!(Token::Lbracket, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_rbracket() {
        let mut lexer = Token::lexer("]");
        assert_eq!(Token::Rbracket, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_fn() {
        let mut lexer = Token::lexer("fn");
        assert_eq!(Token::Function, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_true() {
        let mut lexer = Token::lexer("True");
        assert_eq!(Token::True, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_false() {
        let mut lexer = Token::lexer("False");
        assert_eq!(Token::False, lexer.next().expect("Should lex something"))
    }

    #[test]
    fn tokenizes_if() {
        let mut lexer = Token::lexer("if");
        assert_eq!(Token::If, lexer.next().expect("Should lex something"));
    }

    #[test]
    fn tokenizes_else() {
        let mut lexer = Token::lexer("else");
        assert_eq!(Token::Else, lexer.next().expect("Should lex something"));
    }

    #[test]
    fn tokenizes_return() {
        let mut lexer = Token::lexer("return");
        assert_eq!(Token::Return, lexer.next().expect("Should lex something"));
    }

    #[test]
    fn tokenizes_elseif() {
        let mut lexer = Token::lexer("elif");
        assert_eq!(Token::Elif, lexer.next().expect("Should lex something"));
    }

    #[test]
    fn tokenizes_for() {
        let mut lexer = Token::lexer("for");
        assert_eq!(Token::For, lexer.next().expect("Should lex something"));
    }

    #[test]
    fn tokenizes_in() {
        let mut lexer = Token::lexer("in");
        assert_eq!(Token::In, lexer.next().expect("Should lex something"));
    }

    #[test]
    fn tokenizes_while() {
        let mut lexer = Token::lexer("while");
        assert_eq!(Token::While, lexer.next().expect("Should lex something"));
    }

    #[test]
    fn tokenizes_logical_and() {
        let mut lexer = Token::lexer("&&");
        assert_eq!(
            Token::LogicalAnd,
            lexer.next().expect("Should lex something")
        );
    }

    #[test]
    fn tokenizes_logical_or() {
        let mut lexer = Token::lexer("||");
        assert_eq!(
            Token::LogicalOr,
            lexer.next().expect("Should lex something")
        );
    }

    #[test]
    fn tokenizes_break() {
        let mut lexer = Token::lexer("break");
        assert_eq!(Token::Break, lexer.next().expect("Should lex something"));
    }

    #[test]
    fn tokenizes_continue() {
        let mut lexer = Token::lexer("continue");
        assert_eq!(Token::Continue, lexer.next().expect("Should lex something"));
    }

    #[test]
    fn tokenizes_switch() {
        let mut lexer = Token::lexer("switch");
        assert_eq!(Token::Switch, lexer.next().expect("Should lex something"));
    }

    #[test]
    fn tokenizes_case() {
        let mut lexer = Token::lexer("case");
        assert_eq!(Token::Case, lexer.next().expect("Should lex something"));
    }

    #[test]
    fn tokenizes_nil() {
        let mut lexer = Token::lexer("Nil");
        assert_eq!(Token::Nil, lexer.next().expect("Should lex something"));
    }

    #[test]
    fn tokenizes_default() {
        let mut lexer = Token::lexer("default");
        assert_eq!(Token::Default, lexer.next().expect("Should lex something"));
    }

    #[test]
    fn tokenizes_declaration() {
        let mut lexer = Token::lexer("decl");
        assert_eq!(
            Token::Declaration,
            lexer.next().expect("Should lex something")
        );
    }

    #[test]
    fn tokenizes_pipe() {
        let mut lexer = Token::lexer("|");
        assert_eq!(Token::Pipe, lexer.next().expect("Should lex something"));
    }

    #[test]
    fn recognizes_eof() {
        let mut lexer = Token::lexer("");
        assert_eq!(None, lexer.next());
    }
}
