use logos::{self, Logos};

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    #[regex(r"[_A-Za-z][_A-Za-z0-9]*", |lex| lex.slice().to_owned())]
    Ident(String),
    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse())]
    Float(f32),
    #[regex(r"[0-9]+", |lex| lex.slice().parse())]
    Integer(i32),
    #[regex("\"(?s:[^\"\\\\]|\\\\.)*\"", |lex| lex.slice().strip_prefix("\"").expect("Should have leading \"").strip_suffix("\"").expect("Should have trailing \"").to_owned())]
    String(String),
    #[token("True")]
    True,
    #[token("False")]
    False,

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

    // Not explicitly tokenized, used in parser
    EOF,

    #[regex(r"\n")]
    Newline,

    #[error]
    #[regex(r"[ \t\f]+", logos::skip)]
    Error,
    #[regex("\"(?s:[^\"\\\\]|\\\\.)*")]
    ErrorUnterminatedString,
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
    fn tokenizes_bools() {
        let mut lexer = Token::lexer("True False");
        assert_eq!(Token::True, lexer.next().expect("Should lex something"));
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
    fn recognizes_eof() {
        let mut lexer = Token::lexer("");
        assert_eq!(None, lexer.next());
    }
}
