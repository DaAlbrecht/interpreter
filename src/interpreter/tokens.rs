use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT(String),
    INT(String),
    STRING(String),

    // Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    GT,
    LT,
    EQ,
    NOTEQ,

    // Delimiters
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,

    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TokenType::ILLEGAL => write!(f, "ILLEGAL"),
            TokenType::EOF => write!(f, "EOF"),
            TokenType::IDENT(s) => write!(f, "IDENT({})", s),
            TokenType::INT(s) => write!(f, "INT({})", s),
            TokenType::ASSIGN => write!(f, "="),
            TokenType::PLUS => write!(f, "+"),
            TokenType::MINUS => write!(f, "-"),
            TokenType::BANG => write!(f, "!"),
            TokenType::ASTERISK => write!(f, "*"),
            TokenType::SLASH => write!(f, "/"),
            TokenType::GT => write!(f, ">"),
            TokenType::LT => write!(f, "<"),
            TokenType::EQ => write!(f, "=="),
            TokenType::NOTEQ => write!(f, "!="),
            TokenType::COMMA => write!(f, ","),
            TokenType::SEMICOLON => write!(f, ";"),
            TokenType::LPAREN => write!(f, "("),
            TokenType::RPAREN => write!(f, ")"),
            TokenType::LBRACE => write!(f, "{{"),
            TokenType::RBRACE => write!(f, "}}"),
            TokenType::FUNCTION => write!(f, "FUNCTION"),
            TokenType::LET => write!(f, "LET"),
            TokenType::TRUE => write!(f, "TRUE"),
            TokenType::FALSE => write!(f, "FALSE"),
            TokenType::IF => write!(f, "IF"),
            TokenType::ELSE => write!(f, "ELSE"),
            TokenType::RETURN => write!(f, "RETURN"),
            TokenType::STRING(s) => write!(f, "STRING({})", s),
            TokenType::LBRACKET => write!(f, "["),
            TokenType::RBRACKET => write!(f, "]"),
        }
    }
}
