use std::{print, println};

use super::tokens::TokenType;

struct Lexer {
    input: Vec<u8>,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer {
    fn new(input: String) -> Lexer {
        let mut lexer = Lexer {
            input: input.into(),
            position: 0,
            read_position: 0,
            ch: 0,
        };

        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> TokenType {
        //skip whitespace

        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }

        println!("next_token: {}", self.ch as char);
        let token = match self.ch {
            b'=' => TokenType::ASSIGN,
            b';' => TokenType::SEMICOLON,
            b'(' => TokenType::LPAREN,
            b')' => TokenType::RPAREN,
            b',' => TokenType::COMMA,
            b'+' => TokenType::PLUS,
            b'-' => TokenType::MINUS,
            b'!' => TokenType::BANG,
            b'*' => TokenType::ASTERISK,
            b'/' => TokenType::SLASH,
            b'<' => TokenType::LT,
            b'>' => TokenType::GT,
            b'{' => TokenType::LBRACE,
            b'}' => TokenType::RBRACE,
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_identifier();
                return match ident.as_str() {
                    "let" => TokenType::LET,
                    "fn" => TokenType::FUNCTION,
                    _ => TokenType::IDENT(ident),
                };
            }
            b'0'..=b'9' => return TokenType::INT(self.read_number()),
            0 => TokenType::EOF,
            _ => TokenType::ILLEGAL,
        };
        self.read_char();
        token
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while self.ch.is_ascii_alphabetic() | (self.ch == b'_') {
            self.read_char();
        }

        self.input[position..self.position]
            .iter()
            .map(|&c| c as char)
            .collect()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }
        self.input[position..self.position]
            .iter()
            .map(|&c| c as char)
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use std::{println, vec};

    use crate::lib::tokens::TokenType;

    use super::Lexer;

    #[test]
    fn test_next_token() {
        let input = r###"
            let five = 5;
            let ten = 10;

            let add = fn(x, y) {
              x + y;
            };

            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;
        "###;

        println!("{}", input);

        let tokens = vec![
            TokenType::LET,
            TokenType::IDENT("five".into()),
            TokenType::ASSIGN,
            TokenType::INT("5".into()),
            TokenType::SEMICOLON,
            //--------------------------------//
            TokenType::LET,
            TokenType::IDENT("ten".into()),
            TokenType::ASSIGN,
            TokenType::INT("10".into()),
            TokenType::SEMICOLON,
            //--------------------------------//
            TokenType::LET,
            TokenType::IDENT("add".into()),
            TokenType::ASSIGN,
            TokenType::FUNCTION,
            TokenType::LPAREN,
            TokenType::IDENT("x".into()),
            TokenType::COMMA,
            TokenType::IDENT("y".into()),
            TokenType::RPAREN,
            TokenType::LBRACE,
            TokenType::IDENT("x".into()),
            TokenType::PLUS,
            TokenType::IDENT("y".into()),
            TokenType::SEMICOLON,
            TokenType::RBRACE,
            TokenType::SEMICOLON,
            //--------------------------------//
            TokenType::LET,
            TokenType::IDENT("result".into()),
            TokenType::ASSIGN,
            TokenType::IDENT("add".into()),
            TokenType::LPAREN,
            TokenType::IDENT("five".into()),
            TokenType::COMMA,
            TokenType::IDENT("ten".into()),
            TokenType::RPAREN,
            TokenType::SEMICOLON,
            //--------------------------------//
            TokenType::BANG,
            TokenType::MINUS,
            TokenType::SLASH,
            TokenType::ASTERISK,
            TokenType::INT("5".into()),
            TokenType::SEMICOLON,
            TokenType::INT("5".into()),
            TokenType::LT,
            TokenType::INT("10".into()),
            TokenType::GT,
            TokenType::INT("5".into()),
            TokenType::SEMICOLON,
            TokenType::EOF,
        ];

        let mut lexer = Lexer::new(input.into());

        for token in tokens {
            let next_tok = lexer.next_token();
            println!("expected: {:?}, got: {:?}", token, next_tok);
            assert_eq!(token, next_tok);
        }
    }
}
