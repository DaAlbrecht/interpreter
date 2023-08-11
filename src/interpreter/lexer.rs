use super::tokens::TokenType;

pub struct Lexer {
    input: Vec<u8>,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Iterator for Lexer {
    type Item = TokenType;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        if token == TokenType::EOF {
            None
        } else {
            Some(token)
        }
    }
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
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

        let token = match self.ch {
            b'=' => match self.peek() {
                b'=' => {
                    self.read_char();
                    TokenType::EQ
                }
                _ => TokenType::ASSIGN,
            },
            b';' => TokenType::SEMICOLON,
            b'(' => TokenType::LPAREN,
            b')' => TokenType::RPAREN,
            b',' => TokenType::COMMA,
            b'+' => TokenType::PLUS,
            b'-' => TokenType::MINUS,
            b'!' => match self.peek() {
                b'=' => {
                    self.read_char();
                    TokenType::NOTEQ
                }
                _ => TokenType::BANG,
            },
            b'*' => TokenType::ASTERISK,
            b'/' => TokenType::SLASH,
            b'<' => TokenType::LT,
            b'>' => TokenType::GT,
            b'{' => TokenType::LBRACE,
            b'}' => TokenType::RBRACE,
            b'"' => {
                return self.read_string();
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_identifier();
                return match ident.as_str() {
                    "let" => TokenType::LET,
                    "fn" => TokenType::FUNCTION,
                    "true" => TokenType::TRUE,
                    "false" => TokenType::FALSE,
                    "if" => TokenType::IF,
                    "else" => TokenType::ELSE,
                    "return" => TokenType::RETURN,
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

    fn peek(&self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input[self.read_position]
        }
    }

    fn read_string(&mut self) -> TokenType {
        let position = self.position + 1;
        self.read_char();

        while self.ch != b'"' || self.ch == 0 {
            self.read_char();
        }

        let token = TokenType::STRING(
            self.input[position..self.position]
                .iter()
                .map(|&c| c as char)
                .collect(),
        );

        self.read_char();
        token
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use crate::interpreter::tokens::TokenType;

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

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
            "foobar"
            "foo bar"
        "###;

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
            //--------------------------------//
            TokenType::IF,
            TokenType::LPAREN,
            TokenType::INT("5".into()),
            TokenType::LT,
            TokenType::INT("10".into()),
            TokenType::RPAREN,
            TokenType::LBRACE,
            TokenType::RETURN,
            TokenType::TRUE,
            TokenType::SEMICOLON,
            TokenType::RBRACE,
            TokenType::ELSE,
            TokenType::LBRACE,
            TokenType::RETURN,
            TokenType::FALSE,
            TokenType::SEMICOLON,
            TokenType::RBRACE,
            //--------------------------------//
            TokenType::INT("10".into()),
            TokenType::EQ,
            TokenType::INT("10".into()),
            TokenType::SEMICOLON,
            TokenType::INT("10".into()),
            TokenType::NOTEQ,
            TokenType::INT("9".into()),
            TokenType::SEMICOLON,
            TokenType::STRING("foobar".into()),
            TokenType::STRING("foo bar".into()),
            //--------------------------------//
            TokenType::EOF,
        ];

        let mut lexer = Lexer::new(input.into());

        for token in tokens {
            let next_tok = lexer.next_token();
            assert_eq!(token, next_tok);
        }
    }
}
