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
        let token = match self.ch {
            b'=' => TokenType::ASSIGN,
            b';' => TokenType::SEMICOLON,
            b'(' => TokenType::LPAREN,
            b')' => TokenType::RPAREN,
            b',' => TokenType::COMMA,
            b'+' => TokenType::PLUS,
            b'{' => TokenType::LBRACE,
            b'}' => TokenType::RBRACE,
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
}

#[cfg(test)]
mod tests {
    use std::{println, vec};

    use crate::lib::tokens::TokenType;

    use super::Lexer;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";

        let tokens = vec![
            TokenType::ASSIGN,
            TokenType::PLUS,
            TokenType::LPAREN,
            TokenType::RPAREN,
            TokenType::LBRACE,
            TokenType::RBRACE,
            TokenType::COMMA,
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
