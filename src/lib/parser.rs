use super::{ast, lexer::Lexer, tokens::TokenType};

struct Parser<'a> {
    lexer: &'a mut Lexer,
    curr_token: Option<TokenType>,
    peek_token: Option<TokenType>,
    peek_errors: Vec<String>,
}

impl<'a> Parser<'a> {
    fn new(lexer: &'_ mut Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            curr_token: None,
            peek_token: None,
            peek_errors: vec![],
        };

        parser.next_token();
        parser.next_token();

        parser
    }
    fn next_token(&mut self) {
        self.curr_token = self.peek_token.take();
        self.peek_token = Some(self.lexer.next_token());
    }

    pub fn parse_program(&mut self) -> Option<ast::Program> {
        let mut program = ast::Program { statements: vec![] };

        while self.curr_token != Some(TokenType::EOF) {
            let statement = self.parse_statement();
            if statement.is_some() {
                program.statements.push(statement.unwrap());
            }
            self.next_token();
        }
        Some(program)
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.curr_token {
            Some(TokenType::LET) => self.parse_let_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<ast::Statement> {
        if !self.expect_peek(TokenType::IDENT("".to_string())) {
            return None;
        }

        let name = match self.curr_token {
            Some(TokenType::IDENT(ref name)) => name.clone(),
            _ => return None,
        };

        if !self.expect_peek(TokenType::ASSIGN) {
            return None;
        }
        // TODO: We're skipping the expressions until we
        // encounter a semicolon”

        while !self.curr_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        let let_statement = ast::LetStatement {
            name,
            value: ast::AllExpression::Int(5), // we do not care about statements yet
        };

        Some(ast::Statement::LetStatement(let_statement))
    }

    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        match self.peek_token {
            Some(TokenType::IDENT(_)) => {
                self.next_token();
                true
            }
            Some(TokenType::ASSIGN) => {
                self.next_token();
                true
            }
            Some(TokenType::SEMICOLON) => {
                self.next_token();
                true
            }
            _ => {
                self.peek_error(token_type);
                false
            }
        }
    }

    fn peek_error(&mut self, token_type: TokenType) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            token_type, self.peek_token
        );

        self.peek_errors.push(msg); // not sure if i want to actually keep the errors here or just
                                    // print them and move on
        println!("peek_errors: {:?}", self.peek_errors);
    }

    fn curr_token_is(&self, token_type: TokenType) -> bool {
        self.curr_token == Some(token_type)
    }
}

#[cfg(test)]
mod test {
    use crate::lib::lexer::Lexer;

    use super::ast;
    use super::Parser;

    #[test]
    fn test_let_statements() {
        let input = r#"
         let x = 5;
        let y = 10;
        let foobar =  838383;
        "#
        .to_string();

        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program();

        assert!(program.is_some());

        let program = program.unwrap();

        assert_eq!(program.statements.len(), 3);

        let tests = vec!["x".to_string(), "y".to_string(), "foobar".to_string()];

        for (i, test) in tests.iter().enumerate() {
            let statement = program.statements[i].clone();
            match statement {
                ast::Statement::LetStatement(let_statement) => {
                    assert_eq!(let_statement.name, test.to_string());
                }
            }
        }
    }
}
