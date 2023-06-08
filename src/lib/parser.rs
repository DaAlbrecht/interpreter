use super::{
    ast::{self, Statement},
    lexer::Lexer,
    tokens::TokenType,
};

struct Parser<'a> {
    lexer: &'a mut Lexer,
    curr_token: Option<TokenType>,
    peek_token: Option<TokenType>,
    peek_errors: Vec<String>,
}

enum Presedence {
    LOWEST,
    EQUALS,      // ==
    LESSGREATER, // > or <
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL,        // myFunction(X)
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

    fn curr_token_is(&self, token_type: TokenType) -> bool {
        self.curr_token == Some(token_type)
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
            Some(TokenType::RETURN) => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Option<ast::Statement> {
        let expression = self.parse_expression(Presedence::LOWEST);

        if self.expect_peek(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(ast::Statement::ExpressionStatement(expression.unwrap()))
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

    //-------------------- Parsing statment functions --------------------//
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
        // TODO: We're skipping the expressions until we  encounter a semicolon for now

        while !self.curr_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        let let_statement = ast::LetStatement {
            name,
            value: ast::AllExpression::Int(5), // we do not care about statements yet
        };

        Some(ast::Statement::LetStatement(let_statement))
    }
    fn parse_return_statement(&mut self) -> Option<ast::Statement> {
        self.next_token();

        // TODO: We're skipping the expressions until we encounter a semicolon for now

        while !self.curr_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(ast::Statement::ReturnStatement(ast::AllExpression::Int(5))) // we do not care about statements yet -> 5 is just a placeholder
    }

    //-------------------- Parsing expression functions --------------------//

    fn parse_expression(&mut self, presedence: Presedence) -> Option<ast::AllExpression> {
        match self.curr_token {
            Some(TokenType::IDENT(_)) => self.parse_identifier(),
            Some(TokenType::INT(_)) => self.parse_integer_literal(),
            Some(TokenType::BANG) => self.parse_prefix_expression(),
            Some(TokenType::MINUS) => self.parse_prefix_expression(),
            _ => None,
        }
    }

    fn parse_identifier(&mut self) -> Option<ast::AllExpression> {
        match self.curr_token {
            Some(TokenType::IDENT(ref name)) => Some(ast::AllExpression::Identifier(name.clone())),
            _ => None,
        }
    }

    fn parse_integer_literal(&mut self) -> Option<ast::AllExpression> {
        match self.curr_token {
            Some(TokenType::INT(ref value)) => {
                let value = value.parse::<usize>().unwrap();
                Some(ast::AllExpression::Int(value))
            }
            _ => None,
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<ast::AllExpression> {
        let operator = match self.curr_token {
            Some(TokenType::BANG) => TokenType::BANG,
            Some(TokenType::MINUS) => TokenType::MINUS,
            _ => return None,
        };

        self.next_token();

        let right = match self.parse_expression(Presedence::PREFIX) {
            Some(expression) => expression,
            _ => return None,
        };

        let prefix_expression = ast::PrefixExpression {
            operator,
            right: Box::new(right),
        };

        Some(ast::AllExpression::PrefixExpression(prefix_expression))
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
                _ => panic!("not a let statement"),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let input = r#"
         return 5;
        return 10;
        return 993322;
        "#
        .to_string();

        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program();

        assert!(program.is_some());

        let program = program.unwrap();

        assert_eq!(program.statements.len(), 3);

        for statement in program.statements {
            match statement {
                ast::Statement::ReturnStatement(_) => {
                    assert!(true);
                }
                _ => panic!("not a return statement"),
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar".to_string();

        let mut lexer = Lexer::new(input);

        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program();

        assert!(program.is_some());

        let program = program.unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = program.statements[0].clone();

        match statement {
            ast::Statement::ExpressionStatement(expression_statement) => match expression_statement
            {
                ast::AllExpression::Identifier(name) => {
                    assert_eq!(name, "foobar".to_string());
                }
                _ => panic!("not an identifier expression"),
            },
            _ => panic!("not an expression statement"),
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;".to_string();

        let mut lexer = Lexer::new(input);

        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program();

        assert!(program.is_some());

        let program = program.unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = program.statements[0].clone();

        match statement {
            ast::Statement::ExpressionStatement(expression_statement) => match expression_statement
            {
                ast::AllExpression::Int(value) => {
                    assert_eq!(value, 5);
                }
                _ => panic!("not an integer literal expression"),
            },
            _ => panic!("not an expression statement"),
        }
    }

    #[test]
    fn test_parsing_prefix_expression() {
        let prefix_tests = vec![("!5;".to_string(), "!", 5), ("-15;".to_string(), "-", 15)];

        for (input, operator, value) in prefix_tests {
            let mut lexer = Lexer::new(input);

            let mut parser = Parser::new(&mut lexer);

            let program = parser.parse_program();

            assert!(program.is_some());

            let program = program.unwrap();

            assert_eq!(program.statements.len(), 1);

            let statement = program.statements[0].clone();

            match statement {
                ast::Statement::ExpressionStatement(expression_statement) => {
                    match expression_statement {
                        ast::AllExpression::PrefixExpression(prefix_expression) => {
                            assert_eq!(prefix_expression.operator.to_string(), operator);
                            assert_eq!(*prefix_expression.right, ast::AllExpression::Int(value));
                        }
                        _ => panic!("not a prefix expression"),
                    }
                }
                _ => panic!("not an expression statement"),
            }
        }
    }
}
