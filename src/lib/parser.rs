use std::fmt::Display;

use super::{
    ast::{self, Statement},
    lexer::Lexer,
    tokens::TokenType,
};

pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    curr_token: Option<TokenType>,
    peek_token: Option<TokenType>,
}

#[derive(PartialOrd, PartialEq, Debug)]
enum Presedence {
    LOWEST,
    EQUALS,      // ==
    LESSGREATER, // > or <
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL,        // myFunction(X)
}

impl Display for Presedence {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Presedence::LOWEST => "LOWEST",
            Presedence::EQUALS => "EQUALS",
            Presedence::LESSGREATER => "LESSGREATER",
            Presedence::SUM => "SUM",
            Presedence::PRODUCT => "PRODUCT",
            Presedence::PREFIX => "PREFIX",
            Presedence::CALL => "CALL",
        };
        write!(f, "{}", s)
    }
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'_ mut Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            curr_token: None,
            peek_token: None,
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
        let curr_token = self.curr_token.clone().unwrap();

        std::mem::discriminant(&curr_token) == std::mem::discriminant(&token_type)
    }

    fn peek_token_is(&self, token_type: TokenType) -> bool {
        let peek_token = self.peek_token.clone().unwrap();
        std::mem::discriminant(&peek_token) == std::mem::discriminant(&token_type)
    }

    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_token_is(token_type) {
            self.next_token();
            true
        } else {
            false
        }
    }

    pub fn parse_program(&mut self) -> Result<ast::Program, String> {
        let mut program = ast::Program { statements: vec![] };

        while self.curr_token != Some(TokenType::EOF) {
            let statement = self.parse_statement()?;
            program.statements.push(statement);
            self.next_token();
        }
        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, String> {
        match self.curr_token {
            Some(TokenType::LET) => self.parse_let_statement(),
            Some(TokenType::RETURN) => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<ast::Statement, String> {
        let expression = self.parse_expression(Presedence::LOWEST);

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(Statement::ExpressionStatement(expression.unwrap()))
    }

    fn peek_precedence(&self) -> Presedence {
        match self.peek_token {
            Some(TokenType::EQ) => Presedence::EQUALS,
            Some(TokenType::NOTEQ) => Presedence::EQUALS,
            Some(TokenType::LT) => Presedence::LESSGREATER,
            Some(TokenType::GT) => Presedence::LESSGREATER,
            Some(TokenType::PLUS) => Presedence::SUM,
            Some(TokenType::MINUS) => Presedence::SUM,
            Some(TokenType::SLASH) => Presedence::PRODUCT,
            Some(TokenType::ASTERISK) => Presedence::PRODUCT,
            Some(TokenType::LPAREN) => Presedence::CALL,
            _ => Presedence::LOWEST,
        }
    }

    fn curr_precendence(&self) -> Presedence {
        match self.curr_token {
            Some(TokenType::EQ) => Presedence::EQUALS,
            Some(TokenType::NOTEQ) => Presedence::EQUALS,
            Some(TokenType::LT) => Presedence::LESSGREATER,
            Some(TokenType::GT) => Presedence::LESSGREATER,
            Some(TokenType::PLUS) => Presedence::SUM,
            Some(TokenType::MINUS) => Presedence::SUM,
            Some(TokenType::SLASH) => Presedence::PRODUCT,
            Some(TokenType::ASTERISK) => Presedence::PRODUCT,
            Some(TokenType::LPAREN) => Presedence::CALL,
            _ => Presedence::LOWEST,
        }
    }

    //-------------------- Parsing statment functions --------------------//

    fn parse_let_statement(&mut self) -> Result<ast::Statement, String> {
        if !self.expect_peek(TokenType::IDENT(String::from(""))) {
            return Err(String::from("no identifier found after let statement"));
        }

        let name = match self.curr_token {
            Some(TokenType::IDENT(ref name)) => name.clone(),
            _ => return Err(String::from("no identifier found after let statement")),
        };

        if !self.expect_peek(TokenType::ASSIGN) {
            return Err(String::from(
                "no assign found after let statement identifier",
            ));
        }

        self.next_token();

        let value = self.parse_expression(Presedence::LOWEST)?;

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        let let_statement = ast::LetStatement { name, value };

        Ok(ast::Statement::LetStatement(let_statement))
    }

    fn parse_return_statement(&mut self) -> Result<ast::Statement, String> {
        self.next_token();

        let return_statement = self.parse_expression(Presedence::LOWEST);

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(ast::Statement::ReturnStatement(return_statement?))
    }

    //-------------------- Parsing expression functions --------------------//

    fn parse_expression(&mut self, presedence: Presedence) -> Result<ast::AllExpression, String> {
        let prefix = match self.curr_token {
            Some(TokenType::IDENT(_)) => self.parse_identifier(),
            Some(TokenType::INT(_)) => self.parse_integer_literal(),
            Some(TokenType::BANG) => self.parse_prefix_expression(),
            Some(TokenType::MINUS) => self.parse_prefix_expression(),
            Some(TokenType::TRUE) => self.parse_boolean(),
            Some(TokenType::FALSE) => self.parse_boolean(),
            Some(TokenType::LPAREN) => self.parse_grouped_expression(),
            Some(TokenType::IF) => self.parse_if_expression(),
            Some(TokenType::FUNCTION) => self.parse_function_literal(),
            _ => return Err(String::from("no prefix found")),
        };

        let mut left_exp = prefix?;

        while !self.peek_token_is(TokenType::SEMICOLON) && presedence < self.peek_precedence() {
            let infix = match self.peek_token {
                Some(TokenType::PLUS) => self.parse_infix_expression(left_exp.clone()),
                Some(TokenType::MINUS) => self.parse_infix_expression(left_exp.clone()),
                Some(TokenType::SLASH) => self.parse_infix_expression(left_exp.clone()),
                Some(TokenType::ASTERISK) => self.parse_infix_expression(left_exp.clone()),
                Some(TokenType::EQ) => self.parse_infix_expression(left_exp.clone()),
                Some(TokenType::NOTEQ) => self.parse_infix_expression(left_exp.clone()),
                Some(TokenType::LT) => self.parse_infix_expression(left_exp.clone()),
                Some(TokenType::GT) => self.parse_infix_expression(left_exp.clone()),
                Some(TokenType::LPAREN) => self.parse_call_expression(left_exp.clone()),
                _ => Err(String::from("no infix found")),
            };

            left_exp = infix?;
        }

        Ok(left_exp)
    }

    fn parse_identifier(&mut self) -> Result<ast::AllExpression, String> {
        match self.curr_token {
            Some(TokenType::IDENT(ref name)) => Ok(ast::AllExpression::Identifier(name.clone())),
            _ => Err(String::from("no identifier found")),
        }
    }

    fn parse_integer_literal(&mut self) -> Result<ast::AllExpression, String> {
        match self.curr_token {
            Some(TokenType::INT(ref value)) => {
                let value = value.parse::<usize>().unwrap();
                Ok(ast::AllExpression::Int(value))
            }
            _ => Err(String::from("no integer found")),
        }
    }

    fn parse_boolean(&mut self) -> Result<ast::AllExpression, String> {
        match self.curr_token {
            Some(TokenType::TRUE) => Ok(ast::AllExpression::Boolean(true)),
            Some(TokenType::FALSE) => Ok(ast::AllExpression::Boolean(false)),
            _ => Err(String::from("no boolean found")),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<ast::AllExpression, String> {
        let operator = match self.curr_token {
            Some(TokenType::BANG) => TokenType::BANG,
            Some(TokenType::MINUS) => TokenType::MINUS,
            _ => return Err(String::from("no prefix operator found")),
        };

        self.next_token();

        let right = match self.parse_expression(Presedence::PREFIX) {
            Ok(expression) => expression,
            _ => return Err(String::from("no prefix expression found")),
        };

        let prefix_expression = ast::PrefixExpression {
            operator,
            right: Box::new(right),
        };

        Ok(ast::AllExpression::PrefixExpression(prefix_expression))
    }

    fn parse_infix_expression(
        &mut self,
        left: ast::AllExpression,
    ) -> Result<ast::AllExpression, String> {
        self.next_token();

        let operator = match self.curr_token {
            Some(TokenType::PLUS) => TokenType::PLUS,
            Some(TokenType::MINUS) => TokenType::MINUS,
            Some(TokenType::SLASH) => TokenType::SLASH,
            Some(TokenType::ASTERISK) => TokenType::ASTERISK,
            Some(TokenType::EQ) => TokenType::EQ,
            Some(TokenType::NOTEQ) => TokenType::NOTEQ,
            Some(TokenType::LT) => TokenType::LT,
            Some(TokenType::GT) => TokenType::GT,
            Some(TokenType::LPAREN) => TokenType::LPAREN,
            _ => return Err(String::from("no operator found")),
        };

        let presedence = self.curr_precendence();

        self.next_token();

        let right = self.parse_expression(presedence)?;

        let infix_expression = ast::InfixExpression {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        };

        Ok(ast::AllExpression::InfixExpression(infix_expression))
    }

    fn parse_grouped_expression(&mut self) -> Result<ast::AllExpression, String> {
        self.next_token();

        let expression = self.parse_expression(Presedence::LOWEST);

        if !self.expect_peek(TokenType::RPAREN) {
            return Err(String::from("no right paren found"));
        }

        expression
    }

    fn parse_if_expression(&mut self) -> Result<ast::AllExpression, String> {
        if !self.expect_peek(TokenType::LPAREN) {
            return Err(String::from("no left paren found"));
        }

        self.next_token();

        let condition = self.parse_expression(Presedence::LOWEST);

        if !self.expect_peek(TokenType::RPAREN) {
            return Err(String::from("no right paren found"));
        }

        if !self.expect_peek(TokenType::LBRACE) {
            return Err(String::from("no left brace found"));
        }

        let consequence = self.parse_block_statement();

        match self.peek_token {
            Some(TokenType::ELSE) => {
                self.next_token();

                if !self.expect_peek(TokenType::LBRACE) {
                    return Err(String::from("no left brace found"));
                }

                let alternative = self.parse_block_statement();

                Ok(ast::AllExpression::IfExpression(ast::IfExpression {
                    condition: Box::new(condition?),
                    consequence: consequence?,
                    alternative: Some(alternative?),
                }))
            }
            _ => Ok(ast::AllExpression::IfExpression(ast::IfExpression {
                condition: Box::new(condition?),
                consequence: consequence?,
                alternative: None,
            })),
        }
    }

    fn parse_block_statement(&mut self) -> Result<ast::BlockStatement, String> {
        let mut statements: Vec<ast::Statement> = vec![];

        self.next_token();

        while !self.curr_token_is(TokenType::RBRACE) && !self.curr_token_is(TokenType::EOF) {
            let statement = self.parse_statement();

            if let Ok(statement) = statement {
                statements.push(statement);
            }

            self.next_token();
        }

        Ok(ast::BlockStatement { statements })
    }

    fn parse_function_literal(&mut self) -> Result<ast::AllExpression, String> {
        if !self.expect_peek(TokenType::LPAREN) {
            return Err(String::from("no left paren found"));
        }

        let params = self.parse_function_parameters()?;

        if !self.expect_peek(TokenType::LBRACE) {
            return Err(String::from("no left brace found"));
        }

        let body = self.parse_block_statement()?;

        Ok(ast::AllExpression::FunctionLiteral(ast::FunctionLiteral {
            parameters: params,
            body,
        }))
    }

    fn parse_function_parameters(&mut self) -> Result<Option<Vec<String>>, String> {
        let mut identifiers: Vec<String> = vec![];

        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return Ok(None);
        }

        self.next_token();

        match self.curr_token {
            Some(TokenType::IDENT(ref identifier)) => identifiers.push(identifier.clone()),
            _ => return Err(String::from("no identifier found")),
        }

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();

            match self.curr_token {
                Some(TokenType::IDENT(ref identifier)) => identifiers.push(identifier.clone()),
                _ => return Err(String::from("no identifier found")),
            }
        }

        if !self.expect_peek(TokenType::RPAREN) {
            return Err(String::from("no right paren found"));
        }

        Ok(Some(identifiers))
    }

    fn parse_call_expression(
        &mut self,
        function: ast::AllExpression,
    ) -> Result<ast::AllExpression, String> {
        let arguments = self.parse_call_arguments()?;
        Ok(ast::AllExpression::CallExpression(ast::CallExpression {
            function: Box::new(function),
            arguments,
        }))
    }

    fn parse_call_arguments(&mut self) -> Result<Option<Vec<ast::AllExpression>>, String> {
        let mut arguments: Vec<ast::AllExpression> = vec![];
        self.next_token();

        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return Ok(None);
        }

        self.next_token();

        arguments.push(self.parse_expression(Presedence::LOWEST).unwrap());

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();

            arguments.push(self.parse_expression(Presedence::LOWEST).unwrap());
        }

        if !self.expect_peek(TokenType::RPAREN) {
            return Err(String::from("no right paren found"));
        }

        Ok(Some(arguments))
    }
}

#[cfg(test)]
mod test {
    use crate::lib::lexer::Lexer;
    use crate::lib::tokens::TokenType;

    use super::ast;
    use super::Parser;

    #[test]
    fn test_let_statements() {
        let tests = vec![
            ("let x = 5;", "x", ast::AllExpression::Int(5)),
            ("let y = true;", "y", ast::AllExpression::Boolean(true)),
            (
                "let foobar = y;",
                "foobar",
                ast::AllExpression::Identifier("y".to_string()),
            ),
        ];

        for (input, expected_identifier, expected_value) in tests {
            let mut lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(&mut lexer);

            let program = parser.parse_program();

            assert!(program.is_ok());

            let program = program.unwrap();

            assert_eq!(program.statements.len(), 1);

            let statement = &program.statements[0];

            match statement {
                ast::Statement::LetStatement(let_statement) => {
                    assert_eq!(let_statement.name, expected_identifier);
                    assert_eq!(let_statement.value, expected_value);
                }
                _ => panic!("not a let statement"),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 5;", ast::AllExpression::Int(5)),
            ("return true;", ast::AllExpression::Boolean(true)),
            (
                "return foobar;",
                ast::AllExpression::Identifier("foobar".to_string()),
            ),
        ];

        for (input, expected_value) in tests {
            let mut lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(&mut lexer);

            let program = parser.parse_program();

            assert!(program.is_ok());

            let program = program.unwrap();

            assert_eq!(program.statements.len(), 1);

            let statement = program.statements[0].clone();

            match statement {
                ast::Statement::ReturnStatement(return_statement) => {
                    assert_eq!(return_statement, expected_value);
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

        assert!(program.is_ok());

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

        assert!(program.is_ok());

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

            assert!(program.is_ok());

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

    #[test]
    fn test_parsing_infix_expression() {
        let infix_test = vec![
            ("5 + 5;".to_string(), 5, "+", 5),
            ("5 - 5;".to_string(), 5, "-", 5),
            ("5 * 5;".to_string(), 5, "*", 5),
            ("5 / 5;".to_string(), 5, "/", 5),
            ("5 > 5;".to_string(), 5, ">", 5),
            ("5 < 5;".to_string(), 5, "<", 5),
            ("5 == 5;".to_string(), 5, "==", 5),
            ("5 != 5;".to_string(), 5, "!=", 5),
        ];

        for (input, left, operator, right) in infix_test {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();

            assert!(program.is_ok());
            let program = program.unwrap();
            assert_eq!(program.statements.len(), 1);
            let statement = program.statements[0].clone();
            match statement {
                ast::Statement::ExpressionStatement(expression_statement) => {
                    match expression_statement {
                        ast::AllExpression::InfixExpression(infix_expression) => {
                            assert_eq!(*infix_expression.left, ast::AllExpression::Int(left));
                            assert_eq!(infix_expression.operator.to_string(), operator);
                            assert_eq!(*infix_expression.right, ast::AllExpression::Int(right));
                        }
                        _ => panic!("not a infix expression"),
                    }
                }
                _ => panic!("not an expression statement"),
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b".to_string(), "((-a) * b)".to_string()),
            ("!-a".to_string(), "(!(-a))".to_string()),
            ("a + b + c".to_string(), "((a + b) + c)".to_string()),
            ("a + b - c".to_string(), "((a + b) - c)".to_string()),
            ("a * b * c".to_string(), "((a * b) * c)".to_string()),
            ("a * b / c".to_string(), "((a * b) / c)".to_string()),
            ("a + b / c".to_string(), "(a + (b / c))".to_string()),
            (
                "a + b * c + d / e - f".to_string(),
                "(((a + (b * c)) + (d / e)) - f)".to_string(),
            ),
            ("3 + 4; -5 * 5".to_string(), "(3 + 4)((-5) * 5)".to_string()),
            (
                "5 > 4 == 3 < 4".to_string(),
                "((5 > 4) == (3 < 4))".to_string(),
            ),
            (
                "5 < 4 != 3 > 4".to_string(),
                "((5 < 4) != (3 > 4))".to_string(),
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5".to_string(),
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))".to_string(),
            ),
            (
                "1 + (2 + 3) + 4".to_string(),
                "((1 + (2 + 3)) + 4)".to_string(),
            ),
            ("(5 + 5) * 2".to_string(), "((5 + 5) * 2)".to_string()),
            ("2 / (5 + 5)".to_string(), "(2 / (5 + 5))".to_string()),
            ("-(5 + 5)".to_string(), "(-(5 + 5))".to_string()),
            (
                "!(true == true)".to_string(),
                "(!(true == true))".to_string(),
            ),
            (
                "a + add(b * c) + d".to_string(),
                "((a + add((b * c))) + d)".to_string(),
            ),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))".to_string(),
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))".to_string(),
            ),
            (
                "add(a + b + c * d / f + g)".to_string(),
                "add((((a + b) + ((c * d) / f)) + g))".to_string(),
            ),
        ];

        for (input, expected) in tests {
            let mut lexer = Lexer::new(input);

            let mut parser = Parser::new(&mut lexer);

            let program = parser.parse_program();

            assert!(program.is_ok());

            let program = program.unwrap();

            assert_eq!(program.to_string(), expected);
        }
    }

    #[test]
    fn text_example_prat_parser() {
        let input = "1 + 2 + 3".to_string();
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        let program = program.unwrap();

        assert_eq!(program.to_string(), "((1 + 2) + 3)");
    }

    #[test]
    fn test_boolean_expression() {
        let tests = vec![("true".to_string(), true), ("false".to_string(), false)];

        for (input, value) in tests {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();

            assert!(program.is_ok());
            let program = program.unwrap();
            assert_eq!(program.statements.len(), 1);
            let statement = program.statements[0].clone();
            match statement {
                ast::Statement::ExpressionStatement(expression_statement) => {
                    match expression_statement {
                        ast::AllExpression::Boolean(boolean) => {
                            assert_eq!(boolean, value);
                        }
                        _ => panic!("not a boolean expression"),
                    }
                }
                _ => panic!("not an expression statement"),
            }
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }".to_string();
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert!(program.is_ok());

        let program = program.unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = program.statements[0].clone();

        match statement {
            ast::Statement::ExpressionStatement(expression_statement) => match expression_statement
            {
                ast::AllExpression::IfExpression(if_expression) => {
                    assert_eq!(if_expression.condition.to_string(), "(x < y)".to_string());
                    assert_eq!(if_expression.consequence.statements.len(), 1);
                    let consequence = if_expression.consequence.statements[0].clone();
                    match consequence {
                        ast::Statement::ExpressionStatement(expression_statement) => {
                            match expression_statement {
                                ast::AllExpression::Identifier(identifier) => {
                                    assert_eq!(identifier, "x".to_string());
                                }
                                _ => panic!("not an identifier"),
                            }
                        }
                        _ => panic!("not an expression statement"),
                    }
                    assert!(if_expression.alternative.is_none());
                }
                _ => panic!("not an if expression"),
            },
            _ => panic!("not an expression statement"),
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }".to_string();
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert!(program.is_ok());

        let program = program.unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = program.statements[0].clone();

        match statement {
            ast::Statement::ExpressionStatement(expression_statement) => match expression_statement
            {
                ast::AllExpression::IfExpression(if_expression) => {
                    assert_eq!(if_expression.condition.to_string(), "(x < y)".to_string());
                    assert_eq!(if_expression.consequence.statements.len(), 1);
                    let consequence = if_expression.consequence.statements[0].clone();
                    match consequence {
                        ast::Statement::ExpressionStatement(expression_statement) => {
                            match expression_statement {
                                ast::AllExpression::Identifier(identifier) => {
                                    assert_eq!(identifier, "x".to_string());
                                }
                                _ => panic!("not an identifier"),
                            }
                        }
                        _ => panic!("not an expression statement"),
                    }
                    assert!(if_expression.alternative.is_some());
                    let alternative = if_expression.alternative.unwrap();
                    assert_eq!(alternative.statements.len(), 1);
                    let alternative_statement = alternative.statements[0].clone();
                    match alternative_statement {
                        ast::Statement::ExpressionStatement(expression_statement) => {
                            match expression_statement {
                                ast::AllExpression::Identifier(identifier) => {
                                    assert_eq!(identifier, "y".to_string());
                                }
                                _ => panic!("not an identifier"),
                            }
                        }
                        _ => panic!("not an expression statement"),
                    }
                }
                _ => panic!("not an if expression"),
            },
            _ => panic!("not an expression statement"),
        }
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x, y) { x + y; }".to_string();
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert!(program.is_ok());

        let program = program.unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = program.statements[0].clone();

        match statement {
            ast::Statement::ExpressionStatement(expression_statement) => match expression_statement
            {
                ast::AllExpression::FunctionLiteral(function_literal) => {
                    let parameters = function_literal.parameters.unwrap();
                    assert_eq!(parameters.len(), 2);
                    assert_eq!(parameters[0].to_string(), "x".to_string());
                    assert_eq!(parameters[1].to_string(), "y".to_string());
                    assert_eq!(function_literal.body.statements.len(), 1);
                    let body_statement = function_literal.body.statements[0].clone();
                    match body_statement {
                        ast::Statement::ExpressionStatement(expression_statement) => {
                            match expression_statement {
                                ast::AllExpression::InfixExpression(infix_expression) => {
                                    assert_eq!(infix_expression.operator, TokenType::PLUS);
                                    assert_eq!(infix_expression.left.to_string(), "x".to_string());
                                    assert_eq!(infix_expression.right.to_string(), "y".to_string());
                                }
                                _ => panic!("not an infix expression"),
                            }
                        }
                        _ => panic!("not an expression statement"),
                    }
                }
                _ => panic!("not a function literal"),
            },
            _ => panic!("not an expression statement"),
        }
    }

    #[test]
    fn test_function_parameter_parsing() {
        let tests: Vec<(String, Option<Vec<String>>)> = vec![
            ("fn() {}".to_string(), None),
            ("fn(x) {}".to_string(), Some(vec!["x".to_string()])),
            (
                "fn(x, y, z) {}".to_string(),
                Some(vec!["x".to_string(), "y".to_string(), "z".to_string()]),
            ),
        ];

        for (input, expected_params) in tests {
            let mut lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();

            assert!(program.is_ok());

            let program = program.unwrap();

            assert_eq!(program.statements.len(), 1);

            let statement = program.statements[0].clone();

            match statement {
                ast::Statement::ExpressionStatement(expression_statement) => {
                    match expression_statement {
                        ast::AllExpression::FunctionLiteral(function_literal) => {
                            assert_eq!(function_literal.parameters, expected_params);
                        }
                        _ => panic!("not a function literal"),
                    }
                }
                _ => panic!("not an expression statement"),
            }
        }
    }

    #[test]
    fn test_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);".to_string();
        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert!(program.is_ok());

        let program = program.unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = program.statements[0].clone();

        match statement {
            ast::Statement::ExpressionStatement(expression_statement) => match expression_statement
            {
                ast::AllExpression::CallExpression(call_expression) => {
                    let arguments = call_expression.arguments.unwrap();
                    assert_eq!(call_expression.function.to_string(), "add".to_string());
                    assert_eq!(arguments.len(), 3);
                    assert_eq!(arguments[0].to_string(), "1".to_string());
                    assert_eq!(arguments[1].to_string(), "(2 * 3)".to_string());
                    assert_eq!(arguments[2].to_string(), "(4 + 5)".to_string());
                }
                _ => panic!("not a call expression"),
            },
            _ => panic!("not an expression statement"),
        }
    }
}
