use super::ast;
use super::ast::AllExpression;
use super::ast::Statement;
use super::environment::Environment;
use super::object::Object;
use super::tokens::TokenType;
use std::cell::RefCell;

pub struct Evaluator<'a> {
    env: &'a RefCell<Environment>,
}

impl<'a> Evaluator<'a> {
    pub fn new(env: &'a RefCell<Environment>) -> Evaluator {
        Evaluator { env }
    }

    pub fn eval(&self, program: &ast::Program) -> Object {
        let mut result = Object::Null;
        for statement in &program.statements {
            result = self.eval_statement(statement);

            match result {
                Object::ReturnValue(object) => return *object,
                Object::Error(_) => return result,
                _ => (),
            }
        }
        result
    }

    fn is_truthy(&self, object: &Object) -> bool {
        match object {
            Object::Null => false,
            Object::Boolean(bool) => *bool,
            _ => true,
        }
    }

    fn new_error(&self, message: &str) -> Object {
        Object::Error(message.to_string())
    }

    fn eval_statement(&self, statement: &Statement) -> Object {
        match statement {
            Statement::ExpressionStatement(expression_statement) => {
                self.eval_expression(&expression_statement)
            }
            Statement::BlockStatement(block_statement) => {
                self.eval_block_statement(&block_statement)
            }
            Statement::ReturnStatement(return_statement) => {
                let value = self.eval_expression(&return_statement);
                match value {
                    Object::Error(_) => value,
                    _ => Object::ReturnValue(Box::new(value)),
                }
            }
            Statement::LetStatement(let_statement) => {
                let value = self.eval_expression(&let_statement.value);
                if let Object::Error(_) = value {
                    return value;
                }
                self.env
                    .borrow_mut()
                    .set(&let_statement.name, value.clone());
                value
            }
        }
    }

    fn eval_expression(&self, expression: &AllExpression) -> Object {
        match expression {
            AllExpression::Int(int) => Object::Int(*int as i64),
            AllExpression::Boolean(bool) => Object::Boolean(*bool),
            AllExpression::PrefixExpression(prefix_expression) => {
                self.eval_prefix_expression(&prefix_expression)
            }
            AllExpression::InfixExpression(infix_expression) => {
                let left = self.eval_expression(&infix_expression.left);

                if let Object::Error(_) = left {
                    return left;
                }

                let right = self.eval_expression(&infix_expression.right);

                if let Object::Error(_) = right {
                    return right;
                }

                let operator = &infix_expression.operator;
                self.eval_infix_expression(&operator, &left, &right)
            }
            AllExpression::IfExpression(if_expression) => self.eval_if_expression(&if_expression),
            AllExpression::Identifier(identifier) => self.eval_identifier(&identifier),
            _ => Object::Null,
        }
    }

    fn eval_prefix_expression(&self, prefix_expression: &ast::PrefixExpression) -> Object {
        let right = self.eval_expression(&prefix_expression.right);
        match right {
            Object::Error(_) => right,
            _ => match prefix_expression.operator {
                TokenType::BANG => self.eval_bang_operator_expression(&right),
                TokenType::MINUS => self.eval_minus_prefix_operator_expression(&right),
                _ => self.new_error(&format!(
                    "unknown operator: {}{}",
                    prefix_expression.operator, right
                )),
            },
        }
    }

    fn eval_bang_operator_expression(&self, object: &Object) -> Object {
        match object {
            Object::Boolean(bool) => Object::Boolean(!bool),
            Object::Null => Object::Boolean(true),
            _ => Object::Boolean(false),
        }
    }

    fn eval_minus_prefix_operator_expression(&self, object: &Object) -> Object {
        match object {
            Object::Int(int) => Object::Int(-int),
            _ => self.new_error(&format!("unknown operator: -{}", object)),
        }
    }

    fn eval_infix_expression(&self, operator: &TokenType, left: &Object, right: &Object) -> Object {
        match (left, right) {
            (Object::Int(left), Object::Int(right)) => match operator {
                TokenType::PLUS => Object::Int(left + right),
                TokenType::MINUS => Object::Int(left - right),
                TokenType::ASTERISK => Object::Int(left * right),
                TokenType::SLASH => Object::Int(left / right),
                TokenType::LT => Object::Boolean(left < right),
                TokenType::GT => Object::Boolean(left > right),
                TokenType::EQ => Object::Boolean(left == right),
                TokenType::NOTEQ => Object::Boolean(left != right),
                _ => self.new_error(&format!(
                    "unknown operator: {} {} {}",
                    Object::Int(*left),
                    operator,
                    Object::Int(*right)
                )),
            },
            (Object::Boolean(left), Object::Boolean(right)) => match operator {
                TokenType::EQ => Object::Boolean(left == right),
                TokenType::NOTEQ => Object::Boolean(left != right),
                _ => self.new_error(&format!(
                    "unknown operator: {} {} {}",
                    Object::Boolean(*left),
                    operator,
                    Object::Boolean(*right)
                )),
            },
            _ => self.new_error(&format!("type mismatch: {} {} {}", left, operator, right)),
        }
    }

    fn eval_if_expression(&self, if_expression: &ast::IfExpression) -> Object {
        let condition = self.eval_expression(&if_expression.condition);
        match condition {
            Object::Error(_) => return condition,
            _ => {
                if self.is_truthy(&condition) {
                    self.eval_statement(&if_expression.consequence)
                } else if let Some(alternative) = &if_expression.alternative {
                    self.eval_statement(alternative)
                } else {
                    Object::Null
                }
            }
        }
    }

    fn eval_block_statement(&self, block_statement: &ast::BlockStatement) -> Object {
        let mut result = Object::Null;
        for statement in &block_statement.statements {
            result = self.eval_statement(statement);

            match result {
                Object::ReturnValue(_) | Object::Error(_) => return result,
                _ => (),
            }
        }
        result
    }

    fn eval_identifier(&self, identifier: &str) -> Object {
        match self.env.borrow().get(identifier) {
            Some(value) => value,
            None => self.new_error(&format!("identifier not found: {}", identifier)),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;

    use crate::interpreter::environment::Environment;

    use super::super::lexer::Lexer;
    use super::super::object::Object;
    use super::super::parser::Parser;
    use super::Evaluator;

    fn test_eval(input: &str) -> Object {
        let mut lexer = Lexer::new(input.into());
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program().unwrap();
        let env = RefCell::new(Environment::new());
        let evaluator = Evaluator::new(&env);
        evaluator.eval(&program)
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests: Vec<(&str, i64)> = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(evaluated, expected);
        }
    }

    fn test_integer_object(object: Object, expected: i64) {
        match object {
            Object::Int(int) => assert_eq!(int, expected),
            _ => panic!("object is not integer. got={:?}", object),
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
    }

    fn test_boolean_object(object: Object, expected: bool) {
        match object {
            Object::Boolean(boolean) => assert_eq!(boolean, expected),
            _ => panic!("object is not boolean. got={:?}", object),
        }
    }

    #[test]
    fn test_eval_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
    }

    #[test]
    fn test_eval_if_else_expression() {
        let tests = vec![
            ("if (true) { 10 }", Object::Int(10)),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", Object::Int(10)),
            ("if (1 < 2) { 10 }", Object::Int(10)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 > 2) { 10 } else { 20 }", Object::Int(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Int(10)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match expected {
                Object::Int(int) => test_integer_object(evaluated, int),
                Object::Null => test_null_object(evaluated),
                _ => panic!("object is not integer or null. got={:?}", evaluated),
            }
        }
    }

    fn test_null_object(object: Object) {
        match object {
            Object::Null => (),
            _ => panic!("object is not null. got={:?}", object),
        }
    }

    #[test]
    fn test_eval_return_statement() {
        let tests = vec![
            ("return 10;", Object::Int(10)),
            ("return 10; 9;", Object::Int(10)),
            ("return 2 * 5; 9;", Object::Int(10)),
            ("9; return 2 * 5; 9;", Object::Int(10)),
            (
                r#"
                if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }
                    return 1;
                }
                "#,
                Object::Int(10),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match expected {
                Object::Int(int) => test_integer_object(evaluated, int),
                _ => panic!("object is not integer. got={:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_eval_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                r#"
                if (10 > 1) {
                    if (10 > 1) {
                        return true + false;
                    }
                    return 1;
                }
                "#,
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar", "identifier not found: foobar"),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match evaluated {
                Object::Error(error) => assert_eq!(error, expected),
                _ => panic!("no error object returned. got={:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_eval_let_statements() {
        let tests = vec![
            ("let a = 5; a;", Object::Int(5)),
            ("let a = 5 * 5; a;", Object::Int(25)),
            ("let a = 5; let b = a; b;", Object::Int(5)),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Object::Int(15),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match expected {
                Object::Int(int) => test_integer_object(evaluated, int),
                _ => panic!("object is not integer. got={:?}", evaluated),
            }
        }
    }
}
