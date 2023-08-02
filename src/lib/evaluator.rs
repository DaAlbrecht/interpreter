use super::ast;
use super::ast::AllExpression;
use super::ast::Statement;
use super::object::Object;
use super::tokens::TokenType;

pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {}
    }

    pub fn eval(&self, program: &ast::Program) -> Object {
        let mut result = Object::Null;
        for statement in &program.statements {
            result = self.eval_statement(statement);
        }
        result
    }

    fn eval_statement(&self, statement: &Statement) -> Object {
        match statement {
            Statement::ExpressionStatement(expression_statement) => {
                self.eval_expression(&expression_statement)
            }
            _ => Object::Null,
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
                let right = self.eval_expression(&infix_expression.right);
                let operator = &infix_expression.operator;
                self.eval_infix_expression(&operator, &left, &right)
            }
            _ => unimplemented!(),
        }
    }

    fn eval_prefix_expression(&self, prefix_expression: &ast::PrefixExpression) -> Object {
        let right = self.eval_expression(&prefix_expression.right);
        match prefix_expression.operator {
            TokenType::BANG => self.eval_bang_operator_expression(&right),
            TokenType::MINUS => self.eval_minus_prefix_operator_expression(&right),
            _ => Object::Null,
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
            _ => Object::Null,
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
                _ => Object::Null,
            },
            (Object::Boolean(left), Object::Boolean(right)) => match operator {
                TokenType::EQ => Object::Boolean(left == right),
                TokenType::NOTEQ => Object::Boolean(left != right),
                _ => Object::Null,
            },
            _ => Object::Null,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::lexer::Lexer;
    use super::super::object::Object;
    use super::super::parser::Parser;
    use super::Evaluator;

    fn test_eval(input: &str) -> Object {
        let mut lexer = Lexer::new(input.into());
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program().unwrap();
        let evaluator = Evaluator::new();
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
    fn test_bang_operator() {
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
}
