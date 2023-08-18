use crate::interpreter::object::Function;

use super::ast;
use super::ast::AllExpression;
use super::ast::Statement;
use super::builtins;
use super::environment::Environment;
use super::object::Object;
use super::{object::TypeName, tokens::TokenType};
use std::cell::RefCell;
use std::rc::Rc;

pub struct Evaluator {
    env: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub fn new(env: Rc<RefCell<Environment>>) -> Evaluator {
        Evaluator { env }
    }

    pub fn eval(&mut self, program: &ast::Program) -> Option<Object> {
        let mut result = Some(Object::Null);
        for statement in &program.statements {
            result = match self.eval_statement(statement) {
                Some(result) => Some(result),
                None => None,
            };
            match result {
                Some(Object::ReturnValue(object)) => return Some(*object),
                Some(Object::Error(_)) => return result,
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

    fn eval_statement(&mut self, statement: &Statement) -> Option<Object> {
        match statement {
            Statement::ExpressionStatement(expression_statement) => {
                Some(self.eval_expression(&expression_statement))
            }
            Statement::BlockStatement(block_statement) => {
                self.eval_block_statement(&block_statement)
            }
            Statement::ReturnStatement(return_statement) => {
                let value = self.eval_expression(&return_statement);
                match value {
                    Object::Error(_) => Some(value),
                    _ => Some(Object::ReturnValue(Box::new(value))),
                }
            }
            Statement::LetStatement(let_statement) => {
                let value = self.eval_expression(&let_statement.value);
                match value {
                    Object::Error(_) => Some(value),
                    _ => self.env.borrow_mut().set(&let_statement.name, value),
                }
            }
        }
    }

    fn eval_expression(&mut self, expression: &AllExpression) -> Object {
        match expression {
            AllExpression::Int(int) => Object::Int(*int as i64),
            AllExpression::String(string) => Object::String(string.clone()),
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
            AllExpression::FunctionLiteral(function_literal) => {
                let parameters = function_literal.parameters.clone();
                let body = *function_literal.body.clone();
                match body {
                    ast::Statement::BlockStatement(block_statement) => {
                        Object::FunctionLiteral(Function {
                            parameters,
                            body: block_statement,
                            env: self.env.clone(),
                        })
                    }
                    _ => self.new_error("function body must be a block statement"),
                }
            }
            AllExpression::CallExpression(call_expression) => {
                let function = self.eval_expression(&call_expression.function);
                if let Object::Error(_) = function {
                    return function;
                }
                match &call_expression.arguments {
                    Some(arguments) => {
                        let arguments: Vec<Object> = arguments
                            .iter()
                            .map(|argument| self.eval_expression(argument))
                            .collect();
                        self.apply_function(&function, Some(arguments))
                    }
                    None => self.apply_function(&function, None),
                }
            }
            AllExpression::ArrayLiteral(elements) => match elements {
                Some(elements) => {
                    let elements: Vec<Object> = elements
                        .iter()
                        .map(|element| self.eval_expression(element))
                        .collect();
                    Object::Array(elements)
                }
                None => Object::Array(vec![]),
            },
            AllExpression::IndexExpression(index_expression) => {
                let left = self.eval_expression(&index_expression.left);
                if let Object::Error(_) = left {
                    return left;
                }

                let index = self.eval_expression(&index_expression.index);
                if let Object::Error(_) = index {
                    return index;
                }

                self.eval_index_expression(&left, &index)
            }
        }
    }

    fn eval_prefix_expression(&mut self, prefix_expression: &ast::PrefixExpression) -> Object {
        let right = self.eval_expression(&prefix_expression.right);
        match right {
            Object::Error(_) => right,
            _ => match prefix_expression.operator {
                TokenType::BANG => self.eval_bang_operator_expression(&right),
                TokenType::MINUS => self.eval_minus_prefix_operator_expression(&right),
                _ => self.new_error(&format!(
                    "unknown operator: {}{}",
                    prefix_expression.operator,
                    right.type_name()
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
            _ => self.new_error(&format!("unknown operator: -{}", object.type_name())),
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
                    "INTEGER", operator, "INTEGER"
                )),
            },
            (Object::Boolean(left), Object::Boolean(right)) => match operator {
                TokenType::EQ => Object::Boolean(left == right),
                TokenType::NOTEQ => Object::Boolean(left != right),
                _ => self.new_error(&format!(
                    "unknown operator: {} {} {}",
                    "BOOLEAN", operator, "BOOLEAN"
                )),
            },
            (Object::String(left), Object::String(right)) => match operator {
                TokenType::PLUS => Object::String(format!("{}{}", left, right)),
                TokenType::EQ => Object::Boolean(left == right),
                TokenType::NOTEQ => Object::Boolean(left != right),
                _ => self.new_error(&format!(
                    "unknown operator: {} {} {}",
                    "STRING", operator, "STRING"
                )),
            },
            _ => self.new_error(&format!(
                "type mismatch: {} {} {}",
                left.type_name(),
                operator,
                right.type_name()
            )),
        }
    }

    fn eval_if_expression(&mut self, if_expression: &ast::IfExpression) -> Object {
        let condition = self.eval_expression(&if_expression.condition);
        match condition {
            Object::Error(_) => return condition,
            _ => {
                if self.is_truthy(&condition) {
                    self.eval_statement(&if_expression.consequence).unwrap()
                } else if let Some(alternative) = &if_expression.alternative {
                    self.eval_statement(alternative).unwrap()
                } else {
                    Object::Null
                }
            }
        }
    }

    fn eval_block_statement(&mut self, block_statement: &ast::BlockStatement) -> Option<Object> {
        let mut result = Some(Object::Null);
        for statement in &block_statement.statements {
            result = self.eval_statement(statement);

            match result {
                Some(Object::ReturnValue(_)) | Some(Object::Error(_)) => return result,
                _ => (),
            }
        }
        result
    }

    fn eval_identifier(&self, identifier: &str) -> Object {
        match self.env.borrow().get(identifier) {
            Some((object, _)) => object,
            None => match builtins::get_builtins().get(identifier) {
                Some(builtin) => builtin.clone(),
                None => self.new_error(&format!("identifier not found: {}", identifier)),
            },
        }
    }

    fn apply_function(&mut self, function: &Object, arguments: Option<Vec<Object>>) -> Object {
        match function {
            Object::FunctionLiteral(function) => {
                let mut extended_env = Environment::new_enclosed_environment(function.env.clone());
                match function.parameters.clone() {
                    Some(parameters) => {
                        if arguments.is_none() {
                            return self.new_error(&format!(
                                "wrong number of arguments: expected={}, got=0",
                                parameters.len()
                            ));
                        }
                        let arguments = arguments.unwrap();
                        if parameters.len() != arguments.len() {
                            return self.new_error(&format!(
                                "wrong number of arguments: expected={}, got={}",
                                parameters.len(),
                                arguments.len()
                            ));
                        }
                        for (parameter, argument) in parameters.iter().zip(arguments.iter()) {
                            extended_env.set(parameter, argument.clone());
                        }
                    }
                    None => {
                        if arguments.is_some() {
                            return self.new_error(&format!(
                                "wrong number of arguments: expected=0, got={}",
                                arguments.unwrap().len()
                            ));
                        }
                    }
                }

                let current_env = self.env.clone();
                self.env = Rc::new(RefCell::new(extended_env));

                let object = self.eval_block_statement(&function.body).unwrap();

                self.env = current_env;

                object
            }
            Object::BuiltinFunction(builtin) => builtin(arguments),
            _ => self.new_error(&format!("not a function: {}", function.type_name())),
        }
    }

    fn eval_index_expression(&mut self, left: &Object, index: &Object) -> Object {
        match (left, index) {
            (Object::Array(elements), Object::Int(index)) => {
                self.eval_array_index_expression(elements, *index)
            }
            _ => self.new_error(&format!(
                "index operator not supported: {}",
                left.type_name()
            )),
        }
    }

    fn eval_array_index_expression(&mut self, elements: &[Object], index: i64) -> Object {
        let max = elements.len() as i64 - 1;
        if index < 0 || index > max {
            return Object::Null;
        }
        elements[index as usize].clone()
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::rc::Rc;

    use crate::interpreter::ast::{AllExpression, BlockStatement, InfixExpression, Statement};
    use crate::interpreter::environment::Environment;
    use crate::interpreter::tokens::TokenType;

    use super::super::lexer::Lexer;
    use super::super::object::Object;
    use super::super::parser::Parser;
    use super::Evaluator;

    fn test_eval(input: &str) -> Option<Object> {
        let mut lexer = Lexer::new(input.into());
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program().unwrap();
        let env = Rc::new(RefCell::new(Environment::new()));
        let mut evaluator = Evaluator::new(env);
        let eval = evaluator.eval(&program);
        dbg!(&eval);
        eval
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
            let evaluated = test_eval(input).unwrap();
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
            let evaluated = test_eval(input).unwrap();
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
            let evaluated = test_eval(input).unwrap();
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
            let evaluated = test_eval(input).unwrap();
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
            let evaluated = test_eval(input).unwrap();
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
            (r#""Hello" - "World""#, "unknown operator: STRING - STRING"),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
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
            dbg!(&input);
            let evaluated = test_eval(input).unwrap();
            match expected {
                Object::Int(int) => test_integer_object(evaluated, int),
                _ => panic!("object is not integer. got={:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_eval_function_object() {
        let input = "fn(x) { x + 2; };";
        let evaluated = test_eval(input).unwrap();
        match evaluated {
            Object::FunctionLiteral(function) => {
                let params = function.parameters.unwrap();
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].to_string(), "x");
                let block_statement = BlockStatement {
                    statements: vec![Statement::ExpressionStatement(
                        AllExpression::InfixExpression(InfixExpression {
                            left: Box::new(AllExpression::Identifier("x".to_string())),
                            operator: TokenType::PLUS,
                            right: Box::new(AllExpression::Int(2)),
                        }),
                    )],
                };
                assert_eq!(function.body, block_statement);
            }
            _ => panic!("object is not function. got={:?}", evaluated),
        }
    }

    #[test]
    fn test_eval_function_application() {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", Object::Int(5)),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Object::Int(5),
            ),
            ("let double = fn(x) { x * 2; }; double(5);", Object::Int(10)),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", Object::Int(10)),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Object::Int(20),
            ),
            ("fn(x) { x; }(5)", Object::Int(5)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            match expected {
                Object::Int(int) => test_integer_object(evaluated, int),
                _ => panic!("object is not integer. got={:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_eval_string_literal() {
        let input = "\"Hello World!\"";
        let evaluated = test_eval(input).unwrap();
        match evaluated {
            Object::String(string) => assert_eq!(string, "Hello World!"),
            _ => panic!("object is not string. got={:?}", evaluated),
        }
    }

    #[test]
    fn test_eval_string_concatenation() {
        let input = "\"Hello\" + \" \" + \"World!\"";
        let evaluated = test_eval(input).unwrap();
        match evaluated {
            Object::String(string) => assert_eq!(string, "Hello World!"),
            _ => panic!("object is not string. got={:?}", evaluated),
        }
    }

    #[test]
    fn test_builtin_functions() {
        let tests = vec![
            ("len(\"\")", Object::Int(0)),
            ("len(\"four\")", Object::Int(4)),
            ("len(\"hello world\")", Object::Int(11)),
            (
                "len(1)",
                Object::Error("argument to `len` not supported, got INTEGER".to_string()),
            ),
            (
                "len(\"one\", \"two\")",
                Object::Error("wrong number of arguments. got=2, want=1".to_string()),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            match expected {
                Object::Int(int) => test_integer_object(evaluated, int),
                Object::Error(error) => match evaluated {
                    Object::Error(evaluated_error) => assert_eq!(error, evaluated_error),
                    _ => panic!("object is not error. got={:?}", evaluated),
                },
                _ => panic!("object is not integer or error. got={:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let evaluated = test_eval(input).unwrap();
        match evaluated {
            Object::Array(elements) => {
                assert_eq!(elements.len(), 3);
                test_integer_object(elements[0].clone(), 1);
                test_integer_object(elements[1].clone(), 4);
                test_integer_object(elements[2].clone(), 6);
            }
            _ => panic!("object is not array. got={:?}", evaluated),
        }
    }

    #[test]
    fn test_array_index_expressions() {
        let tests = vec![
            ("[1, 2, 3][0]", Object::Int(1)),
            ("[1, 2, 3][1]", Object::Int(2)),
            ("[1, 2, 3][2]", Object::Int(3)),
            ("let i = 0; [1][i];", Object::Int(1)),
            ("[1, 2, 3][1 + 1];", Object::Int(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Object::Int(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Object::Int(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Object::Int(2),
            ),
            ("[1, 2, 3][3]", Object::Null),
            ("[1, 2, 3][-1]", Object::Null),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            match expected {
                Object::Int(int) => test_integer_object(evaluated, int),
                Object::Null => test_null_object(evaluated),
                _ => panic!("object is not integer or null. got={:?}", evaluated),
            }
        }
    }
}
