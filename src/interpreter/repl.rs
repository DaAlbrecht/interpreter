use std::cell::RefCell;
use std::rc::Rc;

use crate::interpreter::environment::Environment;
use crate::interpreter::evaluator::Evaluator;
use crate::interpreter::object::Object;

use super::lexer::Lexer;
use super::parser::Parser;

pub fn start() {
    println!("Welcome to the Monkey programming language!");
    println!("Feel free to type in commands");
    println!("");

    let env = Rc::new(RefCell::new(Environment::new()));
    loop {
        let mut input = String::new();
        std::io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        match program {
            Ok(program) => {
                let mut evaluator = Evaluator::new(env.clone());
                let evaluated = evaluator.eval(&program);
                match evaluated {
                    Object::ReturnValue(obj) => println!("{}", obj),
                    Object::Error(msg) => println!("{}", msg),
                    Object::Int(int) => println!("{}", int),
                    Object::Boolean(boolean) => println!("{}", boolean),
                    Object::Null => println!("null"),
                    _ => println!(""),
                }
            }
            Err(error) => {
                let monkey_face = r#"
                            __,__
                   .--.  .-"     "-.  .--.
                  / .. \/  .-. .-.  \/ .. \
                 | |  '|  /   Y   \  |'  | |
                 | \   \  \ 0 | 0 /  /   / |
                  \ '- ,\.-"""""""-./, -' /
                   ''-' /_   ^ ^   _\ '-''
                       |  \._   _./  |
                       \   \ '~' /   /
                        '._ '-=-' _.'
                           '-----'
                "#;
                println!("{}", monkey_face);
                println!("Woops! We ran into some monkey business here!");
                println!("{}", error);
            }
        }
    }
}
