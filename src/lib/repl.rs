use crate::lib::evaluator::Evaluator;

use super::lexer::Lexer;
use super::parser::Parser;

pub fn start() {
    println!("Welcome to the Monkey programming language!");
    println!("Feel free to type in commands");
    println!("");

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
                let evaluator = Evaluator::new();
                let evaluated = evaluator.eval(&program);
                println!("{}", evaluated);
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
