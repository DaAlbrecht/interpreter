use super::lexer::Lexer;

pub fn start() {
    println!("Welcome to the Monkey programming language!");
    println!("Feel free to type in commands");
    println!("");

    loop {
        let mut input = String::new();
        std::io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");
        let lexer = Lexer::new(input);
        for token in lexer {
            println!("{:?}", token);
        }
    }
}
