use std::fmt::Display;

use super::tokens::TokenType;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(AllExpression),
    ExpressionStatement(AllExpression),
    BlockStatement(BlockStatement),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AllExpression {
    Int(usize),
    String(String),
    Identifier(String),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    Boolean(bool),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
    ArrayLiteral(Option<Vec<AllExpression>>),
    IndexExpression(IndexExpression),
    HashLiteral(HashLiteral),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

//----------------- Statement -----------------//
#[derive(Debug, Clone, PartialEq)]
pub struct LetStatement {
    pub name: String,
    pub value: AllExpression,
}

//----------------- Expression ----------------//

#[derive(Debug, Clone, PartialEq)]
pub struct PrefixExpression {
    pub operator: TokenType,
    pub right: Box<AllExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InfixExpression {
    pub left: Box<AllExpression>,
    pub operator: TokenType,
    pub right: Box<AllExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression {
    pub condition: Box<AllExpression>,
    pub consequence: Box<Statement>,
    pub alternative: Option<Box<Statement>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionLiteral {
    pub parameters: Option<Vec<String>>,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub function: Box<AllExpression>,
    pub arguments: Option<Vec<AllExpression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpression {
    pub left: Box<AllExpression>,
    pub index: Box<AllExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HashLiteral {
    pub pairs: Vec<(AllExpression, AllExpression)>,
}
//----------------- Display -----------------//

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut s = String::new();
        for statement in &self.statements {
            s.push_str(&statement.to_string());
        }
        write!(f, "{}", s)
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Statement::LetStatement(let_statement) => write!(f, "{}", let_statement),
            Statement::ReturnStatement(return_statement) => {
                write!(f, "{}", return_statement)
            }
            Statement::ExpressionStatement(expression_statement) => {
                write!(f, "{}", expression_statement)
            }
            Statement::BlockStatement(block_statement) => {
                write!(f, "{}", block_statement)
            }
        }
    }
}
impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}
impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut s = String::new();
        s.push_str("if");
        s.push_str(&self.condition.to_string());
        s.push_str(" ");
        s.push_str(&self.consequence.to_string());
        if let Some(alternative) = &self.alternative {
            s.push_str("else ");
            s.push_str(&alternative.to_string());
        }
        write!(f, "{}", s)
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut s = String::new();
        for statement in &self.statements {
            s.push_str(&statement.to_string());
        }
        write!(f, "{}", s)
    }
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut s = String::new();
        s.push_str("fn");
        if let Some(parameters) = &self.parameters {
            s.push_str("(");
            for (i, parameter) in parameters.iter().enumerate() {
                s.push_str(parameter);
                if i != parameters.len() - 1 {
                    s.push_str(", ");
                }
            }
            s.push_str(")");
        }
        s.push_str(" ");
        s.push_str(&self.body.to_string());
        write!(f, "{}", s)
    }
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut s = String::new();
        s.push_str(&self.function.to_string());
        s.push_str("(");
        if let Some(arguments) = &self.arguments {
            for (i, argument) in arguments.iter().enumerate() {
                s.push_str(&argument.to_string());
                if i != arguments.len() - 1 {
                    s.push_str(", ");
                }
            }
        }
        s.push_str(")");
        write!(f, "{}", s)
    }
}

impl Display for IndexExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}

impl Display for HashLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut s = String::new();
        s.push_str("{");
        for (i, (key, value)) in self.pairs.iter().enumerate() {
            s.push_str(&key.to_string());
            s.push_str(": ");
            s.push_str(&value.to_string());
            if i != self.pairs.len() - 1 {
                s.push_str(", ");
            }
        }
        s.push_str("}");
        write!(f, "{}", s)
    }
}

impl Display for AllExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            AllExpression::Int(int) => write!(f, "{}", int),
            AllExpression::String(string) => write!(f, "{}", string),
            AllExpression::Identifier(identifier) => write!(f, "{}", identifier),
            AllExpression::PrefixExpression(prefix_expression) => {
                write!(f, "{}", prefix_expression)
            }
            AllExpression::InfixExpression(infix_expression) => {
                write!(f, "{}", infix_expression)
            }
            AllExpression::Boolean(boolean) => write!(f, "{}", boolean),
            AllExpression::IfExpression(if_expression) => {
                write!(f, "{}", if_expression)
            }
            AllExpression::FunctionLiteral(function_literal) => {
                write!(f, "{}", function_literal)
            }
            AllExpression::CallExpression(call_expression) => {
                write!(f, "{}", call_expression)
            }
            AllExpression::ArrayLiteral(array_literal) => match array_literal {
                Some(array_literal) => {
                    let mut s = String::new();
                    s.push_str("[");
                    for (i, element) in array_literal.iter().enumerate() {
                        s.push_str(&element.to_string());
                        if i != array_literal.len() - 1 {
                            s.push_str(", ");
                        }
                    }
                    s.push_str("]");
                    write!(f, "{}", s)
                }
                None => write!(f, "[]"),
            },
            AllExpression::IndexExpression(index_expression) => {
                write!(f, "{}", index_expression)
            }
            AllExpression::HashLiteral(hash_literal) => {
                write!(f, "{}", hash_literal)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Statement::LetStatement(LetStatement {
                name: "myVar".to_string(),
                value: AllExpression::Identifier("anotherVar".to_string()),
            })],
        };
        assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }
}
