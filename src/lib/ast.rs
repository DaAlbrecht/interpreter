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
}

#[derive(Debug, Clone, PartialEq)]
pub enum AllExpression {
    Int(usize),
    Identifier(String),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
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
            Statement::LetStatement(let_statement) => write!(f, "{}", let_statement.to_string()),
            Statement::ReturnStatement(return_statement) => {
                write!(f, "{}", return_statement.to_string())
            }
            Statement::ExpressionStatement(expression_statement) => {
                write!(f, "{}", expression_statement.to_string())
            }
        }
    }
}
impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "let {} = {};", self.name, self.value.to_string())
    }
}
impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right.to_string())
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "({} {} {})",
            self.left.to_string(),
            self.operator,
            self.right.to_string()
        )
    }
}

impl Display for AllExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            AllExpression::Int(int) => write!(f, "{}", int),
            AllExpression::Identifier(identifier) => write!(f, "{}", identifier),
            AllExpression::PrefixExpression(prefix_expression) => {
                write!(f, "{}", prefix_expression.to_string())
            }
            AllExpression::InfixExpression(infix_expression) => {
                write!(f, "{}", infix_expression.to_string())
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
