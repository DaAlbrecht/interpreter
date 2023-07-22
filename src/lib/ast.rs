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
    Boolean(bool),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
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
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionLiteral {
    pub parameters: Option<Vec<String>>,
    pub body: BlockStatement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub function: Box<AllExpression>,
    pub arguments: Option<Vec<AllExpression>>,
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
            AllExpression::Boolean(boolean) => write!(f, "{}", boolean),
            AllExpression::IfExpression(if_expression) => {
                write!(f, "{}", if_expression.to_string())
            }
            AllExpression::FunctionLiteral(function_literal) => {
                write!(f, "{}", function_literal.to_string())
            }
            AllExpression::CallExpression(call_expression) => {
                write!(f, "{}", call_expression.to_string())
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
