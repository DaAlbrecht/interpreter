use super::tokens::TokenType;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(AllExpression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStatement {
    pub name: String,
    pub value: AllExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AllExpression {
    Int(usize),
}
