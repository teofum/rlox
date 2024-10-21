use crate::rlox::token::Token;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum Value {
    Nil,
    True,
    False,
    Number(f64),
    String(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::True => write!(f, "true"),
            Value::False => write!(f, "false"),
            Value::Number(num) => write!(f, "{}", num),
            Value::String(str) => write!(f, "{}", str),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Literal(Value),
    Grouping(Box<Expr>),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(value) => write!(f, "{}", value),
            Expr::Grouping(expr) => write!(f, "(group {})", expr),
            Expr::Unary(op, expr) => write!(f, "({} {})", op.lexeme, expr),
            Expr::Binary(left, op, right) => write!(f, "({} {} {})", op.lexeme, left, right),
        }
    }
}

// Helpers for boxing expressions
impl Expr {
    pub fn new_literal(value: Value) -> Self {
        Self::Literal(value)
    }

    pub fn new_grouping(expr: Expr) -> Self {
        Self::Grouping(Box::new(expr))
    }

    pub fn new_unary(op: Token, expr: Expr) -> Self {
        Self::Unary(op, Box::new(expr))
    }

    pub fn new_binary(expr_left: Expr, op: Token, expr_right: Expr) -> Self {
        Self::Binary(Box::new(expr_left), op, Box::new(expr_right))
    }
}