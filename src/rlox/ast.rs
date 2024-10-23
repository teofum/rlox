use crate::rlox::token::Token;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{}", b),
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
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    Var(Token),
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(value) => write!(f, "{}", value),
            Expr::Grouping(expr) => write!(f, "(group {})", expr),
            Expr::Unary(op, expr) => write!(f, "({} {})", op.lexeme, expr),
            Expr::Binary(left, op, right) => write!(f, "({} {} {})", op.lexeme, left, right),
            Expr::Ternary(cond, if_true, if_false) => write!(f, "(? {} {} {})", cond, if_true, if_false),
            Expr::Var(identifier) => write!(f, "(var {})", identifier),
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

    pub fn new_ternary(expr_cond: Expr, expr_true: Expr, expr_false: Expr) -> Self {
        Self::Ternary(Box::new(expr_cond), Box::new(expr_true), Box::new(expr_false))
    }

    // TODO get expr line
}

#[derive(Debug)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var(Token, Option<Expr>),
}
