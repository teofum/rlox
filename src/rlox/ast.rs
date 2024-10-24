use crate::rlox::interpreter::Interpreter;
use crate::rlox::token::Token;
use std::fmt::{Display, Formatter};
use crate::rlox::error::LoxError;

type LoxFunction = fn(&mut Interpreter, &Vec<Value>) -> Result<Value, LoxError>;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Fun { arity: u8, name: String, f: LoxFunction },
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Number(num) => write!(f, "{}", num),
            Value::String(str) => write!(f, "{}", str),
            Value::Fun { arity, name, f: _ } => write!(f, "fun {}({} params)", name, arity),
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
    Variable(Token),
    Assignment(Token, Box<Expr>),
    Logical(Box<Expr>, Token, Box<Expr>),
    Call(Box<Expr>, Token, Vec<Expr>),
}

// Helpers for boxing expressions
impl Expr {
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

    pub fn new_assignment(identifier: Token, expr: Expr) -> Self {
        Self::Assignment(identifier, Box::new(expr))
    }

    pub fn new_logical(expr_left: Expr, op: Token, expr_right: Expr) -> Self {
        Self::Logical(Box::new(expr_left), op, Box::new(expr_right))
    }

    pub fn new_call(callee: Expr, paren: Token, args: Vec<Expr>) -> Self {
        Self::Call(Box::new(callee), paren, args)
    }

    // TODO get expr line
}

#[derive(Debug)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Expression(Expr),
    Print(Expr),
    Var(Token, Option<Expr>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
}

impl Stmt {
    pub fn new_if(expr: Expr, if_branch: Stmt, else_branch: Option<Stmt>) -> Self {
        Stmt::If(expr, Box::new(if_branch), else_branch.map(|stmt| Box::new(stmt)))
    }

    pub fn new_while(expr: Expr, body: Stmt) -> Self {
        Stmt::While(expr, Box::new(body))
    }
}
