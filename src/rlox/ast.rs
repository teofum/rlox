use crate::rlox::environment::{Environment, VariableKey};
use crate::rlox::error::LoxResult;
use crate::rlox::interpreter::Interpreter;
use crate::rlox::lookups::Symbol;
use crate::rlox::token::Token;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

pub type ExternalFunction = fn(&Interpreter, &Vec<ValueOrRef>) -> LoxResult<Value>;

#[derive(Debug)]
pub struct LoxFunction {
    pub params: Vec<Symbol>,
    pub body: Vec<Stmt>,
    pub closure: Rc<RefCell<Environment>>,
}

impl LoxFunction {
    pub fn arity(&self) -> usize {
        self.params.len()
    }
}

impl PartialEq for LoxFunction {
    fn eq(&self, _: &Self) -> bool { false }
}

#[derive(Debug, PartialEq)]
pub enum Function {
    External(ExternalFunction, String, u8),
    Lox(LoxFunction, String),
}

impl Function {
    pub fn define(
        name: String,
        params: Vec<Symbol>,
        body: Vec<Stmt>,
        closure: Rc<RefCell<Environment>>,
    ) -> Self {
        Self::Lox(LoxFunction { params, body, closure }, name)
    }

    pub fn arity(&self) -> usize {
        match self {
            Function::Lox(f, _) => f.arity(),
            Function::External(_, _, arity) => *arity as usize,
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Function::Lox(_, name) => name,
            Function::External(_, name, _) => name,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Fun(Rc<Function>),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Number(num) => write!(f, "{}", num),
            Value::String(str) => write!(f, "{}", str),
            Value::Fun(fun) => write!(f, "fun {}", fun.name()),
        }
    }
}

pub trait ToValueOrRef {
    type ValueOrRefType;

    fn wrap(self) -> Self::ValueOrRefType;
}

impl ToValueOrRef for Value {
    type ValueOrRefType = ValueOrRef;
    fn wrap(self) -> Self::ValueOrRefType {
        ValueOrRef::Value(self)
    }
}

impl ToValueOrRef for LoxResult<Value> {
    type ValueOrRefType = LoxResult<ValueOrRef>;
    fn wrap(self) -> Self::ValueOrRefType {
        self.map(|v| v.wrap())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    pub symbol: Symbol,
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub enum ValueOrRef {
    Value(Value),
    StackRef(Var),
    HeapRef(VariableKey),
}

impl Display for ValueOrRef {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            ValueOrRef::Value(v) => write!(f, "{}", v),
            ValueOrRef::StackRef(v) => write!(f, "ref {:?}", v),
            ValueOrRef::HeapRef(v) => write!(f, "heap ref {:?}", v),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Value),
    Grouping(Box<Expr>),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    Variable(Var, Option<usize>),
    Assignment(Var, Box<Expr>, Option<usize>),
    Logical(Box<Expr>, Token, Box<Expr>),
    Call(Box<Expr>, Token, Vec<Expr>),
    Lambda(Vec<Symbol>, Vec<Stmt>),
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

    pub fn new_assignment(identifier: Var, expr: Expr) -> Self {
        Self::Assignment(identifier, Box::new(expr), None)
    }

    pub fn new_logical(expr_left: Expr, op: Token, expr_right: Expr) -> Self {
        Self::Logical(Box::new(expr_left), op, Box::new(expr_right))
    }

    pub fn new_call(callee: Expr, paren: Token, args: Vec<Expr>) -> Self {
        Self::Call(Box::new(callee), paren, args)
    }

    // TODO get expr line
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Expression(Expr),
    Print(Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    Return(Expr),
    Var(Symbol, Option<Expr>),
    Fun(Var, Vec<Symbol>, Vec<Stmt>),
}

impl Stmt {
    pub fn new_if(expr: Expr, if_branch: Stmt, else_branch: Option<Stmt>) -> Self {
        Stmt::If(expr, Box::new(if_branch), else_branch.map(Box::new))
    }

    pub fn new_while(expr: Expr, body: Stmt) -> Self {
        Stmt::While(expr, Box::new(body))
    }
}
