use crate::rlox::environment::{Environment, VariableKey};
use crate::rlox::error::{ErrorType, LoxError, LoxResult};
use crate::rlox::interpreter::Interpreter;
use crate::rlox::lookups::Symbol;
use crate::rlox::token::Token;
use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

pub type ExternalFunctionImpl = fn(&Interpreter, &Vec<ValueOrRef>) -> LoxResult<Value>;

#[derive(Debug, PartialEq)]
pub struct ExternalFunction {
    name: String,
    arity: u8,
    fun: ExternalFunctionImpl,
}

impl ExternalFunction {
    pub fn new(name: &str, arity: u8, fun: ExternalFunctionImpl) -> Self {
        Self { name: name.to_string(), arity, fun }
    }
}

#[derive(Debug)]
pub struct LoxFunction {
    pub name: String,
    pub params: Vec<Symbol>,
    pub body: Vec<Stmt>,
    pub closure: Rc<RefCell<Environment>>,
}

impl PartialEq for LoxFunction {
    fn eq(&self, _: &Self) -> bool { false }
}

#[derive(Debug, PartialEq)]
pub struct Class {
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub enum Function {
    Lox(LoxFunction),
    Constructor(Rc<Class>, LoxFunction),
    External(ExternalFunction),
}

impl Function {
    pub fn define_fun(
        name: String,
        params: Vec<Symbol>,
        body: Vec<Stmt>,
        closure: Rc<RefCell<Environment>>,
    ) -> Self {
        Self::Lox(LoxFunction { name, params, body, closure })
    }

    pub fn define_ctor(
        class: Class,
        name: String,
        params: Vec<Symbol>,
        body: Vec<Stmt>,
        closure: Rc<RefCell<Environment>>,
    ) -> Self {
        Self::Constructor(Rc::new(class), LoxFunction { name, params, body, closure })
    }

    pub fn arity(&self) -> usize {
        match self {
            Function::Lox(f) => f.params.len(),
            Function::Constructor(_, f) => f.params.len(),
            Function::External(f) => f.arity as usize,
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Function::Lox(f) => &f.name,
            Function::Constructor(_, f) => &f.name,
            Function::External(f) => &f.name,
        }
    }

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<ValueOrRef>,
        line: usize,
    ) -> LoxResult<Value> {
        if args.len() == self.arity() {
            match self {
                Function::Lox(fun) => interpreter.call_fun(fun, args),
                Function::Constructor(class, ctor) => interpreter.create_object(class, ctor, args),
                Function::External(ExternalFunction { fun, .. }) => fun(interpreter, &args),
            }
        } else {
            let message = format!(
                "{} expected {} arguments, got {}",
                self, self.arity(), args.len()
            );
            Err(LoxError::new(ErrorType::Runtime, line, &message))
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Lox(fun) => write!(f, "fun {}", fun.name),
            Function::Constructor(class, _) => write!(f, "ctor {}", class.name),
            Function::External(fun) => write!(f, "(extern) fun {}", fun.name),
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
    Object(Rc<Class>),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Number(num) => write!(f, "{}", num),
            Value::String(str) => write!(f, "{}", str),
            Value::Fun(fun) => write!(f, "{}", fun),
            Value::Object(class) => write!(f, "{}", class.name),
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
    Class(Var, Vec<Stmt>),
}

impl Stmt {
    pub fn new_if(expr: Expr, if_branch: Stmt, else_branch: Option<Stmt>) -> Self {
        Stmt::If(expr, Box::new(if_branch), else_branch.map(Box::new))
    }

    pub fn new_while(expr: Expr, body: Stmt) -> Self {
        Stmt::While(expr, Box::new(body))
    }
}
