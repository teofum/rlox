use crate::rlox::ast::{Expr, Function, LoxFunction, Stmt, ToValueOrRef, Value, ValueOrRef, Var};
use crate::rlox::environment::{Env, Environment, Heap};
use crate::rlox::error::{ErrorType, Logger, LoxError, LoxResult};
use crate::rlox::externals::clock;
use crate::rlox::lookups::{Lookups, Symbol};
use crate::rlox::token::{Token, TokenType};
use std::cell::RefCell;
use std::rc::Rc;

pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
    globals: Rc<RefCell<Environment>>,
    heap: Heap,
}

impl Interpreter {
    pub fn new(lookups: &mut Lookups) -> Self {
        let mut heap = Heap::new();
        let mut env = Environment::new("<global scope>".to_string());

        let builtin_clock = heap.define(Value::Fun(Rc::new(Function::External(clock, "clock".to_string(), 0))));
        env.define(lookups.get("clock"), Some(builtin_clock));

        Self { globals: env.clone(), env, heap }
    }

    pub fn interpret(&mut self, stmt_iter: &mut dyn Iterator<Item=Stmt>, logger: &mut Logger) {
        for ref stmt in stmt_iter {
            if let Err(err) = self.execute(stmt) {
                logger.log(err);
                break;
            }
        }
    }

    fn execute(&mut self, stmt: &Stmt) -> LoxResult<Option<Value>> {
        let mut return_value = None;
        match stmt {
            Stmt::Expression(expr) => { self.eval(expr)?; }
            Stmt::Print(expr) => {
                let value_ref = self.eval(expr)?;
                let value = self.deref_value(&value_ref)?;
                println!("{}", value);
            }
            Stmt::Block(statements) => {
                let previous_env = self.env.clone();
                self.env = Environment::from("<anonymous block>".to_string(), self.env.clone());

                for stmt in statements {
                    return_value = self.execute(stmt)?;
                    if return_value.is_some() { break; }
                }

                self.env = previous_env;
            }
            Stmt::If(expr, if_true, if_false) => {
                let value_ref = self.eval(expr)?;
                let truth_value = is_truthy(self.deref_value(&value_ref)?);

                if truth_value {
                    return_value = self.execute(if_true)?;
                } else if let Some(if_false) = if_false {
                    return_value = self.execute(if_false)?;
                }
            }
            Stmt::While(expr, body) => {
                while {
                    let value_ref = self.eval(expr)?;
                    is_truthy(self.deref_value(&value_ref)?)
                } && return_value.is_none() {
                    return_value = self.execute(body.as_ref())?;
                }
            }
            Stmt::Return(expr) => {
                let value_ref = self.eval(expr)?;
                let value = self.clone_value(value_ref)?;
                return_value = Some(value);
            }
            Stmt::Var(identifier, initializer) => {
                let value = match initializer {
                    Some(expr) => {
                        let value_ref = self.eval(expr)?;
                        Some(self.clone_value(value_ref)?)
                    }
                    None => None,
                };
                self.define(*identifier, value);
            }
            Stmt::Fun(var, params, body) => {
                let fun = Value::Fun(Rc::new(Function::Lox(
                    LoxFunction {
                        params: params.clone(),
                        body: body.clone(),
                        closure: self.env.clone(),
                    },
                    var.name.to_string(),
                )));
                self.define(var.symbol, Some(fun));
            }
        }

        Ok(return_value)
    }

    fn eval(&mut self, expr: &Expr) -> LoxResult<ValueOrRef> {
        match expr {
            Expr::Literal(value) => Ok(value.clone().wrap()),
            Expr::Grouping(expr) => self.eval(expr),
            Expr::Variable(var, depth) => self.get_variable(var, depth),
            Expr::Assignment(var, expr, depth) => {
                let value_ref = self.eval(expr)?;
                let value = self.clone_value(value_ref)?;
                self.assign(var, depth, value)
            }
            Expr::Unary(op, rhs) => {
                let rhs_ref = self.eval(rhs)?;
                let rhs = self.deref_value(&rhs_ref)?;

                match op.token_type {
                    TokenType::Bang => Ok(Value::Boolean(!is_truthy(rhs)).wrap()),
                    TokenType::Minus => {
                        match rhs {
                            Value::Number(x) => Ok(Value::Number(-x).wrap()),
                            value => {
                                let message = format!(
                                    "Operator {} expected Number, got {:?}",
                                    op.lexeme, value,
                                );
                                Err(LoxError::new(ErrorType::Type, op.line, &message))
                            }
                        }
                    }
                    _ => panic!("eval: Unary expression with non-unary operator")
                }
            }
            Expr::Binary(lhs, op, rhs) => {
                let (lhs_ref, rhs_ref) = (self.eval(lhs)?, self.eval(rhs)?);
                if op.token_type == TokenType::Comma { return Ok(rhs_ref); }

                let (lhs, rhs) = (self.deref_value(&lhs_ref)?, self.deref_value(&rhs_ref)?);

                match &op.token_type {
                    TokenType::BangEqual => Ok(Value::Boolean(lhs != rhs).wrap()),
                    TokenType::EqualEqual => Ok(Value::Boolean(lhs == rhs).wrap()),
                    TokenType::Minus => typecheck_numbers((lhs, rhs), op)
                        .map(|(lhs, rhs)| Value::Number(lhs - rhs)).wrap(),
                    TokenType::Slash => typecheck_numbers((lhs, rhs), op)
                        .map(|(lhs, rhs)| Value::Number(lhs / rhs)).wrap(),
                    TokenType::Star => typecheck_numbers((lhs, rhs), op)
                        .map(|(lhs, rhs)| Value::Number(lhs * rhs)).wrap(),
                    TokenType::Plus => match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs + rhs).wrap()),
                        (Value::String(lhs), rhs) => Ok(Value::String(lhs.to_string() + &rhs.to_string()).wrap()),
                        (lhs, rhs) => {
                            let message = format!(
                                "Operator {} invalid operand combination {:?} and {:?}",
                                op.lexeme, lhs, rhs,
                            );
                            Err(LoxError::new(ErrorType::Type, op.line, &message))
                        }
                    }
                    comp if TokenType::is_comparison_op(comp) => compare(op, lhs, rhs).wrap(),
                    _ => panic!("eval: Binary expression with non-binary operator")
                }
            }
            Expr::Logical(lhs, op, rhs) => {
                let lhs = self.eval(lhs)?;
                let truth_value = is_truthy(self.deref_value(&lhs)?);

                match &op.token_type {
                    TokenType::Or => if truth_value { Ok(lhs) } else { self.eval(rhs) },
                    TokenType::And => if !truth_value { Ok(lhs) } else { self.eval(rhs) },
                    _ => panic!("eval: Binary expression with non-binary operator")
                }
            }
            Expr::Ternary(condition, if_true, if_false) => {
                let cond_value_ref = self.eval(condition)?;
                let cond_value = is_truthy(self.deref_value(&cond_value_ref)?);
                self.eval(if cond_value { if_true } else { if_false })
            }
            Expr::Call(callee, paren, args) => {
                let callee = self.eval(callee)?;
                let args = args.iter()
                    .map(|arg| self.eval(arg))
                    .collect::<LoxResult<Vec<_>>>()?;

                let callee = self.deref_value(&callee)?;

                if let Value::Fun(fun) = callee {
                    let fun = fun.clone();
                    let fun = fun.as_ref();
                    if args.len() != fun.arity() {
                        let message = format!(
                            "Function {} expected {} arguments, got {}",
                            fun.name(), fun.arity(), args.len()
                        );
                        Err(LoxError::new(ErrorType::Runtime, paren.line, &message))
                    } else {
                        match fun {
                            Function::External(f_impl, _, _) => f_impl(self, &args).wrap(),
                            Function::Lox(f_impl, _) => {
                                let previous_env = self.env.clone();

                                self.env = Environment::from("function ".to_string() + fun.name(), f_impl.closure.clone());
                                for (param, arg) in f_impl.params.iter().zip(args) {
                                    let arg_value = self.clone_value(arg)?;
                                    self.define(*param, Some(arg_value));
                                }

                                let mut return_value = None;
                                for stmt in &f_impl.body {
                                    return_value = self.execute(stmt)?;
                                    if return_value.is_some() { break; }
                                }

                                self.env = previous_env;
                                Ok(return_value.unwrap_or(Value::Nil).wrap())
                            }
                        }
                    }
                } else {
                    let message = format!("{} is not a callable expression", callee);
                    Err(LoxError::new(ErrorType::Runtime, paren.line, &message))
                }
            }
            Expr::Lambda(params, body) => {
                let fun = Value::Fun(Rc::new(Function::Lox(
                    LoxFunction {
                        params: params.clone(),
                        body: body.clone(),
                        closure: self.env.clone(),
                    },
                    "<anonymous function>".to_string(),
                )));
                Ok(ValueOrRef::Value(fun))
            }
        }
    }

    fn deref_value<'a>(&'a self, value_or_ref: &'a ValueOrRef) -> LoxResult<&'a Value> {
        match value_or_ref {
            ValueOrRef::Value(v) => Ok(v),
            ValueOrRef::StackRef(var) => {
                let key = self.env.get(var)?;
                Ok(self.heap.get(key))
            }
            ValueOrRef::HeapRef(key) => Ok(self.heap.get(*key)),
        }
    }

    fn clone_value(&self, value_or_ref: ValueOrRef) -> LoxResult<Value> {
        match value_or_ref {
            ValueOrRef::Value(v) => Ok(v),
            ValueOrRef::StackRef(var) => {
                let key = self.env.get(&var)?;
                Ok(self.heap.get(key).clone())
            }
            ValueOrRef::HeapRef(key) => Ok(self.heap.get(key).clone()),
        }
    }

    fn define(&mut self, identifier: Symbol, value: Option<Value>) {
        let key = value.map(|value| self.heap.define(value));
        self.env.define(identifier, key);
    }

    fn assign(&mut self, var: &Var, depth: &Option<usize>, value: Value) -> LoxResult<ValueOrRef> {
        let key = if let Some(depth) = depth {
            self.env.get_at(var, *depth)?
        } else {
            self.globals.get(var)?
        };
        
        self.heap.assign(key, value);
        
        if let Some(depth) = depth {
            self.env.assign_at(var, key, *depth)
        } else {
            self.globals.assign(var, key)
        }
    }

    fn get_variable(&self, var: &Var, depth: &Option<usize>) -> LoxResult<ValueOrRef> {
        if let Some(depth) = depth {
            Ok(ValueOrRef::HeapRef(self.env.get_at(var, *depth)?))
        } else {
            Ok(ValueOrRef::HeapRef(self.globals.get(var)?))
        }
    }
}

fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Nil => false,
        Value::Boolean(b) => *b,
        _ => true,
    }
}

fn typecheck_numbers(values: (&Value, &Value), op: &Token) -> LoxResult<(f64, f64)> {
    if let (Value::Number(lhs), Value::Number(rhs)) = values {
        Ok((*lhs, *rhs))
    } else {
        let message = format!(
            "Operator {} expected Number and Number, got {:?} and {:?}",
            op.lexeme, values.0, values.1,
        );
        Err(LoxError::new(ErrorType::Type, op.line, &message))
    }
}

fn compare(op: &Token, lhs: &Value, rhs: &Value) -> LoxResult<Value> {
    fn compare_impl<T>(op: &Token, lhs: T, rhs: T) -> LoxResult<Value>
    where
        T: PartialOrd,
    {
        match op.token_type {
            TokenType::Greater => Ok(Value::Boolean(lhs > rhs)),
            TokenType::GreaterEqual => Ok(Value::Boolean(lhs >= rhs)),
            TokenType::Less => Ok(Value::Boolean(lhs < rhs)),
            TokenType::LessEqual => Ok(Value::Boolean(lhs <= rhs)),
            _ => panic!("eval/compare: Not a comparison operator")
        }
    }

    match (lhs, rhs) {
        (Value::Number(lhs), Value::Number(rhs)) => compare_impl(op, lhs, rhs),
        (Value::String(lhs), Value::String(rhs)) => compare_impl(op, lhs, rhs),
        (lhs, rhs) => {
            let message = format!(
                "Operator {} expected Number or String, got {:?} and {:?}",
                op.lexeme, lhs, rhs,
            );
            Err(LoxError::new(ErrorType::Type, op.line, &message))
        }
    }
}
