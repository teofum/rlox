use crate::rlox::ast::{Expr, Function, LoxFunction, Stmt, ToValueOrRef, Value, ValueOrRef};
use crate::rlox::environment::{Environment, Heap, Stack};
use crate::rlox::error::{ErrorType, Logger, LoxError};
use crate::rlox::externals::clock;
use crate::rlox::lookups::Lookups;
use crate::rlox::token::{Token, TokenType};
use std::rc::Rc;

pub struct Interpreter {
    stack: Stack,
    heap: Heap,
}

impl Interpreter {
    pub fn new(lookups: &mut Lookups) -> Self {
        let mut stack = Stack::new();
        stack.define(
            lookups.get("clock"),
            Some(Value::Fun(Rc::new(Function::External(clock, "clock".to_string(), 0)))),
        );

        Self { stack, heap: Heap::new() }
    }

    pub fn interpret(&mut self, stmt_iter: &mut dyn Iterator<Item=Stmt>, logger: &mut Logger) {
        for ref stmt in stmt_iter {
            if let Err(err) = self.execute(stmt) {
                // Unwind the stack
                let mut stack_trace = Vec::new();
                while let Some(scope) = self.stack.pop() {
                    stack_trace.push(scope.name);
                }

                logger.log(err.with_stack(stack_trace));
                break;
            }
        }
    }

    pub fn execute(&mut self, stmt: &Stmt) -> Result<Option<Value>, LoxError> {
        let mut return_value = None;
        match stmt {
            Stmt::Expression(expr) => { self.eval(expr)?; }
            Stmt::Print(expr) => {
                let value_ref = self.eval(expr)?;
                let value = self.deref_value(&value_ref)?;
                println!("{}", value);
            }
            Stmt::Block(statements) => {
                self.stack.push("<anonymous block>".to_string());
                for stmt in statements {
                    return_value = self.execute(stmt)?;
                    if return_value.is_some() { break; }
                }
                self.stack.pop();
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
                self.stack.define(*identifier, value);
            }
            Stmt::Fun(var, params, body) => {
                let fun = Value::Fun(Rc::new(Function::Lox(
                    LoxFunction { params: params.clone(), body: body.clone() },
                    var.name.to_string(),
                )));
                self.stack.define(var.symbol, Some(fun));
            }
        }

        Ok(return_value)
    }

    pub fn eval(&mut self, expr: &Expr) -> Result<ValueOrRef, LoxError> {
        match expr {
            Expr::Literal(value) => Ok(value.clone().wrap()),
            Expr::Grouping(expr) => self.eval(expr),
            Expr::Variable(var) => Ok(self.stack.get_ref(var)?),
            Expr::Assignment(var, expr) => {
                let value_ref = self.eval(expr)?;
                let value = self.clone_value(value_ref)?;
                self.stack.assign(var, value)
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
                    .collect::<Result<Vec<_>, _>>()?;

                let callee = match callee {
                    ValueOrRef::StackRef(var) => self.stack.get_value(&var),
                    _ => {
                        let message = format!("{} is not a callable expression", callee);
                        Err(LoxError::new(ErrorType::Runtime, paren.line, &message))
                    }
                }?;

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
                                self.stack.push("function ".to_string() + fun.name());
                                for (param, arg) in f_impl.params.iter().zip(args) {
                                    let arg_value = self.clone_value(arg)?;
                                    self.stack.define(*param, Some(arg_value));
                                }

                                let mut return_value = None;
                                for stmt in &f_impl.body {
                                    return_value = self.execute(stmt)?;
                                    if return_value.is_some() { break; }
                                }
                                self.stack.pop();
                                Ok(return_value.unwrap_or(Value::Nil).wrap())
                            }
                        }
                    }
                } else {
                    let message = format!("{} is not a callable expression", callee);
                    Err(LoxError::new(ErrorType::Runtime, paren.line, &message))
                }
            }
        }
    }

    fn deref_value<'a>(&'a self, value_or_ref: &'a ValueOrRef) -> Result<&'a Value, LoxError> {
        match value_or_ref {
            ValueOrRef::Value(v) => Ok(v),
            ValueOrRef::StackRef(var) => self.stack.get_value(var),
            ValueOrRef::HeapRef(var) => self.heap.get_value(var),
        }
    }

    fn clone_value(&self, value_or_ref: ValueOrRef) -> Result<Value, LoxError> {
        match value_or_ref {
            ValueOrRef::Value(v) => Ok(v),
            ValueOrRef::StackRef(var) => self.stack.copy_value(&var),
            ValueOrRef::HeapRef(var) => self.heap.copy_value(&var),
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

fn typecheck_numbers(values: (&Value, &Value), op: &Token) -> Result<(f64, f64), LoxError> {
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

fn compare(op: &Token, lhs: &Value, rhs: &Value) -> Result<Value, LoxError> {
    fn compare_impl<T>(op: &Token, lhs: T, rhs: T) -> Result<Value, LoxError>
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
