use crate::rlox::ast::{Expr, Stmt, Value};
use crate::rlox::environment::Environment;
use crate::rlox::error::{ErrorType, Logger, LoxError};
use crate::rlox::token::{Token, TokenType};

#[derive(Default)]
pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self { env: Environment::new() }
    }

    pub fn interpret(&mut self, stmt_iter: &mut dyn Iterator<Item=Stmt>, logger: &mut Logger) {
        for stmt in stmt_iter {
            if let Err(err) = self.execute(stmt) {
                logger.log(err);
                break;
            }
        }
    }

    fn execute(&mut self, stmt: Stmt) -> Result<(), LoxError> {
        match stmt {
            Stmt::Expression(expr) => { self.eval(expr)?; }
            Stmt::Print(expr) => {
                let value = self.eval(expr)?;
                println!("{}", value);
            }
            Stmt::Var(identifier, initializer) => {
                let value = match initializer {
                    Some(expr) => self.eval(expr)?,
                    None => Value::Nil,
                };
                self.env.define(identifier, value);
            }
            Stmt::Block(statements) => {
                self.env = Environment::from(std::mem::take(&mut self.env));

                for stmt in statements {
                    self.execute(stmt)?;
                }

                self.env = std::mem::take(&mut self.env).enclosing();
            }
        }

        Ok(())
    }

    pub fn eval(&mut self, expr: Expr) -> Result<Value, LoxError> {
        match expr {
            Expr::Literal(value) => Ok(value),
            Expr::Grouping(expr) => self.eval(*expr),
            Expr::Variable(identifier) => self.env.get(&identifier).cloned(),
            Expr::Assignment(identifier, expr) => {
                let value = self.eval(*expr)?;
                self.env.assign(identifier, value)
            }
            Expr::Unary(op, rhs) => {
                let rhs = self.eval(*rhs)?;

                match op.token_type {
                    TokenType::Bang => Ok(Value::Boolean(!is_truthy(rhs))),
                    TokenType::Minus => {
                        match rhs {
                            Value::Number(x) => Ok(Value::Number(-x)),
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
                let lhs = self.eval(*lhs)?;
                let rhs = self.eval(*rhs)?;

                match &op.token_type {
                    TokenType::Comma => Ok(rhs),
                    TokenType::BangEqual => Ok(Value::Boolean(lhs != rhs)),
                    TokenType::EqualEqual => Ok(Value::Boolean(lhs == rhs)),
                    TokenType::Minus => typecheck_numbers((lhs, rhs), op)
                        .map(|(lhs, rhs)| Value::Number(lhs - rhs)),
                    TokenType::Slash => typecheck_numbers((lhs, rhs), op)
                        .map(|(lhs, rhs)| Value::Number(lhs / rhs)),
                    TokenType::Star => typecheck_numbers((lhs, rhs), op)
                        .map(|(lhs, rhs)| Value::Number(lhs * rhs)),
                    TokenType::Plus => match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs + rhs)),
                        (Value::String(lhs), rhs) => Ok(Value::String(lhs + &to_string(rhs))),
                        (lhs, rhs) => {
                            let message = format!(
                                "Operator {} invalid operand combination {:?} and {:?}",
                                op.lexeme, lhs, rhs,
                            );
                            Err(LoxError::new(ErrorType::Type, op.line, &message))
                        }
                    }
                    comp if TokenType::is_comparison_op(comp) => compare(op, lhs, rhs),
                    _ => panic!("eval: Binary expression with non-binary operator")
                }
            }
            Expr::Ternary(condition, if_true, if_false) => {
                let condition = self.eval(*condition)?;
                self.eval(if is_truthy(condition) { *if_true } else { *if_false })
            }
        }
    }
}

fn is_truthy(value: Value) -> bool {
    match value {
        Value::Nil => false,
        Value::Boolean(b) => b,
        _ => true,
    }
}

fn typecheck_numbers(values: (Value, Value), op: Token) -> Result<(f64, f64), LoxError> {
    if let (Value::Number(lhs), Value::Number(rhs)) = values {
        Ok((lhs, rhs))
    } else {
        let message = format!(
            "Operator {} expected Number and Number, got {:?} and {:?}",
            op.lexeme, values.0, values.1,
        );
        Err(LoxError::new(ErrorType::Type, op.line, &message))
    }
}

fn compare(op: Token, lhs: Value, rhs: Value) -> Result<Value, LoxError> {
    fn compare_impl<T>(op: Token, lhs: T, rhs: T) -> Result<Value, LoxError>
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

fn to_string(value: Value) -> String {
    match value {
        Value::Nil => "nil".to_string(),
        Value::Boolean(b) => b.to_string(),
        Value::Number(num) => num.to_string(),
        Value::String(str) => str,
    }
}
