use crate::rlox::ast::{Expr, Value};
use crate::rlox::error::{ErrorType, LoxError};
use crate::rlox::token::{Token, TokenType};

pub fn eval(expr: Expr) -> Result<Value, LoxError> {
    match expr {
        Expr::Literal(value) => Ok(value),
        Expr::Grouping(expr) => eval(*expr),
        Expr::Unary(op, rhs) => {
            let rhs = eval(*rhs)?;

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
                            Err(LoxError::new(ErrorType::TypeError, op.line, &message))
                        }
                    }
                }
                _ => panic!("eval: Unary expression with non-unary operator")
            }
        }
        Expr::Binary(lhs, op, rhs) => {
            let lhs = eval(*lhs)?;
            let rhs = eval(*rhs)?;

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
                    (Value::String(lhs), Value::String(rhs)) => Ok(Value::String(lhs + &rhs)),
                    (lhs, rhs) => {
                        let message = format!(
                            "Operator {} expected Number or String, got {:?} and {:?}",
                            op.lexeme, lhs, rhs,
                        );
                        Err(LoxError::new(ErrorType::TypeError, op.line, &message))
                    }
                }
                comp if TokenType::is_comparison_op(&comp) => compare(op, lhs, rhs),
                _ => panic!("eval: Binary expression with non-binary operator")
            }
        }
        Expr::Ternary(condition, if_true, if_false) => {
            eval(if is_truthy(eval(*condition)?) { *if_true } else { *if_false })
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
        Err(LoxError::new(ErrorType::TypeError, op.line, &message))
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
            Err(LoxError::new(ErrorType::TypeError, op.line, &message))
        }
    }
}
