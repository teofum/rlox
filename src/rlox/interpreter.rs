use crate::rlox::ast::{Expr, Function, Stmt, Value};
use crate::rlox::environment::Environment;
use crate::rlox::error::{ErrorType, Logger, LoxError};
use crate::rlox::externals;
use crate::rlox::token::{Token, TokenType};

#[derive(Eq, PartialEq)]
pub enum RuntimeContext {
    Script,
    Interactive,
}

pub struct Interpreter {
    env: Environment,
    ctx: RuntimeContext,
}

impl Interpreter {
    pub fn new(ctx: RuntimeContext) -> Self {
        let mut interpreter = Self { env: Environment::new(), ctx };
        interpreter.env.define("clock".to_string(), Some(Value::Fun {
            name: "clock".to_string(),
            arity: 0,
            f: Function::External(externals::clock),
        }));

        interpreter
    }

    pub fn interpret(&mut self, stmt_iter: &mut dyn Iterator<Item=Stmt>, logger: &mut Logger) {
        for ref stmt in stmt_iter {
            if let Err(err) = self.execute(stmt) {
                // Unwind the stack
                let mut stack_trace = Vec::new();
                while !self.env.is_global() {
                    self.env = std::mem::take(&mut self.env).enclosing();
                    stack_trace.push("<anonymous block>".to_string());
                }

                logger.log(err.with_stack(stack_trace));
                break;
            }
        }
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<(), LoxError> {
        match stmt {
            Stmt::Expression(expr) => { self.eval(expr)?; }
            Stmt::Print(expr) => {
                let value = self.eval(expr)?;
                println!("{}", value);
            }
            Stmt::Var(identifier, initializer) => {
                let value = match initializer {
                    Some(expr) => Some(self.eval(expr)?),
                    None => None,
                };
                self.env.define(identifier.lexeme.clone(), value);
            }
            Stmt::Fun(identifier, params, body) => {
                let fun = Value::Fun {
                    name: identifier.lexeme.clone(),
                    arity: params.len() as u8,
                    f: Function::define(params.clone(), body.clone()),
                };
                self.env.define(identifier.lexeme.clone(), Some(fun));
            }
            Stmt::Block(statements) => {
                self.env = Environment::from(std::mem::take(&mut self.env));
                for stmt in statements { self.execute(stmt)?; }
                self.env = std::mem::take(&mut self.env).enclosing();
            }
            Stmt::If(expr, if_true, if_false) => {
                if is_truthy(&self.eval(expr)?) {
                    self.execute(if_true)?;
                } else if let Some(if_false) = if_false {
                    self.execute(if_false)?;
                }
            }
            Stmt::While(expr, body) => {
                while is_truthy(&self.eval(expr)?) {
                    self.execute(body.as_ref())?;
                }
            }
        }

        Ok(())
    }

    // TODO: implement reference values
    pub fn eval(&mut self, expr: &Expr) -> Result<Value, LoxError> {
        match expr {
            Expr::Literal(value) => Ok(value.clone()),
            Expr::Grouping(expr) => self.eval(expr),
            Expr::Variable(identifier) => self.env.get(identifier).cloned(),
            Expr::Assignment(identifier, expr) => {
                let value = self.eval(expr)?;
                self.env.assign(identifier.clone(), value).cloned()
            }
            Expr::Unary(op, rhs) => {
                let rhs = self.eval(rhs)?;

                match op.token_type {
                    TokenType::Bang => Ok(Value::Boolean(!is_truthy(&rhs))),
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
                let lhs = self.eval(lhs)?;
                let rhs = self.eval(rhs)?;

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
                        (Value::String(lhs), rhs) => Ok(Value::String(lhs + &rhs.to_string())),
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
            Expr::Logical(lhs, op, rhs) => {
                let lhs = self.eval(lhs)?;

                match &op.token_type {
                    TokenType::Or => if is_truthy(&lhs) { Ok(lhs) } else { self.eval(rhs) },
                    TokenType::And => if !is_truthy(&lhs) { Ok(lhs) } else { self.eval(rhs) },
                    _ => panic!("eval: Binary expression with non-binary operator")
                }
            }
            Expr::Ternary(condition, if_true, if_false) => {
                let condition = self.eval(condition)?;
                self.eval(if is_truthy(&condition) { if_true } else { if_false })
            }
            Expr::Call(callee, paren, args) => {
                let callee = self.eval(callee)?;
                let args = args.iter().map(|arg| self.eval(arg)).collect::<Result<Vec<_>, _>>()?;

                if let Value::Fun { arity, name, f } = callee {
                    if args.len() != arity as usize {
                        let message = format!(
                            "Function {} expected {} arguments, got {}",
                            name, arity, args.len()
                        );
                        Err(LoxError::new(ErrorType::Runtime, paren.line, &message))
                    } else {
                        match f {
                            Function::External(f) => f(self, &args),
                            Function::Lox(f) => {
                                self.env = Environment::from(std::mem::take(&mut self.env));
                                for (param, arg) in f.params.iter().zip(args) {
                                    self.env.define(param.lexeme.clone(), Some(arg));
                                }

                                for stmt in f.body { self.execute(&stmt)?; }
                                self.env = std::mem::take(&mut self.env).enclosing();
                                Ok(Value::Nil)
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
}

fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Nil => false,
        Value::Boolean(b) => *b,
        _ => true,
    }
}

fn typecheck_numbers(values: (Value, Value), op: &Token) -> Result<(f64, f64), LoxError> {
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

fn compare(op: &Token, lhs: Value, rhs: Value) -> Result<Value, LoxError> {
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
