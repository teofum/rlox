use crate::rlox::ast::Stmt;
use crate::rlox::error::{Logger, LoxError};
use crate::rlox::eval::eval;

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Self {
        Self {}
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
            Stmt::Expression(expr) => { eval(expr)?; }
            Stmt::Print(expr) => {
                let value = eval(expr)?;
                println!("{}", value);
            }
        }

        Ok(())
    }
}