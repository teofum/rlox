use crate::rlox::ast::Value;
use crate::rlox::error::{ErrorType, LoxError};
use crate::rlox::token::Token;
use std::collections::HashMap;

#[derive(Default)]
pub struct Environment {
    enclosing: Option<Box<Environment>>,
    vars: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Self { vars: HashMap::new(), enclosing: None }
    }

    pub fn from(enclosing: Environment) -> Self {
        Self { vars: HashMap::new(), enclosing: Some(Box::new(enclosing)) }
    }

    pub fn enclosing(self) -> Self {
        *self.enclosing.expect("Environment: attempted to drop the global env!")
    }

    pub fn define(&mut self, name: Token, value: Value) {
        // If we had warnings, we should at least warn on redefinition
        self.vars.insert(name.lexeme, value);
    }

    pub fn get(&self, name: &Token) -> Result<&Value, LoxError> {
        if let Some(value) = self.vars.get(&name.lexeme) {
            Ok(value)
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.get(name)
        } else {
            let message = format!("Variable \"{}\" is undefined", name.lexeme);
            Err(LoxError::new(ErrorType::Runtime, name.line, &message))
        }
    }

    #[allow(clippy::map_entry)]
    pub fn assign(&mut self, name: Token, value: Value) -> Result<Value, LoxError> {
        if self.vars.contains_key(&name.lexeme) {
            self.vars.insert(name.lexeme, value.clone());
            Ok(value)
        } else if let Some(enclosing) = &mut self.enclosing {
            enclosing.assign(name, value)
        } else {
            let message = format!("Variable \"{}\" is undefined", name.lexeme);
            Err(LoxError::new(ErrorType::Runtime, name.line, &message))
        }
    }
}