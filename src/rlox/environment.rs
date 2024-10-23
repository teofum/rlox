use crate::rlox::ast::Value;
use crate::rlox::error::{ErrorType, LoxError};
use crate::rlox::token::Token;
use std::collections::hash_map::Entry;
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

    pub fn is_global(&self) -> bool {
        self.enclosing.is_none()
    }

    pub fn enclosing(self) -> Self {
        *self.enclosing.expect("Environment: attempted to drop the global env!")
    }

    pub fn define(&mut self, name: Token, value: Value) {
        // If we had warnings, we should at least warn on redefinition
        self.vars.insert(name.lexeme, value);
    }

    pub fn get(&mut self, name: &Token) -> Result<&mut Value, LoxError> {
        if let Some(value) = self.vars.get_mut(&name.lexeme) {
            Ok(value)
        } else if let Some(enclosing) = &mut self.enclosing {
            enclosing.get(name)
        } else {
            let message = format!("Variable \"{}\" is undefined", name.lexeme);
            Err(LoxError::new(ErrorType::Runtime, name.line, &message))
        }
    }

    pub fn assign(&mut self, name: Token, value: Value) -> Result<&mut Value, LoxError> {
        if let Entry::Occupied(mut e) = self.vars.entry(name.lexeme.clone()) {
            e.insert(value);
            self.get(&name)
        } else if let Some(enclosing) = &mut self.enclosing {
            enclosing.assign(name, value)
        } else {
            let message = format!("Variable \"{}\" is undefined", name.lexeme);
            Err(LoxError::new(ErrorType::Runtime, name.line, &message))
        }
    }
}