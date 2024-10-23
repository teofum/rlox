use crate::rlox::ast::Value;
use crate::rlox::error::{ErrorType, LoxError};
use crate::rlox::token::Token;
use std::collections::HashMap;

pub struct Environment {
    vars: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Self { vars: HashMap::new() }
    }

    pub fn define(&mut self, name: String, value: Value) {
        // If we had warnings, we should at least warn on redefinition
        self.vars.insert(name, value);
    }

    pub fn get(&self, name: Token) -> Result<Value, LoxError> {
        self.vars.get(&name.lexeme).ok_or_else(|| {
            let message = format!("Variable \"{}\" is undefined", name.lexeme);
            LoxError::new(ErrorType::Runtime, name.line, &message)
        }).cloned()
    }
}