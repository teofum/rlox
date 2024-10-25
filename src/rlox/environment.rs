use std::collections::hash_map::Entry;
use crate::rlox::ast::{Value, ValueOrRef};
use crate::rlox::error::{ErrorType, LoxError};
use crate::rlox::lookups::Symbol;
use std::collections::HashMap;

type VariableMap = HashMap<Symbol, Option<Value>>;

pub trait Environment<T> {
    fn define(&mut self, name: Symbol, value: T);
    fn assign(&mut self, identifier: Symbol, value: Value) -> Result<ValueOrRef, LoxError>;
    fn get_ref(&self, identifier: Symbol) -> Result<ValueOrRef, LoxError>;
    fn get_value(&self, identifier: Symbol) -> Result<&Value, LoxError>;
    fn get_value_mut(&mut self, identifier: Symbol) -> Result<&mut Value, LoxError>;

    fn copy_value(&self, identifier: Symbol) -> Result<Value, LoxError> {
        self.get_value(identifier).cloned()
    }
}

#[derive(Default, Debug)]
pub struct Stack {
    vars: Vec<VariableMap>,
}

impl Stack {
    pub fn new() -> Self {
        Self { vars: Vec::from([HashMap::new()]) }
    }

    pub fn push(&mut self) {
        self.vars.push(HashMap::new());
    }

    pub fn pop(&mut self) -> Option<VariableMap> {
        if self.vars.len() > 1 { self.vars.pop() } else { None }
    }

    fn get_error(line: usize, var_name: &str, problem: &str) -> LoxError {
        let message = format!("Variable \"{}\" is {}", var_name, problem);
        LoxError::new(ErrorType::Runtime, line, &message)
    }
}

impl Environment<Option<Value>> for Stack {
    fn define(&mut self, name: Symbol, value: Option<Value>) {
        self.vars.last_mut().unwrap().insert(name, value);
    }

    fn assign(&mut self, identifier: Symbol, value: Value) -> Result<ValueOrRef, LoxError> {
        for scope in self.vars.iter_mut().rev() {
            if let Entry::Occupied(mut e) = scope.entry(identifier) {
                e.insert(Some(value));
                return Ok(ValueOrRef::StackRef(identifier));
            }
        }

        // TODO variable name and line
        Err(Self::get_error(0, "<var>", "undefined"))
    }

    fn get_ref(&self, identifier: Symbol) -> Result<ValueOrRef, LoxError> {
        for scope in self.vars.iter().rev() {
            if scope.contains_key(&identifier) {
                return Ok(ValueOrRef::StackRef(identifier));
            }
        }

        Err(Self::get_error(0, "<var>", "undefined"))
    }

    fn get_value(&self, identifier: Symbol) -> Result<&Value, LoxError> {
        for scope in self.vars.iter().rev() {
            if let Some(value) = scope.get(&identifier) {
                return value.as_ref().ok_or_else(|| Self::get_error(0, "<var>", "uninitialized"));
            }
        }

        Err(Self::get_error(0, "<var>", "undefined"))
    }

    fn get_value_mut(&mut self, identifier: Symbol) -> Result<&mut Value, LoxError> {
        for scope in self.vars.iter_mut().rev() {
            if let Some(value) = scope.get_mut(&identifier) {
                return value.as_mut().ok_or_else(|| Self::get_error(0, "<var>", "uninitialized"));
            }
        }

        Err(Self::get_error(0, "<var>", "undefined"))
    }
}


#[derive(Default, Debug)]
pub struct Heap {
    vars: HashMap<Symbol, Value>,
}

impl Heap {
    pub fn new() -> Self {
        Self { vars: HashMap::new() }
    }

    fn get_error(line: usize, var_name: &str, problem: &str) -> LoxError {
        let message = format!("Variable \"{}\" is {}", var_name, problem);
        LoxError::new(ErrorType::Runtime, line, &message)
    }
}

impl Environment<Value> for Heap {
    fn define(&mut self, name: Symbol, value: Value) {
        self.vars.insert(name, value);
    }

    fn assign(&mut self, identifier: Symbol, value: Value) -> Result<ValueOrRef, LoxError> {
        if let Entry::Occupied(mut e) = self.vars.entry(identifier) {
            e.insert(value);
            return Ok(ValueOrRef::HeapRef(identifier));
        }

        // TODO variable name and line
        Err(Self::get_error(0, "<var>", "undefined"))
    }

    fn get_ref(&self, identifier: Symbol) -> Result<ValueOrRef, LoxError> {
        if self.vars.contains_key(&identifier) {
            Ok(ValueOrRef::HeapRef(identifier))
        } else {
            Err(Self::get_error(0, "<var>", "undefined"))
        }
    }

    fn get_value(&self, identifier: Symbol) -> Result<&Value, LoxError> {
        self.vars.get(&identifier).ok_or_else(|| Self::get_error(0, "<var>", "undefined"))
    }

    fn get_value_mut(&mut self, identifier: Symbol) -> Result<&mut Value, LoxError> {
        self.vars.get_mut(&identifier).ok_or_else(|| Self::get_error(0, "<var>", "undefined"))
    }
}
