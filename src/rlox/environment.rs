use std::collections::hash_map::Entry;
use crate::rlox::ast::{Value, ValueOrRef, Var};
use crate::rlox::error::{ErrorType, LoxError};
use crate::rlox::lookups::Symbol;
use std::collections::HashMap;
use std::iter::Map;

type VariableMap = HashMap<Symbol, Option<Value>>;

pub trait Environment<T> {
    fn define(&mut self, name: Symbol, value: T);
    fn assign(&mut self, var: &Var, value: Value) -> Result<ValueOrRef, LoxError>;
    fn get_ref(&self, var: &Var) -> Result<ValueOrRef, LoxError>;
    fn get_value(&self, var: &Var) -> Result<&Value, LoxError>;
    fn get_value_mut(&mut self, var: &Var) -> Result<&mut Value, LoxError>;

    fn copy_value(&self, var: &Var) -> Result<Value, LoxError> {
        self.get_value(var).cloned()
    }
}

#[derive(Debug)]
pub struct Scope {
    pub name: String,
    pub vars: VariableMap,
}

#[derive(Default, Debug)]
pub struct Stack {
    vars: Vec<Scope>,
}

impl Stack {
    pub fn new() -> Self {
        let global_scope = Scope {
            name: String::from("<global scope>"),
            vars: HashMap::new(),
        };
        Self { vars: Vec::from([global_scope]) }
    }

    pub fn push(&mut self, name: String) {
        self.vars.push(Scope { name, vars: HashMap::new() });
    }

    pub fn pop(&mut self) -> Option<Scope> {
        if self.vars.len() > 1 { self.vars.pop() } else { None }
    }

    fn get_error(line: usize, var_name: &str, problem: &str) -> LoxError {
        let message = format!("Variable \"{}\" is {}", var_name, problem);
        LoxError::new(ErrorType::Runtime, line, &message)
    }
}

impl Environment<Option<Value>> for Stack {
    fn define(&mut self, name: Symbol, value: Option<Value>) {
        if let Some(Scope { vars, .. }) = self.vars.last_mut() {
            vars.insert(name, value);
        }
    }

    fn assign(&mut self, var: &Var, value: Value) -> Result<ValueOrRef, LoxError> {
        for Scope { vars, .. } in self.vars.iter_mut().rev() {
            if let Entry::Occupied(mut e) = vars.entry(var.symbol) {
                e.insert(Some(value));
                return Ok(ValueOrRef::StackRef(var.clone()));
            }
        }

        // TODO variable name and line
        Err(Self::get_error(0, &var.name, "undefined"))
    }

    fn get_ref(&self, var: &Var) -> Result<ValueOrRef, LoxError> {
        for Scope { vars, .. } in self.vars.iter().rev() {
            if vars.contains_key(&var.symbol) {
                return Ok(ValueOrRef::StackRef(var.clone()));
            }
        }

        Err(Self::get_error(0, &var.name, "undefined"))
    }

    fn get_value(&self, var: &Var) -> Result<&Value, LoxError> {
        for Scope { vars, .. } in self.vars.iter().rev() {
            if let Some(value) = vars.get(&var.symbol) {
                return value.as_ref().ok_or_else(|| Self::get_error(0, &var.name, "uninitialized"));
            }
        }

        Err(Self::get_error(0, &var.name, "undefined"))
    }

    fn get_value_mut(&mut self, var: &Var) -> Result<&mut Value, LoxError> {
        for Scope { vars, .. } in self.vars.iter_mut().rev() {
            if let Some(value) = vars.get_mut(&var.symbol) {
                return value.as_mut().ok_or_else(|| Self::get_error(0, &var.name, "uninitialized"));
            }
        }

        Err(Self::get_error(0, &var.name, "undefined"))
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

    fn assign(&mut self, var: &Var, value: Value) -> Result<ValueOrRef, LoxError> {
        if let Entry::Occupied(mut e) = self.vars.entry(var.symbol) {
            e.insert(value);
            return Ok(ValueOrRef::HeapRef(var.clone()));
        }

        // TODO variable name and line
        Err(Self::get_error(0, &var.name, "undefined"))
    }

    fn get_ref(&self, var: &Var) -> Result<ValueOrRef, LoxError> {
        if self.vars.contains_key(&var.symbol) {
            Ok(ValueOrRef::HeapRef(var.clone()))
        } else {
            Err(Self::get_error(0, &var.name, "undefined"))
        }
    }

    fn get_value(&self, var: &Var) -> Result<&Value, LoxError> {
        self.vars.get(&var.symbol).ok_or_else(|| Self::get_error(0, &var.name, "undefined"))
    }

    fn get_value_mut(&mut self, var: &Var) -> Result<&mut Value, LoxError> {
        self.vars.get_mut(&var.symbol).ok_or_else(|| Self::get_error(0, &var.name, "undefined"))
    }
}
