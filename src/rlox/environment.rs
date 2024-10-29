use crate::rlox::ast::{Value, ValueOrRef, Var};
use crate::rlox::error::{ErrorType, LoxError};
use crate::rlox::lookups::Symbol;
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::rc::Rc;

pub type VariableKey = usize;
type VariableMap = HashMap<Symbol, Option<VariableKey>>;

pub trait Env {
    fn define(&mut self, name: Symbol, value_key: Option<VariableKey>);
    fn assign(&mut self, var: &Var, value_key: VariableKey) -> Result<ValueOrRef, LoxError>;
    fn get(&self, var: &Var) -> Result<VariableKey, LoxError>;
    
    fn is_global(&self) -> bool;
}

fn get_error(line: usize, var_name: &str, problem: &str) -> LoxError {
    let message = format!("Variable \"{}\" is {}", var_name, problem);
    LoxError::new(ErrorType::Runtime, line, &message)
}

#[derive(Debug)]
pub struct Environment {
    name: String,
    vars: VariableMap,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(name: String) -> Rc<RefCell<Self>> {
        let env = Self { name, vars: HashMap::new(), enclosing: None };
        Rc::new(RefCell::new(env))
    }

    pub fn from(name: String, enclosing: Rc<RefCell<Environment>>) -> Rc<RefCell<Self>> {
        let env = Self { name, vars: HashMap::new(), enclosing: Some(enclosing) };
        Rc::new(RefCell::new(env))
    }

    pub fn name(self) -> String {
        self.name
    }
}

impl Env for Environment {
    fn define(&mut self, name: Symbol, value: Option<VariableKey>) {
        self.vars.insert(name, value);
    }

    fn assign(&mut self, var: &Var, value: VariableKey) -> Result<ValueOrRef, LoxError> {
        if let Entry::Occupied(mut e) = self.vars.entry(var.symbol) {
            e.insert(Some(value));
            Ok(ValueOrRef::StackRef(var.clone()))
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow_mut().assign(var, value)
        } else {
            Err(get_error(0, &var.name, "undefined"))
        }
    }

    fn get(&self, var: &Var) -> Result<VariableKey, LoxError> {
        if let Some(value) = self.vars.get(&var.symbol) {
            value.ok_or_else(|| get_error(0, &var.name, "uninitialized"))
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get(var)
        } else {
            Err(get_error(0, &var.name, "undefined"))
        }
    }

    fn is_global(&self) -> bool {
        self.enclosing.is_some()
    }
}

impl Env for Rc<RefCell<Environment>> {
    fn define(&mut self, name: Symbol, value_key: Option<VariableKey>) {
        self.borrow_mut().define(name, value_key)
    }

    fn assign(&mut self, var: &Var, value_key: VariableKey) -> Result<ValueOrRef, LoxError> {
        self.borrow_mut().assign(var, value_key)
    }

    fn get(&self, var: &Var) -> Result<VariableKey, LoxError> {
        self.borrow().get(var)
    }

    fn is_global(&self) -> bool {
        self.borrow().is_global()
    }
}

#[derive(Debug)]
pub struct Heap {
    data: Vec<Value>,
}

impl Heap {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn define(&mut self, value: Value) -> VariableKey {
        self.data.push(value);
        self.data.len() - 1
    }

    pub fn assign(&mut self, key: VariableKey, value: Value) -> VariableKey {
        self.data[key] = value;
        key
    }

    pub fn get(&self, key: VariableKey) -> &Value {
        &self.data[key]
    }
}
