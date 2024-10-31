use crate::rlox::ast::{Value, ValueOrRef};
use crate::rlox::error::{ErrorType, LoxError, LoxResult};
use crate::rlox::interpreter::Interpreter;
use std::time;
use std::time::SystemTime;

pub fn clock(_: &Interpreter, _: &Vec<ValueOrRef>) -> LoxResult<Value> {
    let now = SystemTime::now();
    now.duration_since(time::UNIX_EPOCH)
        .map(|since_epoch| Value::Number(since_epoch.as_secs_f64()))
        .map_err(|_| LoxError::new(ErrorType::Runtime, 0, "Error getting system time"))
}