use crate::rlox::ast::Value;
use crate::rlox::error::{ErrorType, LoxError};
use std::time;
use std::time::SystemTime;
use crate::rlox::interpreter::Interpreter;

pub fn clock(_: &mut Interpreter, _: &Vec<Value>) -> Result<Value, LoxError> {
    let now = SystemTime::now();
    now.duration_since(time::UNIX_EPOCH)
        .map(|since_epoch| Value::Number(since_epoch.as_secs_f64()))
        .map_err(|_| LoxError::new(ErrorType::Runtime, 0, "Error getting system time"))
}