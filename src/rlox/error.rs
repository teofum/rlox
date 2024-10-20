use colored::Colorize;
use std::error::Error;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum ErrorType {
    ScanError,
    ParseError,
    RuntimeError,
}

#[derive(Debug)]
pub struct LoxError {
    error_type: ErrorType,
    line: usize,
    message: String,
}

impl LoxError {
    pub fn new(error_type: ErrorType, line: usize, message: &str) -> Self {
        Self { error_type, line, message: message.to_string() }
    }
}

impl Display for LoxError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = format!("{:?} (on line {}): {}", self.error_type, self.line, self.message);
        write!(f, "{}", str.red())
    }
}

#[derive(Debug)]
pub struct LoxErrors {
    errors: Vec<LoxError>,
}

impl LoxErrors {
    pub fn from(errors: Vec<LoxError>) -> Self {
        Self { errors }
    }
}

impl Display for LoxErrors {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = self.errors.iter().fold(String::new(), |str, err| str + &err.to_string() + "\n");
        write!(f, "{}", str)
    }
}

impl Error for LoxErrors {}

pub struct Logger {
    errors: Vec<LoxError>,
}

impl Logger {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn log(&mut self, error: LoxError) {
        self.errors.push(error);
    }

    pub fn result(self) -> Result<(), Box<dyn Error>> {
        if self.has_errors() {
            Err(Box::new(LoxErrors::from(self.errors)))
        } else {
            Ok(())
        }
    }
}