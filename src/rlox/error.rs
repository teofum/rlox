use colored::Colorize;
use std::error::Error;
use std::fmt::{Display, Formatter};

pub type LoxResult<T> = Result<T, LoxError>;

#[derive(Debug)]
pub enum ErrorType {
    Scanner,
    Parser,
    Syntax,
    Runtime,
    Type,
    Resolve,
}

#[derive(Debug)]
pub struct LoxError {
    error_type: ErrorType,
    line: usize,
    message: String,
    stack_trace: Option<Vec<String>>,
}

impl LoxError {
    pub fn new(error_type: ErrorType, line: usize, message: &str) -> Self {
        Self { error_type, line, message: message.to_string(), stack_trace: None }
    }

    pub fn with_stack(self, stack: Vec<String>) -> Self {
        Self { error_type: self.error_type, line: self.line, message: self.message, stack_trace: Some(stack) }
    }
}

impl Display for LoxError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let line = if self.line == 0 { String::new() } else { format!(" (on line {})", self.line) };
        let mut str = format!("{:?}Error{}: {}", self.error_type, line, self.message);
        if let Some(stack) = &self.stack_trace {
            for item in stack {
                let message = format!("\n\tin {}", item);
                str += &message;
            }
        }

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

impl Default for Logger {
    fn default() -> Self {
        Self::new()
    }
}

impl Logger {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    pub fn from<const N: usize>(loggers: [Logger; N]) -> Self {
        let mut errors = Vec::new();
        for logger in loggers {
            let mut logger = logger;
            errors.append(&mut logger.errors)
        }

        Self { errors }
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn log(&mut self, error: LoxError) {
        self.errors.push(error);
    }

    pub fn result(self) -> std::result::Result<(), Box<dyn Error>> {
        if self.has_errors() {
            Err(Box::new(LoxErrors::from(self.errors)))
        } else {
            Ok(())
        }
    }
}
