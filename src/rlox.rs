mod scanner;
mod error;
mod token;
mod ast;
mod parser;
mod eval;
mod interpreter;

use crate::rlox::error::Logger;
use crate::rlox::interpreter::Interpreter;
use crate::rlox::parser::Parser;
use crate::rlox::scanner::Scanner;
use std::error::Error;
use std::fs::File;
use std::io;
use std::io::{BufReader, Read, Write};

pub fn run_file(file_path: &str) -> Result<(), Box<dyn Error>> {
    let file = File::open(file_path)?;
    let mut reader = BufReader::new(file);

    let mut contents = String::new();
    reader.read_to_string(&mut contents)?;

    let mut interpreter = Interpreter::new();
    run(&contents, &mut interpreter)
}

pub fn run_prompt() -> Result<(), Box<dyn Error>> {
    let mut buffer = String::new();
    let stdin = io::stdin();

    let mut interpreter = Interpreter::new();
    loop {
        print!("> ");
        io::stdout().flush()?;

        buffer.clear();
        let bytes_read = stdin.read_line(&mut buffer)?;
        if bytes_read == 0 { break; }

        if let Err(error) = run(&buffer, &mut interpreter) {
            println!("{}", error);
        }
    }

    Ok(())
}

fn run(source: &str, interpreter: &mut Interpreter) -> Result<(), Box<dyn Error>> {
    let mut scan_logger = Logger::new();
    let mut parse_logger = Logger::new();
    let mut runtime_logger = Logger::new();

    let mut tokens = Scanner::new(source, &mut scan_logger).into_iter();
    let parser = Parser::new(&mut tokens, &mut parse_logger);
    interpreter.interpret(&mut parser.into_iter(), &mut runtime_logger);

    Logger::from([scan_logger, parse_logger, runtime_logger]).result()
}