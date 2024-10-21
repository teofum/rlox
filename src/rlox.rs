mod scanner;
mod error;
mod token;
mod ast;
mod parser;

use crate::rlox::error::Logger;
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

    run(&contents)
}

pub fn run_prompt() -> Result<(), Box<dyn Error>> {
    let mut buffer = String::new();
    let stdin = io::stdin();

    loop {
        print!("> ");
        io::stdout().flush()?;

        buffer.clear();
        let bytes_read = stdin.read_line(&mut buffer)?;
        if bytes_read == 0 { break; }

        if let Err(error) = run(&buffer) {
            println!("{}", error);
        }
    }

    Ok(())
}

fn run(source: &str) -> Result<(), Box<dyn Error>> {
    let mut scan_logger = Logger::new();
    let mut parse_logger = Logger::new();

    let mut tokens = Scanner::new(source, &mut scan_logger).into_iter();
    let mut parser = Parser::from(&mut tokens, &mut parse_logger);

    if let Some(expr) = parser.parse() {
        println!("{}", expr);
    }

    // Print any leftover tokens
    for token in tokens {
        println!("{}", token);
    }

    Logger::from([scan_logger, parse_logger]).result()
}