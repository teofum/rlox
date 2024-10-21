mod scanner;
mod error;
mod token;
mod ast;
mod parser;

use crate::rlox::error::Logger;
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
    let mut logger = Logger::new();

    let scanner = Scanner::new(source, &mut logger);
    for token in scanner {
        println!("{}", token);
    }

    logger.result()
}