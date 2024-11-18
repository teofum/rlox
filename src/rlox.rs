mod scanner;
mod error;
mod token;
mod ast;
mod parser;
mod interpreter;
mod environment;
mod externals;
mod lookups;
mod resolver;

use crate::rlox::error::Logger;
use crate::rlox::interpreter::Interpreter;
use crate::rlox::lookups::Lookups;
use crate::rlox::parser::Parser;
use crate::rlox::resolver::Resolver;
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

    let mut lookups = Lookups::new();
    let mut interpreter = Interpreter::new(&mut lookups);
    run(&contents, &mut interpreter, &mut lookups)
}

pub fn run_prompt() -> Result<(), Box<dyn Error>> {
    let mut buffer = String::new();
    let stdin = io::stdin();

    let mut lookups = Lookups::new();
    let mut interpreter = Interpreter::new(&mut lookups);
    loop {
        print!("> ");
        io::stdout().flush()?;

        buffer.clear();
        let bytes_read = stdin.read_line(&mut buffer)?;
        if bytes_read == 0 { break; }

        if let Err(error) = run(&buffer, &mut interpreter, &mut lookups) {
            println!("{}", error);
        }
    }

    Ok(())
}

fn run(source: &str, interpreter: &mut Interpreter, lookups: &mut Lookups) -> Result<(), Box<dyn Error>> {
    let mut scan_logger = Logger::new();
    let mut parse_logger = Logger::new();

    let mut tokens = Scanner::new(source, &mut scan_logger).into_iter();
    let parser = Parser::new(&mut tokens, lookups, &mut parse_logger);
    let mut ast: Vec<_> = parser.into_iter().collect();

    if let parse_errors @ Err(_) = Logger::from([scan_logger, parse_logger]).result() {
        return parse_errors;
    }

    let mut runtime_logger = Logger::new();
    let mut resolver = Resolver::new(lookups);
    if let Err(resolve_error) = resolver.resolve_stmts(&mut ast) {
        runtime_logger.log(resolve_error);
        return runtime_logger.result();
    }

    interpreter.interpret(&mut ast.into_iter(), &mut runtime_logger);
    runtime_logger.result()
}