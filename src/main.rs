mod rlox;

use colored::Colorize;
use std::env;

fn main() {
    let mut args = env::args().skip(1);
    if args.len() > 1 {
        println!("Usage: rlox [script]");
        return;
    }

    let result = match args.next() {
        Some(script_path) => rlox::run_file(&script_path),
        None => rlox::run_prompt(),
    };

    if let Err(error) = result {
        println!("Stopped [{}]:\n{}", "ERROR".bold().red(), error);
    }
}
