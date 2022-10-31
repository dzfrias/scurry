use clap::Parser as ArgParser;
use rustyline::error::ReadlineError;
use rustyline::{Config, Editor, Result};
use scurry::format_position;
use scurry::parser::{Parser, ParserError};
use std::process;

#[derive(Debug, ArgParser)]
#[command(author, version, about, long_about = None)]
struct Args {}

fn main() -> Result<()> {
    start_repl()
}

fn start_repl() -> Result<()> {
    const PROMPT: &str = ">> ";

    let config = Config::builder().indent_size(4).tab_stop(4).build();
    let mut editor = Editor::<()>::with_config(config)?;
    loop {
        let readline = editor.readline(PROMPT);
        match readline {
            Ok(line) => {
                editor.add_history_entry(line.as_str());

                let parser = Parser::new(&line);
                match parser.parse() {
                    Ok(program) => {
                        for stmt in program.0 {
                            println!("{:?}", stmt);
                        }
                    }
                    Err(errs) => {
                        println!("parser errors:");
                        for err in errs {
                            let s = err.to_string();
                            println!();
                            match err {
                                ParserError::ExpectedToken { pos, .. } => {
                                    println!("{}", format_position(&line, pos))
                                }
                                ParserError::IllegalCharacter { pos } => {
                                    println!("{}", format_position(&line, pos));
                                }
                                ParserError::UnteriminatedString { pos } => {
                                    println!("{}", format_position(&line, pos));
                                }
                                ParserError::InvalidPrefixOperator { pos, .. } => {
                                    println!("{}", format_position(&line, pos));
                                }
                            }
                            println!("{s}");
                        }
                    }
                }
                println!();
            }
            Err(ReadlineError::Interrupted) => break,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                eprintln!("Error reading repl input: {:?}", err);
                process::exit(1);
            }
        }
    }
    Ok(())
}
