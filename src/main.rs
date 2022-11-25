pub mod cli;

use clap::Parser as ArgParser;
use cli::Args;
use rustyline::error::ReadlineError;
use rustyline::highlight::MatchingBracketHighlighter;
use rustyline::validate::MatchingBracketValidator;
use rustyline::{Cmd, ColorMode, Config, EditMode, Editor, KeyEvent};
use rustyline_derive::{Completer, Helper, Highlighter, Hinter, Validator};
use scurry;
use scurry::interpreter::object::RuntimeError;
use scurry::interpreter::Interpreter;
use scurry::parser::Parser;
use std::fs;
use std::path::PathBuf;
use std::process;

#[derive(Helper, Completer, Hinter, Highlighter, Validator)]
struct ReadlineHelper {
    #[rustyline(Highlighter)]
    highlighter: MatchingBracketHighlighter,
    #[rustyline(Validator)]
    brackets: MatchingBracketValidator,
}

fn main() {
    let args = Args::parse();
    match args.file {
        Some(path) => eval_file(path),
        None => start_repl(args),
    }
}

fn eval_file(path: PathBuf) {
    let contents = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(err) => {
            eprintln!("{err}");
            process::exit(1);
        }
    };
    let mut interpreter = Interpreter::new();
    let parser = Parser::new(
        &contents
            .strip_suffix('\n')
            .expect("should have trailing newline in file"),
    );
    match parser.parse() {
        Ok(program) => {
            if let Err(err) = interpreter.eval(program) {
                if let RuntimeError::ParserErrors { contents, errs } = err {
                    println!("\nparser errors:");
                    for err in errs {
                        let s = err.to_string();
                        println!();
                        println!("{}", err.position().format_on_source(&contents));
                        println!("{s}");
                    }
                    return;
                }
                eprintln!("\nruntime error:");
                eprintln!("{err}");
                process::exit(1);
            }
        }
        Err(errs) => {
            println!("\nparser errors:");
            for err in errs {
                let s = err.to_string();
                println!();
                println!("{}", err.position().format_on_source(&contents));
                println!("{s}");
            }
            process::exit(1);
        }
    }
}

fn start_repl(args: Args) {
    const PROMPT: &str = ">> ";

    let helper = ReadlineHelper {
        highlighter: MatchingBracketHighlighter::new(),
        brackets: MatchingBracketValidator::new(),
    };
    let config = Config::builder()
        .indent_size(4)
        .tab_stop(4)
        .color_mode(
            args.no_color
                .then_some(ColorMode::Disabled)
                .unwrap_or(ColorMode::Enabled),
        )
        .edit_mode(
            args.vi_mode
                .then_some(EditMode::Vi)
                .unwrap_or(EditMode::Emacs),
        )
        .build();
    let mut editor = Editor::with_config(config).expect("options should all work");
    editor.set_helper(Some(helper));
    editor.bind_sequence(KeyEvent::from('\t'), Cmd::Insert(1, "    ".to_owned()));
    let mut interpreter = Interpreter::new();
    loop {
        let readline = editor.readline(PROMPT);
        match readline {
            Ok(line) => {
                editor.add_history_entry(line.as_str());

                let parser = Parser::new(&line);
                match parser.parse() {
                    Ok(program) => match interpreter.eval_repl(program) {
                        Ok(obj) if !obj.is_absnil() => println!("{obj}"),
                        Err(err) => {
                            if let RuntimeError::ParserErrors { contents, errs } = err {
                                println!("\nparser errors:");
                                for err in errs {
                                    let s = err.to_string();
                                    println!();
                                    println!("{}", err.position().format_on_source(&contents));
                                    println!("{s}");
                                }
                                continue;
                            }
                            println!("\nruntime error:");
                            println!("{err}");
                        }
                        _ => {}
                    },
                    Err(errs) => {
                        println!("\nparser errors:");
                        for err in errs {
                            let s = err.to_string();
                            println!();
                            println!("{}", err.position().format_on_source(&line));
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
}
