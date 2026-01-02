use std::env;
use std::fs;
use std::io::{self, BufRead, Read, Write};

fn main() {
    let mut args = env::args().skip(1);
    let first_arg = args.next();

    match first_arg {
        Some(arg) if arg == "-e" => {
            // Execute code from command line (like node -e)
            let code = args.next().unwrap_or_else(|| {
                eprintln!("error: -e requires code argument");
                eprintln!("usage: sixseven -e <code>");
                std::process::exit(2);
            });
            let mut input = io::stdin();
            match sixseven::run_source_with_input(&code, &mut input) {
                Ok(out) => {
                    print!("{out}");
                }
                Err(e) => {
                    eprintln!("error: {e:?}");
                    std::process::exit(1);
                }
            }
        }
        Some(path) => {
            // Execute file
            let input_path = args.next();
            let source = match fs::read_to_string(&path) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("failed to read `{path}`: {e}");
                    std::process::exit(2);
                }
            };

            let mut input: Box<dyn Read> = if let Some(input_file) = input_path {
                // Read from file
                match fs::read(&input_file) {
                    Ok(bytes) => Box::new(io::Cursor::new(bytes)),
                    Err(e) => {
                        eprintln!("failed to read input `{input_file}`: {e}");
                        std::process::exit(2);
                    }
                }
            } else {
                // Default: read from stdin
                Box::new(io::stdin())
            };

            match sixseven::run_source_with_input(&source, &mut input) {
                Ok(out) => {
                    print!("{out}");
                }
                Err(e) => {
                    eprintln!("error: {e:?}");
                    std::process::exit(1);
                }
            }
        }
        None => {
            // REPL mode
            repl();
        }
    }
}

fn repl() {
    let stdin = io::stdin();
    let mut stdin_lock = stdin.lock();
    let mut stdout = io::stdout();
    let mut input_buffer = String::new();

    loop {
        print!(">>> ");
        stdout.flush().unwrap();

        input_buffer.clear();
        match stdin_lock.read_line(&mut input_buffer) {
            Ok(0) => {
                // EOF (Ctrl+D on Unix, Ctrl+Z on Windows)
                println!();
                break;
            }
            Ok(_) => {
                let code = input_buffer.trim();
                if code.is_empty() {
                    continue;
                }
                if code == "exit" || code == "quit" {
                    break;
                }

                // For REPL, we use empty input
                let mut empty_input = io::empty();
                match sixseven::run_source_with_input(code, &mut empty_input) {
                    Ok(out) => {
                        if !out.is_empty() {
                            println!("{out}");
                        }
                    }
                    Err(e) => {
                        eprintln!("error: {e:?}");
                    }
                }
            }
            Err(e) => {
                eprintln!("failed to read line: {e}");
                break;
            }
        }
    }
}
