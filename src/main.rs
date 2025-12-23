use std::env;
use std::fs;
use std::io::{self, Read};

fn main() {
    let mut args = env::args().skip(1);
    let program_path = args.next();
    let input_path = args.next();

    let source = match program_path {
        Some(path) => match fs::read_to_string(&path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("failed to read `{path}`: {e}");
                std::process::exit(2);
            }
        },
        None => {
            let mut s = String::new();
            if let Err(e) = io::stdin().read_to_string(&mut s) {
                eprintln!("failed to read stdin: {e}");
                std::process::exit(2);
            }
            s
        }
    };

    let mut input: Box<dyn Read> = if let Some(path) = input_path {
        match fs::read(&path) {
            Ok(bytes) => {
                Box::new(io::Cursor::new(bytes))
            }
            Err(e) => {
                eprintln!("failed to read input `{path}`: {e}");
                std::process::exit(2);
            }
        }
    } else {
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
