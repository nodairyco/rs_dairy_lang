use crate::dairy_lang::interpreter::Interpreter;

mod dairy_lang {
    pub mod environment;
    pub mod expression;
    pub mod interpreter;
    pub mod parser;
    pub mod scanner;
    pub mod stmt;
    pub mod token;
}

fn main() {
    run_interpreter();
}

fn run_interpreter() {
    let file_name;

    let mut args = std::env::args();

    if args.len() > 2 {
        panic!("ERR! More than one args found.");
    }

    let mut interpreter = Interpreter::new();

    if args.len() == 1 {
        dairy_hater::dairy_shell(&mut interpreter);
    } else {
        file_name = args.nth(1).unwrap();

        println!("{}", file_name);

        dairy_hater::run_file(file_name, &mut interpreter);
    }
}

mod dairy_hater {
    use std::{
        fs,
        io::{self, Write},
        sync::atomic::AtomicBool,
        sync::atomic::Ordering,
    };

    use crate::dairy_lang::{
        interpreter::Interpreter,
        parser::Parser,
        scanner::Scanner,
        token::{Token, TokenType},
    };

    static HAD_ERR: AtomicBool = AtomicBool::new(false);

    pub fn run_file(path: String, interpreter: &mut Interpreter) {
        let contets = fs::read_to_string(path).expect("ERR! Cannot read file");

        run(contets, interpreter);
    }

    pub fn dairy_shell(interpreter: &mut Interpreter) {
        println!("Running in dairy shell");

        loop {
            print!("> ");
            io::stdout().flush().expect("err flushing");

            let mut req = String::new();

            io::stdin().read_line(&mut req).expect("err");

            if req.trim() == "break" {
                break;
            }

            run(req, interpreter);
            HAD_ERR.store(false, Ordering::Relaxed);
        }
    }

    pub fn error(line: u32, msg: String) {
        report(line, String::from(""), msg);
    }

    pub fn error_token(token: &Token, msg: String) {
        match token.token_type {
            TokenType::EOF => report(token.line, " at end".to_string(), msg),
            _ => report(token.line, format!(" at '{}'", token.lexem), msg),
        }
    }

    fn report(line: u32, wher: String, msg: String) {
        HAD_ERR.store(true, Ordering::Relaxed);
        eprintln!("[line {}] Error {}: {}", line, wher, msg);
    }

    fn run(file_contents: String, interpreter: &mut Interpreter) {
        let scanner = Scanner::new(file_contents);
        let tokens = scanner.scan_tokens();

        if HAD_ERR.load(Ordering::Relaxed) {
            return;
        }

        let mut parser = Parser::new(tokens);

        if HAD_ERR.load(Ordering::Relaxed) {
            return;
        }

        let mut stmts = parser.parse();

        if HAD_ERR.load(Ordering::Relaxed) {
            return;
        }

        let _ = interpreter.interpret(&mut stmts);
    }
}
