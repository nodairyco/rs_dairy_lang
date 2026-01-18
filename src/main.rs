use crate::dairy_lang::interpreter::Interpreter;

mod dairy_lang {
    pub mod environment;
    pub mod expression;
    pub mod interpreter;
    pub mod parser;
    pub mod scanner;
    pub mod stmt;
    pub mod token;
    pub mod value;
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
        interpreter::{EvalError, Interpreter},
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

    pub fn error(line: u32, msg: &str) {
        report(line, "", msg);
    }

    pub fn error_token(token: &Token, msg: &str) {
        match token.token_type {
            TokenType::EOF => report(token.line, " at end", msg),
            _ => report(token.line, &format!(" at '{}'", token.lexem), msg),
        }
    }

    pub fn error_token_non_fatal(token: &Token, msg: &str) {
        match token.token_type {
            TokenType::EOF => report_non_fatal(token.line, " at end", msg),
            _ => report_non_fatal(token.line, &format!(" at '{}'", token.lexem), msg),
        }
    }

    /// Prints to the error buffer and does not terminate the program.
    fn report_non_fatal(line: u32, wher: &str, msg: &str) {
        eprintln!("[line {}] Error {}: {}", line, wher, msg)
    }

    /// Prints to the error buffer and terminates the program.
    fn report(line: u32, wher: &str, msg: &str) {
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

        let exec_res = interpreter.interpret(&mut stmts);

        if let Err(EvalError {
            error_token: token,
            error_msg,
        }) = exec_res
        {
            error_token(&token, &error_msg);
        }
    }
}
