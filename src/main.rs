mod dairy_lang {
    pub mod expression;
    pub mod parser;
    pub mod scanner;
    pub mod token;
}

fn main() {
    let file_name;

    let mut args = std::env::args();

    if args.len() > 2 {
        panic!("ERR! More than one args found.");
    }

    if args.len() == 1 {
        dairy_hater::dairy_shell();
    } else {
        file_name = args.nth(1).unwrap();

        println!("{}", file_name);

        dairy_hater::run_file(file_name);
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
        expression::AstPrinter,
        parser::Parser,
        scanner::Scanner,
        token::{Token, TokenType},
    };

    static HAD_ERR: AtomicBool = AtomicBool::new(false);

    pub fn run_file(path: String) {
        println!("{}", "=".repeat(100));
        println!("Running {}", path);

        let contets = fs::read_to_string(path).expect("ERR! Cannot read file");

        run(contets);

        if HAD_ERR.load(Ordering::Relaxed) {
            panic!("ERR! There was an error somewhere.")
        }
    }

    pub fn dairy_shell() {
        println!("Running in dairy shell");

        loop {
            print!("> ");
            io::stdout().flush().expect("err flushing");

            let mut req = String::new();

            io::stdin().read_line(&mut req).expect("err");

            if req.trim() == "break" {
                break;
            }

            run(req);
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

    fn run(file_contents: String) {
        let scanner = Scanner::new(file_contents);
        let tokens = scanner.scan_tokens();

        let mut parser = Parser::new(tokens);

        let mut printer = AstPrinter;

        let mut expr = parser.parse();

        println!("{}", printer.print(&mut expr))
    }
}
