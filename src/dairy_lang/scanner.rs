use std::rc::Rc;

use crate::dairy_lang::value::Value;
use crate::{
    dairy_hater,
    dairy_lang::token::{Token, TokenType},
};

pub struct Scanner {
    // Source code stored as string
    source: String,

    // List of generated tokens
    tokens: Vec<Token>,

    // Start of the current lexeme
    start: u32,

    // Current position in a lexeme
    current: u32,

    // Current line we are parsing
    line: u32,
}

impl Scanner {
    /// Create a new Scanner
    pub fn new(source: String) -> Scanner {
        Scanner {
            source: source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    const KEYWORDS: &[(&str, TokenType)] = &[
        ("and", TokenType::AND),
        ("xor", TokenType::XOR),
        ("class", TokenType::CLASS),
        ("else", TokenType::ELSE),
        ("false", TokenType::FALSE),
        ("funct", TokenType::FUNCT),
        ("functio", TokenType::FUNCTIO),
        ("for", TokenType::FOR),
        ("if", TokenType::IF),
        ("VicTim", TokenType::VICTIM),
        ("or", TokenType::OR),
        ("print", TokenType::PRINT),
        ("return", TokenType::RETURN),
        ("super", TokenType::SUPER),
        ("me", TokenType::ME),
        ("true", TokenType::TRUE),
        ("var", TokenType::VAR),
        ("val", TokenType::VAL),
        ("while", TokenType::WHILE),
    ];

    /// Scans over every char in the source code and returns the tokens
    pub fn scan_tokens(mut self) -> Vec<Token> {
        // While we are not pointing at the last char
        while !self.is_at_end() {
            // Start new lexeme at the current position
            self.start = self.current;

            // Scan the current lexeme.
            // The start gets changed with this thus we never scan the same lexeme twice.
            self.scan_token();
        }

        // Push the end of file token to the end of the token list
        self.tokens.push(Token::new(
            TokenType::EOF,
            Rc::from(""),
            self.line,
            Value::Nil,
        ));

        return self.tokens.clone();
    }

    /// Checks if we are currently pointing at the last char of the source
    fn is_at_end(&self) -> bool {
        self.current >= self.source.len() as u32
    }

    /// Scans current lexeme and adds it as a token in the list <br>
    /// Errors if syntax error
    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            '(' => self.add_token(TokenType::LEFT_PAREN),
            ')' => self.add_token(TokenType::RIGHT_PAREN),
            '{' => self.add_token(TokenType::LEFT_BRACE),
            '}' => self.add_token(TokenType::RIGHT_BRACE),
            ',' => self.add_token(TokenType::COMMA),
            '.' => self.add_token(TokenType::DOT),
            '+' => self.add_token(TokenType::PLUS),
            ';' => self.add_token(TokenType::SEMICOLON),
            '*' => self.add_token(TokenType::STAR),
            '%' => self.add_token(TokenType::MODULO),
            ':' => self.add_token(TokenType::COLON),
            '[' => self.add_token(TokenType::LEFT_SQUARE),
            ']' => self.add_token(TokenType::RIGHT_SQUARE),
            '-' => {
                if self.match_next('-') {
                    if self.match_next('>') {
                        self.add_token(TokenType::LONG_ARROW);
                    } else {
                        dairy_hater::error(self.line, "Unexpected character");
                    }
                } else {
                    self.add_token(TokenType::MINUS)
                }
            }
            '!' => {
                let token_type = match self.match_next('=') {
                    true => TokenType::BANG_EQUAL,
                    false => TokenType::BANG,
                };
                self.add_token(token_type)
            }
            '=' => {
                let token_type = match self.match_next('=') {
                    true => TokenType::EQUAL_EQUAL,
                    false => TokenType::EQUAL,
                };
                self.add_token(token_type)
            }
            '<' => {
                let token_type = match self.match_next('=') {
                    true => TokenType::LESS_EQUAL,
                    false => TokenType::LESS,
                };
                self.add_token(token_type)
            }
            '>' => {
                let token_type = match self.match_next('=') {
                    true => TokenType::GREATER_EQUAL,
                    false => TokenType::GREATER,
                };
                self.add_token(token_type)
            }
            '/' => {
                if self.match_next('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else if self.match_next('*') {
                    while !self.is_at_end() {
                        self.advance();

                        if self.peek() == '*' {
                            self.advance();
                            if self.peek() == '/' {
                                self.advance();
                                break;
                            }
                        }
                    }
                } else {
                    self.add_token(TokenType::SLASH);
                }
            }
            ' ' => {}
            'r' => {}
            '\t' => {}
            '\n' => self.line = self.line + 1,
            '"' => self.handle_string(),
            rest => {
                if Self::is_digit(&rest) {
                    self.handle_number();
                } else if Self::is_alpha(&rest) {
                    self.handle_identifier();
                } else {
                    dairy_hater::error(self.line, "Unexpected character");
                }
            }
        }
    }

    /// Get the current char and move pointer by 1
    fn advance(&mut self) -> char {
        let char = self.source.chars().nth(self.current as usize);
        self.current = self.current + 1;

        match char {
            Some(c) => return c,
            None => panic!("ERR! Error advancing in a line"),
        }
    }

    /// Add token with Nil value and specified type to the list
    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_val(Value::Nil, token_type);
    }

    /// Adds current lexeme with the given value and type to the token list
    fn add_token_val(&mut self, val: Value, token_type: TokenType) {
        let text = &self.source[self.start as usize..self.current as usize];

        self.tokens
            .push(Token::new(token_type, Rc::from(text), self.line, val));
    }

    /// Compare the next value to the expected char. If matches move the curr pointer
    fn match_next(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        match self.source.chars().nth(self.current as usize) {
            Some(v) => {
                if v != expected {
                    return false;
                } else {
                    self.current = self.current + 1;
                    return true;
                }
            }
            None => panic!("ERR! Error matching chars"),
        }
    }

    /// Get the char at current position without advancing pointer
    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        return self.source.chars().nth(self.current as usize).unwrap();
    }

    /// Get back nth char from current position
    fn peek_nth(&self, n: u32) -> char {
        if self.is_at_end() {
            return '\0';
        }

        return self
            .source
            .chars()
            .nth(self.current as usize + n as usize)
            .unwrap();
    }

    /// Checks if char is a digit
    fn is_digit(c: &char) -> bool {
        return *c >= '0' && *c <= '9';
    }

    /// Parse from opening brackets and if closing brackets is found it is added as a String Token,
    /// else an error is thrown. <br>Works with multiline
    fn handle_string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line = self.line + 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            dairy_hater::error(self.line, "ERR! Undetermined string");
            return;
        }

        self.advance();
        let val = &self.source[(self.start + 1) as usize..(self.current - 1) as usize];

        self.add_token_val(Value::Str(Rc::from(val)), TokenType::STRING);
    }

    /// Parse from number and if dot is found if proceeding number it is turned into a floating point number.
    fn handle_number(&mut self) {
        while Self::is_digit(&self.peek()) {
            self.advance();
        }

        if self.peek() == '.' && Self::is_digit(&self.peek_nth(1)) {
            self.current = self.current + 1;

            while Self::is_digit(&self.peek()) {
                self.advance();
            }
        }

        let val_str = &self.source[self.start as usize..self.current as usize];

        let val: f64 = val_str.parse().expect("ERR! Error trying to parse number");

        self.add_token_val(Value::Number(val), TokenType::NUMBER);
    }

    /// Check if a char is part of the alphabet or _
    fn is_alpha(c: &char) -> bool {
        (*c >= 'a' && *c <= 'z') || (*c >= 'A' && *c <= 'Z') || *c == '_'
    }

    /// Check if a char is alpha numeric
    fn is_alpha_numeric(c: &char) -> bool {
        Self::is_alpha(c) || Self::is_digit(c)
    }

    /// Identifiers are words reserved for language (var, class, or, and, etc..) <br>
    /// We just iterate over all the alpha numeric remaining chars
    fn handle_identifier(&mut self) {
        while Self::is_alpha_numeric(&self.peek()) {
            self.advance();
        }

        let text = &self.source[self.start as usize..self.current as usize];
        let token_type_opt = Self::KEYWORDS.iter().find(|item| item.0.eq(text));

        let token_type = match token_type_opt {
            Some(t) => t.1,
            None => TokenType::IDENTIFIER,
        };

        self.add_token(token_type);
    }
}
