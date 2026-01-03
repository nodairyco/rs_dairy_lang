#![allow(non_camel_case_types)]
#![allow(unused)]

use core::fmt;
use std::fmt::write;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    MODULO,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    DOUBLE_SQUARE_LEFT,
    DOUBLE_SQUARE_RIGHT,

    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords.
    AND,
    XOR,
    CLASS,
    ELSE,
    FALSE,
    FUNCT,
    FUNCTIO,
    FOR,
    IF,
    VICTIM,
    OR,
    PRINT,
    RETURN,
    SUPER,
    ME,
    TRUE,
    VAR,
    VAL,
    WHILE,

    EOF,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Number(f64),
    Str(String),
    Bool(bool),
    Void,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexem: String,
    pub line: u32,
    pub literal: Value,
}

impl Token {
    pub fn new(token_type: TokenType, lexem: String, line: u32, literal: Value) -> Token {
        Token {
            token_type: token_type,
            lexem: lexem,
            line: line,
            literal: literal,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Number(n) => write!(f, "{}", n),
            Value::Str(str) => write!(f, "{}", str),
            Value::Bool(bool) => write!(f, "{}", bool),
            _ => write!(f, ""),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {} {}", self.token_type, self.lexem, self.literal)
    }
}
