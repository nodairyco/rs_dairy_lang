#![allow(non_camel_case_types)]
#![allow(unused)]

use crate::dairy_lang::value::Value;
use core::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    LEFT_SQUARE,
    RIGHT_SQUARE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    COLON,
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
pub struct Token {
    pub token_type: TokenType,
    pub lexem: Rc<str>,
    pub line: u32,
    pub literal: Value,
}

impl Token {
    pub fn new(token_type: TokenType, lexem: Rc<str>, line: u32, literal: Value) -> Token {
        Token {
            token_type: token_type,
            lexem: lexem,
            line: line,
            literal: literal,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {} {}", self.token_type, self.lexem, self.literal)
    }
}
