use crate::{
    dairy_hater::{self}, expression::Expr, token::{Token, TokenType, Value}
};

pub struct Parser {
    tokens: Vec<Token>,
    current: u32,
}

#[derive(Debug)]
struct ParseError;

impl Parser {
    /// Create new parser with given tokens
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: tokens,
            current: 0,
        }
    }

    /// Top most expression maps directly to equality <br>
    /// expression -> equality
    pub fn expression(&mut self) -> Expr {
        self.equality()
    }

    /// Mapping for the equality expression type to a rust function <br>
    /// Initially expr is created from comparision expression. <br>
    /// Then while the next couple types are equality types,
    /// we get the token and the the right sub tree and add it to the expression <br>
    /// equality -> comparison (("!=" | "==") comparison )* ;
    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();

        while self.match_types(&[TokenType::BANG_EQUAL, TokenType::EQUAL_EQUAL]) {
            let operator = self.prev();

            let op_new = Token::new(
                operator.token_type,
                operator.lexem.clone(),
                operator.line,
                operator.literal.clone(),
            );

            let right = self.comparison();

            expr = Expr::Binary {
                left: Box::new(expr),
                operator: op_new,
                right: Box::new(right),
            }
        }

        expr
    }

    /// Works the same way as equality,but one level lower <br>
    /// comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Expr {
        let mut expr = self.term();

        while self.match_types(&[
            TokenType::GREATER,
            TokenType::GREATER_EQUAL,
            TokenType::LESS,
            TokenType::LESS_EQUAL,
        ]) {
            let operator = self.prev();

            let op_new = Token::new(
                operator.token_type,
                operator.lexem.clone(),
                operator.line,
                operator.literal.clone(),
            );

            let right = self.term();

            expr = Expr::Binary {
                left: Box::new(expr),
                operator: op_new,
                right: Box::new(right),
            }
        }

        expr
    }

    /// Works in the same way as comparison <br>
    /// temr -> factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self) -> Expr {
        let mut expr = self.factor();

        while self.match_types(&[TokenType::MINUS, TokenType::PLUS]) {
            let operator = self.prev();

            let op_new = Token::new(
                operator.token_type,
                operator.lexem.clone(),
                operator.line,
                operator.literal.clone(),
            );

            let right = self.factor();

            expr = Expr::Binary {
                left: Box::new(expr),
                operator: op_new,
                right: Box::new(right),
            }
        }

        expr
    }

    /// Works in the same way as term <br>
    /// factor -> unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> Expr {
        let mut expr = self.unary();

        while self.match_types(&[TokenType::MINUS, TokenType::PLUS]) {
            let operator = self.prev();

            let op_new = Token::new(
                operator.token_type,
                operator.lexem.clone(),
                operator.line,
                operator.literal.clone(),
            );

            let right = self.unary();

            expr = Expr::Binary {
                left: Box::new(expr),
                operator: op_new,
                right: Box::new(right),
            }
        }

        expr
    }

    /// Simply if there is a unary operation go save it and go one deeper, and so on if a unary is found <br>
    /// If not this is a primary and we just return self.primary() <br>
    /// unary -> ( "!" | "-" ) unary | primary ;
    fn unary(&mut self) -> Expr {
        if self.match_types(&[TokenType::BANG, TokenType::MINUS]) {
            let operator = self.prev();

            let op_new = Token::new(
                operator.token_type,
                operator.lexem.clone(),
                operator.line,
                operator.literal.clone(),
            );

            let right = self.unary();

            return Expr::Unary {
                operator: op_new,
                right: Box::new(right),
            };
        }

        return self.primary();
    }

    /// Primary is the only terminal in our language and can come out to STRING, NUMBER,
    /// and constant values <br>
    /// But there is a case with ( and ) where we have to evaluate an expression,
    /// which is evaluated and a grouping is returned <br>
    /// primary -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    fn primary(&mut self) -> Expr {
        if self.match_types(&[TokenType::FALSE]) {
            return Expr::Literal {
                value: Some(Value::Bool(false)),
            };
        }

        if self.match_types(&[TokenType::TRUE]) {
            return Expr::Literal {
                value: Some(Value::Bool(true)),
            };
        }

        if self.match_types(&[TokenType::VICTIM]) {
            return Expr::Literal {
                value: Some(Value::Nil),
            };
        }

        if self.match_types(&[TokenType::NUMBER]) {
            return Expr::Literal {
                value: self.prev().literal.clone(),
            };
        }

        if self.match_types(&[TokenType::STRING]) {
            return Expr::Literal {
                value: self.prev().literal.clone(),
            };
        }

        if self.match_types(&[TokenType::LEFT_PAREN]) {
            let expr = self.expression();

            let _ = self.consume(TokenType::RIGHT_PAREN, String::from("Expect ')' after expression."));

            return Expr::Grouping {
                expression: Box::new(expr),
            };
        } 

        dairy_hater::error_token(self.peek(), "Unemplemented error".to_string());
        Expr::empty()
    }

    /// Get the Token at the current addr
    fn peek(&self) -> &Token {
        &self.tokens[self.current as usize]
    }

    /// Get the previous Token
    fn prev(&self) -> &Token {
        &self.tokens[self.current as usize - 1]
    }

    /// Chekc if the current token is EOF Token
    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EOF
    }

    /// If not at end point to next token and return current
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current = self.current + 1;
        }

        self.prev()
    }

    /// Check the type of the current Token
    fn check_curr_type(&self, token_type: TokenType) -> bool {
        !self.is_at_end() && self.peek().token_type == token_type
    }

    /// If current type matches any of the types from this array, return true and advance the pointer.
    fn match_types(&mut self, types: &[TokenType]) -> bool {
        for t_type in types {
            if self.check_curr_type(*t_type) {
                self.advance();
                return true;
            }
        }

        return false;
    }

    fn error(token: &Token, msg: String) -> ParseError {
        dairy_hater::error_token(&token, msg);
        ParseError
    } 

    fn consume(&mut self, token_type: TokenType, msg: String) -> Result<&Token, ParseError> {
        if self.check_curr_type(token_type) {
            return Ok(self.advance()) 
        }

        return Err(Self::error(self.peek(), msg));
    }
}
