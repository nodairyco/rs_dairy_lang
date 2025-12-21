use crate::{
    dairy_hater::{self},
    dairy_lang::{
        expression::Expr,
        token::{Token, TokenType, Value},
    },
};

pub struct Parser {
    tokens: Vec<Token>,
    current: u32,
}

#[derive(Debug)]
struct ParseError;

type ParseResult = Result<Expr, ParseError>;

impl Parser {
    /// Create new parser with given tokens
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: tokens,
            current: 0,
        }
    }

    pub fn parse(&mut self) -> Expr {
        match self.expression() {
            Ok(res) => res,
            Err(_) => Expr::empty()
        }
    }

    /// Top most expression maps directly to equality <br>
    /// expression -> equality
    fn expression(&mut self) -> ParseResult {
        self.equality()
    }

    /// Mapping for the equality expression type to a rust function <br>
    /// Initially expr is created from comparision expression. <br>
    /// Then while the next couple types are equality types,
    /// we get the token and the the right sub tree and add it to the expression <br>
    /// equality -> comparison (("!=" | "==") comparison )* ;
    fn equality(&mut self) -> ParseResult {
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

            match expr {
                Err(_) => break,
                Ok(res) => {
                    expr = match right {
                        Ok(res_r) => Ok(Expr::Binary {
                            left: Box::new(res),
                            operator: op_new,
                            right: Box::new(res_r),
                        }),
                        Err(err_r) => Err(err_r),
                    }
                }
            };
        }

        expr
    }

    /// Works the same way as equality,but one level lower <br>
    /// comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> ParseResult {
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

            match expr {
                Err(_) => break,
                Ok(res) => {
                    expr = match right {
                        Ok(res_r) => Ok(Expr::Binary {
                            left: Box::new(res),
                            operator: op_new,
                            right: Box::new(res_r),
                        }),
                        Err(err_r) => Err(err_r),
                    }
                }
            };
        }

        expr
    }

    /// Works in the same way as comparison <br>
    /// temr -> factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self) -> ParseResult {
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

            match expr {
                Err(_) => break,
                Ok(res) => {
                    expr = match right {
                        Ok(res_r) => Ok(Expr::Binary {
                            left: Box::new(res),
                            operator: op_new,
                            right: Box::new(res_r),
                        }),
                        Err(err_r) => Err(err_r),
                    }
                }
            };
        }

        expr
    }

    /// Works in the same way as term <br>
    /// factor -> unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> ParseResult {
        let mut expr = self.unary();

        while self.match_types(&[TokenType::SLASH, TokenType::STAR]) {
            let operator = self.prev();

            let op_new = Token::new(
                operator.token_type,
                operator.lexem.clone(),
                operator.line,
                operator.literal.clone(),
            );

            let right = self.unary();

            match expr {
                Err(_) => break,
                Ok(res) => {
                    expr = match right {
                        Ok(res_r) => Ok(Expr::Binary {
                            left: Box::new(res),
                            operator: op_new,
                            right: Box::new(res_r),
                        }),
                        Err(err_r) => Err(err_r),
                    }
                }
            };
        }

        expr
    }

    /// Simply if there is a unary operation go save it and go one deeper, and so on if a unary is found <br>
    /// If not this is a primary and we just return self.primary() <br>
    /// unary -> ( "!" | "-" ) unary | primary ;
    fn unary(&mut self) -> ParseResult {
        if self.match_types(&[TokenType::BANG, TokenType::MINUS]) {
            let operator = self.prev();

            let op_new = Token::new(
                operator.token_type,
                operator.lexem.clone(),
                operator.line,
                operator.literal.clone(),
            );

            let right = self.unary();

            return match right {
                Ok(res) => Ok(Expr::Unary {
                    operator: op_new,
                    right: Box::new(res),
                }),
                Err(err) => Err(err),
            };
        }

        return self.primary();
    }

    /// Primary is the only terminal in our language and can come out to STRING, NUMBER,
    /// and constant values <br>
    /// But there is a case with ( and ) where we have to evaluate an expression,
    /// which is evaluated and a grouping is returned <br>
    /// primary -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    fn primary(&mut self) -> ParseResult {
        if self.match_types(&[TokenType::FALSE]) {
            return Ok(Expr::Literal {
                value: Some(Value::Bool(false)),
            });
        }

        if self.match_types(&[TokenType::TRUE]) {
            return Ok(Expr::Literal {
                value: Some(Value::Bool(true)),
            });
        }

        if self.match_types(&[TokenType::VICTIM]) {
            return Ok(Expr::Literal {
                value: Some(Value::Nil),
            });
        }

        if self.match_types(&[TokenType::NUMBER]) {
            return Ok(Expr::Literal {
                value: self.prev().literal.clone(),
            });
        }

        if self.match_types(&[TokenType::STRING]) {
            return Ok(Expr::Literal {
                value: self.prev().literal.clone(),
            });
        }

        if self.match_types(&[TokenType::LEFT_PAREN]) {
            let expr = self.expression();

            match self.consume(
                TokenType::RIGHT_PAREN,
                String::from("Expect ')' after expression."),
            ) {
                Ok(_) => match expr {
                    Ok(res) => {
                        return Ok(Expr::Grouping {
                            expression: Box::new(res),
                        });
                    },
                    Err(err) => return Err(err)
                },
                Err(err) => return Err(err),
            };
        }

        Err(Self::error(self.peek(), "Unemplemented error".to_string()))
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
            return Ok(self.advance());
        }

        return Err(Self::error(self.peek(), msg));
    }

    /// Function to synchronise in case an error occures. <br>
    /// If a parse error is detected, drop tokens, until a start of a new expression,
    /// or the end of the current one is found.
    fn sync(&mut self) {
        self.advance();

        while !self.is_at_end() {
            match self.peek().token_type {
                TokenType::SEMICOLON => return,
                TokenType::CLASS => return,
                TokenType::FUNCT => return,
                TokenType::FUNCTIO => return,
                TokenType::VAR => return,
                TokenType::VAL => return,
                TokenType::FOR => return,
                TokenType::IF => return,
                TokenType::WHILE => return,
                TokenType::PRINT => return,
                TokenType::RETURN => return,
                _ => {}
            }

            self.advance();
        }
    }
}
