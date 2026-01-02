use std::vec;

use crate::{
    dairy_hater::{self},
    dairy_lang::{
        environment::VarType,
        expression::Expr,
        stmt::Stmt,
        token::{Token, TokenType, Value},
    },
};

pub struct Parser {
    tokens: Vec<Token>,
    current: u32,
}

#[derive(Debug)]
struct ParseError;

type ParseResult<T> = Result<T, ParseError>;

impl Parser {
    /// Create new parser with given tokens
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, current: 0 }
    }

    /// Get statements from current tokens
    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut stmts: Vec<Stmt> = vec![];

        while !self.is_at_end() {
            stmts.push(self.declaration());
        }

        stmts
    }

    /// If the current token is VAR we have a variable declaration and it's parsed as such, else
    /// it's a normal statement. If there is an error it's synced and an empty expression (Nil) is returned
    fn declaration(&mut self) -> Stmt {
        let parser_res;

        if self.match_types(&[TokenType::VAR]) {
            parser_res = self.var_declaration();
        } else if self.match_types(&[TokenType::VAL]) {
            parser_res = self.const_declaration();
        } else {
            parser_res = self.statement();
        }

        match parser_res {
            Ok(stmt) => stmt,
            _ => {
                self.sync();
                Stmt::Expression(Expr::empty())
            }
        }
    }

    /// Declare const with VAL keyword
    fn const_declaration(&mut self) -> ParseResult<Stmt> {
        let name: Token;

        match self.consume(TokenType::IDENTIFIER, "Expected a const name".to_string()) {
            Ok(t) => name = t.clone(),
            _ => return Err(ParseError),
        };

        let mut initializer = Expr::empty();

        if self.match_types(&[TokenType::EQUAL]) {
            match self.expression() {
                Ok(expr) => initializer = expr,
                _ => return Err(ParseError),
            };
        }

        match self.consume(
            TokenType::SEMICOLON,
            String::from("Expected ';' after val declaration"),
        ) {
            Err(_) => return Err(ParseError),
            _ => (),
        }

        Ok(Stmt::new_var(name, initializer, VarType::VAL))
    }

    /// Gets var name from IDENTIFIER Token, then checks if there is an expression following declaration.
    /// If not variable is initialized as Nil
    fn var_declaration(&mut self) -> ParseResult<Stmt> {
        let name: Token;

        match self.consume(
            TokenType::IDENTIFIER,
            String::from("Expected a variable name."),
        ) {
            Ok(t) => name = t.clone(),
            _ => return Err(ParseError),
        };

        let mut initializer = Expr::empty();

        if self.match_types(&[TokenType::EQUAL]) {
            match self.expression() {
                Ok(expr) => initializer = expr,
                _ => return Err(ParseError),
            }
        };

        match self.consume(
            TokenType::SEMICOLON,
            String::from("Expected ';' after variable declaration"),
        ) {
            Err(_) => return Err(ParseError),
            _ => (),
        }

        Ok(Stmt::new_var(name, initializer, VarType::VAR))
    }

    /// Get a statement from the current expression
    fn statement(&mut self) -> ParseResult<Stmt> {
        match self.get_curr_type_non_advance() {
            TokenType::PRINT => {
                self.advance();
                self.print_stmt()
            }
            TokenType::LEFT_BRACE => {
                self.advance();
                Ok(Stmt::Block(self.block()))
            }
            TokenType::IF => {
                self.advance();
                self.if_stmt()
            }
            _ => self.expr_stmt(),
        }
    }

    fn if_stmt(&mut self) -> ParseResult<Stmt> {
        let _ = self.consume(
            TokenType::DOUBLE_SQUARE_LEFT,
            String::from("Expected '[[' before an if statement condition"),
        );

        let condition_expr: Expr;

        match self.equality() {
            Ok(expr) => condition_expr = expr,
            Err(_) => return Err(ParseError),
        };

        let _ = self.consume(
            TokenType::DOUBLE_SQUARE_RIGHT,
            String::from("Expected ']]' after an if statement condition"),
        );

        let if_block: Stmt;

        match self.statement() {
            Ok(stmt) => if_block = stmt,
            Err(_) => return Err(ParseError),
        };

        Ok(Stmt::If {
            condition: condition_expr,
            if_block: Box::from(if_block),
        })
    }

    /// Parse a block of code and return a vector of statements in it
    /// block -> "{" declaration* "}"
    fn block(&mut self) -> Vec<Stmt> {
        let mut stmts = vec![];

        while !self.check_curr_type(TokenType::RIGHT_BRACE) && !self.is_at_end() {
            stmts.push(self.declaration());
        }

        let _ = self.consume(
            TokenType::RIGHT_BRACE,
            String::from("Expect '}' after a block"),
        );

        stmts
    }

    /// Get a print statement from the current expression
    fn print_stmt(&mut self) -> ParseResult<Stmt> {
        let expr: Expr;
        match self.expression() {
            Ok(res) => expr = res,
            Err(_) => return Err(ParseError),
        };

        match self.consume(
            TokenType::SEMICOLON,
            "Expect ';' after a statement".to_string(),
        ) {
            Ok(_) => Ok(Stmt::Print(expr)),
            Err(_) => Err(ParseError),
        }
    }

    /// Get an expression statement from the current expression.
    fn expr_stmt(&mut self) -> ParseResult<Stmt> {
        let expr: Expr;
        match self.expression() {
            Ok(res) => expr = res,
            Err(_) => return Err(ParseError),
        };

        match self.consume(
            TokenType::SEMICOLON,
            "Expect ';' after a statement".to_string(),
        ) {
            Ok(_) => Ok(Stmt::Expression(expr)),
            Err(_) => Err(ParseError),
        }
    }

    /// Top most expression maps directly to equality <br>
    /// expression -> equality
    fn expression(&mut self) -> ParseResult<Expr> {
        self.assignment()
    }

    /// Assignment
    /// assign -> IDNETIFIER "=" assignment | equality
    fn assignment(&mut self) -> ParseResult<Expr> {
        let expr_res: ParseResult<Expr> = self.equality();

        if self.match_types(&[TokenType::EQUAL]) {
            let previous_token: Token = self.prev().clone();

            let var_value: ParseResult<Expr> = self.assignment();

            return match expr_res {
                Ok(expr) => match expr {
                    Expr::Var { name } => match var_value {
                        Ok(var_value_expression) => Ok(Expr::Assign {
                            name,
                            value: Box::from(var_value_expression),
                        }),
                        _ => Err(ParseError),
                    },
                    _ => {
                        dairy_hater::error_token(
                            &previous_token,
                            String::from("Invalid assignment target."),
                        );
                        Err(ParseError)
                    }
                },
                Err(_) => Err(ParseError),
            };
        }

        expr_res
    }

    /// Mapping for the equality expression type to a rust function <br>
    /// Initially expr is created from comparision expression. <br>
    /// Then while the next couple types are equality types,
    /// we get the token and the the right sub tree and add it to the expression <br>
    /// equality -> comparison (("!=" | "==") comparison )* ;
    fn equality(&mut self) -> ParseResult<Expr> {
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
    fn comparison(&mut self) -> ParseResult<Expr> {
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
    fn term(&mut self) -> ParseResult<Expr> {
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
    fn factor(&mut self) -> ParseResult<Expr> {
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
    fn unary(&mut self) -> ParseResult<Expr> {
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

        self.primary()
    }

    /// Primary is the only terminal in our language and can come out to STRING, NUMBER,
    /// and constant values <br>
    /// But there is a case with ( and ) where we have to evaluate an expression,
    /// which is evaluated and a grouping is returned <br>
    /// primary -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    fn primary(&mut self) -> ParseResult<Expr> {
        match self.get_curr_type() {
            TokenType::FALSE => Ok(Expr::Literal {
                value: Value::Bool(false),
            }),
            TokenType::TRUE => Ok(Expr::Literal {
                value: Value::Bool(true),
            }),
            TokenType::VICTIM => Ok(Expr::Literal { value: Value::Nil }),
            TokenType::NUMBER => Ok(Expr::Literal {
                value: self.prev().literal.clone(),
            }),
            TokenType::STRING => Ok(Expr::Literal {
                value: self.prev().literal.clone(),
            }),
            TokenType::IDENTIFIER => Ok(Expr::Var {
                name: self.prev().clone(),
            }),
            TokenType::LEFT_PAREN => {
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
                        }
                        Err(err) => return Err(err),
                    },
                    Err(err) => return Err(err),
                };
            }
            _ => Err(Self::error(
                self.prev(),
                "Variable does not exist".to_string(),
            )),
        }
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

    fn get_curr_type(&mut self) -> TokenType {
        let token_type = self.peek().token_type;
        self.advance();
        token_type
    }

    fn get_curr_type_non_advance(&self) -> TokenType {
        self.peek().token_type
    }

    fn error(token: &Token, msg: String) -> ParseError {
        dairy_hater::error_token(&token, msg);
        ParseError
    }

    /// Consumes the current token and returns the next one. <br>
    /// Throws error if the current token is not of the given type.
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
                TokenType::RIGHT_BRACE => return,
                TokenType::LEFT_BRACE => return,
                _ => {}
            }

            self.advance();
        }
    }
}
