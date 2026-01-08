use std::{rc::Rc, vec};

use crate::{
    dairy_hater,
    dairy_lang::{
        environment::Modifier,
        expression::Expr,
        stmt::Stmt,
        token::{Token, TokenType, Value},
    },
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

#[derive(Debug)]
struct ParseError {
    err_token_pos: usize,
    err_msg: Rc<str>,
}

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
            Err(ParseError {
                err_token_pos,
                err_msg,
            }) => {
                dairy_hater::error_token_non_fatal(&self.tokens[err_token_pos], &err_msg);
                self.sync();
                Stmt::Expression(Expr::empty())
            }
        }
    }

    /// Declare const with VAL keyword
    fn const_declaration(&mut self) -> ParseResult<Stmt> {
        self.create_var_decl_stmt(Modifier::VAL)
    }

    /// Gets var name from IDENTIFIER Token, then checks if there is an expression following declaration.
    /// If not variable is initialized as Nil
    fn var_declaration(&mut self) -> ParseResult<Stmt> {
        self.create_var_decl_stmt(Modifier::VAR)
    }

    /// Creates a variable declaration statement with the given variable type.
    fn create_var_decl_stmt(&mut self, var_type: Modifier) -> ParseResult<Stmt> {
        let name = self
            .consume(
                TokenType::IDENTIFIER,
                if var_type == Modifier::VAR {
                    "Expected a const name"
                } else {
                    "Expected a variable name"
                },
            )?
            .clone();

        let mut initializer = Expr::empty();

        if self.match_types(&[TokenType::EQUAL]) {
            initializer = self.expression()?;
        }

        self.consume(
            TokenType::SEMICOLON,
            if var_type == Modifier::VAR {
                "Expected ';' after variable declaration"
            } else {
                "Expected ';' after a constant declaration"
            },
        )?;

        Ok(Stmt::new_var(name, initializer, var_type))
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
            TokenType::WHILE => {
                self.advance();
                self.while_stmt()
            }
            _ => self.expr_stmt(),
        }
    }

    fn while_stmt(&mut self) -> ParseResult<Stmt> {
        let cond_stmt: (Expr, Box<Stmt>, Token) = self.get_conditional_stmt()?;

        Ok(Stmt::While {
            condition: cond_stmt.0,
            block: cond_stmt.1,
            caller: cond_stmt.2,
        })
    }

    fn if_stmt(&mut self) -> ParseResult<Stmt> {
        let cond_stmt: (Expr, Box<Stmt>, Token) = self.get_conditional_stmt()?;

        let mut else_block: Option<Box<Stmt>> = None;

        if self.match_types(&[TokenType::ELSE]) {
            else_block = Some(Box::from(self.statement()?));
        }

        Ok(Stmt::If {
            condition: cond_stmt.0,
            if_block: cond_stmt.1,
            else_block,
            caller: cond_stmt.2,
        })
    }

    /// Get condition expression, body, and the caller of a conditional statement
    fn get_conditional_stmt(&mut self) -> ParseResult<(Expr, Box<Stmt>, Token)> {
        let caller: Token = self.prev().clone();

        let msg = if caller.token_type == TokenType::IF {
            "Expected '[[' before an if statement condition"
        } else {
            "Expected '[[' before a while statement condition"
        };

        _ = self.consume(TokenType::DOUBLE_SQUARE_LEFT, msg)?;

        let condition_expr: Expr = self.equality()?;

        let msg = if caller.token_type == TokenType::IF {
            "Expected ']]' after an if statement condition"
        } else {
            "Expected ']]' after a while statement condition"
        };

        _ = self.consume(TokenType::DOUBLE_SQUARE_RIGHT, msg)?;

        let block: Stmt = self.statement()?;

        Ok((condition_expr, Box::from(block), caller))
    }

    /// Parse a block of code and return a vector of statements in it
    /// block -> "{" declaration* "}"
    fn block(&mut self) -> Vec<Stmt> {
        let mut stmts = vec![];

        while !self.check_curr_type(TokenType::RIGHT_BRACE) && !self.is_at_end() {
            stmts.push(self.declaration());
        }

        let _ = self.consume(TokenType::RIGHT_BRACE, "Expect '}' after a block");

        stmts
    }

    /// Get a print statement from the current expression
    fn print_stmt(&mut self) -> ParseResult<Stmt> {
        let expr: Expr = self.expression()?;

        _ = self.consume(TokenType::SEMICOLON, "Expect ';' after a statement")?;

        Ok(Stmt::Print(expr))
    }

    /// Get an expression statement from the current expression.
    fn expr_stmt(&mut self) -> ParseResult<Stmt> {
        let expr: Expr = self.expression()?;

        _ = self.consume(TokenType::SEMICOLON, "Expect ';' after a statement")?;

        Ok(Stmt::Expression(expr))
    }

    /// Top most expression maps directly to equality <br>
    /// expression -> equality
    fn expression(&mut self) -> ParseResult<Expr> {
        self.assignment()
    }

    /// Assignment
    /// assign -> IDNETIFIER "=" assignment | equality
    fn assignment(&mut self) -> ParseResult<Expr> {
        let mut expr: Expr = self.logical()?;

        if self.match_types(&[TokenType::EQUAL]) {
            let var_value: Expr = self.assignment()?;

            match expr {
                Expr::Var { name } => {
                    expr = Expr::Assign {
                        name,
                        value: Box::from(var_value),
                    }
                }
                _ => {
                    return Err(ParseError {
                        err_token_pos: self.current - 1,
                        err_msg: Rc::from("Invalid assignment target."),
                    });
                }
            };
        }

        Ok(expr)
    }

    /// Logical and, or, and xor operators
    /// logical -> equality (("and" | "or" | "xor") equality)* ;
    fn logical(&mut self) -> ParseResult<Expr> {
        self.create_binary_expr(
            &mut |arg: &mut Self| arg.equality(),
            &[TokenType::AND, TokenType::OR, TokenType::XOR],
        )
    }

    /// Mapping for the equality expression type to a rust function <br>
    /// Initially expr is created from comparision expression. <br>
    /// Then while the next couple types are equality types,
    /// we get the token and the the right sub tree and add it to the expression <br>
    /// equality -> comparison (("!=" | "==") comparison )* ;
    fn equality(&mut self) -> ParseResult<Expr> {
        self.create_binary_expr(
            &mut |arg: &mut Self| arg.comparison(),
            &[TokenType::EQUAL_EQUAL, TokenType::BANG_EQUAL],
        )
    }

    /// Works the same way as equality,but one level lower <br>
    /// comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> ParseResult<Expr> {
        self.create_binary_expr(
            &mut |arg: &mut Self| arg.modulos(),
            &[
                TokenType::GREATER,
                TokenType::GREATER_EQUAL,
                TokenType::LESS,
                TokenType::LESS_EQUAL,
            ],
        )
    }

    /// modulos
    fn modulos(&mut self) -> ParseResult<Expr> {
        self.create_binary_expr(&mut |arg: &mut Self| arg.term(), &[TokenType::MODULO])
    }

    /// Works in the same way as comparison <br>
    /// term -> factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self) -> ParseResult<Expr> {
        self.create_binary_expr(
            &mut |arg: &mut Self| arg.factor(),
            &[TokenType::MINUS, TokenType::PLUS],
        )
    }

    /// Works in the same way as term <br>
    /// factor -> unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> ParseResult<Expr> {
        self.create_binary_expr(
            &mut |arg: &mut Self| arg.unary(),
            &[TokenType::SLASH, TokenType::STAR],
        )
    }

    /// Parse a binary expression with the next level operation being the provided next_funct function,
    /// and necessary token types being the given token_types array
    fn create_binary_expr<F>(
        &mut self,
        next_funct: &mut F,
        token_types: &[TokenType],
    ) -> ParseResult<Expr>
    where
        F: FnMut(&mut Self) -> ParseResult<Expr>,
    {
        let mut left: Expr = next_funct(self)?;

        while self.match_types(token_types) {
            let operator: Token = self.prev().clone();

            let right: Expr = next_funct(self)?;

            left = Expr::Binary {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    /// Simply if there is a unary operation go save it and go one deeper, and so on if a unary is found <br>
    /// If not this is a primary and we just return self.primary() <br>
    /// unary -> ( "!" | "-" ) unary | primary ;
    fn unary(&mut self) -> ParseResult<Expr> {
        if self.match_types(&[TokenType::BANG, TokenType::MINUS]) {
            let operator = self.prev().clone();

            let right = self.unary()?;

            return Ok(Expr::Unary {
                operator: operator,
                right: Box::new(right),
            });
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
                let expr = self.expression()?;

                _ = self.consume(TokenType::RIGHT_PAREN, "Expect ')' after expression.")?;

                Ok(Expr::Grouping {
                    expression: Box::new(expr),
                })
            }
            TokenType::SEMICOLON => Err(ParseError {
                err_token_pos: self.current - 2,
                err_msg: Rc::from("No expression provided for the current statement"),
            }),
            _ => Err(ParseError {
                err_token_pos: self.current - 1,
                err_msg: Rc::from("Variable does not exist"),
            }),
        }
    }

    /// Get the Token at the current addr
    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    /// Get the previous Token
    fn prev(&self) -> &Token {
        &self.tokens[self.current - 1]
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

    /// Consumes the current token and returns the next one. <br>
    /// Throws error if the current token is not of the given type.
    fn consume(&mut self, token_type: TokenType, msg: &str) -> Result<&Token, ParseError> {
        if self.check_curr_type(token_type) {
            return Ok(self.advance());
        }

        return Err(ParseError {
            err_token_pos: self.current - 1,
            err_msg: Rc::from(msg),
        });
    }

    /// Function to synchronise in case an error occures. <br>
    /// If a parse error is detected, drop tokens, until a start of a new expression,
    /// or the end of the current one is found.
    fn sync(&mut self) {
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
