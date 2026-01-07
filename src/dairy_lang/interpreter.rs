use std::{
    cell::RefCell,
    io::{self, BufWriter, Stdout, Write},
    rc::Rc,
};

use crate::{
    dairy_hater,
    dairy_lang::{
        environment::{Environment, VarType},
        expression::{self, Expr},
        stmt::{self, Stmt},
        token::{Token, TokenType, Value},
    },
};

pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
    printer: BufWriter<Stdout>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            env: Rc::new(RefCell::new(Environment::new())),
            printer: BufWriter::new(io::stdout()),
        }
    }

    fn evaluate(&mut self, expr: &mut Expr) -> EvalResult {
        expr.accept(self)
    }

    fn is_truthy(val: &Value) -> bool {
        match val {
            Value::Nil => false,
            Value::Bool(b) => *b,
            _ => true,
        }
    }

    fn is_equal(left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::Nil, Value::Nil) => true,
            (Value::Nil, _) => false,
            (_, Value::Nil) => false,
            (_, _) => left == right,
        }
    }

    pub fn interpret(&mut self, stmts: &mut Vec<Stmt>) {
        for stmt in stmts {
            let _ = self.execute_stmt(stmt);
        }
        let _ = self.printer.flush();
    }

    fn execute_stmt(&mut self, stmt: &mut Stmt) -> StmtResult {
        stmt.accept(self)
    }

    fn execute_block(&mut self, stmts: &mut Vec<Stmt>, new_env: Rc<RefCell<Environment>>) {
        let prev_env = self.env.clone();
        self.env = new_env;

        for stmt in stmts {
            match self.execute_stmt(stmt) {
                Err(_) => {
                    break;
                }
                _ => {}
            };
        }

        self.env = prev_env;
    }

    fn evaluate_condition(
        &mut self,
        condition: &mut Expr,
        caller: &mut Token,
    ) -> Result<bool, EvalError> {
        let condition_value: Value;

        match self.evaluate(condition) {
            Err(_) => return Err(EvalError),
            Ok(val) => condition_value = val,
        };

        match condition_value {
            Value::Bool(bool) => Ok(bool),
            _ => {
                dairy_hater::error_token(
                    caller,
                    "Cannot have a none bool value in an if statement's condition",
                );
                Err(EvalError)
            }
        }
    }
}

#[derive(Debug)]
struct EvalError;
type EvalResult = Result<Value, EvalError>;
type StmtResult = Result<(), EvalError>;

impl stmt::Visitor<StmtResult> for Interpreter {
    fn visit_print_stmt(&mut self, print_expr: &mut Expr) -> StmtResult {
        let val = self.evaluate(print_expr);

        match val {
            Ok(value) => {
                let _ = self.printer.write_fmt(format_args!("{value}\n"));
                Ok(())
            }
            Err(_) => Err(EvalError),
        }
    }

    fn visit_expr_stmt(&mut self, expr_expr: &mut Expr) -> StmtResult {
        match self.evaluate(expr_expr) {
            Ok(_) => Ok(()),
            Err(_) => Err(EvalError),
        }
    }

    fn visit_var_stmt(
        &mut self,
        name: &mut Token,
        initializer: &mut Expr,
        var_type: VarType,
    ) -> StmtResult {
        let mut val = Value::Nil;

        if initializer != &mut Expr::empty() {
            match self.evaluate(initializer) {
                Ok(res_val) => val = res_val,
                _ => return Err(EvalError),
            }
        }

        self.env
            .borrow_mut()
            .define(name.lexem.clone(), val, var_type);
        Ok(())
    }

    fn visit_block(&mut self, stmts: &mut Vec<Stmt>) -> StmtResult {
        self.execute_block(stmts, Environment::from_enclosing_env(self.env.clone()));

        Ok(())
    }

    fn visit_if(
        &mut self,
        condition: &mut Expr,
        if_block: &mut Stmt,
        else_block: &mut Option<Box<Stmt>>,
        caller: &mut Token,
    ) -> StmtResult {
        let condition_bool: bool;

        match self.evaluate_condition(condition, caller) {
            Ok(bool) => condition_bool = bool,
            _ => return Err(EvalError),
        };

        if condition_bool {
            return self.execute_stmt(if_block);
        } else if else_block.is_some() {
            return self.execute_stmt(else_block.as_mut().unwrap().as_mut());
        }

        Ok(())
    }

    fn visit_while(
        &mut self,
        condition: &mut Expr,
        while_block: &mut Stmt,
        caller: &mut Token,
    ) -> StmtResult {
        let condition_bool: bool;

        match self.evaluate_condition(condition, caller) {
            Ok(bool) => condition_bool = bool,
            _ => return Err(EvalError),
        };

        if condition_bool {
            match self.execute_stmt(while_block) {
                Err(_) => return Err(EvalError),
                _ => (),
            }
        }

        while self.evaluate_condition(condition, caller).unwrap() {
            let _ = self.execute_stmt(while_block);
        }

        Ok(())
    }
}

impl expression::Visitor<EvalResult> for Interpreter {
    fn visit_binary(
        &mut self,
        left: &mut Expr,
        operator: &mut Token,
        right: &mut Expr,
    ) -> EvalResult {
        let left_val = self.evaluate(left).unwrap();
        let right_val = self.evaluate(right).unwrap();

        if operator.token_type == TokenType::EQUAL_EQUAL {
            return Ok(Value::Bool(Self::is_equal(&left_val, &right_val)));
        }

        if operator.token_type == TokenType::BANG_EQUAL {
            return Ok(Value::Bool(!Self::is_equal(&left_val, &right_val)));
        }

        return match (&left_val, &right_val) {
            (Value::Number(l), Value::Number(r)) => match operator.token_type {
                TokenType::MINUS => Ok(Value::Number(l - r)),
                TokenType::PLUS => Ok(Value::Number(l + r)),
                TokenType::STAR => Ok(Value::Number(l * r)),
                TokenType::SLASH => {
                    dairy_hater::error_token(&operator, &format!("division by zero"));
                    Ok(Value::Number(l / r))
                }
                TokenType::GREATER => Ok(Value::Bool(l > r)),
                TokenType::LESS => Ok(Value::Bool(l < r)),
                TokenType::GREATER_EQUAL => Ok(Value::Bool(l >= r)),
                TokenType::LESS_EQUAL => Ok(Value::Bool(l <= r)),
                TokenType::MODULO => Ok(Value::Number(l % r)),
                _ => {
                    dairy_hater::error_token(
                        &operator,
                        &format!("unsupported operation {} for number", operator.lexem),
                    );
                    Err(EvalError)
                }
            },
            (Value::Str(l), Value::Str(r)) => match operator.token_type {
                TokenType::PLUS => Ok(Value::Str(Rc::from(format!("{}{}", l, r)))),
                _ => {
                    dairy_hater::error_token(
                        &operator,
                        &format!("unsupported operation {} for string", operator.lexem),
                    );
                    Err(EvalError)
                }
            },
            (Value::Str(l), Value::Number(r)) => match operator.token_type {
                TokenType::PLUS => Ok(Value::Str(Rc::from(format!("{}{}", l, r)))),
                _ => {
                    dairy_hater::error_token(
                        &operator,
                        &format!("unsupported operation {} for string", operator.lexem),
                    );
                    Err(EvalError)
                }
            },
            (Value::Number(l), Value::Str(r)) => match operator.token_type {
                TokenType::PLUS => Ok(Value::Str(Rc::from(format!("{}{}", l, r)))),
                _ => {
                    dairy_hater::error_token(
                        &operator,
                        &format!("unsupported operation {} for string", operator.lexem),
                    );
                    Err(EvalError)
                }
            },
            (Value::Bool(l), Value::Bool(r)) => match operator.token_type {
                TokenType::AND => Ok(Value::Bool(*l && *r)),
                TokenType::OR => Ok(Value::Bool(*l || *r)),
                TokenType::XOR => Ok(Value::Bool(*l ^ *r)),
                _ => {
                    dairy_hater::error_token(
                        &operator,
                        &format!("unsupported operation {} for bool", operator.lexem),
                    );
                    Err(EvalError)
                }
            },
            (_, _) => {
                dairy_hater::error_token(
                    &operator,
                    &format!(
                        "unsupported types l:{:?}, r:{:?} for {}",
                        left_val, right_val, operator.lexem
                    ),
                );
                Err(EvalError)
            }
        };
    }

    fn visit_grouping(&mut self, expr: &mut Expr) -> EvalResult {
        self.evaluate(expr)
    }

    fn visit_literal(&mut self, val: &mut Value) -> EvalResult {
        Ok(val.clone())
    }

    fn visit_unary(&mut self, operator: &mut Token, right: &mut Expr) -> EvalResult {
        let val = self.evaluate(right).unwrap();
        match val {
            Value::Number(num) => match operator.token_type {
                TokenType::MINUS => Ok(Value::Number(-num)),
                _ => {
                    dairy_hater::error_token(
                        &operator,
                        &format!("unsupported operation {} for number", operator.lexem),
                    );
                    Err(EvalError)
                }
            },
            Value::Bool(_) => match operator.token_type {
                TokenType::BANG => Ok(Value::Bool(!Self::is_truthy(&val))),
                _ => {
                    dairy_hater::error_token(
                        &operator,
                        &format!("unsupported operation {} for bool", operator.lexem),
                    );
                    Err(EvalError)
                }
            },
            _ => {
                dairy_hater::error_token(
                    &operator,
                    &format!("unsupported type {}", operator.lexem),
                );
                Err(EvalError)
            }
        }
    }

    fn visit_var(&mut self, var_name: &mut Token) -> EvalResult {
        match self.env.borrow().get(var_name) {
            Some(val) => Ok(val),
            None => Err(EvalError),
        }
    }

    fn visit_assign(&mut self, var_name: &mut Token, val_expr: &mut Expr) -> EvalResult {
        let var_val: Value;

        match self.evaluate(val_expr) {
            Ok(value) => var_val = value,
            Err(_) => return Err(EvalError),
        }

        self.env.borrow_mut().assign(var_name, &var_val);
        Ok(var_val)
    }
}
