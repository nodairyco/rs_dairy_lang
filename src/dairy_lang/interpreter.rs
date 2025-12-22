use crate::{
    dairy_hater,
    dairy_lang::{
        expression::{Expr, Visitor},
        token::{Token, TokenType, Value},
    },
};

pub struct Interpreter;

impl Interpreter {
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

    pub fn interprete(&mut self, expr: &mut Expr) -> String {
        let val = self.evaluate(expr).unwrap();

        format!("{}", val)
    }
}

#[derive(Debug)]
struct EvalError;
type EvalResult = Result<Value, EvalError>;

impl Visitor<EvalResult> for Interpreter {
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

        return match (left_val, right_val) {
            (Value::Number(l), Value::Number(r)) => match operator.token_type {
                TokenType::MINUS => Ok(Value::Number(l - r)),
                TokenType::PLUS => Ok(Value::Number(l + r)),
                TokenType::STAR => Ok(Value::Number(l * r)),
                TokenType::SLASH => {
                    dairy_hater::error_token(&operator, format!("division by zero"));
                    Ok(Value::Number(l / r))
                }
                TokenType::GREATER => Ok(Value::Bool(l > r)),
                TokenType::LESS => Ok(Value::Bool(l < r)),
                TokenType::GREATER_EQUAL => Ok(Value::Bool(l >= r)),
                TokenType::LESS_EQUAL => Ok(Value::Bool(l <= r)),
                _ => {
                    dairy_hater::error_token(
                        &operator,
                        format!("unsupported operation {} for number", operator.lexem),
                    );
                    Err(EvalError)
                }
            },
            (Value::Str(l), Value::Str(r)) => match operator.token_type {
                TokenType::PLUS => Ok(Value::Str(l + &r)),
                _ => {
                    dairy_hater::error_token(
                        &operator,
                        format!("unsupported operation {} for string", operator.lexem),
                    );
                    Err(EvalError)
                }
            },
            (Value::Str(l), Value::Number(r)) => match operator.token_type {
                TokenType::PLUS => Ok(Value::Str(l + &r.to_string())),
                _ => {
                    dairy_hater::error_token(
                        &operator,
                        format!("unsupported operation {} for string", operator.lexem),
                    );
                    Err(EvalError)
                }
            },
            (Value::Number(l), Value::Str(r)) => match operator.token_type {
                TokenType::PLUS => Ok(Value::Str(l.to_string() + &r)),
                _ => {
                    dairy_hater::error_token(
                        &operator,
                        format!("unsupported operation {} for string", operator.lexem),
                    );
                    Err(EvalError)
                }
            },
            (Value::Bool(l), Value::Bool(r)) => match operator.token_type {
                TokenType::AND => Ok(Value::Bool(l && r)),
                TokenType::OR => Ok(Value::Bool(l || r)),
                _ => {
                    dairy_hater::error_token(
                        &operator,
                        format!("unsupported operation {} for bool", operator.lexem),
                    );
                    Err(EvalError)
                }
            },
            (_, _) => {
                dairy_hater::error_token(
                    &operator,
                    format!(
                        "unsupported types l:{:?}, r:{:?} for {}",
                        left, right, operator.lexem
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
                        format!("unsupported operation {} for number", operator.lexem),
                    );
                    Err(EvalError)
                }
            },
            Value::Bool(_) => match operator.token_type {
                TokenType::BANG => Ok(Value::Bool(!Self::is_truthy(&val))),
                _ => {
                    dairy_hater::error_token(
                        &operator,
                        format!("unsupported operation {} for bool", operator.lexem),
                    );
                    Err(EvalError)
                }
            },
            _ => {
                dairy_hater::error_token(&operator, format!("unsupported type {}", operator.lexem));
                Err(EvalError)
            }
        }
    }
}
