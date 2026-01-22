use std::{
    cell::RefCell,
    io::{self, BufWriter, Stdout, Write},
    rc::Rc,
};

use crate::dairy_lang::value::Value;
use crate::dairy_lang::{
    environment::{EnvError, Environment, Modifier},
    expression::{self, Expr},
    stmt::{self, Stmt},
    token::{Token, TokenType},
    value::BuiltinType,
};

/// This struct handles executing statements. Contains a reference to an environment where
/// variables are stored, and a BufWriter where printed values are written to, then gets flushed at
/// the end of the program.<br>
/// This type implements both Visitors of Stmt and Expr and can visit and evaluate them.
pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
    printer: BufWriter<Stdout>,
}

/// Error type for when an error is caught during execution. If at any point is caught program
/// stops evaluating and panics with an error. <br>
/// Using Rc and .clone() for this type results in no performance overhead, since if it is caught
/// the program terminates.
#[derive(Debug)]
pub struct EvalError {
    pub error_token: Rc<Token>,
    pub error_msg: Rc<str>,
}

// Commonly used type short hands
type EvalResult = Result<Value, EvalError>;
type StmtResult = Result<(), EvalError>;

/// Used to convert from EnvError to an EvalError, since they are fundumentally the same types.
impl From<EnvError> for EvalError {
    fn from(value: EnvError) -> Self {
        EvalError {
            error_token: value.error_token,
            error_msg: value.error_msg,
        }
    }
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            env: Rc::new(RefCell::new(Environment::new())),
            printer: BufWriter::new(io::stdout()),
        }
    }

    /// Takes in a vec of statements and executes them. If at any point an error is caught eval
    /// execution stops and the BufWriter is flushed.
    pub fn interpret(&mut self, stmts: &mut Vec<Stmt>) -> Result<(), EvalError> {
        let mut err_option = None;

        for stmt in stmts {
            if let Err(eval_error) = self.execute_stmt(stmt) {
                err_option = Some(eval_error);
                break;
            }
        }

        let _ = self.printer.flush();

        if let Some(err) = err_option {
            return Err(err);
        }

        Ok(())
    }

    /// Evaluate a given expression.
    fn evaluate_expr(&mut self, expr: &mut Expr) -> EvalResult {
        expr.accept(self)
    }

    /// Evaluate a given statement.
    fn execute_stmt(&mut self, stmt: &mut Stmt) -> StmtResult {
        stmt.accept(self)
    }

    /// Check if this value is truthy. AKA not null, or true bool value.
    fn is_truthy(val: &Value) -> bool {
        match val {
            Value::Nil => false,
            Value::Bool(b) => *b,
            _ => true,
        }
    }

    /// Check if 2 values are equal to each other.
    fn is_equal(left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::Nil, Value::Nil) => true,
            (Value::Nil, _) => false,
            (_, Value::Nil) => false,
            (_, _) => left == right,
        }
    }

    /// Execute statements inside a block statement. Execution is stopped when an error is caught.
    fn execute_block(
        &mut self,
        stmts: &mut Vec<Stmt>,
        new_env: Rc<RefCell<Environment>>,
    ) -> Result<(), EvalError> {
        let prev_env = self.env.clone();
        self.env = new_env;

        for stmt in stmts {
            self.execute_stmt(stmt)?;
        }

        self.env = prev_env;
        Ok(())
    }

    /// Evaluate the conditional expression of an if or a while statement.<br>
    /// Throws error if the value of the condition is not bool. <br>
    /// Caller is used for error handling
    fn evaluate_condition(
        &mut self,
        condition: &mut Expr,
        caller: &mut Token,
    ) -> Result<bool, EvalError> {
        match self.evaluate_expr(condition)? {
            Value::Bool(bool) => Ok(bool),
            _ => Err(EvalError {
                error_token: Rc::from(caller.clone()),
                error_msg: Rc::from(
                    "Cannot have a none bool value in a conditional statement's condition",
                ),
            }),
        }
    }
}

impl stmt::Visitor<StmtResult> for Interpreter {
    /// Evaluate the expression of a print statement and add it to the BufWriter.
    fn visit_print_stmt(&mut self, print_expr: &mut Expr) -> StmtResult {
        let val = self.evaluate_expr(print_expr)?;

        let _ = self.printer.write_fmt(format_args!("{val}\n"));

        Ok(())
    }

    /// Evalute an expression statment. Expressions statements are stand-alone statements
    fn visit_expr_stmt(&mut self, expr_expr: &mut Expr) -> StmtResult {
        self.evaluate_expr(expr_expr)?;
        Ok(())
    }

    /// Execute var declaration statement. If the initializer Expression is not empty
    fn visit_var_stmt(
        &mut self,
        name: &mut Token,
        initializer: &mut Expr,
        var_modifier: Modifier,
        mut var_type: BuiltinType,
    ) -> StmtResult {
        let mut val = Value::Nil;

        if initializer != &mut Expr::empty() {
            val = self.evaluate_expr(initializer)?;
        }

        let temp_type = BuiltinType::from(&val);

        if val != Value::Nil {
            if var_type != BuiltinType::Unknown && temp_type != var_type {
                return Err(EvalError {
                    error_token: Rc::from(name.clone()),
                    error_msg: Rc::from(format!(
                        "Type Mistmatch. Static var type does not match expression type.\n\t {:?}!={:?}",
                        var_type, temp_type
                    )),
                });
            }

            var_type = temp_type;
        }
        self.env
            .borrow_mut()
            .define(name.lexem.clone(), val, var_type, var_modifier);

        Ok(())
    }

    /// Execute a collection of statements in the current block.
    fn visit_block(&mut self, stmts: &mut Vec<Stmt>) -> StmtResult {
        self.execute_block(stmts, Environment::from_enclosing_env(self.env.clone()))?;

        Ok(())
    }

    /// Execute an if statement. If the condition expression is truthy executes if block, else if
    /// the else_block is not empty executes that.
    fn visit_if(
        &mut self,
        condition: &mut Expr,
        if_block: &mut Stmt,
        else_block: &mut Option<Box<Stmt>>,
        caller: &mut Token,
    ) -> StmtResult {
        let condition_bool: bool = self.evaluate_condition(condition, caller)?;

        if condition_bool {
            return self.execute_stmt(if_block);
        } else if let Some(else_stmt) = else_block {
            return self.execute_stmt(else_stmt);
        }

        Ok(())
    }

    /// Execute a while statement. While the condition expression is truthy executes the contents.
    fn visit_while(
        &mut self,
        condition: &mut Expr,
        while_block: &mut Stmt,
        caller: &mut Token,
    ) -> StmtResult {
        while self.evaluate_condition(condition, caller)? {
            self.execute_stmt(while_block)?;
        }

        Ok(())
    }
}

impl expression::Visitor<EvalResult> for Interpreter {
    /// Evaluate binary expressions. Matches the types of left and right to determine which
    /// operatos can be used and how.
    fn visit_binary(
        &mut self,
        left: &mut Expr,
        operator: &mut Token,
        right: &mut Expr,
    ) -> EvalResult {
        // evaluate left and right side of expression
        let left_val = self.evaluate_expr(left)?;
        let right_val = self.evaluate_expr(right)?;

        // check for ==
        if operator.token_type == TokenType::EQUAL_EQUAL {
            return Ok(Value::Bool(Self::is_equal(&left_val, &right_val)));
        }

        // check for !=
        if operator.token_type == TokenType::BANG_EQUAL {
            return Ok(Value::Bool(!Self::is_equal(&left_val, &right_val)));
        }

        // match the values of left and right
        match (&left_val, &right_val) {
            (Value::Number(l), Value::Number(r)) => match operator.token_type {
                TokenType::MINUS => Ok(Value::Number(l - r)),
                TokenType::PLUS => Ok(Value::Number(l + r)),
                TokenType::STAR => Ok(Value::Number(l * r)),
                TokenType::SLASH => {
                    if *r == 0.0 {
                        Err(EvalError {
                            error_token: Rc::from(operator.clone()),
                            error_msg: Rc::from("Division by zero"),
                        })
                    } else {
                        Ok(Value::Number(l / r))
                    }
                }
                TokenType::GREATER => Ok(Value::Bool(l > r)),
                TokenType::LESS => Ok(Value::Bool(l < r)),
                TokenType::GREATER_EQUAL => Ok(Value::Bool(l >= r)),
                TokenType::LESS_EQUAL => Ok(Value::Bool(l <= r)),
                TokenType::MODULO => Ok(Value::Number(l % r)),
                TokenType::LONG_ARROW => {
                    let mut res: Vec<Value> = Vec::new();
                    if *l > *r {
                        (*r as i64..*l as i64)
                            .rev()
                            .for_each(|f| res.push(Value::Number(f as f64)));
                    } else {
                        (*l as i64..*r as i64).for_each(|f| res.push(Value::Number(f as f64)));
                    }

                    Ok(Value::List(res))
                }
                _ => Err(EvalError {
                    error_token: Rc::from(operator.clone()),
                    error_msg: Rc::from(
                        format!("unsupported operation {} for number", operator.lexem).as_str(),
                    ),
                }),
            },
            (Value::Str(l), Value::Str(r)) => match operator.token_type {
                TokenType::PLUS => Ok(Value::Str(Rc::from(format!("{}{}", l, r)))),
                _ => Err(EvalError {
                    error_token: Rc::from(operator.clone()),
                    error_msg: Rc::from(
                        format!("unsupported operation {} for string", operator.lexem).as_str(),
                    ),
                }),
            },
            (Value::Str(l), Value::Number(r)) => match operator.token_type {
                TokenType::PLUS => Ok(Value::Str(Rc::from(format!("{}{}", l, r)))),
                _ => Err(EvalError {
                    error_token: Rc::from(operator.clone()),
                    error_msg: Rc::from(
                        format!("unsupported operation {} for string", operator.lexem).as_str(),
                    ),
                }),
            },
            (Value::Number(l), Value::Str(r)) => match operator.token_type {
                TokenType::PLUS => Ok(Value::Str(Rc::from(format!("{}{}", l, r)))),
                _ => Err(EvalError {
                    error_token: Rc::from(operator.clone()),
                    error_msg: Rc::from(
                        format!("unsupported operation {} for string", operator.lexem).as_str(),
                    ),
                }),
            },
            (Value::Bool(l), Value::Bool(r)) => match operator.token_type {
                TokenType::AND => Ok(Value::Bool(*l && *r)),
                TokenType::OR => Ok(Value::Bool(*l || *r)),
                TokenType::XOR => Ok(Value::Bool(*l ^ *r)),
                _ => Err(EvalError {
                    error_token: Rc::from(operator.clone()),
                    error_msg: Rc::from(
                        format!("unsupported operation {} for bool", operator.lexem).as_str(),
                    ),
                }),
            },
            (Value::List(vec), Value::Number(num)) => match operator.token_type {
                TokenType::STAR => {
                    let mut res: Vec<Value> = Vec::new();

                    for _ in 0..*num as u64 {
                        for v in vec {
                            res.push(v.clone());
                        }
                    }

                    Ok(Value::List(res))
                }
                _ => Err(EvalError {
                    error_token: Rc::from(operator.clone()),
                    error_msg: Rc::from(
                        format!("unsupported operation {} for bool", operator.lexem).as_str(),
                    ),
                }),
            },
            (_, _) => Err(EvalError {
                error_token: Rc::from(operator.clone()),
                error_msg: Rc::from(
                    format!(
                        "unsupported types l:{:?}, r:{:?} for {}",
                        left_val, right_val, operator.lexem
                    )
                    .as_str(),
                ),
            }),
        }
    }

    /// Evaluate a grouping expression. (just evaluates the expression inside the grouping)
    fn visit_grouping(&mut self, expr: &mut Expr) -> EvalResult {
        self.evaluate_expr(expr)
    }

    /// Returns the given value
    fn visit_literal(&mut self, val: &mut Value) -> EvalResult {
        Ok(val.clone())
    }

    /// Evaluates the right hand side of the unary expression, then applies the operator to the
    /// value
    fn visit_unary(&mut self, operator: &mut Token, right: &mut Expr) -> EvalResult {
        let val = self.evaluate_expr(right)?;
        match val {
            Value::Number(num) => match operator.token_type {
                TokenType::MINUS => Ok(Value::Number(-num)),
                _ => Err(EvalError {
                    error_token: Rc::from(operator.clone()),
                    error_msg: Rc::from(
                        format!("unsupported operation {} for number", operator.lexem).as_str(),
                    ),
                }),
            },
            Value::Bool(_) => match operator.token_type {
                TokenType::BANG => Ok(Value::Bool(!Self::is_truthy(&val))),
                _ => Err(EvalError {
                    error_token: Rc::from(operator.clone()),
                    error_msg: Rc::from(
                        format!("unsupported operation {} for bool", operator.lexem).as_str(),
                    ),
                }),
            },
            _ => Err(EvalError {
                error_token: Rc::from(operator.clone()),
                error_msg: Rc::from(format!("unsupported type {}", operator.lexem).as_str()),
            }),
        }
    }

    /// Returns the value from the current environment
    fn visit_var(&mut self, var_name: &mut Token) -> EvalResult {
        Ok(self.env.borrow().get(var_name)?.value)
    }

    /// Evaluates the right hand side of the expression and returns it as value if the assignment
    /// was successful. Then assigns that value to the variable with the given name.
    fn visit_assign(&mut self, var_name: &mut Token, val_expr: &mut Expr) -> EvalResult {
        let var_val = self.evaluate_expr(val_expr)?;

        self.env.borrow_mut().assign(var_name, &var_val)?;
        Ok(var_val)
    }

    /// Evaluates a list expression
    fn visit_list(&mut self, values: &mut Vec<Expr>, caller: &mut Token) -> EvalResult {
        let mut vals: Vec<Value> = Vec::new();
        let mut curr_type: BuiltinType = BuiltinType::Unknown;

        for val in values {
            let expr_eval = self.evaluate_expr(val)?;
            let expr_type = BuiltinType::from(&expr_eval);

            if curr_type == BuiltinType::Unknown {
                curr_type = expr_type;
            } else if curr_type != expr_type {
                return Err(EvalError {
                    error_token: Rc::from(caller.clone()),
                    error_msg: Rc::from(format!(
                        "Type Mismatch. Cannot have multiple types in the same list ({:?}, {:?})",
                        curr_type, expr_type,
                    )),
                });
            }

            vals.push(expr_eval);
        }

        Ok(Value::List(vals))
    }

    /// Evaluates an index expression
    fn visit_index(
        &mut self,
        index: &mut Box<Expr>,
        originator: &mut Box<Expr>,
        caller: &mut Token,
    ) -> EvalResult {
        let index_val = self.evaluate_expr(index)?;

        let originator_val = self.evaluate_expr(originator)?;

        return if let Value::Number(i) = index_val {
            match originator_val {
                Value::Str(str) => {
                    let char_res = str.chars().nth(if i >= 0f64 {
                        i as usize
                    } else {
                        (str.len() as f64 + i) as usize
                    });

                    return if let Some(chr) = char_res
                        && i.abs() <= str.len() as f64
                    {
                        Ok(Value::Str(Rc::from(String::from(chr))))
                    } else {
                        Err(EvalError {
                            error_token: Rc::from(caller.clone()),
                            error_msg: Rc::from(format!("Index {} out of bounds for {}", i, str)),
                        })
                    };
                }
                Value::List(values) => {
                    return if let Some(val) = values.get(if i >= 0f64 {
                        i as usize
                    } else {
                        (values.len() as f64 + i) as usize
                    }) && i.abs() <= values.len() as f64
                    {
                        Ok(val.clone())
                    } else {
                        Err(EvalError {
                            error_token: Rc::from(caller.clone()),
                            error_msg: Rc::from(format!(
                                "Index {} out of bounds for list of size {}",
                                i,
                                values.len()
                            )),
                        })
                    };
                }
                _type => Err(EvalError {
                    error_token: Rc::from(caller.clone()),
                    error_msg: Rc::from(format!(
                        "{} evaluated to a non-indexable type, {:?}",
                        caller.lexem, _type
                    )),
                }),
            }
        } else {
            Err(EvalError {
                error_token: Rc::from(caller.clone()),
                error_msg: Rc::from(format!("Index {}, is not a number", index_val)),
            })
        };
    }
}
