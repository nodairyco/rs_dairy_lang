use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::dairy_lang::token::Token;
use crate::dairy_lang::value::{BuiltinType, Value};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Modifier {
    VAR,
    VAL,
}

#[derive(Clone, Debug)]
pub struct EnvValue {
    pub value: Value,
    var_type: BuiltinType,
    var_modifier: Modifier,
}

#[derive(Clone)]
pub struct Environment {
    values: HashMap<Rc<str>, EnvValue>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

pub struct EnvError {
    pub error_token: Rc<Token>,
    pub error_msg: Rc<str>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn from_enclosing_env(enclosing_env: Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
            enclosing: Some(enclosing_env),
            values: HashMap::new(),
        }))
    }

    pub fn define(
        &mut self,
        name: Rc<str>,
        value: Value,
        var_type: BuiltinType,
        var_modifier: Modifier,
    ) {
        self.values.insert(
            name,
            EnvValue {
                value,
                var_type,
                var_modifier,
            },
        );
    }

    pub fn get(&self, name: &Token) -> Result<EnvValue, EnvError> {
        if let Some(env_val) = self.values.get(&name.lexem) {
            return Ok(env_val.clone());
        }

        if let Some(env) = &self.enclosing {
            return env.borrow().get(name);
        }

        Err(EnvError {
            error_token: Rc::from(name.clone()),
            error_msg: Rc::from("Variable does not exist"),
        })
    }

    pub fn assign(&mut self, name: &Token, value: &Value) -> Result<(), EnvError> {
        if self.values.contains_key(&name.lexem) {
            let mut var: EnvValue = self.get(name)?;

            if var.var_modifier == Modifier::VAL && var.value != Value::Nil {
                return Err(EnvError {
                    error_token: Rc::from(name.clone()),
                    error_msg: Rc::from("Cannot assign to a constant value"),
                });
            }

            let temp_type = BuiltinType::from(value);

            if var.var_type != BuiltinType::Unknown && temp_type != var.var_type {
                return Err(EnvError {
                    error_token: Rc::from(name.clone()),
                    error_msg: Rc::from(format!(
                        "Type mismatch. Variable {} has type {:?} != {:?}",
                        name.lexem, var.var_type, temp_type
                    )),
                });
            }

            if var.var_type == BuiltinType::Unknown {
                var.var_type = temp_type;
            }

            var.value = value.clone();

            let name_env_val_tuple = (name.lexem.clone(), var);

            self.values
                .insert(name_env_val_tuple.0, name_env_val_tuple.1);

            return Ok(());
        }

        if let Some(enclosing_env) = &self.enclosing {
            return enclosing_env.borrow_mut().assign(name, value);
        }

        Err(EnvError {
            error_token: Rc::from(name.clone()),
            error_msg: Rc::from("Variable with this name does not exist"),
        })
    }
}
