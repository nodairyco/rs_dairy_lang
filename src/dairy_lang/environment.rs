use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    dairy_hater,
    dairy_lang::token::{Token, Value},
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum VarType {
    VAR,
    VAL,
}

#[derive(Clone, Debug)]
struct EnvValue {
    value: Value,
    var_type: VarType,
}

#[derive(Clone)]
pub struct Environment {
    values: HashMap<Rc<str>, EnvValue>,
    enclosing: Option<Rc<RefCell<Environment>>>,
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

    pub fn define(&mut self, name: Rc<str>, value: Value, var_type: VarType) {
        self.values.insert(name, EnvValue { value, var_type });
    }

    pub fn get(&self, name: &Token) -> Option<Value> {
        if self.values.contains_key(&name.lexem) {
            return Some(
                self.values
                    .get(&name.lexem)
                    .expect("var not is there")
                    .value
                    .clone(),
            );
        }

        if let Some(env) = &self.enclosing {
            return env.borrow().get(name).clone();
        }

        None
    }

    pub fn get_type(&self, name: &Token) -> Option<&VarType> {
        if self.values.contains_key(&name.lexem) {
            return Some(
                &self
                    .values
                    .get(&name.lexem)
                    .expect("var not is there")
                    .var_type,
            );
        }

        None
    }

    pub fn assign(&mut self, name: &Token, value: &Value) {
        if self.values.contains_key(&name.lexem) {
            let var_type = self.get_type(name);
            let var = self.get(name);

            if var_type == Some(&VarType::VAL) && var != Some(Value::Nil) {
                dairy_hater::error_token(name, "Cannot assign to a constant");
                return;
            }

            let name_env_val_tuple = (
                name.lexem.clone(),
                EnvValue {
                    value: value.clone(),
                    var_type: var_type.unwrap().clone(),
                },
            );

            self.values
                .insert(name_env_val_tuple.0, name_env_val_tuple.1);

            return;
        }

        if let Some(enclosing_env) = &self.enclosing {
            return enclosing_env.borrow_mut().assign(name, value);
        }

        dairy_hater::error_token(name, "Variable with this name does not exist");
    }
}
