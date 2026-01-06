use std::{collections::HashMap, rc::Rc};

use crate::{
    dairy_hater,
    dairy_lang::token::{Token, Value},
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum VarType {
    VAR,
    VAL,
    NONE,
}

#[derive(Clone, Debug)]
struct EnvValue {
    value: Value,
    var_type: VarType,
}

#[derive(Clone)]
pub struct Environment {
    values: HashMap<Rc<str>, EnvValue>,
    enclosing: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn from_enclosing_env(enclosing_env: Self) -> Self {
        Environment {
            enclosing: Some(Box::from(enclosing_env)),
            ..Self::new()
        }
    }

    pub fn define(&mut self, name: Rc<str>, value: Value, var_type: VarType) {
        self.values.insert(name, EnvValue { value, var_type });
    }

    pub fn get(&self, name: &Token) -> &Value {
        if self.values.contains_key(&name.lexem) {
            return &self
                .values
                .get(&name.lexem)
                .expect("var not is there")
                .value;
        }

        match &self.enclosing {
            Some(env) => return env.get(name),
            None => {
                dairy_hater::error_token(&name, format!("Undefined variable, {}", name.lexem));
                &Value::Void
            }
        }
    }

    pub fn get_type(&self, name: &Token) -> &VarType {
        if self.values.contains_key(&name.lexem) {
            return &self
                .values
                .get(&name.lexem)
                .expect("var not is there")
                .var_type;
        }

        &VarType::NONE
    }

    pub fn assign(&mut self, name: &Token, value: &Value) {
        if self.values.contains_key(&name.lexem) {
            let var_type = self.get_type(name);
            let var = self.get(name);

            if var_type == &VarType::VAL && var != &Value::Nil {
                dairy_hater::error_token(name, String::from("Cannot assign to a constant"));
                return;
            }

            let name_env_val_tuple = (
                name.lexem.clone(),
                EnvValue {
                    value: value.clone(),
                    var_type: var_type.clone(),
                },
            );

            self.values
                .insert(name_env_val_tuple.0, name_env_val_tuple.1);

            return;
        }

        match &mut self.enclosing {
            Some(enclosing_env) => {
                enclosing_env.assign(name, value);
                return;
            }
            None => {}
        }

        dairy_hater::error_token(name, String::from("Variable with this name does not exist"));
    }
}
