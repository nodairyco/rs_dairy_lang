use std::collections::HashMap;

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

struct EnvValue {
    value: Value,
    var_type: VarType,
}

pub struct Environment {
    values: HashMap<String, EnvValue>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Value, var_type: VarType) {
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

        dairy_hater::error_token(&name, format!("Undefined variable, {}", name.lexem));
        &Value::Void
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

            self.values.insert(
                name.lexem.clone(),
                EnvValue {
                    value: value.clone(),
                    var_type: var_type.clone(),
                },
            );

            return;
        }

        dairy_hater::error_token(name, String::from("Variable with this name does not exist"));
    }
}
