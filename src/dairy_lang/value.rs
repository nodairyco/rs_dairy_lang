use std::fmt;
use std::rc::Rc;
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Number(f64),
    Str(Rc<str>),
    Bool(bool),
    // Void,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BuiltinType {
    Number,
    Str,
    Bool,
    Unknown,
    // Void,
}

impl FromStr for BuiltinType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Number" => Ok(BuiltinType::Number),
            "Str" => Ok(BuiltinType::Str),
            "Bool" => Ok(BuiltinType::Bool),
            "Unknown" => Ok(BuiltinType::Unknown),
            _ => Err(()),
        }
    }
}

impl From<&Value> for BuiltinType {
    fn from(value: &Value) -> Self {
        match value {
            Value::Nil => Self::Unknown,
            Value::Number(_) => Self::Number,
            Value::Str(_) => Self::Str,
            Value::Bool(_) => Self::Bool,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Number(n) => write!(f, "{}", n),
            Value::Str(str) => write!(f, "{}", *str),
            Value::Bool(bool) => write!(f, "{}", bool),
        }
    }
}
