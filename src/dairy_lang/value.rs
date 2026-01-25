use std::fmt;
use std::ops::Range;
use std::rc::Rc;
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Number(f64),
    Str(Rc<str>),
    Bool(bool),
    List(Vec<Value>),
    Range(Range<i64>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BuiltinType {
    Number,
    Str,
    Bool,
    Unknown,
    List(Rc<BuiltinType>),
    Range,
}

impl FromStr for BuiltinType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with('[') {
            if !s.ends_with(']') {
                return Err(());
            }

            let mut s1 = s.replacen("[", "", 1);
            s1 = s1.replacen("]", "", 1);

            return Ok(Self::List(Rc::from(Self::from_str(&s1)?)));
        }

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
            Value::List(l) => {
                if l.is_empty() {
                    Self::List(Rc::from(Self::Unknown))
                } else {
                    Self::List(Rc::from(Self::from(&l[0])))
                }
            }
            Value::Range(_) => Self::Range,
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
            Value::List(l) => {
                let mut str = String::from("[");

                for (i, v) in l.iter().enumerate() {
                    if i != l.len() - 1 {
                        str.push_str(&format!(
                            "{}, ",
                            if let Value::Str(v1) = v {
                                format!("\"{}\"", v1)
                            } else {
                                v.to_string()
                            }
                        ));
                    } else {
                        str.push_str(&format!(
                            "{}",
                            if let Value::Str(v1) = v {
                                format!("\"{}\"", v1)
                            } else {
                                v.to_string()
                            }
                        ));
                    }
                }

                write!(f, "{}]", str)
            }
            Value::Range(r) => write!(f, "Range({},{})", r.start, r.end),
        }
    }
}
