use crate::dairy_lang::token::Token;
use crate::dairy_lang::value::Value;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Literal {
        value: Value,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Var {
        name: Token,
    },
    Assign {
        name: Token,
        value: Box<Expr>,
    },
    List {
        values: Vec<Expr>,
        caller: Token,
    },
}

impl Expr {
    pub fn accept<R>(&mut self, visitor: &mut dyn Visitor<R>) -> R {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => visitor.visit_binary(left.as_mut(), operator, right.as_mut()),
            Expr::Grouping { expression } => visitor.visit_grouping(expression.as_mut()),
            Expr::Literal { value } => visitor.visit_literal(value),
            Expr::Unary { operator, right } => visitor.visit_unary(operator, right.as_mut()),
            Expr::Var { name } => visitor.visit_var(name),
            Expr::Assign { name, value } => visitor.visit_assign(name, value),
            Expr::List { values, caller } => visitor.visit_list(values, caller),
        }
    }

    pub fn empty() -> Expr {
        Expr::Literal { value: Value::Nil }
    }
}

pub trait Visitor<R> {
    fn visit_binary(&mut self, left: &mut Expr, operator: &mut Token, right: &mut Expr) -> R;
    fn visit_grouping(&mut self, expr: &mut Expr) -> R;
    fn visit_literal(&mut self, val: &mut Value) -> R;
    fn visit_unary(&mut self, operator: &mut Token, right: &mut Expr) -> R;
    fn visit_var(&mut self, var_name: &mut Token) -> R;
    fn visit_assign(&mut self, var_name: &mut Token, val: &mut Expr) -> R;
    fn visit_list(&mut self, values: &mut Vec<Expr>, caller: &mut Token) -> R;
}
