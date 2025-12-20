use crate::token::{Token, Value};

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
        value: Option<Value>,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
}

impl Expr {
    fn accept<R>(&mut self, visitor: &mut dyn Visitor<R>) -> R {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => visitor.visit_binary(left.as_mut(), operator, right.as_mut()),
            Expr::Grouping { expression } => visitor.visit_grouping(expression.as_mut()),
            Expr::Literal { value } => visitor.visit_literal(value),
            Expr::Unary { operator, right } => visitor.visit_unary(operator, right.as_mut()),
        }
    }

    pub fn empty() -> Expr {
        Expr::Literal { value: None }
    }
}

pub trait Visitor<R> {
    fn visit_binary(&mut self, left: &mut Expr, operator: &mut Token, right: &mut Expr) -> R;
    fn visit_grouping(&mut self, expr: &mut Expr) -> R;
    fn visit_literal(&mut self, val: &mut Option<Value>) -> R;
    fn visit_unary(&mut self, operator: &mut Token, right: &mut Expr) -> R;
}

pub struct AstPrinter;

impl AstPrinter {
    pub fn print(&mut self, expr: &mut Expr) -> String {
        expr.accept(self)
    }

    fn paranthesize(&mut self, name: &String, exprs: Vec<&mut Expr>) -> String {
        let mut to_ret = String::new();

        to_ret.push_str("(");

        to_ret.push_str(&name);

        for expr in exprs {
            to_ret.push(' ');
            to_ret.push_str(&self.print(expr));
        }

        to_ret.push(')');

        to_ret
    }
}

impl Visitor<String> for AstPrinter {
    fn visit_binary(&mut self, left: &mut Expr, operator: &mut Token, right: &mut Expr) -> String {
        self.paranthesize(&operator.lexem, vec![left, right])
    }

    fn visit_grouping(&mut self, expr: &mut Expr) -> String {
        self.paranthesize(&String::from("group"), vec![expr])
    }

    fn visit_literal(&mut self, val: &mut Option<Value>) -> String {
        match val {
            Some(v) => format!("{}", v),
            None => String::from("nil"),
        }
    }

    fn visit_unary(&mut self, operator: &mut Token, right: &mut Expr) -> String {
        self.paranthesize(&operator.lexem, vec![right])
    }
}
