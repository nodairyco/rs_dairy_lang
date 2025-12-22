use crate::dairy_lang::expression::{Expr};

pub enum Stmt {
    Print(Expr),
    Expression(Expr),
}

impl Stmt {
    pub fn accept<R>(&mut self, visitor: &mut dyn Visitor<R>) -> R {
        match self {
            Stmt::Print(expr) => visitor.visit_print_stmt(expr),
            Stmt::Expression(expr) => visitor.visit_expr_stmt(expr)
        }
    }
}

pub trait Visitor<R> {
    fn visit_print_stmt(&mut self, print_expr: &mut Expr) -> R;
    fn visit_expr_stmt(&mut self, expr_expr: &mut Expr) -> R;
}
