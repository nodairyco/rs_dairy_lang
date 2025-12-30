use crate::dairy_lang::{environment::VarType, expression::Expr, token::Token};

pub enum Stmt {
    Print(Expr),
    Expression(Expr),
    Var {
        name: Token,
        initializer: Expr,
        var_type: VarType,
    },
}

impl Stmt {
    pub fn accept<R>(&mut self, visitor: &mut dyn Visitor<R>) -> R {
        match self {
            Stmt::Print(expr) => visitor.visit_print_stmt(expr),
            Stmt::Expression(expr) => visitor.visit_expr_stmt(expr),
            Stmt::Var {
                name,
                initializer,
                var_type,
            } => visitor.visit_var_stmt(name, initializer, var_type.clone()),
        }
    }

    pub fn new_var(name: Token, initializer: Expr, var_type: VarType) -> Stmt {
        Stmt::Var { name, initializer, var_type }
    }
}

pub trait Visitor<R> {
    fn visit_print_stmt(&mut self, print_expr: &mut Expr) -> R;
    fn visit_expr_stmt(&mut self, expr_expr: &mut Expr) -> R;
    fn visit_var_stmt(&mut self, name: &mut Token, initializer: &mut Expr, var_type: VarType) -> R;
}
