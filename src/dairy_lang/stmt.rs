use crate::dairy_lang::{
    environment::Modifier, expression::Expr, token::Token, value::BuiltinType,
};

#[derive(Debug)]
pub enum Stmt {
    Print(Expr),
    Expression(Expr),
    Var {
        name: Token,
        initializer: Expr,
        var_modifier: Modifier,
        var_type: BuiltinType,
    },
    Block(Vec<Stmt>),
    If {
        condition: Expr,
        if_block: Box<Stmt>,
        else_block: Option<Box<Stmt>>,
        caller: Token,
    },
    While {
        condition: Expr,
        block: Box<Stmt>,
        caller: Token,
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
                var_modifier,
                var_type,
            } => visitor.visit_var_stmt(name, initializer, var_modifier.clone(), var_type.clone()),
            Stmt::Block(stmts) => visitor.visit_block(stmts),
            Stmt::If {
                condition,
                if_block,
                else_block,
                caller,
            } => visitor.visit_if(condition, if_block, else_block, caller),
            Stmt::While {
                condition,
                block,
                caller,
            } => visitor.visit_while(condition, block, caller),
        }
    }

    pub fn new_var(
        name: Token,
        initializer: Expr,
        var_modifier: Modifier,
        var_type: BuiltinType,
    ) -> Stmt {
        Stmt::Var {
            name,
            initializer,
            var_modifier,
            var_type,
        }
    }
}

pub trait Visitor<R> {
    fn visit_print_stmt(&mut self, print_expr: &mut Expr) -> R;
    fn visit_expr_stmt(&mut self, expr_expr: &mut Expr) -> R;
    fn visit_var_stmt(
        &mut self,
        name: &mut Token,
        initializer: &mut Expr,
        var_modifier: Modifier,
        var_type: BuiltinType,
    ) -> R;
    fn visit_block(&mut self, stmts: &mut Vec<Stmt>) -> R;
    fn visit_if(
        &mut self,
        condition: &mut Expr,
        if_block: &mut Stmt,
        else_block: &mut Option<Box<Stmt>>,
        caller: &mut Token,
    ) -> R;
    fn visit_while(
        &mut self,
        condition: &mut Expr,
        while_block: &mut Stmt,
        caller: &mut Token,
    ) -> R;
}
