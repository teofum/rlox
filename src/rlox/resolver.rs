use crate::rlox::ast::{Expr, Stmt};
use crate::rlox::error::{ErrorType, LoxError, LoxResult};
use crate::rlox::lookups::Symbol;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

struct VarStatus {
    initialized: bool,
    used: bool,
}

impl VarStatus {
    fn new() -> Self {
        Self { initialized: false, used: false }
    }
}

impl Default for VarStatus {
    fn default() -> Self { Self::new() }
}

enum FunctionType {
    None,
    Function,
}

pub struct Resolver {
    scopes: Vec<HashMap<Symbol, VarStatus>>,
    current_function: FunctionType,
}

impl Resolver {
    pub fn new() -> Self {
        Self { scopes: Vec::new(), current_function: FunctionType::None }
    }

    pub fn resolve_stmts(&mut self, stmts: &mut Vec<Stmt>) -> LoxResult<()> {
        for stmt in stmts {
            self.resolve_stmt(stmt)?;
        }
        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &mut Stmt) -> LoxResult<()> {
        match stmt {
            Stmt::Block(stmts) => {
                self.begin_scope();
                self.resolve_stmts(stmts)?;
                self.end_scope()?;
            }
            Stmt::Var(symbol, initializer) => {
                self.declare(symbol)?;
                if let Some(initializer) = initializer {
                    self.resolve_expr(initializer)?;
                }
                self.define(symbol);
            }
            Stmt::Fun(var, params, body) => {
                self.declare(&var.symbol)?;
                self.define(&var.symbol);
                self.resolve_fun(params, body, FunctionType::Function)?;
            }
            Stmt::Class(var, _) => {
                self.declare(&var.symbol)?;
                self.define(&var.symbol);
            }
            Stmt::If(condition, true_branch, false_branch) => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(true_branch)?;
                if let Some(false_branch) = false_branch { self.resolve_stmt(false_branch)?; }
            }
            Stmt::While(condition, body) => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(body)?;
            }
            Stmt::Expression(expr) | Stmt::Print(expr) => {
                self.resolve_expr(expr)?;
            }
            Stmt::Return(expr) => {
                if let FunctionType::None = self.current_function {
                    return Err(LoxError::new(ErrorType::Resolve, 0, "Return statement outside function body"));
                }
                self.resolve_expr(expr)?;
            }
        }
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &mut Expr) -> LoxResult<()> {
        match expr {
            Expr::Variable(var, depth) => {
                if self.scopes.last().is_some_and(|scope| scope.get(&var.symbol).is_some_and(|var| !var.initialized)) {
                    return Err(LoxError::new(ErrorType::Resolve, 0, "Local variable referenced in its own initializer"));
                } else {
                    self.resolve_local(var.symbol, depth);
                }
            }
            Expr::Assignment(var, value_expr, depth) => {
                self.resolve_expr(value_expr)?;
                self.resolve_local(var.symbol, depth);
            }
            Expr::Call(callee, _, args) => {
                self.resolve_expr(callee)?;
                for arg in args {
                    self.resolve_expr(arg)?;
                }
            }
            Expr::Lambda(params, body) => {
                self.resolve_fun(params, body, FunctionType::Function)?;
            }
            Expr::Ternary(condition, if_true, if_false) => {
                self.resolve_expr(condition)?;
                self.resolve_expr(if_true)?;
                self.resolve_expr(if_false)?;
            }
            Expr::Binary(lhs, _, rhs) | Expr::Logical(lhs, _, rhs) => {
                self.resolve_expr(lhs)?;
                self.resolve_expr(rhs)?;
            }
            Expr::Unary(_, expr) | Expr::Grouping(expr) => {
                self.resolve_expr(expr)?;
            }
            Expr::Literal(_) => {}
        };
        Ok(())
    }

    fn resolve_local(&mut self, symbol: Symbol, depth: &mut Option<usize>) {
        self.mark_used(&symbol);
        *depth = self.scopes.iter()
            .rev()
            .enumerate()
            .find(|(_, scope)| scope.contains_key(&symbol))
            .map(|(i, _)| i);
    }

    fn resolve_fun(
        &mut self,
        params: &Vec<Symbol>,
        body: &mut Vec<Stmt>,
        function_type: FunctionType,
    ) -> LoxResult<()> {
        let enclosing_function = std::mem::replace(&mut self.current_function, function_type);
        self.begin_scope();

        for param in params {
            self.declare(param)?;
            self.define(param);
        }
        self.resolve_stmts(body)?;

        self.end_scope()?;
        self.current_function = enclosing_function;
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    fn end_scope(&mut self) -> LoxResult<()> {
        if let Some(scope) = self.scopes.pop() {
            if scope.values().any(|var| !var.used) {
                // TODO better error reporting, should have the var name here
                return Err(LoxError::new(ErrorType::Resolve, 0, "Unused variable in local scope"));
            }
        }

        Ok(())
    }

    fn declare(&mut self, symbol: &Symbol) -> LoxResult<()> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(symbol) {
                return Err(LoxError::new(ErrorType::Resolve, 0, "Shadowing of variable in the same scope"));
            }
            scope.insert(*symbol, VarStatus::default());
        }
        Ok(())
    }

    fn define(&mut self, symbol: &Symbol) {
        if let Some(scope) = self.scopes.last_mut() {
            if let Entry::Occupied(mut e) = scope.entry(*symbol) {
                e.get_mut().initialized = true;
            }
        }
    }

    fn mark_used(&mut self, symbol: &Symbol) {
        if let Some(scope) = self.scopes.last_mut() {
            if let Entry::Occupied(mut e) = scope.entry(*symbol) {
                e.get_mut().used = true;
            }
        }
    }
}