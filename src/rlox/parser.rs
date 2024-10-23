use crate::rlox::ast::{Expr, Stmt, Value};
use crate::rlox::error::{ErrorType, Logger, LoxError};
use crate::rlox::token::{Token, TokenType};
use std::iter::Peekable;

type Result<T> = std::result::Result<T, LoxError>;

pub struct StmtIter<'a> {
    tokens: Peekable<&'a mut dyn Iterator<Item=Token>>,
    logger: &'a mut Logger,
}

impl<'a> StmtIter<'a> {
    pub fn new(tokens: &'a mut dyn Iterator<Item=Token>, logger: &'a mut Logger) -> Self {
        Self { tokens: tokens.peekable(), logger }
    }

    fn stmt_or_declaration(&mut self) -> Result<Stmt> {
        let result = match self.next_token_if(TokenType::is_declaration) {
            Some(token) if token.token_type == TokenType::Var => self.var_declaration(),
            Some(_) => todo!("interpret: Unsupported declaration type"),
            None => self.statement(),
        };

        // TODO synchronize
        result.map_err(|err| err)
    }

    fn var_declaration(&mut self) -> Result<Stmt> {
        let name = self.expect_token(TokenType::Identifier, 0, "Expected variable name after \"var\"")?;

        let mut initializer = None;
        if self.next_token_if(TokenType::is(TokenType::Equal)).is_some() {
            initializer = Some(self.expression()?);
        }

        self.expect_token(TokenType::Semicolon, name.line, "Expected ';' after statement")?;
        Ok(Stmt::Var(name, initializer))
    }

    fn statement(&mut self) -> Result<Stmt> {
        match self.next_token_if(TokenType::is_statement_begin) {
            Some(token) if token.token_type == TokenType::Print => self.stmt_print(),
            Some(token) if token.token_type == TokenType::LeftBrace => self.stmt_block(),
            Some(token) if token.token_type == TokenType::If => self.stmt_if(),
            Some(_) => todo!("interpret: Unsupported statement type"),
            None => self.stmt_expression(),
        }
    }

    fn stmt_expression(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.expect_token(TokenType::Semicolon, 0, "Expected ';' after statement")?;

        Ok(Stmt::Expression(expr))
    }

    fn stmt_print(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.expect_token(TokenType::Semicolon, 0, "Expected ';' after statement")?;

        Ok(Stmt::Print(expr))
    }

    fn stmt_block(&mut self) -> Result<Stmt> {
        let mut statements = Vec::new();

        while self.tokens.peek().is_some_and(|token| token.token_type != TokenType::RightBrace) {
            statements.push(self.stmt_or_declaration()?);
        }
        self.expect_token(TokenType::RightBrace, 0, "Expected '}' after block")?;

        Ok(Stmt::Block(statements))
    }

    fn stmt_if(&mut self) -> Result<Stmt> {
        self.expect_token(TokenType::LeftParen, 0, "Expected '(' after if statement")?;
        let expr = self.expression()?;
        self.expect_token(TokenType::RightParen, 0, "Expected ')' after condition")?;
        
        let if_true = self.statement()?;
        
        let mut if_false = None;
        if self.next_token_if(TokenType::is(TokenType::Else)).is_some() {
            if_false = Some(self.statement()?);
        }
        
        Ok(Stmt::new_if(expr, if_true, if_false))
    }

    /// Helper function to parse productions with a binary, left-associative operator.
    /// This describes most binary operators, so this helps avoid repetition.
    fn left_associative_binary_op<F, P>(
        &mut self,
        next_higher_precedence: F,
        op_pred: P,
    ) -> Result<Expr>
    where
        F: Fn(&mut Self) -> Result<Expr>,
        P: Fn(&TokenType) -> bool,
    {
        let mut expr = next_higher_precedence(self)?;
        while let Some(op) = self.next_token_if(&op_pred) {
            let right = next_higher_precedence(self)?;
            expr = Expr::new_binary(expr, op, right);
        }

        Ok(expr)
    }

    fn expression(&mut self) -> Result<Expr> {
        self.expr_assignment()
    }

    fn expr_assignment(&mut self) -> Result<Expr> {
        let expr = self.expr_comma()?;

        if let Some(eq) = self.next_token_if(TokenType::is(TokenType::Equal)) {
            let value = self.expr_assignment()?;

            if let Expr::Variable(name) = expr {
                Ok(Expr::new_assignment(name, value))
            } else {
                Err(self.error(eq.line, "Invalid assignment target"))
            }
        } else {
            Ok(expr)
        }
    }


    fn expr_comma(&mut self) -> Result<Expr> {
        self.left_associative_binary_op(Self::expr_ternary, TokenType::is(TokenType::Comma))
    }

    /// Parses a ternary operator. Right associative.
    fn expr_ternary(&mut self) -> Result<Expr> {
        let mut expr = self.expr_equality()?;
        while let Some(op) = self.next_token_if(TokenType::is(TokenType::QuestionMark)) {
            let if_true = self.expression()?;
            self.expect_token(TokenType::Colon, op.line, "Expected ':' after expression")?;
            let if_false = self.expr_ternary()?;

            expr = Expr::new_ternary(expr, if_true, if_false);
        }

        Ok(expr)
    }

    fn expr_equality(&mut self) -> Result<Expr> {
        self.left_associative_binary_op(Self::expr_comparison, TokenType::is_equality_op)
    }

    fn expr_comparison(&mut self) -> Result<Expr> {
        self.left_associative_binary_op(Self::expr_term, TokenType::is_comparison_op)
    }

    fn expr_term(&mut self) -> Result<Expr> {
        self.left_associative_binary_op(Self::expr_factor, TokenType::is_term_op)
    }

    fn expr_factor(&mut self) -> Result<Expr> {
        self.left_associative_binary_op(Self::expr_unary, TokenType::is_factor_op)
    }

    /// Parses a right-associative unary expression.
    /// Highest precedence within non-primary expressions.
    fn expr_unary(&mut self) -> Result<Expr> {
        if let Some(op) = self.next_token_if(TokenType::is_unary_op) {
            let expr = self.expr_unary()?;
            Ok(Expr::new_unary(op, expr))
        } else {
            self.expr_primary()
        }
    }

    /// Parses a primary expression.
    fn expr_primary(&mut self) -> Result<Expr> {
        if let Some(token) = self.next_token() {
            match token.token_type {
                TokenType::Nil => Ok(Expr::Literal(Value::Nil)),
                TokenType::True => Ok(Expr::Literal(Value::Boolean(true))),
                TokenType::False => Ok(Expr::Literal(Value::Boolean(false))),
                TokenType::Number(num) => Ok(Expr::Literal(Value::Number(num))),
                TokenType::String(str) => Ok(Expr::Literal(Value::String(str))),
                TokenType::Identifier => Ok(Expr::Variable(token)),

                TokenType::LeftParen => {
                    let expr = self.expression()?;
                    self.expect_token(TokenType::RightParen, token.line, "Expected ')' after expression")
                        .map(|_| Expr::new_grouping(expr))
                }

                _ => Err(self.error(token.line, "Expected expression")),
            }
        } else {
            Err(self.error(0, "Expected expression"))
        }
    }

    /// Consumes the next available token and returns it. Returns `None` if there are no tokens.
    fn next_token(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    /// Consumes the next available token and returns it only if `pred` is true for that token.
    /// Returns `None` if there are no tokens, or if `pred` is false.
    fn next_token_if<F>(&mut self, pred: F) -> Option<Token>
    where
        F: Fn(&TokenType) -> bool,
    {
        match self.tokens.peek().is_some_and(|token| pred(&token.token_type)) {
            true => self.tokens.next(),
            false => None,
        }
    }

    /// Consumes the next available token and returns it only if `pred` is true for that token.
    /// Returns an error if there are no tokens, or if `pred` is false.
    fn expect<F>(&mut self, pred: F, line: usize, message: &str) -> Result<Token>
    where
        F: Fn(&TokenType) -> bool,
    {
        match self.next_token_if(pred) {
            Some(token) => Ok(token),
            None => Err(self.error(line, message))
        }
    }

    /// Consumes the next available token and returns it only if it matches the provided token.
    /// Returns an error if there are no tokens, or if the next token does not match.
    ///
    /// Shorthand for calling `expect` with `TokenType::is`.
    fn expect_token(&mut self, token: TokenType, line: usize, message: &str) -> Result<Token> {
        self.expect(TokenType::is(token), line, message)
    }

    fn end(&mut self) -> bool {
        self.tokens.peek().is_some_and(|token| TokenType::is(TokenType::Eof)(&token.token_type))
    }

    fn error(&mut self, line: usize, message: &str) -> LoxError {
        LoxError::new(ErrorType::Syntax, line, message)
    }
}

impl<'a> Iterator for StmtIter<'a> {
    type Item = Stmt;

    fn next(&mut self) -> Option<Self::Item> {
        let mut stmt = None;
        while stmt.is_none() && !self.end() {
            stmt = match self.stmt_or_declaration() {
                Ok(stmt) => Some(stmt),
                Err(err) => {
                    self.logger.log(err);
                    None
                }
            }
        }
        stmt
    }
}

pub struct Parser<'a> {
    tokens: &'a mut dyn Iterator<Item=Token>,
    logger: &'a mut Logger,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a mut dyn Iterator<Item=Token>, logger: &'a mut Logger) -> Self {
        Self { tokens, logger }
    }

    pub fn iter_mut(&'a mut self) -> StmtIter<'a> {
        self.into_iter()
    }
}

impl<'a> IntoIterator for Parser<'a> {
    type Item = Stmt;
    type IntoIter = StmtIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        StmtIter::new(self.tokens, self.logger)
    }
}

impl<'a, 'b> IntoIterator for &'b mut Parser<'a>
where
    'b: 'a,
{
    type Item = Stmt;
    type IntoIter = StmtIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        StmtIter::new(self.tokens, self.logger)
    }
}
