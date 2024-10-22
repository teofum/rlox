use crate::rlox::ast::{Expr, Value};
use crate::rlox::error::{ErrorType, Logger, LoxError};
use crate::rlox::token::{Token, TokenType};
use std::iter::Peekable;

type Result<T> = std::result::Result<T, LoxError>;

pub struct Parser<'a> {
    tokens: Peekable<&'a mut dyn Iterator<Item=Token>>,
    logger: &'a mut Logger,
}

impl<'a> Parser<'a> {
    pub fn from(tokens: &'a mut dyn Iterator<Item=Token>, logger: &'a mut Logger) -> Self {
        Self { tokens: tokens.peekable(), logger }
    }

    pub fn parse(&mut self) -> Option<Expr> {
        match self.expression() {
            Ok(expr) => Some(expr),
            Err(err) => {
                self.logger.log(err);
                None
            }
        }
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
        self.comma()
    }

    fn comma(&mut self) -> Result<Expr> {
        self.left_associative_binary_op(Self::ternary, TokenType::is(TokenType::Comma))
    }

    /// Parses a ternary operator. Right associative.
    fn ternary(&mut self) -> Result<Expr> {
        let mut expr = self.equality()?;
        while let Some(op) = self.next_token_if(TokenType::is(TokenType::QuestionMark)) {
            let if_true = self.expression()?;
            let if_false = match self.next_token_if(|t| *t == TokenType::Colon) {
                Some(_) => self.ternary(),
                None => Err(self.error(Some(op), "Expected ':' after expression"))
            }?;

            expr = Expr::new_ternary(expr, if_true, if_false);
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr> {
        self.left_associative_binary_op(Self::comparison, TokenType::is_equality_op)
    }

    fn comparison(&mut self) -> Result<Expr> {
        self.left_associative_binary_op(Self::term, TokenType::is_comparison_op)
    }

    fn term(&mut self) -> Result<Expr> {
        self.left_associative_binary_op(Self::factor, TokenType::is_term_op)
    }

    fn factor(&mut self) -> Result<Expr> {
        self.left_associative_binary_op(Self::unary, TokenType::is_factor_op)
    }

    /// Parses a right-associative unary expression.
    /// Highest precedence within non-primary expressions.
    fn unary(&mut self) -> Result<Expr> {
        if let Some(op) = self.next_token_if(TokenType::is_unary_op) {
            let expr = self.unary()?;
            Ok(Expr::new_unary(op, expr))
        } else {
            self.primary()
        }
    }

    /// Parses a primary expression.
    fn primary(&mut self) -> Result<Expr> {
        if let Some(token) = self.next_token() {
            match token.token_type {
                TokenType::Nil => Ok(Expr::new_literal(Value::Nil)),
                TokenType::True => Ok(Expr::new_literal(Value::True)),
                TokenType::False => Ok(Expr::new_literal(Value::False)),
                TokenType::Number(num) => Ok(Expr::new_literal(Value::Number(num))),
                TokenType::String(str) => Ok(Expr::new_literal(Value::String(str))),

                TokenType::LeftParen => {
                    let expr = self.expression()?;
                    match self.next_token_if(|t| *t == TokenType::RightParen) {
                        Some(_) => Ok(Expr::new_grouping(expr)),
                        None => Err(self.error(Some(token), "Expected ')' after expression"))
                    }
                }

                _ => Err(self.error(Some(token), "Expected expression")),
            }
        } else {
            Err(self.error(None, "Expected expression"))
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

    fn end(&mut self) -> bool {
        self.tokens.peek().is_none()
    }

    fn error(&mut self, token: Option<Token>, message: &str) -> LoxError {
        let line = token.map_or(0, |t| t.line);
        LoxError::new(ErrorType::SyntaxError, line, message)
    }
}