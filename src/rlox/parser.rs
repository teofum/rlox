use crate::rlox::ast::{Expr, Value};
use crate::rlox::error::{ErrorType, Logger, LoxError};
use crate::rlox::token::{Token, TokenType};
use std::iter::Peekable;

pub struct Parser<'a> {
    token_buf: Vec<Token>,
    tokens: Peekable<&'a mut dyn Iterator<Item=Token>>,
    logger: &'a mut Logger,
}

impl<'a> Parser<'a> {
    pub fn from(tokens: &'a mut dyn Iterator<Item=Token>, logger: &'a mut Logger) -> Self {
        Self { token_buf: Vec::new(), tokens: tokens.peekable(), logger }
    }

    pub fn parse(&mut self) -> Option<Expr> {
        self.expression()
    }

    fn left_associative_binary_op<F, P>(
        &mut self, next_higher_precedence: F,
        token_pred: P,
    ) -> Option<Expr>
    where
        F: Fn(&mut Self) -> Option<Expr>,
        P: Fn(&TokenType) -> bool,
    {
        let mut expr = next_higher_precedence(self)?;
        while let Some(op) = self.next_token_if(&token_pred) {
            let right = next_higher_precedence(self)?;
            expr = Expr::new_binary(expr, op, right);
        }

        Some(expr)
    }

    fn expression(&mut self) -> Option<Expr> {
        self.equality()
    }

    fn equality(&mut self) -> Option<Expr> {
        self.left_associative_binary_op(Self::comparison, TokenType::is_equality_op)
    }

    fn comparison(&mut self) -> Option<Expr> {
        self.left_associative_binary_op(Self::term, TokenType::is_comparison_op)
    }

    fn term(&mut self) -> Option<Expr> {
        self.left_associative_binary_op(Self::factor, TokenType::is_term_op)
    }

    fn factor(&mut self) -> Option<Expr> {
        self.left_associative_binary_op(Self::unary, TokenType::is_factor_op)
    }

    fn unary(&mut self) -> Option<Expr> {
        if let Some(op) = self.next_token_if(TokenType::is_unary_op) {
            let expr = self.unary()?;
            Some(Expr::new_unary(op, expr))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Option<Expr> {
        if let Some(token) = self.next_token() {
            match token.token_type {
                TokenType::Nil => Some(Expr::new_literal(Value::Nil)),
                TokenType::True => Some(Expr::new_literal(Value::True)),
                TokenType::False => Some(Expr::new_literal(Value::False)),
                TokenType::Number(num) => Some(Expr::new_literal(Value::Number(num))),
                TokenType::String(str) => Some(Expr::new_literal(Value::String(str))),

                TokenType::LeftParen => {
                    let expr = self.expression()?;
                    if let Some(_) = self.next_token_if(|t| *t == TokenType::RightParen) {
                        Some(Expr::new_grouping(expr))
                    } else {
                        self.log_error(Some(token), "Expected ')' after expression");
                        None
                    }
                }

                _ => {
                    self.log_error(Some(token), "Expected expression");
                    None
                }
            }
        } else {
            self.log_error(None, "Expected expression");
            None
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        self.tokens.next()
    }

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

    fn log_error(&mut self, token: Option<Token>, message: &str) {
        let line = token.map_or(0, |t| t.line);
        self.logger.log(LoxError::new(ErrorType::SyntaxError, line, message));
    }
}