use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Colon,
    Comma,
    Dot,
    Minus,
    Plus,
    QuestionMark,
    Semicolon,
    Slash,
    Star,

    // 1-2 character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String(String),
    Number(f64),

    // Keywords
    If,
    Else,
    And,
    Or,
    For,
    While,
    True,
    False,
    Nil,
    Var,
    Fun,
    Class,
    This,
    Super,
    Print,
    Return,

    Eof,
}

impl TokenType {
    pub fn from_word(word: &str) -> TokenType {
        match word {
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "and" => TokenType::And,
            "or" => TokenType::Or,
            "for" => TokenType::For,
            "while" => TokenType::While,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "nil" => TokenType::Nil,
            "var" => TokenType::Var,
            "fun" => TokenType::Fun,
            "class" => TokenType::Class,
            "this" => TokenType::This,
            "super" => TokenType::Super,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            _ => TokenType::Identifier,
        }
    }

    pub fn is_literal(token_type: &Self) -> bool {
        matches!(token_type, TokenType::Identifier | TokenType::String(_) | TokenType::Number(_))
    }

    pub fn is_unary_op(token_type: &Self) -> bool {
        matches!(token_type, TokenType::Bang | TokenType::Minus)
    }

    pub fn is_binary_op(token_type: &Self) -> bool {
        matches!(token_type, TokenType::EqualEqual | TokenType::BangEqual | TokenType::Less |
            TokenType::LessEqual | TokenType::Greater | TokenType::GreaterEqual |
            TokenType::Plus | TokenType::Minus | TokenType::Star | TokenType::Slash)
    }

    pub fn is_equality_op(token_type: &Self) -> bool {
        matches!(token_type, TokenType::EqualEqual | TokenType::BangEqual)
    }

    pub fn is_comparison_op(token_type: &Self) -> bool {
        matches!(token_type, TokenType::Less | TokenType::LessEqual | TokenType::Greater | TokenType::GreaterEqual)
    }

    pub fn is_term_op(token_type: &Self) -> bool {
        matches!(token_type, TokenType::Plus | TokenType::Minus)
    }

    pub fn is_factor_op(token_type: &Self) -> bool {
        matches!(token_type, TokenType::Star | TokenType::Slash)
    }

    pub fn is_declaration(token_type: &Self) -> bool {
        matches!(token_type, TokenType::Var | TokenType::Fun | TokenType::Class)
    }

    pub fn is(pattern: Self) -> impl Fn(&Self) -> bool {
        move |token_type| *token_type == pattern
    }
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {}", self.token_type, &self.lexeme)
    }
}
