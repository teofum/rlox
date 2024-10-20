use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
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

    // EOF, // Unused
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
}

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
