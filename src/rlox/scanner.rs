use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::str::Chars;

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

    EOF,
}

fn get_word_token(word: &str) -> TokenType {
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

pub struct TokenIter<'a> {
    source: &'a str,
    chars: Peekable<Chars<'a>>,
    cursor: usize,
    line: usize,
}

impl<'a> TokenIter<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source, chars: source.chars().peekable(), cursor: 0, line: 1 }
    }

    fn eof(&mut self) -> bool {
        self.chars.peek().is_none()
    }

    fn lexeme(&self) -> &'a str {
        &self.source[..self.cursor]
    }

    /// Peeks ahead multiple characters and matches against a character.
    ///
    /// Returns `true` if the given character matches, `false` if it does not.
    fn lookahead<F>(&self, from: usize, pred: &F) -> bool
    where
        F: Fn(char) -> bool,
    {
        self.source[self.cursor + from..].starts_with(pred)
    }

    /// Peeks the next character and checks if it is equal to `c`. Equivalent to `lookahead(0, c)`.
    ///
    /// Returns `true` if the character matches, `false` if it does not.
    fn next_is(&self, c: char) -> bool {
        self.lookahead(0, &|next| next == c)
    }

    /// Consumes the next character and returns it.
    ///
    /// Returns `None` if there are no more characters.
    fn next_char(&mut self) -> Option<char> {
        let c = self.chars.next();
        if c.is_some() { self.cursor += 1; }
        c
    }

    /// Consumes the next character only if it matches `c`.
    ///
    /// Returns `true` if the character matched and was consumed, `false` otherwise.
    fn match_next(&mut self, c: char) -> bool {
        if self.next_is(c) {
            self.next_char();
            true
        } else {
            false
        }
    }

    /// Consumes characters while `c` matches the start of source at the cursor.
    fn skip_while<F>(&mut self, pred: &F)
    where
        F: Fn(char) -> bool,
    {
        while self.lookahead(0, pred) { self.next_char(); }
    }

    /// Consumes characters while `c` _does not_ match the start of source at the cursor.
    fn skip_until<F>(&mut self, pred: &F)
    where
        F: Fn(char) -> bool,
    {
        while !self.lookahead(0, pred) { self.next_char(); }
    }

    fn create_token(&mut self, token_type: Option<TokenType>) -> Option<Token> {
        let lexeme_slice = self.lexeme();
        self.source = &self.source[self.cursor..];
        self.cursor = 0;

        token_type.map(|token_type| Token {
            token_type,
            lexeme: lexeme_slice.to_string(),
            line: self.line,
        })
    }

    fn scan_token(&mut self) -> Option<TokenType> {
        match self.next_char() {
            Some('(') => Some(TokenType::LeftParen),
            Some(')') => Some(TokenType::RightParen),
            Some('{') => Some(TokenType::LeftBrace),
            Some('}') => Some(TokenType::RightBrace),
            Some(',') => Some(TokenType::Comma),
            Some('.') => Some(TokenType::Dot),
            Some('-') => Some(TokenType::Minus),
            Some('+') => Some(TokenType::Plus),
            Some(';') => Some(TokenType::Semicolon),
            Some('*') => Some(TokenType::Star),

            // 1-2 character tokens
            Some('!') => if self.match_next('=') {
                Some(TokenType::BangEqual)
            } else {
                Some(TokenType::Bang)
            },
            Some('=') => if self.match_next('=') {
                Some(TokenType::EqualEqual)
            } else {
                Some(TokenType::Equal)
            },
            Some('<') => if self.match_next('=') {
                Some(TokenType::LessEqual)
            } else {
                Some(TokenType::Less)
            },
            Some('>') => if self.match_next('=') {
                Some(TokenType::GreaterEqual)
            } else {
                Some(TokenType::Greater)
            },

            // Slash can be either a single slash or comment
            Some('/') => {
                if self.match_next('/') {
                    // Comment: skip until newline
                    self.skip_until(&|next| next == '\n');
                    None
                } else if self.match_next('*') {
                    // Block comment: skip until block end
                    while !self.next_is('*') || !self.lookahead(1, &|next| next == '/') {
                        if self.next_is('\n') { self.line += 1; }
                        self.next_char();
                    }
                    self.next_char();
                    self.next_char();
                    None
                } else {
                    Some(TokenType::Slash)
                }
            }

            // Ignore whitespace
            Some(' ' | '\r' | '\t') => None,

            // Ignore newlines, but increment line counter
            Some('\n') => {
                self.line += 1;
                None
            }

            // String literals
            Some('"') => self.scan_string(),

            // Number literals
            Some('0'..='9') => self.scan_number(),

            // Identifiers
            Some('a'..='z' | 'A'..='Z' | '_') => self.scan_identifier(),

            _ => None, // TODO: error handling
        }
    }

    fn scan_string(&mut self) -> Option<TokenType> {
        while !self.next_is('"') {
            if self.next_is('\n') { self.line += 1; }
            self.next_char();
        }

        match self.next_char() {
            None => None, // TODO: error handling
            Some(_) => Some(TokenType::String(self.source[1..self.cursor - 1].to_string())),
        }
    }

    fn scan_number(&mut self) -> Option<TokenType> {
        self.skip_while(&|next| next.is_ascii_digit());

        let is_digit = |c| c >= '0' && c <= '9';
        if self.next_is('.') && self.source[self.cursor + 1..].starts_with(is_digit) {
            self.next_char();
            self.skip_while(&|next| next.is_ascii_digit());
        }

        Some(TokenType::Number(self.lexeme().parse().unwrap()))
    }

    fn scan_identifier(&mut self) -> Option<TokenType> {
        self.skip_while(&|next| next.is_alphanumeric() || next == '_');
        Some(get_word_token(self.lexeme()))
    }
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.eof() {
            None
        } else {
            let mut token = None;
            while token.is_none() && !self.eof() {
                let token_type = self.scan_token();
                token = self.create_token(token_type);
            }
            token
        }
    }
}

pub struct Scanner<'a> {
    source: &'a str,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source }
    }

    pub fn iter(&self) -> TokenIter {
        TokenIter::new(self.source)
    }
}