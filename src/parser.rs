use core::panic::{self, PanicMessage};
use std::{
    fmt::{Display, Write},
    mem,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Add,
    Sub,
    Mul,
    Div,
    // Operation(Operation),
    Equal,

    Number,
    Ident,
    // Atom(Atom),
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,

    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    kind: TokenKind,
    data: String,
}

impl Token {
    pub const EOF: Self = Self {
        kind: TokenKind::Eof,
        data: String::new(),
    };

    pub fn new(kind: TokenKind, data: impl Into<String>) -> Self {
        Self {
            kind,
            data: data.into(),
        }
    }
}

pub struct Lexer {
    /// Tokens order is reversed so the `next` and `peek` methods work
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        let mut tokens = Self::parse_tokens(source);
        tokens.reverse();
        Self { tokens }
    }

    fn parse_tokens(source: &str) -> Vec<Token> {
        let mut tokens = Vec::new();

        let mut chars = source.chars().peekable();
        while let Some(c) = chars.next() {
            match c {
                ' ' => {}
                '(' => tokens.push(Token::new(TokenKind::LeftParen, c)),
                ')' => tokens.push(Token::new(TokenKind::RightParen, c)),
                '[' => tokens.push(Token::new(TokenKind::LeftBracket, c)),
                ']' => tokens.push(Token::new(TokenKind::RightBracket, c)),
                '{' => tokens.push(Token::new(TokenKind::LeftBrace, c)),
                '}' => tokens.push(Token::new(TokenKind::RightBrace, c)),
                '+' => tokens.push(Token::new(TokenKind::Add, c)),
                '-' => tokens.push(Token::new(TokenKind::Sub, c)),
                '*' => tokens.push(Token::new(TokenKind::Mul, c)),
                '=' => tokens.push(Token::new(TokenKind::Equal, c)),
                '/' if chars.peek().is_some_and(|c| *c == '/') => {
                    // Skip until the newline
                    while let Some(_c) = chars.next_if(|c| *c != '\n') {}
                }
                '/' => tokens.push(Token::new(TokenKind::Div, c)),
                c if c.is_ascii_digit() => {
                    let mut content = String::from(c);
                    while let Some(c) = chars.next_if(|c| c.is_ascii_digit()) {
                        content.push(c);
                    }
                    tokens.push(Token::new(TokenKind::Number, content));
                }
                c if c.is_ascii_alphabetic() => {
                    let mut content = String::from(c);
                    while let Some(c) = chars.next_if(|c| c.is_ascii_alphanumeric()) {
                        content.push(c);
                    }
                    tokens.push(Token::new(TokenKind::Ident, content));
                }
                _ => unimplemented!(),
            }
        }

        tokens
    }

    pub fn next_token(&mut self) -> Token {
        self.tokens.pop().unwrap_or(Token::EOF)
    }

    pub fn peek_token(&mut self) -> Option<&Token> {
        self.tokens.last()
    }
}

const DISPLAY_DATA_PAD: usize = 10;
impl Display for Lexer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for token in self.tokens.iter().rev() {
            write!(f, "{:?}", token.kind)?;
            // TODO: Use custom write impl that counts bytes
            let length = format!("{:?}", token.kind).len();
            for _ in 0..(DISPLAY_DATA_PAD - length) {
                write!(f, " ")?;
            }
            writeln!(f, "{}", token.data)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Item(Item, String),
    Op(Operation, Vec<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Item {
    Ident,
    Number,
}

impl Item {
    fn from_kind(kind: TokenKind) -> Option<Self> {
        use TokenKind::*;
        let it = match kind {
            Ident => Self::Ident,
            Number => Self::Number,
            _ => return None,
        };
        Some(it)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Operation {
    Add,
    Sub,
    Mul,
    Div,
}

impl Operation {
    fn from_kind(kind: TokenKind) -> Option<Self> {
        use TokenKind::*;
        let op = match kind {
            Add => Self::Add,
            Sub => Self::Sub,
            Mul => Self::Mul,
            Div => Self::Div,
            _ => return None,
        };
        Some(op)
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op = match self {
            Operation::Add => "+",
            Operation::Sub => "-",
            Operation::Mul => "*",
            Operation::Div => "/",
        };
        write!(f, "{op}")
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Item(_item, data) => write!(f, "{data}"),
            Expr::Op(operation, exprs) => write!(f, "({} {operation} {})", exprs[0], exprs[1]),
        }
    }
}

fn expr(lexer: &mut Lexer, min_bp: u8) -> Expr {
    let Token { kind, data } = lexer.next_token();
    let lhs = Operation::from_kind(kind);
}

fn infix_binding_power(op: Operation) -> (u8, u8) {
    match op {
        Operation::Add | Operation::Sub => (1, 2),
        Operation::Mul | Operation::Div => (3, 4),
        _ => panic!("Bad op: {:?}", op),
    }
}

#[test]
fn test() {
    let s = expr(&mut Lexer::new("1"), 0);
    assert_eq!(s.to_string(), "1");

    let s = expr(&mut Lexer::new("187312"), 0);
    assert_eq!(s.to_string(), "187312");

    let s = expr(&mut Lexer::new("1 + 2 * 3"), 0);
    assert_eq!(s.to_string(), "(+ 1 (* 2 3))");

    let s = expr(&mut Lexer::new("a + b * c * d + e"), 0);
    assert_eq!(s.to_string(), "(+ (+ a (* (* b c) d)) e)");
}
