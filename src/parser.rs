use core::panic::{self, PanicMessage};
use std::{
    arch::x86_64::_mm_extract_ps,
    fmt::{Display, Write},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Plus,
    Munis,
    Star,
    Slash,
    // Operation(Operation),
    Equal,
    ExplanationMark,
    QuestionMark,
    Colon,

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
                '+' => tokens.push(Token::new(TokenKind::Plus, c)),
                '-' => tokens.push(Token::new(TokenKind::Munis, c)),
                '*' => tokens.push(Token::new(TokenKind::Star, c)),
                '=' => tokens.push(Token::new(TokenKind::Equal, c)),
                '!' => tokens.push(Token::new(TokenKind::ExplanationMark, c)),
                '?' => tokens.push(Token::new(TokenKind::QuestionMark, c)),
                ':' => tokens.push(Token::new(TokenKind::Colon, c)),

                '/' if chars.peek().is_some_and(|c| *c == '/') => {
                    // Skip until the newline
                    while let Some(_c) = chars.next_if(|c| *c != '\n') {}
                }
                '/' => tokens.push(Token::new(TokenKind::Slash, c)),
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
                _ => unimplemented!("Unsupported token: {c}"),
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
    Op(OperationToken, OperationKind, Vec<Expr>),
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
enum OperationToken {
    Plus,
    Minus,
    Star,
    Slash,
    ExplanationMark,
    LeftBracket,
    QuestionMark,
}

impl OperationToken {
    fn from_kind(kind: TokenKind) -> Option<Self> {
        use TokenKind::*;
        let op = match kind {
            Plus => Self::Plus,
            Munis => Self::Minus,
            Star => Self::Star,
            Slash => Self::Slash,
            ExplanationMark => Self::ExplanationMark,
            LeftBracket => Self::LeftBracket,
            _ => return None,
        };
        Some(op)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OperationKind {
    Binary,
    Unary,
    Postfix,
    Ternary,
}

impl Display for OperationToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op = match self {
            OperationToken::Plus => "+",
            OperationToken::Minus => "-",
            OperationToken::Star => "*",
            OperationToken::Slash => "/",
            OperationToken::ExplanationMark => "!",
            OperationToken::LeftBracket => "[",
            OperationToken::QuestionMark => "?",
        };
        write!(f, "{op}")
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn format(
            f: &mut std::fmt::Formatter<'_>,
            expr: &Expr,
            is_first: &mut bool,
        ) -> std::fmt::Result {
            match expr {
                Expr::Item(_item, data) => {
                    write!(f, "{data}")?;
                }
                Expr::Op(operation, kind, exprs) => {
                    let is_this_first = *is_first;
                    if *is_first {
                        *is_first = false;
                    }

                    if !is_this_first {
                        write!(f, "(")?;
                    }

                    // TODO: Fix this mostrosity
                    match exprs.len() {
                        1 if matches!(kind, OperationKind::Unary) => {
                            write!(f, "{operation}")?;
                            format(f, &exprs[0], is_first)?;
                        }
                        1 if matches!(kind, OperationKind::Postfix) => {
                            format(f, &exprs[0], is_first)?;
                            write!(f, "{operation}")?;
                        }
                        2 if matches!(kind, OperationKind::Binary) => {
                            format(f, &exprs[0], is_first)?;
                            write!(f, " {operation} ")?;
                            format(f, &exprs[1], is_first)?;
                        }
                        _ => {
                            write!(f, "{{")?;
                            write!(f, "{operation}")?;
                            for e in exprs {
                                write!(f, " ")?;
                                format(f, e, is_first)?;
                            }
                            write!(f, "}}")?;
                        }
                    }

                    if !is_this_first {
                        write!(f, ")")?;
                    }
                }
            }

            Ok(())
        }

        let mut state = true;
        format(f, self, &mut state)
    }
}

fn expr(lexer: &mut Lexer, min_bp: u8) -> Expr {
    let Token { kind, data } = lexer.next_token();
    let mut lhs = if let Some(item) = Item::from_kind(kind) {
        Expr::Item(item, data)
    } else if let Some(op) = OperationToken::from_kind(kind) {
        let (_, r_bp) = binding_power(op, OperationKind::Unary).unwrap();
        let rhs = expr(lexer, r_bp);
        Expr::Op(op, OperationKind::Unary, vec![rhs])
    } else if let TokenKind::LeftParen = kind {
        let lhs = expr(lexer, 0);
        assert_eq!(lexer.next_token(), Token::new(TokenKind::RightParen, ")"));
        lhs
    } else {
        panic!("Bad token: {data}")
    };

    loop {
        let Some(Token { kind, data }) = lexer.peek_token() else {
            break;
        };
        // TODO: Do we even need this?
        if matches!(kind, TokenKind::Eof) {
            break;
        }
        let Some(op) = OperationToken::from_kind(*kind) else {
            break;
        };

        if let Some((l_bp, _)) = binding_power(op, OperationKind::Postfix) {
            if l_bp < min_bp {
                break;
            }
            lexer.next_token();

            lhs = if op == OperationToken::LeftBracket {
                let rhs = expr(lexer, 0);
                assert_eq!(lexer.next_token(), Token::new(TokenKind::RightBracket, "]"));
                Expr::Op(op, OperationKind::Postfix, vec![lhs, rhs])
            } else {
                Expr::Op(op, OperationKind::Postfix, vec![lhs])
            };

            continue;
        }

        let (l_bp, r_bp) = binding_power(op, OperationKind::Binary).unwrap();
        if l_bp < min_bp {
            break;
        }
        lexer.next_token();

        lhs = if op == OperationToken::QuestionMark {
            println!("Ternary");
            let mhs = expr(lexer, 0);
            assert_eq!(lexer.next_token(), Token::new(TokenKind::Colon, ":"));
            let rhs = expr(lexer, 0);
            Expr::Op(op, OperationKind::Ternary, vec![lhs, mhs, rhs])
        } else {
            let rhs = expr(lexer, r_bp);
            Expr::Op(op, OperationKind::Binary, vec![lhs, rhs])
        }
    }

    lhs
}

/// This function returns `Some` with binding powers of the operator if it satisfies the
/// specified `expected_kind`. It will return none if the operation does not match a kind.
///
/// In some cases on of the binging powers does not make sense and will be set to 0. For
/// `Unary` it will always be the first one and for `Postfix` it will always be the second one.
fn binding_power(op: OperationToken, expected_kind: OperationKind) -> Option<(u8, u8)> {
    use OperationKind as K;
    use OperationToken as T;

    let is_binary = matches!(expected_kind, K::Binary);
    let is_unary = matches!(expected_kind, K::Unary);
    let is_postfix = matches!(expected_kind, K::Postfix);

    let power = match op {
        T::QuestionMark if is_binary => (4, 3),
        T::Plus | T::Minus if is_binary => (5, 6),
        T::Star | T::Slash if is_binary => (7, 8),
        T::Plus | T::Minus if is_unary => (0, 9),
        T::ExplanationMark | T::LeftBracket if is_postfix => (11, 0),
        _ => return None,
    };

    Some(power)
}

#[test]
fn test() {
    let s = expr(&mut Lexer::new("1"), 0);
    assert_eq!(s.to_string(), "1");

    let s = expr(&mut Lexer::new("187312"), 0);
    assert_eq!(s.to_string(), "187312");

    let s = expr(&mut Lexer::new("1 + 2 * 3"), 0);
    assert_eq!(s.to_string(), "1 + (2 * 3)");

    let s = expr(&mut Lexer::new("a + b * c * d + e"), 0);
    assert_eq!(s.to_string(), "(a + ((b * c) * d)) + e");

    let s = expr(&mut Lexer::new("--1 * 2"), 0);
    assert_eq!(s.to_string(), "(-(-1)) * 2");

    let s = expr(&mut Lexer::new("-9!"), 0);
    assert_eq!(s.to_string(), "-(9!)");

    let s = expr(&mut Lexer::new("((((1))))"), 0);
    assert_eq!(s.to_string(), "1");

    let s = expr(&mut Lexer::new("x[0][1]"), 0);
    assert_eq!(s.to_string(), "{[ ({[ x 0}) 1}");

    let s = expr(&mut Lexer::new("a ? b : c ? d : e"), 0);
    assert_eq!(s.to_string(), "{[ ({[ x 0}) 1}");
}
