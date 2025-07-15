use std::{fmt::Display, num::ParseIntError};

use crate::lexer::{Lexer, Token, TokenKind};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Unexpected token found {found:?} expected {expected}{}", expected_msg.as_deref().unwrap_or(""))]
    UnexpectedToken {
        found: Token,
        expected: ExpectedTokens,
        expected_msg: Option<&'static str>,
    },
    #[error("Reached end of file")]
    Eof,
    #[error("Binding power can't be computed for {op:?}")]
    BindingPowerInvalidOp { op: TokenKind },
    #[error(transparent)]
    ParseIntError(#[from] ParseIntError),
}

#[derive(Debug)]
enum ExpectedTokens {
    FullToken(Token),
    Single(TokenKind),
    OneOf(&'static [TokenKind]),
}

impl Display for ExpectedTokens {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpectedTokens::FullToken(token) => {
                write!(f, "{:?}", token)?;
            }
            ExpectedTokens::Single(token_kind) => {
                write!(f, "{:?}", token_kind)?;
            }
            ExpectedTokens::OneOf(token_kinds) => {
                write!(f, "one of [");
                let last_i = token_kinds.len() - 1;
                for (i, k) in token_kinds.iter().enumerate() {
                    write!(f, "{:?}", k);
                    if i == last_i {
                        break;
                    }
                    write!(f, ", ");
                }
                write!(f, "]");
            }
        };
        Ok(())
    }
}

pub type AResult<T, E = Error> = core::result::Result<T, E>;

pub trait Parse {
    fn parse(lexer: &mut Lexer) -> AResult<Self>
    where
        Self: Sized;
}

#[derive(Debug)]
pub enum Expr {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Block(Vec<Box<Expr>>),
    Lambda(LambdaExpr),
    Literal(LiteralExp),
    Integer(IntegerExpr),
    Ident(IdentExpr),
}

#[derive(Debug)]
pub struct BinaryExpr {
    lhs: Box<Expr>,
    rhs: Box<Expr>,
    op: BinaryOp,
}

#[derive(Debug)]
enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl BinaryOp {
    fn new(kind: TokenKind, data: &str) -> AResult<Self> {
        use TokenKind::*;
        let op = match kind {
            Plus => Self::Add,
            Minus => Self::Sub,
            Star => Self::Mul,
            Slash => Self::Div,
            k => {
                return Err(Error::UnexpectedToken {
                    found: Token::new(k, data.clone()),
                    expected: ExpectedTokens::OneOf(&[Plus, Minus, Star, Slash]),
                    expected_msg: None,
                });
            }
        };

        Ok(op)
    }
}

#[derive(Debug)]
pub struct UnaryExpr {
    item: Box<Expr>,
    op: UnaryOp,
}

#[derive(Debug)]
enum UnaryOp {
    Negate,
}

#[derive(Debug)]
pub struct LambdaExpr {
    args: Sequence<IdentExpr>,
    body: Box<Expr>,
}

#[derive(Debug)]
pub struct LiteralExp {
    // TODO: Maybe use &'a str
    literal: String,
}

impl Parse for LiteralExp {
    fn parse(lexer: &mut Lexer) -> AResult<Self>
    where
        Self: Sized,
    {
        match lexer.next_token() {
            Some(Token {
                kind: TokenKind::Literal,
                data,
            }) => Ok(Self { literal: data }),
            Some(t) => Err(Error::UnexpectedToken {
                found: t,
                expected: ExpectedTokens::Single(TokenKind::Literal),
                expected_msg: None,
            }),
            None => Err(Error::Eof),
        }
    }
}

#[derive(Debug)]
pub struct IntegerExpr {
    int: i32,
}

impl Parse for IntegerExpr {
    fn parse(lexer: &mut Lexer) -> AResult<Self>
    where
        Self: Sized,
    {
        match lexer.next_token() {
            Some(Token {
                kind: TokenKind::Number,
                data,
            }) => Ok(Self {
                int: data.parse::<i32>()?,
            }),
            Some(t) => Err(Error::UnexpectedToken {
                found: t,
                expected: ExpectedTokens::Single(TokenKind::Number),
                expected_msg: None,
            }),
            None => Err(Error::Eof),
        }
    }
}

#[derive(Debug)]
pub struct IdentExpr {
    ident: String,
}

impl Parse for IdentExpr {
    fn parse(lexer: &mut Lexer) -> AResult<Self>
    where
        Self: Sized,
    {
        match lexer.next_token() {
            Some(Token {
                kind: TokenKind::Ident,
                data,
            }) => Ok(Self { ident: data }),
            Some(t) => Err(Error::UnexpectedToken {
                found: t,
                expected: ExpectedTokens::Single(TokenKind::Ident),
                expected_msg: None,
            }),
            None => Err(Error::Eof),
        }
    }
}

pub fn expr(lexer: &mut Lexer, min_bp: u8) -> AResult<Expr> {
    let Some(Token { kind, data }) = lexer.next_token() else {
        return Err(Error::Eof);
    };

    use TokenKind::*;
    let mut lhs = match kind {
        Number => Expr::Integer(IntegerExpr { int: data.parse()? }),
        Ident => Expr::Ident(IdentExpr { ident: data }),
        Literal => Expr::Literal(LiteralExp { literal: data }),
        LeftParen => {
            todo!()
        }
        _ => {
            return Err(Error::UnexpectedToken {
                found: Token { kind, data },
                expected: ExpectedTokens::OneOf(&[Number, Ident, Literal]),
                expected_msg: Some(" or a lambda expression"),
            });
        }
    };

    loop {
        let Some(Token { kind, data }) = lexer.peek_token().cloned() else {
            break;
        };
        if kind == Eof {
            break;
        }

        if let Some((l_bp, r_bp)) = binding_power(kind)?.infix {
            if l_bp < min_bp {
                break;
            }

            lexer.next_token();
            let rhs = expr(lexer, r_bp)?;
            let ex = Expr::Binary(BinaryExpr {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op: BinaryOp::new(kind, &data)?,
            });
            lhs = ex;

            continue;
        }

        break;
    }

    Ok(lhs)
}

#[derive(Debug, Default)]
struct BindingPower {
    /// Right power.
    pub prefix: Option<u8>,
    /// Left and right powers.
    pub infix: Option<(u8, u8)>,
    /// Left power.
    pub postfix: Option<u8>,
}

impl BindingPower {
    fn new() -> Self {
        Self::default()
    }

    fn prefix(mut self, power: u8) -> Self {
        self.prefix = Some(power);
        self
    }

    fn infix(mut self, left: u8, right: u8) -> Self {
        self.infix = Some((left, right));
        self
    }

    fn postfix(mut self, power: u8) -> Self {
        self.postfix = Some(power);
        self
    }
}

fn binding_power(kind: TokenKind) -> AResult<BindingPower> {
    use TokenKind::*;
    let power = match kind {
        Plus | Minus => BindingPower::new().prefix(9).infix(5, 6),
        Star | Slash => BindingPower::new().infix(7, 8),
        ExplanationMark | LeftBracket => BindingPower::new().postfix(11),
        _ => return Err(Error::BindingPowerInvalidOp { op: kind }),
    };
    Ok(power)
}

#[derive(Debug)]
pub struct Sequence<T> {
    pub seq: Vec<T>,
}

impl<T: Parse> Sequence<T> {
    /// Elements must be seprated by the `separator` token.
    /// Parsing will be stoped when `stop` token is met. Stop-token will **not** be consumed.
    pub fn parse_till(lexer: &mut Lexer, stop: Token, separator: Token) -> AResult<Self> {
        let mut seq = Vec::new();

        // Do we expect to see an item right now?
        let mut is_item = true;
        loop {
            let Some(peek) = lexer.peek_token() else {
                break;
            };

            if peek == &stop {
                break;
            }

            if is_item {
                let item = T::parse(lexer)?;
                seq.push(item);
                is_item = !is_item;
                continue;
            }

            // PANICS: TODO
            let token = lexer.next_token().unwrap();
            if token == separator {
                continue;
            } else {
                return Err(Error::UnexpectedToken {
                    found: token,
                    expected: ExpectedTokens::FullToken(stop),
                    expected_msg: None,
                });
            }
        }

        Ok(Self { seq })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expr_test() {
        let mut lexer = Lexer::new("a + b * \"literal!!\"");
        let expr = expr(&mut lexer, 0).unwrap();
        println!("{expr:#?}")
    }
}
