use std::{borrow::Cow, fmt::Display, num::ParseIntError};

use crate::lexer::{Token, TokenKind, Tokens};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Unexpected token found {found:?} expected {expected}{}", expected_msg.as_deref().unwrap_or(""))]
    UnexpectedToken {
        found: Token,
        expected: ExpectedTokens,
        expected_msg: Option<&'static str>,
    },
    #[error("Failed to parse {0}")]
    FailedToParse(Cow<'static, str>),
    #[error("Reached end of file")]
    Eof,
    #[error("Binding power can't be computed for {op:?}")]
    BindingPowerInvalidOp { op: TokenKind },
    #[error("Unclosed delimiter found expected {0:?} to be closed")]
    UnclosedDelimiter(TokenKind),
    #[error(transparent)]
    ParseIntError(#[from] ParseIntError),
}

#[derive(Debug)]
pub enum ExpectedTokens {
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
                write!(f, "one of [")?;
                let last_i = token_kinds.len() - 1;
                for (i, k) in token_kinds.iter().enumerate() {
                    write!(f, "{:?}", k)?;
                    if i == last_i {
                        break;
                    }
                    write!(f, ", ")?;
                }
                write!(f, "]")?;
            }
        };
        Ok(())
    }
}

pub type AResult<T, E = Error> = core::result::Result<T, E>;

pub trait Parse {
    fn parse(tokens: &mut Tokens) -> AResult<Self>
    where
        Self: Sized;
}

#[derive(Debug)]
pub enum Expr {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    // Block(Vec<Box<Expr>>),
    Sequence(Sequence<Expr>),
    Lambda(LambdaExpr),
    Literal(LiteralExp),
    Integer(IntegerExpr),
    Ident(IdentExpr),
}

impl Expr {
    pub fn flatten_sequence(self) -> Self {
        fn flatten(buffer: &mut Vec<Expr>, sequence: Expr) -> Option<()> {
            match sequence {
                Expr::Sequence(Sequence { mut seq }) => {
                    let expr = seq.pop()?;
                    let child_seq = seq.pop()?;
                    buffer.push(expr);
                    flatten(buffer, child_seq)
                }
                e => {
                    buffer.push(e);
                    Some(())
                }
            }
        }

        let mut seq = Vec::new();
        flatten(&mut seq, self);
        seq.reverse();
        Self::Sequence(Sequence { seq })
    }
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: BinaryOp,
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl BinaryOp {
    fn new(token: Token) -> AResult<Self> {
        use TokenKind::*;
        let op = match token.kind {
            Plus => Self::Add,
            Minus => Self::Sub,
            Star => Self::Mul,
            Slash => Self::Div,
            _ => {
                return Err(Error::UnexpectedToken {
                    found: token,
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
    pub item: Box<Expr>,
    pub op: UnaryOp,
}

#[derive(Debug)]
pub enum UnaryOp {
    // TODO: Maybe change the name...
    MarkPositive,
    Negate,
}

impl TryFrom<Token> for UnaryOp {
    type Error = Error;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        let op = match value.kind {
            TokenKind::Plus => UnaryOp::MarkPositive,
            TokenKind::Minus => UnaryOp::Negate,
            _ => {
                return Err(Error::UnexpectedToken {
                    found: value,
                    expected: ExpectedTokens::OneOf(&[TokenKind::Plus, TokenKind::Minus]),
                    expected_msg: None,
                });
            }
        };

        Ok(op)
    }
}

#[derive(Debug)]
pub struct LambdaExpr {
    pub args: Box<Expr>,
    pub body: Box<Expr>,
}

#[derive(Debug)]
pub struct LiteralExp {
    // TODO: Maybe use &'a str
    pub literal: String,
}

impl Parse for LiteralExp {
    fn parse(tokens: &mut Tokens) -> AResult<Self>
    where
        Self: Sized,
    {
        match tokens.next().cloned() {
            Some(Token {
                kind: TokenKind::Literal,
                span,
            }) => Ok(Self {
                literal: tokens.get_span(span).to_owned(),
            }),
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
    pub int: i32,
}

impl Parse for IntegerExpr {
    fn parse(tokens: &mut Tokens) -> AResult<Self>
    where
        Self: Sized,
    {
        match tokens.next().cloned() {
            Some(Token {
                kind: TokenKind::Number,
                span,
            }) => Ok(Self {
                int: tokens.get_span(span).parse::<i32>()?,
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
    pub ident: String,
}

impl Parse for IdentExpr {
    fn parse(tokens: &mut Tokens) -> AResult<Self>
    where
        Self: Sized,
    {
        match tokens.next().cloned() {
            Some(Token {
                kind: TokenKind::Ident,
                span,
            }) => Ok(Self {
                ident: tokens.get_span(span).to_owned(),
            }),
            Some(t) => Err(Error::UnexpectedToken {
                found: t,
                expected: ExpectedTokens::Single(TokenKind::Ident),
                expected_msg: None,
            }),
            None => Err(Error::Eof),
        }
    }
}

pub fn expr(tokens: &mut Tokens, min_bp: u8) -> AResult<Expr> {
    fn eat_atom(tokens: &mut Tokens) -> AResult<Expr> {
        let Some(token) = tokens.next() else {
            return Err(Error::Eof);
        };

        let bp_opt = binding_power(token.kind).ok();

        use TokenKind::*;
        let expr = match token.kind {
            Number => Expr::Integer(IntegerExpr {
                int: tokens.get_span(token.span).to_owned().parse()?,
            }),
            Ident => Expr::Ident(IdentExpr {
                ident: tokens.get_span(token.span).to_owned(),
            }),
            Literal => Expr::Literal(LiteralExp {
                literal: tokens.get_span(token.span).to_owned(),
            }),
            LeftParen => {
                let lhs = expr(tokens, 0)?;
                assert!(tokens.next().is_some_and(|t| t.is(RightParen)));
                lhs
            }
            _ if bp_opt.is_some_and(|bp| bp.prefix.is_some()) => {
                // PANICS: We can unwrap here because the statement is
                // unreachable unless it's `Some`.
                let right_bp = bp_opt.and_then(|bp| bp.prefix).unwrap();
                let rhs = expr(tokens, right_bp)?;
                Expr::Unary(UnaryExpr {
                    item: Box::new(rhs),
                    op: UnaryOp::try_from(*token)?,
                })
            }
            _ => {
                return Err(Error::UnexpectedToken {
                    found: *token,
                    expected: ExpectedTokens::OneOf(&[Number, Ident, Literal]),
                    expected_msg: Some(" or a lambda expression"),
                });
            }
        };
        Ok(expr)
    }

    use TokenKind::*;
    let mut lhs = eat_atom(tokens)?;

    loop {
        let Some(token) = tokens.peek().cloned() else {
            break;
        };
        if token.kind == Eof {
            break;
        }

        let bp = binding_power(token.kind).ok();

        if let Some((l_bp, r_bp)) = bp.and_then(|bp| bp.infix) {
            if l_bp < min_bp {
                break;
            }

            tokens.next();
            let rhs = expr(tokens, r_bp)?;

            if token.kind == LambdaStart {
                let ex = Expr::Lambda(LambdaExpr {
                    args: Box::new(lhs.flatten_sequence()),
                    body: Box::new(rhs),
                });
                lhs = ex;
                continue;
            }

            let ex = Expr::Binary(BinaryExpr {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op: BinaryOp::new(token)?,
            });
            lhs = ex;

            continue;
        }

        // Sequence handling
        match eat_atom(tokens) {
            Ok(atom) => {
                lhs = Expr::Sequence(Sequence {
                    seq: vec![lhs, atom],
                });
                continue;
            }
            Err(e) => {
                println!("Sequence parsing error: {e}");
            }
        }

        // if bp.is_none() {
        //     // All previous varians have failed which means it's a sequece
        //     let rhs = dbg!(expr(tokens, 0))?;
        //     let sequece = Sequence {
        //         seq: vec![lhs, rhs],
        //     };
        //     lhs = Expr::Sequence(sequece);

        //     continue;
        // }

        break;
    }

    Ok(lhs)
}

#[derive(Debug, Default, Clone, Copy)]
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
        LambdaStart => BindingPower::new().infix(2, 1),
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
    pub fn parse_till(
        tokens: &mut Tokens,
        stop: &Token,
        separator: Option<&Token>,
    ) -> AResult<Self> {
        let mut seq = Vec::new();

        // Do we expect to see an item right now?
        let mut is_item = true;
        loop {
            let Some(token) = tokens.peek() else {
                break;
            };

            if token == stop {
                break;
            }

            if is_item {
                let item = T::parse(tokens)?;
                seq.push(item);
                if separator.is_some() {
                    is_item = !is_item;
                }
            }

            if let Some(sep) = separator {
                // PANICS: TODO
                let token = tokens.next().unwrap();
                if token == sep {
                    continue;
                } else {
                    return Err(Error::UnexpectedToken {
                        found: token.clone(),
                        expected: ExpectedTokens::FullToken(stop.clone()),
                        expected_msg: None,
                    });
                }
            }
        }

        Ok(Self { seq })
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn expr_test() {
        // let lexer = Lexer::new("a + b * \"literal!!\"");
        // let _expr = expr(&mut lexer.tokens(), 0).unwrap();
        // println!("{expr:#?}")
    }
}
