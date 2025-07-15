use std::num::ParseIntError;

use crate::lexer::{Lexer, Token, TokenKind};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Unexpected token found {found:?} {}", expected_msg.as_deref().unwrap_or(""))]
    UnexpectedToken {
        found: Token,
        expected_msg: Option<String>,
    },
    #[error("Binding power can't be computed for {op:?}")]
    BindingPowerInvalidOp { op: TokenKind },
    #[error(transparent)]
    ParseIntError(#[from] ParseIntError),
}

pub type AResult<T, E = Error> = core::result::Result<T, E>;

#[derive(Debug)]
pub enum Expr {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Block(Vec<Box<Expr>>),
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
                    expected_msg: Some(format!(" expected a valid binary operation.")),
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
pub struct LiteralExp {
    // TODO: Maybe use &'a str
    literal: String,
}

#[derive(Debug)]
pub struct IntegerExpr {
    int: i32,
}

#[derive(Debug)]
pub struct IdentExpr {
    ident: String,
}

pub fn expr(lexer: &mut Lexer, min_bp: u8) -> AResult<Expr> {
    let Token { kind, data } = lexer.next_token();

    use TokenKind::*;
    let mut lhs = match kind {
        Number => Expr::Integer(IntegerExpr { int: data.parse()? }),
        Ident => Expr::Ident(IdentExpr { ident: data }),
        Literal => Expr::Literal(LiteralExp { literal: data }),
        _ => {
            return Err(Error::UnexpectedToken {
                found: Token { kind, data },
                expected_msg: None,
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
