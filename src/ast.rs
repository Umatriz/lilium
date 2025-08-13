use std::{borrow::Cow, fmt::Display, num::ParseIntError};

use crate::lexer::{Token, TokenKind, Tokens};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Unexpected token found {found:?} expected {expected}")]
    UnexpectedToken {
        found: Token,
        expected: ExpectedTokensWithMessage,
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
    Single(TokenKind),
    OneOf(&'static [TokenKind]),
}

impl ExpectedTokens {
    pub fn check(&self, token: Token) -> bool {
        use ExpectedTokens::*;
        match self {
            Single(token_kind) => token.is(*token_kind),
            OneOf(token_kinds) => token_kinds.iter().any(|k| token.is(*k)),
        }
    }
}

impl From<TokenKind> for ExpectedTokens {
    fn from(value: TokenKind) -> Self {
        Self::Single(value)
    }
}

impl From<&'static [TokenKind]> for ExpectedTokens {
    fn from(value: &'static [TokenKind]) -> Self {
        Self::OneOf(value)
    }
}

impl Display for ExpectedTokens {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
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

#[derive(Debug)]
pub struct ExpectedTokensWithMessage {
    pub expected: ExpectedTokens,
    pub message: Option<Cow<'static, str>>,
}

impl Display for ExpectedTokensWithMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expected);
        if let Some(msg) = self.message.as_ref() {
            write!(f, " {msg}");
        }
        Ok(())
    }
}

impl<E, M> From<(E, M)> for ExpectedTokensWithMessage
where
    E: Into<ExpectedTokens>,
    M: Into<Cow<'static, str>>,
{
    fn from(value: (E, M)) -> Self {
        Self {
            expected: value.0.into(),
            message: Some(value.1.into()),
        }
    }
}

impl<E> From<E> for ExpectedTokensWithMessage
where
    E: Into<ExpectedTokens>,
{
    fn from(value: E) -> Self {
        Self {
            expected: value.into(),
            message: None,
        }
    }
}

/// This function can be used to expect a token and return an error if it
/// doesn't match the pattern.
///
/// # Examples
/// ```rust
/// # use lilium::ast::expect_token;
/// # use lilium::lexer::{TokenKind::*, Token};
/// # let t = Token::EOF;
/// expect_token(t, ([Comma, Colon].as_slice(), "This message will be added to the error"));
/// ```
pub fn expect_token<T: Into<Token>, E: Into<ExpectedTokensWithMessage>>(
    found: T,
    expect: E,
) -> AResult<Token> {
    let found = found.into();
    let expected = expect.into();
    if expected.expected.check(found) {
        Ok(found)
    } else {
        Err(Error::UnexpectedToken { found, expected })
    }
}

pub trait ExpectTokensExt {
    fn expect_token<E: Into<ExpectedTokensWithMessage>>(&mut self, expect: E) -> AResult<Token>;
    fn peek_expect<E: Into<ExpectedTokensWithMessage>>(&mut self, expect: E) -> AResult<Token>;
}

impl ExpectTokensExt for Tokens<'_> {
    fn expect_token<E: Into<ExpectedTokensWithMessage>>(&mut self, expect: E) -> AResult<Token> {
        expect_token(self.next().cloned(), expect)
    }

    fn peek_expect<E: Into<ExpectedTokensWithMessage>>(&mut self, expect: E) -> AResult<Token> {
        expect_token(self.peek().cloned(), expect)
    }
}

/// Construct `UnexpectedToken` error.
///
/// This function just creates an instance of the error. It **does not** check token
/// for matching the expected pattern.
///
/// See also [`expect_token`].
pub fn unexpected_token<T: Into<Token>, E: Into<ExpectedTokensWithMessage>>(
    token: T,
    expect: E,
) -> Error {
    let found = token.into();
    let expected = expect.into();
    Error::UnexpectedToken { found, expected }
}

pub type AResult<T, E = Error> = core::result::Result<T, E>;

pub trait Parse {
    fn parse(tokens: &mut Tokens) -> AResult<Self>
    where
        Self: Sized;
}

#[derive(Debug)]
pub enum Expr {
    Empty,
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Block(BlockExpr),
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
                return Err(unexpected_token(
                    token,
                    [Plus, Minus, Star, Slash].as_slice(),
                ));
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
                return Err(unexpected_token(
                    value,
                    [TokenKind::Plus, TokenKind::Minus].as_slice(),
                ));
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
        let token = expect_token(tokens.next().cloned(), TokenKind::Literal)?;
        Ok(Self {
            literal: tokens.get_span(token.span).to_owned(),
        })
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
        let token = expect_token(tokens.next().cloned(), TokenKind::Number)?;
        Ok(Self {
            int: tokens.get_span(token.span).parse::<i32>()?,
        })
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
        let token = expect_token(tokens.next().cloned(), TokenKind::Ident)?;
        Ok(Self {
            ident: tokens.get_span(token.span).to_owned(),
        })
    }
}

#[derive(Debug)]
pub struct BlockExpr {
    pub statements: Vec<Stmt>,
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
            LeftBrace => {
                let mut statements = Vec::new();
                loop {
                    let statement = Stmt::parse(tokens)?;
                    statements.push(statement);
                    if tokens.peek().is_some_and(|t| t.is(TokenKind::RightBrace)) {
                        break;
                    }
                }
                tokens.expect_token(TokenKind::RightBrace)?;
                Expr::Block(BlockExpr { statements })
            }
            _ if bp_opt.is_some_and(|bp| bp.prefix.is_some()) => {
                // PANICS: We can unwrap here because this branch is
                // unreachable unless `bp` is `Some`.
                let right_bp = bp_opt.and_then(|bp| bp.prefix).unwrap();
                let rhs = expr(tokens, right_bp)?;
                Expr::Unary(UnaryExpr {
                    item: Box::new(rhs),
                    op: UnaryOp::try_from(*token)?,
                })
            }
            _ => {
                return Err(unexpected_token(
                    *token,
                    (
                        [Number, Ident, Literal].as_slice(),
                        "or a lambda expression",
                    ),
                ));
            }
        };
        Ok(expr)
    }

    use TokenKind::*;
    let mut lhs = eat_atom(tokens)?;
    dbg!(&lhs);
    loop {
        let Some(token) = tokens.peek().cloned() else {
            break;
        };
        if token.kind == Eof {
            break;
        }

        if token.is(TokenKind::Semi) {
            break;
        }

        let bp = binding_power(token.kind).ok();

        if let Some((l_bp, r_bp)) = bp.and_then(|bp| bp.infix) {
            if l_bp < min_bp {
                break;
            }

            tokens.next();
            let rhs = expr(tokens, r_bp);

            let ex = match token.kind {
                LambdaStart => Expr::Lambda(LambdaExpr {
                    args: Box::new(lhs.flatten_sequence()),
                    body: Box::new(rhs?),
                }),
                Comma => {
                    // Trailing comma handling
                    // If the expression was successfuly parsed that means it is a valid expression and is a part of a sequence.
                    // If it failed to parse that means it's a trailing comma and right hand side is not valid.
                    if rhs.is_ok() {
                        Expr::Sequence(Sequence {
                            seq: vec![lhs, rhs?],
                        })
                        .flatten_sequence()
                    } else {
                        lhs
                    }
                }
                _ => Expr::Binary(BinaryExpr {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs?),
                    op: BinaryOp::new(token)?,
                }),
            };
            lhs = ex;

            continue;
        }

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
        Comma => BindingPower::new().infix(1, 2),
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

// impl<T: Parse> Sequence<T> {
//     /// Elements must be separated by the `separator` token.
//     /// Parsing will be stopped when `stop` token is met. Stop-token will **not** be consumed.
//     pub fn parse_till(
//         tokens: &mut Tokens,
//         stop: &Token,
//         separator: Option<&Token>,
//     ) -> AResult<Self> {
//         let mut seq = Vec::new();

//         // Do we expect to see an item right now?
//         let mut is_item = true;
//         loop {
//             let Some(token) = tokens.peek() else {
//                 break;
//             };

//             if token == stop {
//                 break;
//             }

//             if is_item {
//                 let item = T::parse(tokens)?;
//                 seq.push(item);
//                 if separator.is_some() {
//                     is_item = !is_item;
//                 }
//             }

//             if let Some(sep) = separator {
//                 // PANICS: TODO
//                 let token = tokens.next().unwrap();
//                 if token == sep {
//                     continue;
//                 } else {
//                     return Err(Error::UnexpectedToken {
//                         found: token.clone(),
//                         expected: ExpectedTokens::FullToken(stop.clone()),
//                         expected_msg: None,
//                     });
//                 }
//             }
//         }

//         Ok(Self { seq })
//     }
// }

/// {var}: {ty}
#[derive(Debug)]
pub struct TypeAssignment {
    var: IdentExpr,
    ty: IdentExpr,
}

impl Parse for TypeAssignment {
    fn parse(tokens: &mut Tokens) -> AResult<Self>
    where
        Self: Sized,
    {
        let var = IdentExpr::parse(tokens)?;
        tokens.expect_token(TokenKind::Colon)?;
        let ty = IdentExpr::parse(tokens)?;
        Ok(Self { var, ty })
    }
}

/// Statement
#[derive(Debug)]
pub enum Stmt {
    TerminatedExpr(Expr),
    Expr(Expr),
    Function {
        name: IdentExpr,
        args: Option<Vec<TypeAssignment>>,
        return_type: Option<IdentExpr>,
        body: Expr,
    },
    /// {ident} = {expr};
    VariableAssignment {
        name: IdentExpr,
        value: Expr,
    },
}

impl Parse for Stmt {
    fn parse(tokens: &mut Tokens) -> AResult<Self>
    where
        Self: Sized,
    {
        let Some(token) = tokens.next() else {
            return Err(Error::Eof);
        };

        match token.kind {
            TokenKind::Ident if tokens.peek().is_some_and(|t| t.is(TokenKind::Equal)) => {
                // Skip assignment operator
                tokens.next();
                let expr = expr(tokens, 0)?;
                Ok(Self::VariableAssignment {
                    name: IdentExpr {
                        ident: tokens.get_span(token.span).to_owned(),
                    },
                    value: expr,
                })
            }
            TokenKind::Def => {
                let name = IdentExpr::parse(tokens)?;

                tokens.expect_token(TokenKind::Colon)?;
                tokens.expect_token(TokenKind::Colon)?;

                let args = if tokens.peek_expect(TokenKind::LeftParen).is_ok() {
                    tokens.next();
                    let mut args = Vec::new();
                    loop {
                        let type_assignment = TypeAssignment::parse(tokens)?;
                        args.push(type_assignment);

                        let comma = tokens.expect_token(TokenKind::Comma);
                        match (comma, tokens.peek()) {
                            // Trailing comma
                            (Ok(_comma), Some(peek)) if peek.is(TokenKind::LeftParen) => {
                                // Eat the paren so we can continue parsing
                                tokens.expect_token(TokenKind::RightParen)?;
                                break;
                            }
                            // No trailing comma
                            (Err(Error::UnexpectedToken { found, .. }), _)
                                if found.is(TokenKind::RightParen) =>
                            {
                                break;
                            }
                            // Comma. We do not care about the peek and letting the next iteration handle any errors
                            (Ok(_comma), _) => {
                                continue;
                            }
                            // In case of an actuall error return it
                            (Err(err), _) => {
                                return Err(err);
                            }
                        }
                    }

                    Some(args)
                } else {
                    None
                };

                let return_type = if tokens.peek_expect(TokenKind::ArrowRight).is_ok() {
                    tokens.next();
                    Some(IdentExpr::parse(tokens)?)
                } else {
                    None
                };

                let body = expr(tokens, 0)?;
                Ok(Self::Function {
                    name,
                    args,
                    return_type,
                    body,
                })
            }
            _ => {
                // Put the unknown token back
                tokens.move_cursor_to(tokens.cursor() - 1);
                let expr = expr(tokens, 0)?;
                let peek = tokens.peek();
                match peek {
                    Some(token) if token.is(TokenKind::Semi) => {
                        tokens.expect_token(TokenKind::Semi)?;
                        Ok(Self::TerminatedExpr(expr))
                    }
                    _ => Ok(Self::Expr(expr)),
                }
            }
        }
    }
}

pub fn ast(tokens: &mut Tokens) -> AResult<Vec<Stmt>> {
    let mut stmts = Vec::new();
    loop {
        let stmt = Stmt::parse(tokens)?;
        stmts.push(stmt);

        if tokens.peek().is_none() {
            break;
        }
    }
    Ok(stmts)
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
