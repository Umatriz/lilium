use std::fmt::Display;

use clap::builder::IntoResettable;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    ExplanationMark,
    QuestionMark,
    Colon,
    Comma,

    Number,
    Ident,
    Literal,

    /// |->
    LambdaStart,
    /// ->
    ArrawRight,
    /// =>
    ArrawRightBold,

    // Keywords
    Def,

    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,

    NewLine,

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

/// Returns (rest, tag).
// TODO: Use custom result.
pub fn tag(tag: &str) -> impl Fn(&str) -> Option<(&str, &str)> {
    // let tag = tag.clone();
    move |input| {
        let len = tag.len();
        if input.starts_with(tag) {
            take(len)(input)
        } else {
            None
        }
    }
}

/// ## Examples
/// ```rust
/// # use lilium::parser::take;
/// let s = "Hello World!";
/// let out = take(5)(s);
/// assert_eq!(out, Some((" World!", "Hello")));
/// ```
// TODO: Use custom result.
pub fn take(offset_bytes: usize) -> impl Fn(&str) -> Option<(&str, &str)> {
    move |input| {
        input
            .split_at_checked(offset_bytes)
            .map(|(tag, rest)| (rest, tag))
    }
}

pub fn take_while<F>(condition: F) -> impl Fn(&str) -> Option<(&str, &str)>
where
    F: Fn(char) -> bool,
{
    move |input| {
        input
            .find(|c| !condition(c))
            .and_then(|index| take(index)(input))
            .or_else(|| {
                let len = input.len();
                take(len)(input)
            })
    }
}

pub fn take_while1<F>(condition: F) -> impl Fn(&str) -> Option<(&str, &str)>
where
    F: Fn(char) -> bool,
{
    move |input| {
        input
            .find(|c| !condition(c))
            .filter(|index| *index != 0)
            .and_then(|index| take(index)(input))
    }
}

pub fn take_till<F>(condition: F) -> impl Fn(&str) -> Option<(&str, &str)>
where
    F: Fn(char) -> bool,
{
    move |input| input.find(&condition).and_then(|index| take(index)(input))
}

pub fn many0<I: Clone, O>(
    mut combinator: impl Combinator<I, Output = O>,
) -> impl FnMut(I) -> Option<(I, Vec<O>)> {
    move |input| {
        let mut outs = Vec::new();
        let mut input = input;
        while let Some((i, out)) = combinator.process(input.clone()) {
            outs.push(out);
            input = i;
        }
        Some((input, outs))
    }
}

pub fn many1<I: Clone, O>(
    mut combinator: impl Combinator<I, Output = O>,
) -> impl FnMut(I) -> Option<(I, Vec<O>)> {
    move |input| {
        let mut outs = Vec::new();
        let mut input = input;
        while let Some((i, out)) = combinator.process(input.clone()) {
            outs.push(out);
            input = i;
        }
        if outs.is_empty() {
            None
        } else {
            Some((input, outs))
        }
    }
}

pub trait Combinator<Input> {
    type Output;
    /// Process input and return (rest, output).
    fn process(&mut self, input: Input) -> Option<(Input, Self::Output)>;
}

impl<I, O, F> Combinator<I> for F
where
    F: FnMut(I) -> Option<(I, O)>,
{
    type Output = O;
    fn process(&mut self, input: I) -> Option<(I, O)> {
        (self)(input)
    }
}

macro_rules! impl_combinator {
    (
        $(($C:ident, $O:ident, $c:ident, $o:ident)),*
    ) => {
        impl<I, $($C, $O),*> Combinator<I> for ($($C,)*)
        where
            $(
                $C: Fn(I) -> Option<(I, $O)>,
            )*
        {
            type Output = ($($O,)*);
            fn process(&mut self, input: I) -> Option<(I, Self::Output)> {
                let ($(ref mut $c,)*) = *self;
                let i = input;

                $(let (i, $o) = $c.process(i)?;)*

                Some((i, ($($o,)*)))
            }
        }
    };
}

variadics_please::all_tuples!(impl_combinator, 0, 16, C, O, c, o);

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

    pub fn parse_tokens(source: &str) -> Vec<Token> {
        /// Returns `true` if the parse was successful and `false` if not.
        fn t<I: Copy, O>(
            inp: &mut I,
            mut combinator: impl Combinator<I, Output = O>,
            fun: impl FnOnce(O),
        ) -> bool {
            if let Some((i, out)) = combinator.process(*inp) {
                *inp = i;
                fun(out);
                true
            } else {
                false
            }
        }

        fn skip<O>(_: O) {}

        let mut tokens = Vec::new();

        let mut push_t = |kind, data: &str| {
            tokens.push(Token::new(kind, data));
        };

        let mut i = source;

        while !i.is_empty() {
            // Tabs and whitespaces
            t(&mut i, take_while(|c| c == ' '), skip);
            t(&mut i, take_while(|c| c == '\t'), skip);

            // Newlines
            //
            // We continue when we match one of the variants so it doesn't produce truple tokens on Windows
            if t(&mut i, tag("\r\n"), |_| {
                push_t(TokenKind::NewLine, "NEW_LINE")
            }) {
                continue;
            };
            if t(&mut i, tag("\n"), |_| {
                push_t(TokenKind::NewLine, "NEW_LINE")
            }) {
                continue;
            };
            if t(&mut i, tag("\r"), |_| {
                push_t(TokenKind::NewLine, "NEW_LINE")
            }) {
                continue;
            };

            // Delimiters
            t(&mut i, tag("("), |o| push_t(TokenKind::LeftParen, o));
            t(&mut i, tag(")"), |o| push_t(TokenKind::RightParen, o));
            t(&mut i, tag("["), |o| push_t(TokenKind::LeftBracket, o));
            t(&mut i, tag("]"), |o| push_t(TokenKind::RightBracket, o));
            t(&mut i, tag("{"), |o| push_t(TokenKind::LeftBrace, o));
            t(&mut i, tag("}"), |o| push_t(TokenKind::RightBrace, o));

            // Arrows
            t(&mut i, tag("->"), |o| push_t(TokenKind::ArrawRight, o));
            t(&mut i, tag("|->"), |o| push_t(TokenKind::LambdaStart, o));
            t(&mut i, tag("=>"), |o| push_t(TokenKind::ArrawRightBold, o));

            // Operations
            t(&mut i, tag("+"), |o| push_t(TokenKind::Plus, o));
            t(&mut i, tag("-"), |o| push_t(TokenKind::Minus, o));
            t(&mut i, tag("*"), |o| push_t(TokenKind::Star, o));
            t(&mut i, tag("/"), |o| push_t(TokenKind::Slash, o));
            t(&mut i, tag("="), |o| push_t(TokenKind::Equal, o));

            t(&mut i, tag("!"), |o| push_t(TokenKind::ExplanationMark, o));
            t(&mut i, tag("?"), |o| push_t(TokenKind::QuestionMark, o));
            t(&mut i, tag(":"), |o| push_t(TokenKind::Colon, o));
            t(&mut i, tag(","), |o| push_t(TokenKind::Comma, o));

            // Literal
            t(
                &mut i,
                (tag("\""), take_till(|c| c == '\"'), tag("\"")),
                |(_, literal, _)| push_t(TokenKind::Literal, literal),
            );

            // Number
            t(&mut i, take_while1(|c| c.is_ascii_digit()), |o| {
                push_t(TokenKind::Number, o)
            });

            // Identifier
            t(
                &mut i,
                (
                    // Idents can't start from numbers so we use two `take_while`s
                    take_while1(|c| c.is_ascii_alphabetic()),
                    take_while(|c| c.is_ascii_alphanumeric()),
                ),
                |(first, second)| {
                    let mut s = String::new();
                    s.push_str(first);
                    s.push_str(second);
                    push_t(TokenKind::Ident, &s);
                },
            );

            // Comment
            t(
                &mut i,
                (tag("//"), take_till(|c| c == '\n' || c == '\r')),
                skip,
            );
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

const DISPLAY_DATA_PAD: usize = 20;
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
    Op(TokenKind, OperationKind, Vec<Expr>),
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
enum OperationKind {
    Binary,
    Unary,
    Postfix,
    Ternary,
    Indexing,
}

impl TokenKind {
    fn format_op(&self) -> Option<&str> {
        let op = match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Star => "*",
            Self::Slash => "/",
            Self::ExplanationMark => "!",
            Self::LeftBracket => "[",
            Self::QuestionMark => "?",
            _ => return None,
        };
        Some(op)
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

                    match kind {
                        OperationKind::Binary => {
                            format(f, &exprs[0], is_first)?;
                            write!(f, " {} ", operation.format_op().unwrap())?;
                            format(f, &exprs[1], is_first)?;
                        }
                        OperationKind::Unary => {
                            write!(f, "{}", operation.format_op().unwrap())?;
                            format(f, &exprs[0], is_first)?;
                        }
                        OperationKind::Postfix => {
                            format(f, &exprs[0], is_first)?;
                            write!(f, "{}", operation.format_op().unwrap())?;
                        }
                        OperationKind::Ternary => {
                            format(f, &exprs[0], is_first)?;
                            write!(f, " ? ")?;
                            format(f, &exprs[1], is_first)?;
                            write!(f, " : ")?;
                            format(f, &exprs[2], is_first)?;
                        }
                        OperationKind::Indexing => {
                            format(f, &exprs[0], is_first)?;
                            write!(f, "[")?;
                            format(f, &exprs[1], is_first)?;
                            write!(f, "]")?;
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
    // TODO: Use match somehow...
    let mut lhs = if let Some(item) = Item::from_kind(kind) {
        Expr::Item(item, data)
    } else if let Some((_, r_bp)) = binding_power(kind, OperationKind::Unary) {
        let rhs = expr(lexer, r_bp);
        Expr::Op(kind, OperationKind::Unary, vec![rhs])
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
        let kind = *kind;

        if let Some((l_bp, _)) = binding_power(kind, OperationKind::Postfix) {
            if l_bp < min_bp {
                break;
            }
            lexer.next_token();

            lhs = if kind == TokenKind::LeftBracket {
                let rhs = expr(lexer, 0);
                assert_eq!(lexer.next_token(), Token::new(TokenKind::RightBracket, "]"));
                Expr::Op(kind, OperationKind::Indexing, vec![lhs, rhs])
            } else {
                Expr::Op(kind, OperationKind::Postfix, vec![lhs])
            };

            continue;
        }

        if let Some((l_bp, r_bp)) = binding_power(kind, OperationKind::Binary) {
            if l_bp < min_bp {
                break;
            }
            lexer.next_token();

            lhs = if kind == TokenKind::QuestionMark {
                let mhs = expr(lexer, 0);
                assert_eq!(lexer.next_token(), Token::new(TokenKind::Colon, ":"));
                let rhs = expr(lexer, 0);
                Expr::Op(kind, OperationKind::Ternary, vec![lhs, mhs, rhs])
            } else {
                let rhs = expr(lexer, r_bp);
                Expr::Op(kind, OperationKind::Binary, vec![lhs, rhs])
            };

            continue;
        }

        break;
    }

    lhs
}

/// This function returns `Some` with binding powers of the operator if it satisfies the
/// specified `expected_kind`. It will return none if the operation does not match a kind.
///
/// In some cases on of the binging powers does not make sense and will be set to 0. For
/// `Unary` it will always be the first one and for `Postfix` it will always be the second one.
fn binding_power(op: TokenKind, expected_kind: OperationKind) -> Option<(u8, u8)> {
    use OperationKind as K;
    use TokenKind as T;

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tag_test() {
        let out = tag("abc")("abc123");
        assert_eq!(out, Some(("123", "abc")));

        let out = tag("\r\n")("123\n\r");
        assert!(out.is_none());

        let out = tag("\r\n")("\r\ndef x");
        assert_eq!(out, Some(("def x", "\r\n")));

        let out = tag(" ")("     <- 5 whitespaces");
        assert_eq!(out, Some(("    <- 5 whitespaces", " ")));
    }

    #[test]
    fn take_while_test() {
        let out = take_while(|c| c.is_ascii_alphabetic())("abc123");
        assert_eq!(out, Some(("123", "abc")));

        let ident = (
            // TODO: SOMETHING'S WRONG WITH THESE AND HOW THEY COMBINE
            take_while(|c| c.is_ascii_alphabetic()),
            take_while(|c| c.is_ascii_alphanumeric()),
        )
            .process("def");
        println!("{ident:?}");

        // let literal = (tag("\""), take_till(|c| c == '\"'), tag("\"")).process("\"literal\" rest");
    }

    #[test]
    fn take_till_test() {
        let out = take_till(|c| c == '!')("123abc q! qwe");
        assert_eq!(out, Some(("! qwe", "123abc q")));
    }

    // #[test]
    // fn parse_literal_test() {
    //     let out = (tag("\""), take_till(|c| c == '\"'), tag("\"")).process("\"literal\"");
    //     assert_eq!(out, Some(("", ("\"", "literal", "\"",))));
    // }

    #[test]
    fn parse_test() {
        // let s = "def";
        // let mut tokens = Lexer::parse_tokens2(s);
        // tokens.reverse();
        // let lexer = Lexer { tokens };
        // println!("{lexer}");
    }

    #[test]
    fn expr_test() {
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
        assert_eq!(s.to_string(), "(x[0])[1]");

        let s = expr(&mut Lexer::new("a ? b : c ? d : e"), 0);
        assert_eq!(s.to_string(), "a ? b : (c ? d : e)");
    }
}
