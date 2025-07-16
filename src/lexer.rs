use std::{fmt::Display, ops::Range};

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
    ArrowRight,
    /// =>
    ArrawRightBold,

    // Keywords
    Def,
    As,

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
    pub kind: TokenKind,
    pub data: String,
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
/// # use lilium::lexer::take;
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
        let tokens = Self::parse_tokens(source);
        // tokens.reverse();
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

        let mut inp = source;
        let i = &mut inp;

        while !i.is_empty() {
            // Tabs and whitespaces
            t(i, take_while(|c| c == ' '), skip);
            t(i, take_while(|c| c == '\t'), skip);

            // Newlines
            t(i, many1(tag("\r\n")), |_| {
                push_t(TokenKind::NewLine, "NEW_LINE")
            });
            t(i, many1(tag("\n")), |_| {
                push_t(TokenKind::NewLine, "NEW_LINE")
            });
            t(i, many1(tag("\r")), |_| {
                push_t(TokenKind::NewLine, "NEW_LINE")
            });

            // Delimiters
            t(i, tag("("), |o| push_t(TokenKind::LeftParen, o));
            t(i, tag(")"), |o| push_t(TokenKind::RightParen, o));
            t(i, tag("["), |o| push_t(TokenKind::LeftBracket, o));
            t(i, tag("]"), |o| push_t(TokenKind::RightBracket, o));
            t(i, tag("{"), |o| push_t(TokenKind::LeftBrace, o));
            t(i, tag("}"), |o| push_t(TokenKind::RightBrace, o));

            // Arrows
            t(i, tag("->"), |o| push_t(TokenKind::ArrowRight, o));
            t(i, tag("|->"), |o| push_t(TokenKind::LambdaStart, o));
            t(i, tag("=>"), |o| push_t(TokenKind::ArrawRightBold, o));

            // Operations
            t(i, tag("+"), |o| push_t(TokenKind::Plus, o));
            t(i, tag("-"), |o| push_t(TokenKind::Minus, o));
            t(i, tag("*"), |o| push_t(TokenKind::Star, o));
            t(i, tag("/"), |o| push_t(TokenKind::Slash, o));
            t(i, tag("="), |o| push_t(TokenKind::Equal, o));

            t(i, tag("!"), |o| push_t(TokenKind::ExplanationMark, o));
            t(i, tag("?"), |o| push_t(TokenKind::QuestionMark, o));
            t(i, tag(":"), |o| push_t(TokenKind::Colon, o));
            t(i, tag(","), |o| push_t(TokenKind::Comma, o));

            // Keywords
            t(i, tag("def"), |o| push_t(TokenKind::Def, o));
            t(i, tag("as"), |o| push_t(TokenKind::As, o));

            // Literal
            t(
                i,
                (tag("\""), take_till(|c| c == '\"'), tag("\"")),
                |(_, literal, _)| push_t(TokenKind::Literal, literal),
            );

            // Number
            t(i, take_while1(|c| c.is_ascii_digit()), |o| {
                push_t(TokenKind::Number, o)
            });

            // Identifier
            t(
                i,
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
            t(i, (tag("//"), take_till(|c| c == '\n' || c == '\r')), skip);
        }

        tokens
    }

    pub fn tokens(&self) -> Tokens<'_> {
        Tokens {
            buffer: &self.tokens,
            cursor: 0,
        }
    }
}

const DISPLAY_DATA_PAD: usize = 20;
impl Display for Lexer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for token in &self.tokens {
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

pub struct Tokens<'a> {
    buffer: &'a [Token],
    cursor: usize,
}

impl<'a> Tokens<'a> {
    /// Take an idependent iterator over a region bounded by `range`.
    pub fn region(&self, range: Range<usize>) -> Tokens<'a> {
        let buffer = &self.buffer[range];
        Tokens { buffer, cursor: 0 }
    }

    // TODO: Should be wrapped around `Peekable<...>` instead?
    pub fn peek(&self) -> Option<&Token> {
        self.buffer.get(self.cursor)
    }
}

impl<'a> Iterator for Tokens<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<Self::Item> {
        let idx = self.cursor;
        if self.cursor != self.buffer.len() {
            self.cursor += 1;
        }
        self.buffer.get(idx)
    }
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
}
