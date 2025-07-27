use std::{fmt::Display, ops::Range};

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

    pub fn is(&self, kind: TokenKind) -> bool {
        self.kind == kind
    }
}

/// Returns (rest, tag).
// TODO: Use custom result.
pub fn tag(tag: &str) -> impl Fn(&str) -> Option<(&str, &str)> {
    // let tag = tag.clone();
    move |input| {
        let len = tag.len();
        if input.starts_with(tag) {
            Some((take_from(input, len), take(input, len)))
        } else {
            None
        }
    }
}

// / ## Examples
// / ```rust
// / # use lilium::lexer::take;
// / let s = "Hello World!";
// / let out = take(s, 5);
// / assert_eq!(out, "Hello");
// / ```
// TODO: Use custom result.
pub fn take(input: &str, index: usize) -> &str {
    &input[..index]
}

pub fn take_from(input: &str, index: usize) -> &str {
    &input[index..]
}

pub fn take_while<F>(condition: F) -> impl Fn(&str) -> Option<(&str, &str)>
where
    F: Fn(char) -> bool,
{
    move |input| {
        input
            .find(|c| !condition(c))
            .map(|index| (take_from(input, index), take(input, index)))
            // If there's no non-matching character then all input is valid
            .or_else(|| {
                let len = input.len();
                Some((take_from(input, len), take(input, len)))
            })
    }
}

pub fn take_while1<F>(condition: F) -> impl Fn(&str) -> Option<(&str, &str)>
where
    F: Fn(char) -> bool,
{
    move |input| take_while(&condition)(input).filter(|(_, o)| !o.is_empty())
}

pub fn take_till<F>(condition: F) -> impl Fn(&str) -> Option<(&str, &str)>
where
    F: Fn(char) -> bool,
{
    move |input| {
        input
            .find(&condition)
            .map(|index| (take_from(input, index), take(input, index)))
    }
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
        Tokens::new(&self.tokens)
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

#[derive(Debug, Clone)]
pub struct Tokens<'a> {
    buffer: &'a [Token],
    /// Points to the **next** token.
    cursor: usize,
    range: Range<usize>,
}

impl<'a> Tokens<'a> {
    pub fn new(buffer: &'a [Token]) -> Self {
        Self {
            buffer,
            cursor: 0,
            range: 0..buffer.len(),
        }
    }

    /// Take an idependent iterator over a region bounded by `range`.
    pub fn region(&self, range: Range<usize>) -> Tokens<'a> {
        Tokens {
            buffer: &self.buffer,
            cursor: range.start,
            range,
        }
    }

    pub fn child(&self) -> Self {
        self.clone()
    }

    // TODO: Should be wrapped around `Peekable<...>` instead?
    pub fn peek(&self) -> Option<&Token> {
        self.buffer.get(self.cursor)
    }

    /// Peeks `n` elements furthen.
    /// `peek_n(0)` is equal to `peek()`.
    pub fn peek_n(&self, n: usize) -> Option<&Token> {
        self.buffer.get(self.cursor + n)
    }

    pub fn cursor(&self) -> usize {
        self.cursor
    }

    pub fn index(&self) -> usize {
        self.cursor - 1
    }

    /// Returns `true` if the cursor was moved and `false` if it is out of bounds.
    pub fn move_cursor_to(&mut self, index: usize) -> bool {
        if self.range.contains(&index) {
            self.cursor = index;
            true
        } else {
            false
        }
    }

    /// Moves cursor to the token next to the one for which `condition` returned `true`.
    /// Returns `true` if the cursor was moved and `false` if matching token was not found.
    /// In case of a falue all `next` calls will return `None`.
    ///
    /// Calling `next` after this method will return the token following the mactched one.
    pub fn skip_till(&mut self, mut condition: impl FnMut(&Token) -> bool) -> bool {
        while let Some(token) = self.next() {
            if condition(token) {
                return true;
            }
        }
        false
    }
}

impl<'a> Iterator for Tokens<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<Self::Item> {
        let idx = self.cursor;
        self.cursor = (self.cursor + 1).min(self.range.end);
        self.buffer.get(idx)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer_test() {
        let o = take_while1(|c| c.is_ascii_digit())("");
        println!("o={o:#?}");

        let lexer = Lexer::new("a  + b *\"literal!!\"");
        println!("lexer:\n{lexer}");
    }

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

        let out = take_while(|c| c.is_ascii_digit())("53");
        assert_eq!(out, Some(("", "53")));

        let out = take_while1(|c| c.is_ascii_digit())("as");
        assert_eq!(out, None);

        let out = take_while1(|c| c.is_ascii_digit())("123as");
        assert_eq!(out, Some(("as", "123")));

        let out = take_while1(|c| c.is_ascii_digit())("as123");
        assert_eq!(out, None);

        let out = take_while1(|c| c.is_ascii_digit())("123");
        assert_eq!(out, Some(("", "123")));
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
