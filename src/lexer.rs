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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    start: usize,
    end: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
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
    source: String,
    /// Tokens order is reversed so the `next` and `peek` methods work
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        let tokens = Self::parse_tokens(source);
        // tokens.reverse();
        Self {
            source: source.to_string(),
            tokens,
        }
    }

    pub fn parse_tokens(source: &str) -> Vec<Token> {
        /// Returns `true` if the parse was successful and `false` if not.
        fn t<I: Copy, O>(
            inp: &mut I,
            mut combinator: impl Combinator<I, Output = O>,
            fun: impl FnOnce(I, O),
        ) -> bool {
            if let Some((i, out)) = combinator.process(*inp) {
                *inp = i;
                fun(i, out);
                true
            } else {
                false
            }
        }

        fn skip<I, O>(_: I, _: O) {}

        let mut tokens = Vec::new();

        // let mut push_t = |kind, data: &str| {
        //     tokens.push(Token::new(kind, data));
        // };

        let initial_len = source.len();
        let make_span = |rest: &str, data: &str| {
            let start = initial_len - rest.len();
            Span {
                start,
                end: start + data.len(),
            }
        };
        let mut push = |kind, span| {
            tokens.push(Token::new(kind, span));
        };

        let mut inp = source;
        let i = &mut inp;

        while !i.is_empty() {
            // Tabs and whitespaces
            t(i, take_while(|c| c == ' '), skip);
            t(i, take_while(|c| c == '\t'), skip);

            // Newlines
            t(i, many1(tag("\r\n")), skip);
            t(i, many1(tag("\n")), skip);
            t(i, many1(tag("\r")), skip);

            // Delimiters
            t(i, tag("("), |r, o| {
                push(TokenKind::LeftParen, make_span(r, o))
            });
            t(i, tag(")"), |r, o| {
                push(TokenKind::RightParen, make_span(r, o))
            });
            t(i, tag("["), |r, o| {
                push(TokenKind::LeftBracket, make_span(r, o))
            });
            t(i, tag("]"), |r, o| {
                push(TokenKind::RightBracket, make_span(r, o))
            });
            t(i, tag("{"), |r, o| {
                push(TokenKind::LeftBrace, make_span(r, o))
            });
            t(i, tag("}"), |r, o| {
                push(TokenKind::RightBrace, make_span(r, o))
            });

            // Arrows
            t(i, tag("->"), |r, o| {
                push(TokenKind::ArrowRight, make_span(r, o))
            });
            t(i, tag("|->"), |r, o| {
                push(TokenKind::LambdaStart, make_span(r, o))
            });
            t(i, tag("=>"), |r, o| {
                push(TokenKind::ArrawRightBold, make_span(r, o))
            });

            // Operations
            t(i, tag("+"), |r, o| push(TokenKind::Plus, make_span(r, o)));
            t(i, tag("-"), |r, o| push(TokenKind::Minus, make_span(r, o)));
            t(i, tag("*"), |r, o| push(TokenKind::Star, make_span(r, o)));
            t(i, tag("/"), |r, o| push(TokenKind::Slash, make_span(r, o)));
            t(i, tag("="), |r, o| push(TokenKind::Equal, make_span(r, o)));

            t(i, tag("!"), |r, o| {
                push(TokenKind::ExplanationMark, make_span(r, o))
            });
            t(i, tag("?"), |r, o| {
                push(TokenKind::QuestionMark, make_span(r, o))
            });
            t(i, tag(":"), |r, o| push(TokenKind::Colon, make_span(r, o)));
            t(i, tag(","), |r, o| push(TokenKind::Comma, make_span(r, o)));

            // Keywords
            t(i, tag("def"), |r, o| push(TokenKind::Def, make_span(r, o)));
            t(i, tag("as"), |r, o| push(TokenKind::As, make_span(r, o)));

            // Literal
            t(
                i,
                (tag("\""), take_till(|c| c == '\"'), tag("\"")),
                |r, (_, literal, _)| {
                    let mut span = make_span(r, literal);
                    span.start -= 1;
                    span.end += 1;
                    push(TokenKind::Literal, span);
                },
            );

            // Number
            t(i, take_while1(|c| c.is_ascii_digit()), |r, o| {
                push(TokenKind::Number, make_span(r, o))
            });

            // Identifier
            t(
                i,
                (
                    // Idents can't start from numbers so we use two `take_while`s
                    take_while1(|c| c.is_ascii_alphabetic()),
                    take_while(|c| c.is_ascii_alphanumeric()),
                ),
                |r, (first, second)| {
                    let mut s = String::new();
                    s.push_str(first);
                    s.push_str(second);
                    push(TokenKind::Ident, make_span(r, &s));
                },
            );

            // Comment
            t(i, (tag("//"), take_till(|c| c == '\n' || c == '\r')), skip);
        }

        tokens
    }

    pub fn tokens(&self) -> Tokens<'_> {
        Tokens::new(&self.source, &self.tokens)
    }
}

#[derive(Debug, Clone)]
pub struct Tokens<'a> {
    source: &'a str,
    buffer: &'a [Token],
    /// Points to the **next** token.
    cursor: usize,
    range: Range<usize>,
}

impl<'a> Tokens<'a> {
    pub fn new(source: &'a str, buffer: &'a [Token]) -> Self {
        Self {
            source,
            buffer,
            cursor: 0,
            range: 0..buffer.len(),
        }
    }

    pub fn source(&self) -> &str {
        self.source
    }

    pub fn get_span(&self, span: Span) -> &str {
        &self.source()[span.start..span.end]
    }

    /// Take an idependent iterator over a region bounded by `range`.
    pub fn region(&self, range: Range<usize>) -> Tokens<'a> {
        Tokens {
            source: &self.source,
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

const DISPLAY_DATA_PAD: usize = 20;
impl Display for Tokens<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for token in self.clone() {
            write!(f, "{:?}", token.kind)?;
            // TODO: Use custom write impl that counts bytes
            let length = format!("{:?}", token.kind).len();
            for _ in 0..(DISPLAY_DATA_PAD - length) {
                write!(f, " ")?;
            }
            writeln!(f, "{}", &self.source[token.span.start..token.span.end])?;
        }

        Ok(())
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
        let tokens = lexer.tokens();
        println!("tokens:\n{tokens}");
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
