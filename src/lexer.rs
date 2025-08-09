use std::{fmt::Display, marker::PhantomData, ops::Range};

use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    /// Special kind of token that is used by the scanner when a token
    /// should be ignored.
    None,

    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    ExplanationMark,
    QuestionMark,
    Colon,
    /// ;
    Semi,
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

    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub const EOF: Self = Self { start: 0, end: 0 };

    pub fn concat(self, other: Self) -> Self {
        assert_eq!(self.end, other.start);
        Self {
            start: self.start,
            end: other.end,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub const EOF: Self = Self {
        kind: TokenKind::Eof,
        span: Span::EOF,
    };

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

#[derive(Debug, Default)]
pub struct Scanner<C, P, H, I, O, L> {
    matched: bool,
    context: C,
    input_extractor: P,
    handler: H,
    _markers: PhantomData<(I, O, L)>,
}

fn scan_explicit<'a, C, P, H, L, I, O>(
    matched: &mut bool,
    context: &'a mut C,
    input_extractor: &mut P,
    handler: H,
    local_context: L,
    mut combinator: impl Combinator<I, Output = O>,
) where
    P: Fn(&mut C) -> &mut I,
    I: Copy + 'a,
    H: FnOnce(&'a mut C, L, I, O),
{
    if *matched {
        return;
    }

    let is_matched = match combinator.process(*(input_extractor)(context)) {
        Some((i, o)) => {
            let input = (input_extractor)(context);
            *input = i;

            (handler)(context, local_context, i, o);

            true
        }
        None => false,
    };

    if is_matched {
        *matched = true;
    }
}

impl<C, P, H, I, O, L> Scanner<C, P, H, I, O, L>
where
    P: Fn(&mut C) -> &mut I,
    I: Copy,
    H: FnMut(&mut C, L, I, O),
{
    pub fn new(context: C, extractor: P, handler: H) -> Self {
        Self {
            matched: false,
            context,
            input_extractor: extractor,
            handler,
            _markers: PhantomData,
        }
    }

    pub fn scan(&mut self, combinator: impl Combinator<I, Output = O>, local_context: L) {
        scan_explicit(
            &mut self.matched,
            &mut self.context,
            &mut self.input_extractor,
            &mut self.handler,
            local_context,
            combinator,
        );
    }

    /// Returns the current value of `matched` and sets it to `false`
    pub fn take_matched(&mut self) -> bool {
        let matched = self.matched;
        self.matched = false;
        matched
    }
}

impl<C, P, H, I, O, L> Scanner<C, P, H, I, O, L>
where
    P: Fn(&mut C) -> &mut I,
    I: Copy,
{
    pub fn input(&mut self) -> I {
        *(self.input_extractor)(&mut self.context)
    }

    /// Scan but with a custom handler.
    pub fn scan_special<'a, O1, L1, H1>(
        &'a mut self,
        combinator: impl Combinator<I, Output = O1>,
        local_context: L1,
        handler: H1,
    ) where
        H1: FnOnce(&'a mut C, L1, I, O1),
    {
        scan_explicit(
            &mut self.matched,
            &mut self.context,
            &mut self.input_extractor,
            handler,
            local_context,
            combinator,
        );
    }
}

#[derive(Debug, Error)]
pub enum LexerError {
    #[error("Unsuported symbol encountered")]
    UnsupportedSymbol(Span),
}

pub struct Lexer {
    source: String,
    /// Tokens order is reversed so the `next` and `peek` methods work
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn new(source: &str) -> Result<Self, LexerError> {
        let tokens = Self::parse_tokens(source);
        // tokens.reverse();
        Ok(Self {
            source: source.to_string(),
            tokens: tokens?,
        })
    }

    // FIXME: Enters infinite loop if an unsoported token is met
    pub fn parse_tokens(source: &str) -> Result<Vec<Token>, LexerError> {
        enum Action {
            Skip,
            Push,
        }
        use Action::*;

        let mut tokens = Vec::new();

        let initial_len = source.len();
        let make_span_lens = |rest: usize, data: usize| {
            let end = initial_len - rest;
            Span {
                start: end - data,
                end,
            }
        };

        let make_span = |rest: &str, data: &str| make_span_lens(rest.len(), data.len());

        let mut inp = source;

        let mut scanner = Scanner::new(
            (&mut inp, &mut tokens),
            |ctx| ctx.0,
            |ctx, (kind, action), i, o| match action {
                Skip => {}
                Push => {
                    let span = make_span(i, o);
                    ctx.1.push(Token::new(kind, span));
                }
            },
        );

        fn skip_handler<C, L, I, O>(_ctx: C, _local: L, _i: I, _o: O) {}

        while !scanner.input().is_empty() {
            println!("Scanning {}", &scanner.input()[0..1]);
            // Tabs and whitespaces
            scanner.scan(take_while1(|c| c == ' '), (TokenKind::None, Skip));
            scanner.scan(take_while1(|c| c == '\t'), (TokenKind::None, Skip));

            // Newlines
            scanner.scan_special(many1(tag("\r\n")), (), skip_handler);
            scanner.scan_special(many1(tag("\n")), (), skip_handler);
            scanner.scan_special(many1(tag("\r")), (), skip_handler);

            // Comment
            scanner.scan_special(
                (tag("//"), take_till(|c| c == '\n' || c == '\r')),
                (),
                |_ctx, (), _i, _o| {},
            );

            // Delimiters
            scanner.scan(tag("("), (TokenKind::LeftParen, Push));
            scanner.scan(tag(")"), (TokenKind::RightParen, Push));
            scanner.scan(tag("["), (TokenKind::LeftBracket, Push));
            scanner.scan(tag("]"), (TokenKind::RightBracket, Push));
            scanner.scan(tag("{"), (TokenKind::LeftBrace, Push));
            scanner.scan(tag("}"), (TokenKind::RightBrace, Push));

            // Arrows
            scanner.scan(tag("->"), (TokenKind::ArrowRight, Push));
            scanner.scan(tag("|->"), (TokenKind::LambdaStart, Push));
            scanner.scan(tag("=>"), (TokenKind::ArrawRightBold, Push));

            // Operations
            scanner.scan(tag("+"), (TokenKind::Plus, Push));
            scanner.scan(tag("-"), (TokenKind::Minus, Push));
            scanner.scan(tag("*"), (TokenKind::Star, Push));
            scanner.scan(tag("/"), (TokenKind::Slash, Push));
            scanner.scan(tag("="), (TokenKind::Equal, Push));

            scanner.scan(tag("!"), (TokenKind::ExplanationMark, Push));
            scanner.scan(tag("?"), (TokenKind::QuestionMark, Push));
            scanner.scan(tag(":"), (TokenKind::Colon, Push));
            scanner.scan(tag(";"), (TokenKind::Semi, Push));
            scanner.scan(tag(","), (TokenKind::Comma, Push));

            // Keywords
            scanner.scan(tag("def"), (TokenKind::Def, Push));
            scanner.scan(tag("as"), (TokenKind::As, Push));

            // Literal
            scanner.scan_special(
                (tag("\""), take_till(|c| c == '\"'), tag("\"")),
                (),
                |ctx, (), i, (_, literal, _)| {
                    let span = make_span_lens(i.len(), literal.len() + 2);
                    ctx.1.push(Token::new(TokenKind::Literal, span));
                },
            );

            // Number
            scanner.scan(
                take_while1(|c| c.is_ascii_digit()),
                (TokenKind::Number, Push),
            );

            // Identifier
            scanner.scan_special(
                (
                    // Idents can't start from numbers so we use two `take_while`s
                    take_while1(|c| c.is_ascii_alphabetic()),
                    take_while(|c| c.is_ascii_alphanumeric()),
                ),
                (),
                |ctx, (), i, (first, second)| {
                    let len = first.len() + second.len();
                    ctx.1
                        .push(Token::new(TokenKind::Ident, make_span_lens(i.len(), len)));
                },
            );

            if !scanner.take_matched() {
                println!("Unsupported symbol {}", &scanner.input()[0..1]);
                return Err(LexerError::UnsupportedSymbol(Span { start: 0, end: 1 }));
            }
        }

        Ok(tokens)
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
            source: self.source,
            buffer: self.buffer,
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
