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
