pub enum Expr {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Block(Vec<Box<Expr>>),
    Literal(LiteralExp),
    Integer(IntegerExpr),
}

pub struct BinaryExpr {
    lhs: Box<Expr>,
    rhs: Box<Expr>,
    op: BinaryOp,
}

enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

pub struct UnaryExpr {
    item: Box<Expr>,
    op: UnaryOp,
}

enum UnaryOp {
    Negate,
}

pub struct LiteralExp {
    // TODO: Maybe use &'a str
    literal: String,
}

pub struct IntegerExpr {
    int: i32,
}
