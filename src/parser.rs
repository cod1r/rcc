use crate::lexer::{self};

#[derive(Clone)]
pub enum PrimaryInner {
    Token(lexer::Token),
    Expr(Box<Expr>),
}

impl PrimaryInner {
    pub fn new_p_token(t: lexer::Token) -> Result<Self, String> {
        if matches!(
            t,
            lexer::Token::IDENT(_)
                | lexer::Token::StringLiteral { .. }
                | lexer::Token::CONSTANT_DEC_INT { .. }
                | lexer::Token::CONSTANT_HEXA_INT { .. }
                | lexer::Token::CONSTANT_DEC_FLOAT { .. }
                | lexer::Token::CONSTANT_HEXA_FLOAT { .. }
                | lexer::Token::CONSTANT_CHAR(_)
                | lexer::Token::CONSTANT_OCTAL_INT { .. }
                | lexer::Token::CONSTANT_ENUM(_)
        ) {
            return Ok(Self::Token(t));
        }
        Err(format!("not allowed token passed in"))
    }
    pub fn new_p_expr(e: Expr) -> Self {
        Self::Expr(Box::new(e))
    }
}

pub enum Type {
    Void,
}

#[derive(Clone)]
pub struct Conditional {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
    pub third: Option<Box<Expr>>,
}


#[derive(Clone)]
pub struct LogicalOR {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

#[derive(Clone)]
pub struct LogicalAND {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

#[derive(Clone)]
pub struct BitOR {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

#[derive(Clone)]
pub struct BitXOR {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

#[derive(Clone)]
pub struct BitAND {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

#[derive(Clone)]
pub enum EqualityOp {
    Equal,
    NotEqual,
}

#[derive(Clone)]
pub struct Equality {
    pub op: EqualityOp,
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}


#[derive(Clone)]
pub enum RelationalOp {
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
}

#[derive(Clone)]
pub struct Relational {
    pub op: RelationalOp,
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

#[derive(Clone)]
pub enum BitShiftOp {
    Left,
    Right,
}

#[derive(Clone)]
pub struct BitShift {
    pub op: BitShiftOp,
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

#[derive(Clone)]
pub enum AdditiveOps {
    Add,
    Sub,
}

#[derive(Clone)]
pub struct Additive {
    pub op: AdditiveOps,
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}
#[derive(Clone)]
pub enum MultiplicativeOps {
    Mult,
    Div,
    Mod,
}
#[derive(Clone)]
pub struct Multiplicative {
    pub op: MultiplicativeOps,
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}
#[derive(Clone)]
pub enum UnaryOp {
    Ampersand,
    Sub,
    BitNOT,
    LogicalNOT,
}
#[derive(Clone)]
pub struct Unary {
    pub op: UnaryOp,
    pub first: Option<Box<Expr>>,
}
#[derive(Clone)]
pub struct Cast {}
#[derive(Clone)]
pub struct PostFix {}

#[derive(Clone)]
pub enum Expr {
    Conditional(Conditional),
    LogicalOR(LogicalOR),
    LogicalAND(LogicalAND),
    BitOR(BitOR),
    BitXOR(BitXOR),
    BitAND(BitAND),
    Equality(Equality),
    Relational(Relational),
    BitShift(BitShift),
    Additive(Additive),
    Multiplicative(Multiplicative),
    Unary(Unary),
    Cast(Cast),
    PostFix(PostFix),
    Primary(Option<PrimaryInner>),
}

impl Expr {
    pub fn priority(&self) -> u8 {
        match self {
            Expr::Primary(_) => u8::MAX,
            Expr::PostFix(_) => u8::MAX - 1,
            Expr::Unary(_) => u8::MAX - 2,
            Expr::Cast(_) => u8::MAX - 3,
            Expr::Multiplicative(_) => u8::MAX - 4,
            Expr::Additive(_) => u8::MAX - 5,
            Expr::BitShift(_) => u8::MAX - 6,
            Expr::Relational(_) => u8::MAX - 7,
            Expr::Equality(_) => u8::MAX - 8,
            Expr::BitAND(_) => u8::MAX - 9,
            Expr::BitXOR(_) => u8::MAX - 10,
            Expr::BitOR(_) => u8::MAX - 11,
            Expr::LogicalAND(_) => u8::MAX - 12,
            Expr::LogicalOR(_) => u8::MAX - 13,
            Expr::Conditional(_) => u8::MAX - 14,
        }
    }
}

pub fn parser() {}
