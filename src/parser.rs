use crate::lexer::{self};

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

pub struct Conditional {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
    pub third: Option<Box<Expr>>,
}


pub struct LogicalOR {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

pub struct LogicalAND {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

pub struct BitOR {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

pub struct BitXOR {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

pub struct BitAND {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

pub struct Equality {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

pub enum OpType {
    GreaterThan,
    LessThan,
    LessThanEq,
    GreaterThanEq,
}

pub struct Relational {
    pub op: OpType,
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

pub struct BitShift {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

pub struct Additive {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}
pub struct Multiplicative {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}
pub enum UnaryOp {
    Ampersand,
    Mult,
    Add,
    Sub,
    BitNOT,
    LogicalNOT,
}
pub struct Unary {
    pub op: UnaryOp,
    pub first: Option<Box<Expr>>,
}
pub struct Cast {}
pub struct PostFix {}

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
    Primary(PrimaryInner),
}

impl Expr {
    fn priority(&self) -> u8 {
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
