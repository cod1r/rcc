use crate::lexer::{self};

enum PrimaryInner {
    Token(lexer::Token),
    Expr(Box<Expr>),
}

impl PrimaryInner {
    fn new_p_token(t: lexer::Token) -> Result<Self, String> {
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
    fn new_p_expr(e: Expr) -> Self {
        Self::Expr(Box::new(e))
    }
}

enum Type {
    Void,
}

struct Conditional {
    first: Option<Box<Expr>>,
    second: Option<Box<Expr>>,
    third: Option<Box<Expr>>,
}


struct LogicalOR {
    first: Option<Box<Expr>>,
    second: Option<Box<Expr>>,
}

struct LogicalAND {
    first: Option<Box<Expr>>,
    second: Option<Box<Expr>>,
}

struct BitOR {
    first: Option<Box<Expr>>,
    second: Option<Box<Expr>>,
}

struct BitXOR {
    first: Option<Box<Expr>>,
    second: Option<Box<Expr>>,
}

struct BitAND {
    first: Option<Box<Expr>>,
    second: Option<Box<Expr>>,
}

struct Equality {
    first: Option<Box<Expr>>,
    second: Option<Box<Expr>>,
}

enum OpType {
    GreaterThan,
    LessThan,
    LessThanEq,
    GreaterThanEq,
}

struct Relational {
    op: OpType,
    first: Option<Box<Expr>>,
    second: Option<Box<Expr>>,
}

struct BitShift {
    first: Option<Box<Expr>>,
    second: Option<Box<Expr>>,
}

struct Additive {
    first: Option<Box<Expr>>,
    second: Option<Box<Expr>>,
}
struct Multiplicative {
    first: Option<Box<Expr>>,
    second: Option<Box<Expr>>,
}
enum UnaryOp {
    Ampersand,
    Mult,
    Add,
    Sub,
    BitNOT,
    LogicalNOT,
}
struct Unary {
    op: UnaryOp,
    first: Option<Box<Expr>>,
}
struct Cast {}
struct PostFix {}

enum Expr {
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

fn parser() {}
