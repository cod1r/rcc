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
                | lexer::Token::CONSTANT_CHAR { .. }
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
    SignedChar,
    UnsignedChar,
    ShortInt,
    UnsignedShortInt,
    Int,
    UnsignedInt,
    LongInt,
    LongLongInt,
    UnsignedLongLongInt,
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
    Add,
    Deref,
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
pub struct Assignment {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

#[derive(Clone)]
pub enum Expr {
    Assignment(Assignment),
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
            Expr::Assignment(_) => todo!(),
        }
    }
}
macro_rules! case_where_it_could_be_unary_or_additive {
    ($parserTypeVar:ident, $rightExprVar:ident, $token:expr) => {
        if $parserTypeVar.second.is_none() {
            $rightExprVar = Some(Expr::Unary(Unary {
                op: match $token {
                    lexer::Token::PUNCT_PLUS => UnaryOp::Add,
                    lexer::Token::PUNCT_MINUS => UnaryOp::Sub,
                    lexer::Token::PUNCT_NOT_BOOL => UnaryOp::LogicalNOT,
                    lexer::Token::PUNCT_TILDE => UnaryOp::BitNOT,
                    _ => unreachable!(),
                },
                first: None,
            }));
        } else {
            $rightExprVar = Some(Expr::Additive(Additive {
                op: match $token {
                    lexer::Token::PUNCT_PLUS => AdditiveOps::Add,
                    lexer::Token::PUNCT_MINUS => AdditiveOps::Sub,
                    _ => unreachable!(),
                },
                first: None,
                second: None,
            }));
        }
    };
}
/*
   Expr::Primary
   Expr::PostFix
   Expr::Unary
   Expr::Cast
   Expr::Multiplicative
   Expr::Additive
   Expr::BitShift
   Expr::Relational
   Expr::Equality
   Expr::BitAND
   Expr::BitXOR
   Expr::BitOR
   Expr::LogicalAND
   Expr::LogicalOR
   Expr::Conditional
*/
fn right_has_higher_priority(left: &mut Expr, right: &mut Expr) {
    assert!(right.priority() > left.priority());
    match left {
        Expr::Unary(u) => match right {
            Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(u.first.is_none());
            }
            _ => unreachable!(),
        },
        Expr::Multiplicative(m) => match right {
            Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(m.second.is_none());
            }
            Expr::Unary(_) => {
                assert!(m.second.is_none());
            }
            _ => unreachable!(),
        },
        Expr::Additive(a) => match right {
            Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(a.second.is_none());
            }
            Expr::Unary(_) => {
                assert!(a.second.is_none());
            }
            Expr::Multiplicative(m) => {
                assert!(m.first.is_none());
                m.first = a.second.clone();
            }
            _ => unreachable!(),
        },
        Expr::BitShift(bs) => match right {
            Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(bs.second.is_none());
            }
            Expr::Unary(_) => {
                assert!(bs.second.is_none());
            }
            Expr::Multiplicative(m) => {
                assert!(m.first.is_none());
                m.first = bs.second.clone();
            }
            Expr::Additive(a) => {
                assert!(a.first.is_none());
                a.first = bs.second.clone();
            }
            _ => unreachable!(),
        },
        Expr::Relational(r) => match right {
            Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(r.second.is_none());
            }
            Expr::Unary(_) => {
                assert!(r.second.is_none());
            }
            Expr::Multiplicative(m) => {
                assert!(m.first.is_none());
                m.first = r.second.clone();
            }
            Expr::Additive(a) => {
                assert!(a.first.is_none());
                a.first = r.second.clone();
            }
            Expr::BitShift(bs) => {
                assert!(bs.first.is_none());
                bs.first = r.second.clone();
            }
            _ => unreachable!(),
        },
        Expr::Equality(e) => match right {
            Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(e.second.is_none());
            }
            Expr::Unary(_) => {
                assert!(e.second.is_none());
            }
            Expr::Multiplicative(m) => {
                assert!(m.first.is_none());
                m.first = e.second.clone();
            }
            Expr::Additive(a) => {
                assert!(a.first.is_none());
                a.first = e.second.clone();
            }
            Expr::BitShift(bs) => {
                assert!(bs.first.is_none());
                bs.first = e.second.clone();
            }
            Expr::Relational(r) => {
                assert!(r.first.is_none());
                r.first = e.second.clone();
            }
            _ => unreachable!(),
        },
        Expr::BitAND(ba) => match right {
            Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(ba.second.is_none());
            }
            Expr::Unary(_) => {
                assert!(ba.second.is_none());
            }
            Expr::Multiplicative(m) => {
                assert!(m.first.is_none());
                m.first = ba.second.clone();
            }
            Expr::Additive(a) => {
                assert!(a.first.is_none());
                a.first = ba.second.clone();
            }
            Expr::BitShift(bs) => {
                assert!(bs.first.is_none());
                bs.first = ba.second.clone();
            }
            Expr::Relational(r) => {
                assert!(r.first.is_none());
                r.first = ba.second.clone();
            }
            Expr::Equality(e) => {
                assert!(e.first.is_none());
                e.first = ba.second.clone();
            }
            _ => unreachable!(),
        },
        Expr::BitXOR(bx) => match right {
            Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(bx.second.is_none());
            }
            Expr::Unary(_) => {
                assert!(bx.second.is_none());
            }
            Expr::Multiplicative(m) => {
                assert!(m.first.is_none());
                m.first = bx.second.clone();
            }
            Expr::Additive(a) => {
                assert!(a.first.is_none());
                a.first = bx.second.clone();
            }
            Expr::BitShift(bs) => {
                assert!(bs.first.is_none());
                bs.first = bx.second.clone();
            }
            Expr::Relational(r) => {
                assert!(r.first.is_none());
                r.first = bx.second.clone();
            }
            Expr::Equality(e) => {
                assert!(e.first.is_none());
                e.first = bx.second.clone();
            }
            Expr::BitAND(ba) => {
                assert!(ba.first.is_none());
                ba.first = bx.second.clone();
            }
            _ => unreachable!(),
        },
        Expr::BitOR(bo) => match right {
            Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(bo.second.is_none());
            }
            Expr::Unary(_) => {
                assert!(bo.second.is_none());
            }
            Expr::Multiplicative(m) => {
                assert!(m.first.is_none());
                m.first = bo.second.clone();
            }
            Expr::Additive(a) => {
                assert!(a.first.is_none());
                a.first = bo.second.clone();
            }
            Expr::BitShift(bs) => {
                assert!(bs.first.is_none());
                bs.first = bo.second.clone();
            }
            Expr::Relational(r) => {
                assert!(r.first.is_none());
                r.first = bo.second.clone();
            }
            Expr::Equality(e) => {
                assert!(e.first.is_none());
                e.first = bo.second.clone();
            }
            Expr::BitAND(ba) => {
                assert!(ba.first.is_none());
                ba.first = bo.second.clone();
            }
            Expr::BitXOR(bx) => {
                assert!(bx.first.is_none());
                bx.first = bo.second.clone();
            }
            _ => unreachable!(),
        },
        Expr::LogicalAND(la) => match right {
            Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(la.second.is_none());
            }
            Expr::Unary(_) => {
                assert!(la.second.is_none());
            }
            Expr::Multiplicative(m) => {
                assert!(m.first.is_none());
                m.first = la.second.clone();
            }
            Expr::Additive(a) => {
                assert!(a.first.is_none());
                a.first = la.second.clone();
            }
            Expr::BitShift(bs) => {
                assert!(bs.first.is_none());
                bs.first = la.second.clone();
            }
            Expr::Relational(r) => {
                assert!(r.first.is_none());
                r.first = la.second.clone();
            }
            Expr::Equality(e) => {
                assert!(e.first.is_none());
                e.first = la.second.clone();
            }
            Expr::BitAND(ba) => {
                assert!(ba.first.is_none());
                ba.first = la.second.clone();
            }
            Expr::BitXOR(bx) => {
                assert!(bx.first.is_none());
                bx.first = la.second.clone();
            }
            Expr::BitOR(bo) => {
                assert!(bo.first.is_none());
                bo.first = la.second.clone();
            }
            _ => unreachable!(),
        },
        Expr::LogicalOR(lo) => match right {
            Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(lo.second.is_none());
            }
            Expr::Unary(_) => {
                assert!(lo.second.is_none());
            }
            Expr::Multiplicative(m) => {
                assert!(m.first.is_none());
                m.first = lo.second.clone();
            }
            Expr::Additive(a) => {
                assert!(a.first.is_none());
                a.first = lo.second.clone();
            }
            Expr::BitShift(bs) => {
                assert!(bs.first.is_none());
                bs.first = lo.second.clone();
            }
            Expr::Relational(r) => {
                assert!(r.first.is_none());
                r.first = lo.second.clone();
            }
            Expr::Equality(e) => {
                assert!(e.first.is_none());
                e.first = lo.second.clone();
            }
            Expr::BitAND(ba) => {
                assert!(ba.first.is_none());
                ba.first = lo.second.clone();
            }
            Expr::BitXOR(bx) => {
                assert!(bx.first.is_none());
                bx.first = lo.second.clone();
            }
            Expr::BitOR(bo) => {
                assert!(bo.first.is_none());
                bo.first = lo.second.clone();
            }
            Expr::LogicalAND(la) => {
                assert!(la.first.is_none());
                la.first = lo.second.clone();
            }
            _ => unreachable!(),
        },
        Expr::Conditional(c) => {
            assert!(c.first.is_some());
            assert!(c.second.is_some());
            match right {
                Expr::Primary(p) => {
                    assert!(p.is_some());
                    assert!(c.third.is_none());
                }
                Expr::Unary(_u) => {
                    assert!(c.third.is_none());
                }
                Expr::Multiplicative(m) => {
                    assert!(m.first.is_none());
                    m.first = c.third.clone();
                }
                Expr::Additive(a) => {
                    assert!(a.first.is_none());
                    a.first = c.third.clone();
                }
                Expr::BitShift(bs) => {
                    assert!(bs.first.is_none());
                    bs.first = c.third.clone();
                }
                Expr::Relational(r) => {
                    assert!(r.first.is_none());
                    r.first = c.third.clone();
                }
                Expr::Equality(e) => {
                    assert!(e.first.is_none());
                    e.first = c.third.clone();
                }
                Expr::BitAND(ba) => {
                    assert!(ba.first.is_none());
                    ba.first = c.third.clone();
                }
                Expr::BitXOR(bx) => {
                    assert!(bx.first.is_none());
                    bx.first = c.third.clone();
                }
                Expr::BitOR(bo) => {
                    assert!(bo.first.is_none());
                    bo.first = c.third.clone();
                }
                Expr::LogicalAND(la) => {
                    assert!(la.first.is_none());
                    la.first = c.third.clone();
                }
                Expr::LogicalOR(lo) => {
                    assert!(lo.first.is_none());
                    lo.first = c.third.clone();
                }
                _ => unreachable!(),
            }
        }
        Expr::Assignment(a) => match right {
            Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(a.second.is_none());
            }
            Expr::Unary(_u) => {
                assert!(a.second.is_none());
            }
            Expr::Multiplicative(m) => {
                assert!(m.first.is_none());
                m.first = a.second.clone();
            }
            Expr::Additive(a) => {
                assert!(a.first.is_none());
                a.first = a.second.clone();
            }
            Expr::BitShift(bs) => {
                assert!(bs.first.is_none());
                bs.first = a.second.clone();
            }
            Expr::Relational(r) => {
                assert!(r.first.is_none());
                r.first = a.second.clone();
            }
            Expr::Equality(e) => {
                assert!(e.first.is_none());
                e.first = a.second.clone();
            }
            Expr::BitAND(ba) => {
                assert!(ba.first.is_none());
                ba.first = a.second.clone();
            }
            Expr::BitXOR(bx) => {
                assert!(bx.first.is_none());
                bx.first = a.second.clone();
            }
            Expr::BitOR(bo) => {
                assert!(bo.first.is_none());
                bo.first = a.second.clone();
            }
            Expr::LogicalAND(la) => {
                assert!(la.first.is_none());
                la.first = a.second.clone();
            }
            Expr::LogicalOR(lo) => {
                assert!(lo.first.is_none());
                lo.first = a.second.clone();
            }
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}

fn left_has_higher_eq_priority(left: Expr, right: &mut Expr) {
    assert!(left.priority() >= right.priority());
    let boxed = Some(Box::new(left));
    match right {
        Expr::Multiplicative(m) => {
            m.first = boxed;
        }
        Expr::Additive(a) => {
            a.first = boxed;
        }
        Expr::BitShift(bs) => {
            bs.first = boxed;
        }
        Expr::Relational(r) => {
            r.first = boxed;
        }
        Expr::Equality(e) => {
            e.first = boxed;
        }
        Expr::BitAND(ba) => {
            ba.first = boxed;
        }
        Expr::BitXOR(bx) => {
            bx.first = boxed;
        }
        Expr::BitOR(bo) => {
            bo.first = boxed;
        }
        Expr::LogicalAND(la) => {
            la.first = boxed;
        }
        Expr::LogicalOR(lo) => {
            lo.first = boxed;
        }
        Expr::Conditional(_c) => {
            unreachable!()
        }
        _ => unreachable!(),
    }
}

fn parse_expressions(tokens: &[lexer::Token], start_index: usize) -> Result<(usize, Expr), String> {
    let mut index = start_index;
    match tokens[index] {
        lexer::Token::IDENT(_)
        | lexer::Token::CONSTANT_DEC_INT { .. }
        | lexer::Token::CONSTANT_CHAR(_)
        | lexer::Token::CONSTANT_HEXA_INT { .. }
        | lexer::Token::CONSTANT_OCTAL_INT { .. }
        | lexer::Token::CONSTANT_DEC_FLOAT { .. }
        | lexer::Token::CONSTANT_HEXA_FLOAT { .. }
        | lexer::Token::StringLiteral(_) => Ok((
            index + 1,
            Expr::Primary(Some(PrimaryInner::new_p_token(tokens[index])?)),
        )),
        lexer::Token::PUNCT_OPEN_PAR => {
            index += 1;
            let (new_index, expr) = parse_expressions(tokens, index)?;
            index = new_index;
            while matches!(
                tokens.get(index),
                Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
            ) {
                index += 1;
            }
            if !matches!(tokens.get(index), Some(lexer::Token::PUNCT_CLOSE_PAR)) {
                return Err(format!(
                    "Expected closing parenthesis, got: {:?}",
                    tokens.get(index)
                ));
            }
            Ok((index, Expr::Primary(Some(PrimaryInner::new_p_expr(expr)))))
        }

        lexer::Token::WHITESPACE | lexer::Token::NEWLINE => {todo!()}
        _ => todo!(),
    }
}

//Notes:
//The expression that controls conditional inclusion shall be an integer constant expression
//Because the controlling constant expression is evaluated during translation phase 4, all identifiers either are or are not macro names — there simply are no keywords, enumeration constants, etc
//All macro identifiers are evaluated as defined or not defined.
// TODO: rewrite this. It works but is WAYY too convoluted.
pub fn eval_constant_expression_integer(
    tokens: &[lexer::Token],
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<i128, String> {
    if tokens
        .iter()
        .filter(|t| !matches!(t, lexer::Token::WHITESPACE))
        .count()
        == 0
    {
        return Err(format!("empty expression given"));
    }
    if let Some(not_allowed_t) = tokens.iter().find(|t| {
        matches!(
            t,
            lexer::Token::PUNCT_ASSIGNMENT
                | lexer::Token::PUNCT_INCREMENT
                | lexer::Token::PUNCT_DECREMENT
                | lexer::Token::PUNCT_OPEN_CURLY
                | lexer::Token::PUNCT_CLOSE_CURLY
                | lexer::Token::PUNCT_OPEN_SQR
                | lexer::Token::PUNCT_CLOSE_SQR
                | lexer::Token::CONSTANT_DEC_FLOAT { .. }
                | lexer::Token::CONSTANT_HEXA_FLOAT { .. }
                | lexer::Token::PUNCT_COMMA
                | lexer::Token::StringLiteral { .. }
                | lexer::Token::PUNCT_ARROW
                | lexer::Token::PUNCT_ADD_ASSIGN
                | lexer::Token::PUNCT_DIV_ASSIGN
                | lexer::Token::PUNCT_SUB_ASSIGN
                | lexer::Token::PUNCT_MULT_ASSIGN
                | lexer::Token::PUNCT_MODULO_ASSIGN
                | lexer::Token::PUNCT_AND_BIT_ASSIGN
                | lexer::Token::PUNCT_OR_BIT_ASSIGN
                | lexer::Token::PUNCT_XOR_BIT_ASSIGN
                | lexer::Token::PUNCT_L_SHIFT_BIT_ASSIGN
                | lexer::Token::PUNCT_R_SHIFT_BIT_ASSIGN
        )
    }) {
        return Err(format!(
            "{:?}'s are not allowed in constant expressions",
            not_allowed_t
        ));
    }
    let mut parenth_balance = Vec::<lexer::Token>::with_capacity(tokens.len());
    for par_bal_index in 0..tokens.len() {
        match tokens[par_bal_index] {
            lexer::Token::PUNCT_OPEN_PAR => {
                parenth_balance.push(tokens[par_bal_index]);
            }
            lexer::Token::PUNCT_CLOSE_PAR => {
                if let Some(lexer::Token::PUNCT_OPEN_PAR) = parenth_balance.last() {
                    parenth_balance.pop();
                } else {
                    return Err(String::from("parentheses in expression not balanced"));
                }
            }
            _ => {}
        }
    }
    // TODO: describe our algorithm in comments below
    // or we will forget how any of this shit works
    if !parenth_balance.is_empty() {
        return Err(String::from("parentheses in expression not balanced"));
    }
    let mut stack = Vec::<Expr>::new();
    // if our current expression is 'complete' as in it has all it
    // needs to be defined as whatever type of expression it is, we
    // need to look at our expression stack to see if any expression
    // uses this curr_expr as a sub-expression.
    // There should be no cases where we have an expression on the
    // stack that is complete and curr_expr also being complete.
    //
    // In the case where curr_expr would use an expression on the stack
    // as a sub expression, we would pop off the expression on the
    // stack and put it as a sub expression in the curr_expr.
    //
    // curr_expr would be the end result of constructing the expression
    // tree
    //
    //
    // postfix and cast expressions aren't needed.
    // (postfix_expr -) isn't an expression that we would construct as we would just construct a
    // additive expression anyways
    let mut curr_expr: Option<Expr> = None;
    let mut left_expression: Option<Expr> = None;
    let mut right_expression: Option<Expr> = None;
    let mut index = 0;
    while index < tokens.len() {
        match &tokens[index] {
            lexer::Token::IDENT(_)
            | lexer::Token::CONSTANT_DEC_INT { .. }
            | lexer::Token::CONSTANT_CHAR(_) => {
                let mut token_within = tokens[index];
                let primary = Expr::Primary(Some(PrimaryInner::new_p_token(token_within)?));
                if curr_expr.is_none() {
                    curr_expr = Some(primary);
                } else {
                    match &mut curr_expr {
                        Some(Expr::Additive(a)) => {
                            assert!(a.first.is_some());
                            a.second = Some(Box::new(primary));
                        }
                        Some(Expr::PostFix(_)) => todo!(),
                        Some(Expr::Cast(_)) => todo!(),
                        Some(Expr::Unary(u)) => {
                            assert!(u.first.is_none());
                            u.first = Some(Box::new(primary));
                        }
                        Some(Expr::LogicalOR(lo)) => {
                            assert!(lo.first.is_some());
                            lo.second = Some(Box::new(primary));
                        }
                        Some(Expr::LogicalAND(la)) => {
                            assert!(la.first.is_some());
                            la.second = Some(Box::new(primary));
                        }
                        Some(Expr::BitOR(bo)) => {
                            assert!(bo.first.is_some());
                            bo.second = Some(Box::new(primary));
                        }
                        Some(Expr::BitXOR(bx)) => {
                            assert!(bx.first.is_some());
                            bx.second = Some(Box::new(primary));
                        }
                        Some(Expr::BitAND(ba)) => {
                            assert!(ba.first.is_some());
                            ba.second = Some(Box::new(primary));
                        }
                        Some(Expr::Equality(e)) => {
                            assert!(e.first.is_some());
                            e.second = Some(Box::new(primary));
                        }
                        Some(Expr::Relational(r)) => {
                            assert!(r.first.is_some());
                            r.second = Some(Box::new(primary));
                        }
                        Some(Expr::BitShift(bs)) => {
                            assert!(bs.first.is_some());
                            bs.second = Some(Box::new(primary));
                        }
                        Some(Expr::Multiplicative(m)) => {
                            assert!(m.first.is_some());
                            m.second = Some(Box::new(primary));
                        }
                        Some(Expr::Conditional(c)) => {
                            if c.first.is_none() {
                                c.first = Some(Box::new(primary));
                            } else if c.second.is_none() {
                                c.second = Some(Box::new(primary));
                            } else if c.third.is_none() {
                                c.third = Some(Box::new(primary));
                            }
                        }
                        Some(Expr::Assignment(a)) => todo!(),
                        _ => return Err(format!("err at index: {}", index)),
                    }
                }

                loop {
                    index += 1;
                    if !matches!(tokens.get(index), Some(lexer::Token::WHITESPACE)) {
                        break;
                    }
                }
                if !matches!(
                    tokens.get(index),
                    Some(
                        lexer::Token::PUNCT_PLUS
                            | lexer::Token::PUNCT_MINUS
                            | lexer::Token::PUNCT_MULT
                            | lexer::Token::PUNCT_DIV
                            | lexer::Token::PUNCT_MODULO
                            | lexer::Token::PUNCT_BITSHFT_LEFT
                            | lexer::Token::PUNCT_BITSHFT_RIGHT
                            | lexer::Token::PUNCT_LESS_THAN
                            | lexer::Token::PUNCT_LESS_THAN_EQ
                            | lexer::Token::PUNCT_GREATER_THAN
                            | lexer::Token::PUNCT_GREATER_THAN_EQ
                            | lexer::Token::PUNCT_EQ_BOOL
                            | lexer::Token::PUNCT_NOT_EQ_BOOL
                            | lexer::Token::PUNCT_AND_BIT
                            | lexer::Token::PUNCT_XOR_BIT
                            | lexer::Token::PUNCT_OR_BIT
                            | lexer::Token::PUNCT_AND_BOOL
                            | lexer::Token::PUNCT_OR_BOOL
                            | lexer::Token::PUNCT_CLOSE_PAR
                            | lexer::Token::PUNCT_QUESTION_MARK
                            | lexer::Token::PUNCT_COLON
                    ) | None
                ) {
                    return Err(format!("unexpected operator: {:?}", tokens));
                }
            }
            lexer::Token::PUNCT_OPEN_PAR => {
                if let Some(expr) = curr_expr {
                    stack.push(expr);
                }
                stack.push(Expr::Primary(None));
                curr_expr = None;
                loop {
                    index += 1;
                    if !matches!(tokens.get(index), Some(lexer::Token::WHITESPACE)) {
                        break;
                    }
                }
                if !matches!(
                    tokens.get(index),
                    Some(
                        lexer::Token::IDENT(_)
                            | lexer::Token::CONSTANT_DEC_INT { .. }
                            | lexer::Token::CONSTANT_CHAR(_)
                            | lexer::Token::PUNCT_OPEN_PAR
                            | lexer::Token::PUNCT_PLUS
                            | lexer::Token::PUNCT_MINUS
                            | lexer::Token::PUNCT_NOT_BOOL
                            | lexer::Token::PUNCT_TILDE
                    )
                ) {
                    return Err(format!(
                        "expected '-', '(', or an identifier/integer constant: {:?}",
                        tokens[index]
                    ));
                }
            }
            lexer::Token::PUNCT_CLOSE_PAR => {
                // thought process here is that we want to pop until we hit the opening parenthesis
                // that created the primary expression.
                //  -- Side Note: if there is a unary operator before the opening parenthesis, we
                //     need to keep going until we pop the unary operator
                // if we do not encounter the primary expression, we treat other expressions as
                // having a lower priority (it has to be because that's the only reason our 'stack'
                // exists) which means that we set curr_expr to that expression with that
                // expression having the old curr_expr as a child in the expression tree
                let mut popped_opening_parenth_already = false;
                while let Some(mut e) = stack.pop() {
                    match e {
                        Expr::Primary(ref mut p) => {
                            *p = Some(PrimaryInner::new_p_expr(curr_expr.unwrap()));
                            curr_expr = Some(e);
                            if !matches!(stack.last(), Some(Expr::Unary(_))) {
                                break;
                            } else {
                                popped_opening_parenth_already = true;
                            }
                        }
                        Expr::Unary(ref mut u) => {
                            u.first = Some(Box::new(curr_expr.unwrap()));
                            curr_expr = Some(e);
                            if popped_opening_parenth_already {
                                break;
                            }
                        }
                        _ => {
                            assert!(e.priority() <= curr_expr.clone().unwrap().priority());
                            let unwrapped = Some(Box::new(curr_expr.unwrap()));
                            match e {
                                Expr::Unary(ref mut u) => {
                                    u.first = unwrapped;
                                }
                                Expr::Multiplicative(ref mut m) => {
                                    m.second = unwrapped;
                                }
                                Expr::Additive(ref mut a) => {
                                    a.second = unwrapped;
                                }
                                Expr::BitShift(ref mut bs) => {
                                    bs.second = unwrapped;
                                }
                                Expr::Relational(ref mut r) => {
                                    r.second = unwrapped;
                                }
                                Expr::Equality(ref mut e) => {
                                    e.second = unwrapped;
                                }
                                Expr::BitAND(ref mut ba) => {
                                    ba.second = unwrapped;
                                }
                                Expr::BitXOR(ref mut bx) => {
                                    bx.second = unwrapped;
                                }
                                Expr::BitOR(ref mut bo) => {
                                    bo.second = unwrapped;
                                }
                                Expr::LogicalAND(ref mut la) => {
                                    la.second = unwrapped;
                                }
                                Expr::LogicalOR(ref mut lo) => {
                                    lo.second = unwrapped;
                                }
                                Expr::Conditional(ref mut c) => {
                                    c.third = unwrapped;
                                }
                                _ => unreachable!(),
                            }
                            curr_expr = Some(e);
                        }
                    }
                }
                loop {
                    index += 1;
                    if !matches!(tokens.get(index), Some(lexer::Token::WHITESPACE)) {
                        break;
                    }
                }
                if matches!(
                    tokens.get(index),
                    Some(
                        lexer::Token::IDENT(_)
                            | lexer::Token::CONSTANT_DEC_INT { .. }
                            | lexer::Token::CONSTANT_CHAR(_)
                            | lexer::Token::PUNCT_OPEN_PAR
                    )
                ) {
                    return Err(format!(
                        "not allowed token after operator ')': {:?}",
                        tokens[index]
                    ));
                }
            }
            lexer::Token::PUNCT_PLUS
            | lexer::Token::PUNCT_MINUS
            | lexer::Token::PUNCT_NOT_BOOL
            | lexer::Token::PUNCT_TILDE => {
                left_expression = curr_expr;
                match &left_expression {
                    Some(Expr::Primary(_p)) => {
                        // if a '~' or '!' follow a primary expression, that is not allowed.
                        match tokens[index] {
                            lexer::Token::PUNCT_TILDE | lexer::Token::PUNCT_NOT_BOOL => {
                                return Err(format!("unexpected {:?} token", tokens[index]));
                            }
                            _ => {}
                        }
                        right_expression = Some(Expr::Additive(Additive {
                            op: match tokens[index] {
                                lexer::Token::PUNCT_PLUS => AdditiveOps::Add,
                                lexer::Token::PUNCT_MINUS => AdditiveOps::Sub,
                                _ => unreachable!("{:?}", tokens[index]),
                            },
                            first: None,
                            second: None,
                        }));
                        curr_expr = None;
                    }
                    None => {
                        curr_expr = Some(Expr::Unary(Unary {
                            op: match tokens[index] {
                                lexer::Token::PUNCT_PLUS => UnaryOp::Add,
                                lexer::Token::PUNCT_MINUS => UnaryOp::Sub,
                                lexer::Token::PUNCT_NOT_BOOL => UnaryOp::LogicalNOT,
                                lexer::Token::PUNCT_TILDE => UnaryOp::BitNOT,
                                _ => unreachable!(),
                            },
                            first: None,
                        }));
                    }
                    Some(Expr::Unary(Unary { op: _, first })) => {
                        if first.is_none() {
                            stack.push(left_expression.clone().unwrap());
                            curr_expr = Some(Expr::Unary(Unary {
                                op: match tokens[index] {
                                    lexer::Token::PUNCT_PLUS => UnaryOp::Add,
                                    lexer::Token::PUNCT_MINUS => UnaryOp::Sub,
                                    lexer::Token::PUNCT_NOT_BOOL => UnaryOp::LogicalNOT,
                                    lexer::Token::PUNCT_TILDE => UnaryOp::BitNOT,
                                    _ => unreachable!(),
                                },
                                first: None,
                            }));
                        } else {
                            // if a '~' or '!' follow a unary expression, that is not allowed.
                            match tokens[index] {
                                lexer::Token::PUNCT_TILDE | lexer::Token::PUNCT_NOT_BOOL => {
                                    return Err(format!("unexpected {:?} token", tokens[index]));
                                }
                                _ => {}
                            }
                            right_expression = Some(Expr::Additive(Additive {
                                op: match tokens[index] {
                                    lexer::Token::PUNCT_PLUS => AdditiveOps::Add,
                                    lexer::Token::PUNCT_MINUS => AdditiveOps::Sub,
                                    _ => unreachable!("{:?}", tokens[index]),
                                },
                                first: None,
                                second: None,
                            }));
                            curr_expr = None;
                        }
                    }
                    Some(Expr::Multiplicative(m)) => {
                        case_where_it_could_be_unary_or_additive!(
                            m,
                            right_expression,
                            tokens[index]
                        );
                        curr_expr = None;
                    }
                    Some(Expr::Additive(a)) => {
                        case_where_it_could_be_unary_or_additive!(
                            a,
                            right_expression,
                            tokens[index]
                        );
                        curr_expr = None;
                    }
                    Some(Expr::BitShift(bs)) => {
                        case_where_it_could_be_unary_or_additive!(
                            bs,
                            right_expression,
                            tokens[index]
                        );
                        curr_expr = None;
                    }
                    Some(Expr::Relational(r)) => {
                        case_where_it_could_be_unary_or_additive!(
                            r,
                            right_expression,
                            tokens[index]
                        );
                        curr_expr = None;
                    }
                    Some(Expr::Equality(e)) => {
                        case_where_it_could_be_unary_or_additive!(
                            e,
                            right_expression,
                            tokens[index]
                        );
                        curr_expr = None;
                    }
                    Some(Expr::BitAND(ba)) => {
                        case_where_it_could_be_unary_or_additive!(
                            ba,
                            right_expression,
                            tokens[index]
                        );
                        curr_expr = None;
                    }
                    Some(Expr::BitXOR(bx)) => {
                        case_where_it_could_be_unary_or_additive!(
                            bx,
                            right_expression,
                            tokens[index]
                        );
                        curr_expr = None;
                    }
                    Some(Expr::BitOR(bo)) => {
                        case_where_it_could_be_unary_or_additive!(
                            bo,
                            right_expression,
                            tokens[index]
                        );
                        curr_expr = None;
                    }
                    Some(Expr::LogicalAND(la)) => {
                        case_where_it_could_be_unary_or_additive!(
                            la,
                            right_expression,
                            tokens[index]
                        );
                        curr_expr = None;
                    }
                    Some(Expr::LogicalOR(lo)) => {
                        case_where_it_could_be_unary_or_additive!(
                            lo,
                            right_expression,
                            tokens[index]
                        );
                        curr_expr = None;
                    }
                    _ => unreachable!(),
                }
                loop {
                    index += 1;
                    if !matches!(tokens.get(index), Some(lexer::Token::WHITESPACE)) {
                        break;
                    }
                }
                if !matches!(
                    tokens.get(index),
                    Some(
                        lexer::Token::IDENT(_)
                            | lexer::Token::PUNCT_OPEN_PAR
                            | lexer::Token::PUNCT_PLUS
                            | lexer::Token::PUNCT_MINUS
                            | lexer::Token::PUNCT_NOT_BOOL
                            | lexer::Token::PUNCT_TILDE
                            | lexer::Token::CONSTANT_DEC_INT { .. }
                            | lexer::Token::CONSTANT_CHAR(_)
                    )
                ) {
                    //TODO: this can panic if unary is the last token
                    return Err(format!(
                        "not allowed token after operators '+', '-', '!', '~': {:?}",
                        tokens[index]
                    ));
                }
            }
            lexer::Token::PUNCT_MULT | lexer::Token::PUNCT_DIV | lexer::Token::PUNCT_MODULO => {
                if curr_expr.is_none() {
                    return Err(format!("unexpected token: {:?}", tokens[index]));
                }
                left_expression = curr_expr;
                curr_expr = None;
                right_expression = Some(Expr::Multiplicative(Multiplicative {
                    op: match tokens[index] {
                        lexer::Token::PUNCT_MULT => MultiplicativeOps::Mult,
                        lexer::Token::PUNCT_DIV => MultiplicativeOps::Div,
                        lexer::Token::PUNCT_MODULO => MultiplicativeOps::Mod,
                        _ => unreachable!(),
                    },
                    first: None,
                    second: None,
                }));
                loop {
                    index += 1;
                    if !matches!(tokens.get(index), Some(lexer::Token::WHITESPACE)) {
                        break;
                    }
                }
                if !matches!(
                    tokens.get(index),
                    Some(
                        lexer::Token::IDENT(_)
                            | lexer::Token::CONSTANT_DEC_INT { .. }
                            | lexer::Token::PUNCT_OPEN_PAR
                            | lexer::Token::PUNCT_PLUS
                            | lexer::Token::PUNCT_MINUS
                            | lexer::Token::PUNCT_NOT_BOOL
                            | lexer::Token::PUNCT_TILDE
                    )
                ) {
                    return Err(format!(
                        "not allowed token after operators '*', '/', and '%': {:?}",
                        tokens[index]
                    ));
                }
            }
            lexer::Token::PUNCT_BITSHFT_RIGHT | lexer::Token::PUNCT_BITSHFT_LEFT => {
                if curr_expr.is_none() {
                    return Err(format!("unexpected token: {:?}", tokens[index]));
                }
                left_expression = curr_expr;
                curr_expr = None;
                right_expression = Some(Expr::BitShift(BitShift {
                    op: match tokens[index] {
                        lexer::Token::PUNCT_BITSHFT_LEFT => BitShiftOp::Left,
                        lexer::Token::PUNCT_BITSHFT_RIGHT => BitShiftOp::Right,
                        _ => unreachable!(),
                    },
                    first: None,
                    second: None,
                }));
                loop {
                    index += 1;
                    if !matches!(tokens.get(index), Some(lexer::Token::WHITESPACE)) {
                        break;
                    }
                }
                if !matches!(
                    tokens.get(index),
                    Some(
                        lexer::Token::IDENT(_)
                            | lexer::Token::CONSTANT_DEC_INT { .. }
                            | lexer::Token::CONSTANT_CHAR(_)
                            | lexer::Token::PUNCT_OPEN_PAR
                            | lexer::Token::PUNCT_PLUS
                            | lexer::Token::PUNCT_MINUS
                            | lexer::Token::PUNCT_NOT_BOOL
                            | lexer::Token::PUNCT_TILDE
                    )
                ) {
                    return Err(format!(
                        "not allowed token after operators '<<' and '>>': {:?}",
                        tokens[index]
                    ));
                }
            }
            lexer::Token::PUNCT_LESS_THAN
            | lexer::Token::PUNCT_LESS_THAN_EQ
            | lexer::Token::PUNCT_GREATER_THAN
            | lexer::Token::PUNCT_GREATER_THAN_EQ => {
                if curr_expr.is_none() {
                    return Err(format!("unexpected token: {:?}", tokens[index]));
                }
                left_expression = curr_expr;
                curr_expr = None;
                right_expression = Some(Expr::Relational(Relational {
                    op: match tokens[index] {
                        lexer::Token::PUNCT_LESS_THAN => RelationalOp::LessThan,
                        lexer::Token::PUNCT_LESS_THAN_EQ => RelationalOp::LessThanEq,
                        lexer::Token::PUNCT_GREATER_THAN => RelationalOp::GreaterThan,
                        lexer::Token::PUNCT_GREATER_THAN_EQ => RelationalOp::GreaterThanEq,
                        _ => unreachable!(),
                    },
                    first: None,
                    second: None,
                }));
                loop {
                    index += 1;
                    if !matches!(tokens.get(index), Some(lexer::Token::WHITESPACE)) {
                        break;
                    }
                }
                if !matches!(
                    tokens.get(index),
                    Some(
                        lexer::Token::IDENT(_)
                            | lexer::Token::CONSTANT_DEC_INT { .. }
                            | lexer::Token::CONSTANT_CHAR(_)
                            | lexer::Token::PUNCT_OPEN_PAR
                            | lexer::Token::PUNCT_PLUS
                            | lexer::Token::PUNCT_MINUS
                            | lexer::Token::PUNCT_NOT_BOOL
                            | lexer::Token::PUNCT_TILDE
                    )
                ) {
                    return Err(format!(
                        "not allowed token after operators '<', '<=', '>', '>=', {:?}",
                        tokens[index]
                    ));
                }
            }
            lexer::Token::PUNCT_EQ_BOOL | lexer::Token::PUNCT_NOT_EQ_BOOL => {
                if curr_expr.is_none() {
                    return Err(format!("unexpected token: {:?}", tokens[index]));
                }
                left_expression = curr_expr;
                curr_expr = None;
                right_expression = Some(Expr::Equality(Equality {
                    op: match tokens[index] {
                        lexer::Token::PUNCT_EQ_BOOL => EqualityOp::Equal,
                        lexer::Token::PUNCT_NOT_EQ_BOOL => EqualityOp::NotEqual,
                        _ => unreachable!(),
                    },
                    first: None,
                    second: None,
                }));
                loop {
                    index += 1;
                    if !matches!(tokens.get(index), Some(lexer::Token::WHITESPACE)) {
                        break;
                    }
                }
                if !matches!(
                    tokens.get(index),
                    Some(
                        lexer::Token::IDENT(_)
                            | lexer::Token::CONSTANT_DEC_INT { .. }
                            | lexer::Token::CONSTANT_CHAR(_)
                            | lexer::Token::PUNCT_OPEN_PAR
                            | lexer::Token::PUNCT_PLUS
                            | lexer::Token::PUNCT_MINUS
                            | lexer::Token::PUNCT_NOT_BOOL
                            | lexer::Token::PUNCT_TILDE
                    )
                ) {
                    return Err(format!(
                        "not allowed token after operators '==', '!=', {:?}",
                        tokens[index]
                    ));
                }
            }
            lexer::Token::PUNCT_AND_BIT => {
                if curr_expr.is_none() {
                    return Err(format!("unexpected token: {:?}", tokens[index]));
                }
                left_expression = curr_expr;
                curr_expr = None;
                right_expression = Some(Expr::BitAND(BitAND {
                    first: None,
                    second: None,
                }));
                loop {
                    index += 1;
                    if !matches!(tokens.get(index), Some(lexer::Token::WHITESPACE)) {
                        break;
                    }
                }
                if !matches!(
                    tokens.get(index),
                    Some(
                        lexer::Token::IDENT(_)
                            | lexer::Token::CONSTANT_DEC_INT { .. }
                            | lexer::Token::CONSTANT_CHAR(_)
                            | lexer::Token::PUNCT_OPEN_PAR
                            | lexer::Token::PUNCT_PLUS
                            | lexer::Token::PUNCT_MINUS
                            | lexer::Token::PUNCT_NOT_BOOL
                            | lexer::Token::PUNCT_TILDE
                    )
                ) {
                    return Err(format!(
                        "not allowed token after operator '&' {:?}",
                        tokens[index]
                    ));
                }
            }
            lexer::Token::PUNCT_XOR_BIT => {
                if curr_expr.is_none() {
                    return Err(format!("unexpected token: {:?}", tokens[index]));
                }
                left_expression = curr_expr;
                curr_expr = None;
                right_expression = Some(Expr::BitXOR(BitXOR {
                    first: None,
                    second: None,
                }));
                loop {
                    index += 1;
                    if !matches!(tokens.get(index), Some(lexer::Token::WHITESPACE)) {
                        break;
                    }
                }
                if !matches!(
                    tokens.get(index),
                    Some(
                        lexer::Token::IDENT(_)
                            | lexer::Token::CONSTANT_DEC_INT { .. }
                            | lexer::Token::CONSTANT_CHAR(_)
                            | lexer::Token::PUNCT_OPEN_PAR
                            | lexer::Token::PUNCT_PLUS
                            | lexer::Token::PUNCT_MINUS
                            | lexer::Token::PUNCT_NOT_BOOL
                            | lexer::Token::PUNCT_TILDE
                    )
                ) {
                    return Err(format!(
                        "not allowed token after operator '^' {:?}",
                        tokens[index]
                    ));
                }
            }
            lexer::Token::PUNCT_OR_BIT => {
                if curr_expr.is_none() {
                    return Err(format!("unexpected token: {:?}", tokens[index]));
                }
                left_expression = curr_expr;
                curr_expr = None;
                right_expression = Some(Expr::BitOR(BitOR {
                    first: None,
                    second: None,
                }));
                loop {
                    index += 1;
                    if !matches!(tokens.get(index), Some(lexer::Token::WHITESPACE)) {
                        break;
                    }
                }
                if !matches!(
                    tokens.get(index),
                    Some(
                        lexer::Token::IDENT(_)
                            | lexer::Token::CONSTANT_DEC_INT { .. }
                            | lexer::Token::CONSTANT_CHAR(_)
                            | lexer::Token::PUNCT_OPEN_PAR
                            | lexer::Token::PUNCT_PLUS
                            | lexer::Token::PUNCT_MINUS
                            | lexer::Token::PUNCT_NOT_BOOL
                            | lexer::Token::PUNCT_TILDE
                    )
                ) {
                    return Err(format!(
                        "not allowed token after operator '|' {:?}",
                        tokens[index]
                    ));
                }
            }
            lexer::Token::PUNCT_AND_BOOL => {
                if curr_expr.is_none() {
                    return Err(format!("unexpected token: {:?}", tokens[index]));
                }
                left_expression = curr_expr;
                curr_expr = None;
                right_expression = Some(Expr::LogicalAND(LogicalAND {
                    first: None,
                    second: None,
                }));
                loop {
                    index += 1;
                    if !matches!(tokens.get(index), Some(lexer::Token::WHITESPACE)) {
                        break;
                    }
                }
                if !matches!(
                    tokens.get(index),
                    Some(
                        lexer::Token::IDENT(_)
                            | lexer::Token::CONSTANT_DEC_INT { .. }
                            | lexer::Token::CONSTANT_CHAR(_)
                            | lexer::Token::PUNCT_OPEN_PAR
                            | lexer::Token::PUNCT_PLUS
                            | lexer::Token::PUNCT_MINUS
                            | lexer::Token::PUNCT_NOT_BOOL
                            | lexer::Token::PUNCT_TILDE
                    )
                ) {
                    return Err(format!(
                        "not allowed token after operator '&&' {:?}",
                        tokens[index]
                    ));
                }
            }
            lexer::Token::PUNCT_OR_BOOL => {
                if curr_expr.is_none() {
                    return Err(format!("unexpected token: {:?}", tokens[index]));
                }
                left_expression = curr_expr;
                curr_expr = None;
                right_expression = Some(Expr::LogicalOR(LogicalOR {
                    first: None,
                    second: None,
                }));
                loop {
                    index += 1;
                    if !matches!(tokens.get(index), Some(lexer::Token::WHITESPACE)) {
                        break;
                    }
                }
                if !matches!(
                    tokens.get(index),
                    Some(
                        lexer::Token::IDENT(_)
                            | lexer::Token::CONSTANT_DEC_INT { .. }
                            | lexer::Token::CONSTANT_CHAR(_)
                            | lexer::Token::PUNCT_OPEN_PAR
                            | lexer::Token::PUNCT_PLUS
                            | lexer::Token::PUNCT_MINUS
                            | lexer::Token::PUNCT_NOT_BOOL
                            | lexer::Token::PUNCT_TILDE
                    )
                ) {
                    return Err(format!(
                        "not allowed token after operator '||', {:?}",
                        tokens[index]
                    ));
                }
            }
            lexer::Token::PUNCT_QUESTION_MARK => {
                if let Some(expr) = curr_expr {
                    let expr_cond = Expr::Conditional(Conditional {
                        first: Some(Box::new(expr)),
                        second: None,
                        third: None,
                    });
                    stack.push(expr_cond);
                    curr_expr = None;
                } else {
                    return Err(String::from(
                        "missing first operator for conditional expression",
                    ));
                }
                loop {
                    index += 1;
                    if !matches!(tokens.get(index), Some(lexer::Token::WHITESPACE)) {
                        break;
                    }
                }
            }
            lexer::Token::PUNCT_COLON => {
                if curr_expr.is_none() {
                    return Err(format!("expected expression before ':'"));
                }
                while let Some(mut expr) = stack.pop() {
                    let unwrapped = Some(Box::new(curr_expr.unwrap()));
                    match expr {
                        Expr::Unary(ref mut u) => {
                            u.first = unwrapped;
                        }
                        Expr::Multiplicative(ref mut m) => {
                            m.second = unwrapped;
                        }
                        Expr::Additive(ref mut a) => {
                            a.second = unwrapped;
                        }
                        Expr::BitShift(ref mut bs) => {
                            bs.second = unwrapped;
                        }
                        Expr::Relational(ref mut r) => {
                            r.second = unwrapped;
                        }
                        Expr::Equality(ref mut e) => {
                            e.second = unwrapped;
                        }
                        Expr::BitAND(ref mut ba) => {
                            ba.second = unwrapped;
                        }
                        Expr::BitXOR(ref mut bx) => {
                            bx.second = unwrapped;
                        }
                        Expr::BitOR(ref mut bo) => {
                            bo.second = unwrapped;
                        }
                        Expr::LogicalAND(ref mut la) => {
                            la.second = unwrapped;
                        }
                        Expr::LogicalOR(ref mut lo) => {
                            lo.second = unwrapped;
                        }
                        Expr::Conditional(ref mut c) => {
                            c.second = unwrapped;
                            curr_expr = Some(expr);
                            break;
                        }
                        _ => unreachable!(),
                    }
                    curr_expr = Some(expr);
                }
                if !matches!(curr_expr, Some(Expr::Conditional(_))) {
                    return Err(format!("unexpected ':'. ':' are used in conditional expressions (<expr> ? <expr> : <expr>)"));
                }
                stack.push(curr_expr.unwrap());
                curr_expr = None;
                loop {
                    index += 1;
                    if !matches!(tokens.get(index), Some(lexer::Token::WHITESPACE)) {
                        break;
                    }
                }
            }
            lexer::Token::WHITESPACE => {
                index += 1;
            }
            _ => return Err(format!("unknown token: {:?}", tokens[index])),
        }
        if left_expression.is_some() && right_expression.is_some() {
            let Some(mut left) = left_expression else { unreachable!() };
            let Some(mut right) = right_expression else { unreachable!() };
            if left.priority() >= right.priority() {
                left_has_higher_eq_priority(left, &mut right);
            } else {
                right_has_higher_priority(&mut left, &mut right);
                stack.push(left);
            }
            curr_expr = Some(right);
            left_expression = None;
            right_expression = None;
        }
    }
    while let Some(mut expr) = stack.pop() {
        assert!(curr_expr.is_some());
        let unwrapped = Some(Box::new(curr_expr.unwrap()));
        match expr {
            Expr::Unary(ref mut u) => {
                u.first = unwrapped;
            }
            Expr::Multiplicative(ref mut m) => {
                m.second = unwrapped;
            }
            Expr::Additive(ref mut a) => {
                a.second = unwrapped;
            }
            Expr::BitShift(ref mut bs) => {
                bs.second = unwrapped;
            }
            Expr::Relational(ref mut r) => {
                r.second = unwrapped;
            }
            Expr::Equality(ref mut e) => {
                e.second = unwrapped;
            }
            Expr::BitAND(ref mut ba) => {
                ba.second = unwrapped;
            }
            Expr::BitXOR(ref mut bx) => {
                bx.second = unwrapped;
            }
            Expr::BitOR(ref mut bo) => {
                bo.second = unwrapped;
            }
            Expr::LogicalAND(ref mut la) => {
                la.second = unwrapped;
            }
            Expr::LogicalOR(ref mut lo) => {
                lo.second = unwrapped;
            }
            Expr::Conditional(ref mut c) => {
                assert!(c.first.is_some() && c.second.is_some());
                c.third = unwrapped;
            }
            _ => unreachable!("{}", expr.priority()),
        }
        curr_expr = Some(expr);
    }
    Ok(recursive_eval(curr_expr.unwrap(), str_maps)?)
}
fn recursive_eval(expr: Expr, str_maps: &mut lexer::ByteVecMaps) -> Result<i128, String> {
    match expr {
        Expr::Primary(p) => {
            match p {
                Some(PrimaryInner::Expr(e)) => recursive_eval(*e, str_maps),
                Some(PrimaryInner::Token(t)) => {
                    assert!(matches!(
                        t,
                        lexer::Token::CONSTANT_DEC_INT { .. } | lexer::Token::CONSTANT_CHAR(_)
                    ));
                    match t {
                        lexer::Token::CONSTANT_DEC_INT { value_key, .. } => {
                            // "For the purposes of this token conversion and evaluation,
                            // all signed integer types and all unsigned integer types act as if they have the same representation
                            // as, respectively, the types intmax_t and uintmax_t defined in the header <stdint.h>."
                            //
                            // We just 'cheat' by using i128 integer types. That way, regardless
                            // whether we get u64 (uintmax_t) or i64 (intmax_t), we can still
                            // compare and not have to do any weird casts.
                            // TODO: add overflow checks...
                            let value = &str_maps.key_to_byte_vec[value_key];
                            let Ok(to_be_parsed) = String::from_utf8(value.to_vec()) else { unreachable!() };
                            match to_be_parsed.parse::<i128>() {
                                Ok(v) if v <= u64::MAX as i128 && v >= i64::MIN as i128 => Ok(v),
                                _ => {
                                    return Err(format!(
                                        "{} cannot be represented as i64 or u64",
                                        to_be_parsed
                                    ));
                                }
                            }
                        }
                        lexer::Token::CONSTANT_CHAR(cc) => {
                            let parsed_val = cc.parse_to_value(str_maps)? as i128;
                            Ok(parsed_val)
                        }
                        _ => unreachable!(),
                    }
                }
                None => unreachable!(),
            }
        }
        Expr::Unary(u) => {
            let Some(first) = u.first else { unreachable!() };
            match u.op {
                UnaryOp::Add => Ok(recursive_eval(*first, str_maps)?),
                UnaryOp::Sub => Ok(-recursive_eval(*first, str_maps)?),
                UnaryOp::BitNOT => Ok(!recursive_eval(*first, str_maps)?),
                UnaryOp::LogicalNOT => Ok(if recursive_eval(*first, str_maps)? == 0 {
                    1
                } else {
                    0
                }),
                UnaryOp::Ampersand | UnaryOp::Deref => {
                    unreachable!()
                }
            }
        }
        Expr::Multiplicative(m) => {
            let Some(first) = m.first else { unreachable!() };
            let Some(second) = m.second else { unreachable!() };
            match m.op {
                MultiplicativeOps::Mult => {
                    Ok(recursive_eval(*first, str_maps)? * recursive_eval(*second, str_maps)?)
                }
                MultiplicativeOps::Div | MultiplicativeOps::Mod => {
                    let right = recursive_eval(*second, str_maps)?;
                    if right == 0 {
                        return Err(String::from("cannot divide by zero"));
                    }
                    match m.op {
                        MultiplicativeOps::Div => Ok(recursive_eval(*first, str_maps)? / right),
                        MultiplicativeOps::Mod => Ok(recursive_eval(*first, str_maps)? % right),
                        _ => unreachable!(),
                    }
                }
            }
        }
        Expr::Additive(a) => {
            let Some(left) = a.first else { unreachable!() };
            let Some(right) = a.second else { unreachable!() };
            match a.op {
                AdditiveOps::Add => {
                    Ok(recursive_eval(*left, str_maps)? + recursive_eval(*right, str_maps)?)
                }
                AdditiveOps::Sub => {
                    Ok(recursive_eval(*left, str_maps)? - recursive_eval(*right, str_maps)?)
                }
            }
        }
        Expr::BitShift(bs) => {
            let Some(left) = bs.first else { unreachable!() };
            let Some(right) = bs.second else { unreachable!() };
            match bs.op {
                BitShiftOp::Left => {
                    Ok(recursive_eval(*left, str_maps)? << recursive_eval(*right, str_maps)?)
                }
                BitShiftOp::Right => {
                    Ok(recursive_eval(*left, str_maps)? >> recursive_eval(*right, str_maps)?)
                }
            }
        }
        Expr::Relational(r) => {
            let Some(first) = r.first else { unreachable!() };
            let Some(second) = r.second else { unreachable!() };
            match r.op {
                RelationalOp::LessThan => Ok(
                    if recursive_eval(*first, str_maps)? < recursive_eval(*second, str_maps)? {
                        1
                    } else {
                        0
                    },
                ),
                RelationalOp::LessThanEq => Ok(
                    if recursive_eval(*first, str_maps)? <= recursive_eval(*second, str_maps)? {
                        1
                    } else {
                        0
                    },
                ),
                RelationalOp::GreaterThan => Ok(
                    if recursive_eval(*first, str_maps)? > recursive_eval(*second, str_maps)? {
                        1
                    } else {
                        0
                    },
                ),
                RelationalOp::GreaterThanEq => Ok(
                    if recursive_eval(*first, str_maps)? >= recursive_eval(*second, str_maps)? {
                        1
                    } else {
                        0
                    },
                ),
            }
        }
        Expr::Equality(e) => {
            let Some(first) = e.first else { unreachable!() };
            let Some(second) = e.second else { unreachable!() };
            match e.op {
                EqualityOp::Equal => Ok(
                    if recursive_eval(*first, str_maps)? == recursive_eval(*second, str_maps)? {
                        1
                    } else {
                        0
                    },
                ),
                EqualityOp::NotEqual => Ok(
                    if recursive_eval(*first, str_maps)? != recursive_eval(*second, str_maps)? {
                        1
                    } else {
                        0
                    },
                ),
            }
        }
        Expr::BitAND(ba) => {
            let Some(first) = ba.first else { unreachable!() };
            let Some(second) = ba.second else { unreachable!() };
            Ok(
                if (recursive_eval(*first, str_maps)? & recursive_eval(*second, str_maps)?) != 0 {
                    1
                } else {
                    0
                },
            )
        }
        Expr::BitXOR(bx) => {
            let Some(first) = bx.first else { unreachable!() };
            let Some(second) = bx.second else { unreachable!() };
            Ok(
                if (recursive_eval(*first, str_maps)? ^ recursive_eval(*second, str_maps)?) != 0 {
                    1
                } else {
                    0
                },
            )
        }
        Expr::BitOR(bo) => {
            let Some(first) = bo.first else { unreachable!() };
            let Some(second) = bo.second else { unreachable!() };
            Ok(
                if (recursive_eval(*first, str_maps)? | recursive_eval(*second, str_maps)?) != 0 {
                    1
                } else {
                    0
                },
            )
        }
        Expr::LogicalAND(la) => {
            let Some(first) = la.first else { unreachable!() };
            let Some(second) = la.second else { unreachable!() };
            Ok(
                if recursive_eval(*first, str_maps)? != 0 && recursive_eval(*second, str_maps)? != 0
                {
                    1
                } else {
                    0
                },
            )
        }
        Expr::LogicalOR(lo) => {
            let Some(first) = lo.first else { unreachable!() };
            let Some(second) = lo.second else { unreachable!() };
            Ok(
                if recursive_eval(*first, str_maps)? != 0 || recursive_eval(*second, str_maps)? != 0
                {
                    1
                } else {
                    0
                },
            )
        }
        Expr::Conditional(c) => {
            let Some(first) = c.first else { unreachable!() };
            let Some(second) = c.second else { unreachable!() };
            let Some(third) = c.third else { unreachable!() };
            if recursive_eval(*first, str_maps)? != 0 {
                Ok(recursive_eval(*second, str_maps)?)
            } else {
                Ok(recursive_eval(*third, str_maps)?)
            }
        }
        _ => unreachable!(),
    }
}
#[cfg(test)]
mod tests {
    use crate::lexer;
    use crate::parser::expressions::{self};
    use std::collections::HashMap;
    #[test]
    fn eval_expression_test_empty() -> Result<(), String> {
        let src = r##""##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps);
        match res {
            Err(_) => {}
            Ok(_) => return Err(String::from("empty expression not caught")),
        }
        Ok(())
    }
    #[test]
    fn eval_expression_test_primary() -> Result<(), String> {
        {
            let src = r##"(1 + 1) * 0"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, false, "(1 + 1) * 0");
        }
        {
            let src = r##"1 + (1 * 0)"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, true, "1 + (1 * 0)");
        }
        {
            let src = r##"((1 + 1) * 0)"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, false, "((1 + 1) * 0)");
        }
        {
            let src = r##"((((1))))"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, true, "((((1))))");
        }
        {
            let src = r##"((((1)))))"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps);
            match res {
                Err(_) => {}
                Ok(_) => return Err(String::from("unbalanced parentheses not caught")),
            }
        }
        {
            let src = r##"(((((1))))"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps);
            match res {
                Err(_) => {}
                Ok(_) => return Err(String::from("unbalanced parentheses not caught")),
            }
        }
        {
            let src = r##"0 - (1 + 1)"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, true, "0 - (1 + 1)");
        }
        {
            let src = r##"1"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, true, "1");
        }
        {
            let src = r##"'1'"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, true, "'1'");
        }
        Ok(())
    }
    #[test]
    fn eval_expression_test_unary() -> Result<(), String> {
        let src = r##"!1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false, "!1");
        let src = r##"!0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "!0");
        let src = r##"~0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "~0");
        let src = r##"~~~0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "~~~0");
        let src = r##"~~~~0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false, "~~~~0");
        let src = r##"--------------1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps);
        match res {
            Err(_) => {}
            Ok(_) => {
                return Err(String::from(
                    "'--' operator not caught in cpp constant expression",
                ))
            }
        }
        Ok(())
    }
    #[test]
    fn eval_expression_test_multiplicative() -> Result<(), String> {
        let src = r##"1 * 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 * !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 / 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 / 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps);
        match res {
            Err(_) => {}
            Ok(_) => return Err("division by zero not caught".to_string()),
        }
        let src = r##"1 + 1 * 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 * 1 + 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_additive() -> Result<(), String> {
        let src = r##"1 + 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 - 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"0 - 1 + 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false, "0 - 1 + 1");
        let src = r##"0 - 1 + !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "0 - 1 + !1");
        {
            let src = r##"'1' - '1'"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, false, "'1' - '1'");
        }
        {
            let src = r##"'2' - '1'"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, true, "'1' - '1'");
        }
        Ok(())
    }
    #[test]
    fn eval_expression_test_bitshift() -> Result<(), String> {
        let src = r##"1 << 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 >> 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 >> !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        Ok(())
    }
    #[test]
    fn eval_expression_test_relational() -> Result<(), String> {
        let src = r##"1 < 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 < 2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 < !2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 <= 2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"2 <= 2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 > 2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 > 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 >= 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 >= 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        Ok(())
    }
    #[test]
    fn eval_expression_test_equality() -> Result<(), String> {
        let src = r##"1 == 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 != 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 != !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 != 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        Ok(())
    }
    #[test]
    fn eval_expression_test_bit_and() -> Result<(), String> {
        let src = r##"1 & 0 == 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 & 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 & !0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 & 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 == 0 & 1 == 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_bit_xor() -> Result<(), String> {
        let src = r##"1 ^ 0 == 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"(1 ^ !0) == 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        Ok(())
    }
    #[test]
    fn eval_expression_test_bit_or() -> Result<(), String> {
        let src = r##"1 | 0 == 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 | !0 == 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "1 | !0 == 0");
        Ok(())
    }
    #[test]
    fn eval_expression_test_logical_and() -> Result<(), String> {
        let src = r##"1 && 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 && 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 && !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_logical_or() -> Result<(), String> {
        let src = r##"1 || 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 || 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 || 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_conditional() -> Result<(), String> {
        let src = r##"1 ? 1 : 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"(1 + 1 == 3) ? 1 : 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"~0 ? (1 + 1 == 2) : 0 * 4"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 ? 0 : 1 * 4"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 ? 0 : !(1 * 4)"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        Ok(())
    }
}
