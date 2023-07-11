use crate::lexer;
use crate::parser;
#[derive(Copy, Clone)]
pub enum PrimaryInner {
    Token(lexer::Token),
    Expr(usize),
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
    pub fn new_p_expr(e_index: usize) -> Self {
        Self::Expr(e_index)
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

pub type ExpressionIndex = usize;
pub trait Binary {
    fn get_first(&self) -> Option<ExpressionIndex>;
    fn get_second(&self) -> Option<ExpressionIndex>;
    fn change_first(&mut self, expr_index: Option<ExpressionIndex>);
    fn change_second(&mut self, expr_index: Option<ExpressionIndex>);
}

macro_rules! impl_binary {
    ($($type: ty) *) => {
        $(impl Binary for $type {
            fn get_first(&self) -> Option<ExpressionIndex> {
                self.first
            }
            fn get_second(&self) -> Option<ExpressionIndex> {
                self.second
            }
            fn change_first(&mut self, expr_index: Option<ExpressionIndex>) {
                self.first = expr_index;
            }
            fn change_second(&mut self, expr_index: Option<ExpressionIndex>) {
                self.second = expr_index;
            }
        })*
    }
}

impl_binary!(Multiplicative Additive BitShift Relational Equality BitAND BitXOR BitOR LogicalAND LogicalOR);

#[derive(Copy, Clone)]
pub struct Conditional {
    pub first: Option<ExpressionIndex>,
    pub second: Option<ExpressionIndex>,
    pub third: Option<ExpressionIndex>,
}

#[derive(Copy, Clone)]
pub struct LogicalOR {
    pub first: Option<ExpressionIndex>,
    pub second: Option<ExpressionIndex>,
}

#[derive(Copy, Clone)]
pub struct LogicalAND {
    pub first: Option<ExpressionIndex>,
    pub second: Option<ExpressionIndex>,
}

#[derive(Copy, Clone)]
pub struct BitOR {
    pub first: Option<ExpressionIndex>,
    pub second: Option<ExpressionIndex>,
}

#[derive(Copy, Clone)]
pub struct BitXOR {
    pub first: Option<ExpressionIndex>,
    pub second: Option<ExpressionIndex>,
}

#[derive(Copy, Clone)]
pub struct BitAND {
    pub first: Option<ExpressionIndex>,
    pub second: Option<ExpressionIndex>,
}

#[derive(Copy, Clone)]
pub enum EqualityOp {
    Equal,
    NotEqual,
}

#[derive(Copy, Clone)]
pub struct Equality {
    pub op: EqualityOp,
    pub first: Option<ExpressionIndex>,
    pub second: Option<ExpressionIndex>,
}

#[derive(Copy, Clone)]
pub enum RelationalOp {
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
}

#[derive(Copy, Clone)]
pub struct Relational {
    pub op: RelationalOp,
    pub first: Option<ExpressionIndex>,
    pub second: Option<ExpressionIndex>,
}

#[derive(Copy, Clone)]
pub enum BitShiftOp {
    Left,
    Right,
}

#[derive(Copy, Clone)]
pub struct BitShift {
    pub op: BitShiftOp,
    pub first: Option<ExpressionIndex>,
    pub second: Option<ExpressionIndex>,
}

#[derive(Copy, Clone)]
pub enum AdditiveOps {
    Add,
    Sub,
}

#[derive(Copy, Clone)]
pub struct Additive {
    pub op: AdditiveOps,
    pub first: Option<ExpressionIndex>,
    pub second: Option<ExpressionIndex>,
}
#[derive(Copy, Clone)]
pub enum MultiplicativeOps {
    Mult,
    Div,
    Mod,
}
#[derive(Copy, Clone)]
pub struct Multiplicative {
    pub op: MultiplicativeOps,
    pub first: Option<ExpressionIndex>,
    pub second: Option<ExpressionIndex>,
}
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Ampersand,
    Sub,
    Add,
    Deref,
    BitNOT,
    LogicalNOT,
    Increment,
    Sizeof,
    AlignOf,
}
#[derive(Copy, Clone)]
pub struct Unary {
    pub op: UnaryOp,
    pub first: Option<ExpressionIndex>,
}
#[derive(Copy, Clone)]
pub struct Cast {
    type_name: Option<parser::declarations::TypeNameIndex>,
    cast_expr: Option<usize>,
}
#[derive(Copy, Clone)]
pub enum PostFixIncrementDecrement {
    Increment,
    Decrement,
}
#[derive(Copy, Clone)]
pub enum PostFix {
    WithSubscript {
        first: ExpressionIndex,
        subscript: ExpressionIndex,
    },
    WithMember {
        first: ExpressionIndex,
        member_ident_key: usize,
    },
    WithPointerToMember {
        first: ExpressionIndex,
        member_ident_key: usize,
    },
    WithIncrementDecrement {
        first: ExpressionIndex,
        op: PostFixIncrementDecrement,
    },
    WithTypeNameInitializerList {
        type_name: parser::declarations::TypeNameIndex,
        initializer_list: parser::declarations::InitializerListIndex,
    },
}
#[derive(Copy, Clone)]
pub struct Assignment {
    pub first: Option<ExpressionIndex>,
    pub second: Option<ExpressionIndex>,
}

#[derive(Copy, Clone)]
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
    macro_rules! match_right_and_do_operation {
        ($($e: ident) * , $a: ident) => {
            match right {
                Expr::Primary(p) => {
                    assert!(p.is_some());
                    assert!($a.second.is_none());
                }
                Expr::PostFix(_) => todo!(),
                Expr::Unary(_u) => {
                    assert!($a.second.is_none());
                }
                Expr::Cast(_) => todo!(),
                $(Expr::$e(i) => {
                    assert!(i.first.is_none());
                    i.first = $a.second;
                })*
                _ => unreachable!(),
            }
        };
    }
    macro_rules! match_right_assign_to_third {
        ($($e: ident) *, $c: ident) => {
            match right {
                Expr::Primary(p) => {
                    assert!(p.is_some());
                    assert!($c.third.is_none());
                }
                Expr::Unary(_u) => {
                    assert!($c.third.is_none());
                }
                $(Expr::$e(i) => {
                    assert!(i.first.is_none());
                    i.first = $c.third;
                })*
                _ => unreachable!(),
            }
        };
    }
    macro_rules! match_left_and_match_right {
        ($($e: ident) *) => {
            match left {
                Expr::Unary(u) => match right {
                    Expr::Primary(p) => {
                        assert!(p.is_some());
                        assert!(u.first.is_none());
                    }
                    _ => unreachable!(),
                },
                $(Expr::$e(i) => {
                    match_right_and_do_operation!(Multiplicative Additive BitShift Relational Equality BitAND BitXOR BitOR LogicalAND LogicalOR, i);
                })*
                Expr::Conditional(c) => {
                    assert!(c.first.is_some());
                    assert!(c.second.is_some());
                    match_right_assign_to_third!(Multiplicative Additive BitShift Relational Equality BitAND BitXOR BitOR LogicalAND LogicalOR, c);
                }
                Expr::Assignment(a) => {
                    match_right_and_do_operation!(Multiplicative Additive BitShift Relational Equality BitAND BitXOR BitOR LogicalAND LogicalOR, a)
                }
                _ => unreachable!(),
            }
        };
    }
    match_left_and_match_right!(Multiplicative Additive BitShift Relational Equality BitAND BitXOR BitOR LogicalAND LogicalOR);
}

fn left_has_higher_eq_priority(left: usize, right: &mut Expr) {
    let index = Some(left);
    macro_rules! set_to_unwrapped {
        ($($e: ident) *) => {
            match right {
                $(Expr::$e(ref mut i) => {
                    i.first = index;
                })*
                _ => unreachable!(),
            }
        };
    }
    set_to_unwrapped!(Multiplicative Additive BitShift Relational Equality BitAND BitXOR BitOR LogicalAND LogicalOR);
}
macro_rules! expression_operators {
    () => {
        lexer::Token::PUNCT_PLUS
            | lexer::Token::PUNCT_MINUS
            | lexer::Token::PUNCT_MULT
            | lexer::Token::PUNCT_DIV
            | lexer::Token::PUNCT_MODULO
            | lexer::Token::PUNCT_BITSHIFT_LEFT
            | lexer::Token::PUNCT_BITSHIFT_RIGHT
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
            | lexer::Token::PUNCT_ASSIGNMENT
            | lexer::Token::PUNCT_MULT_ASSIGN
            | lexer::Token::PUNCT_DIV_ASSIGN
            | lexer::Token::PUNCT_MODULO_ASSIGN
            | lexer::Token::PUNCT_ADD_ASSIGN
            | lexer::Token::PUNCT_SUB_ASSIGN
            | lexer::Token::PUNCT_L_SHIFT_BIT_ASSIGN
            | lexer::Token::PUNCT_R_SHIFT_BIT_ASSIGN
            | lexer::Token::PUNCT_AND_BIT_ASSIGN
            | lexer::Token::PUNCT_XOR_BIT_ASSIGN
            | lexer::Token::PUNCT_OR_BIT_ASSIGN
            | lexer::Token::PUNCT_INCREMENT
            | lexer::Token::PUNCT_DECREMENT
            | lexer::Token::PUNCT_DOT
            | lexer::Token::PUNCT_ARROW
    };
}

macro_rules! primary_tokens {
    () => {
        lexer::Token::IDENT(_)
            | lexer::Token::StringLiteral { .. }
            | lexer::Token::CONSTANT_DEC_INT { .. }
            | lexer::Token::CONSTANT_HEXA_INT { .. }
            | lexer::Token::CONSTANT_DEC_FLOAT { .. }
            | lexer::Token::CONSTANT_HEXA_FLOAT { .. }
            | lexer::Token::CONSTANT_CHAR { .. }
            | lexer::Token::CONSTANT_OCTAL_INT { .. }
            | lexer::Token::CONSTANT_ENUM(_)
    };
}
pub fn parse_expressions(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(usize, Expr), String> {
    // stack is used for expressions that have a lower priority
    let mut stack = Vec::<Expr>::new();
    // curr_expr is used for expressions with higher priority
    let mut curr_expr: Option<Expr> = None;
    let mut left_expression: Option<Expr> = None;
    let mut index = start_index;
    while index < tokens.len() {
        match &tokens[index] {
            lexer::Token::PUNCT_ASSIGNMENT
            | lexer::Token::PUNCT_MULT_ASSIGN
            | lexer::Token::PUNCT_DIV_ASSIGN
            | lexer::Token::PUNCT_MODULO_ASSIGN
            | lexer::Token::PUNCT_ADD_ASSIGN
            | lexer::Token::PUNCT_SUB_ASSIGN
            | lexer::Token::PUNCT_L_SHIFT_BIT_ASSIGN
            | lexer::Token::PUNCT_R_SHIFT_BIT_ASSIGN
            | lexer::Token::PUNCT_AND_BIT_ASSIGN
            | lexer::Token::PUNCT_XOR_BIT_ASSIGN
            | lexer::Token::PUNCT_OR_BIT_ASSIGN => {
                if curr_expr.is_none() {
                    return Err(format!("Unexpected operator: {:?}", tokens[index]));
                }
                match curr_expr {
                    Some(Expr::Unary(_) | Expr::Primary(_)) => {
                        let Some(expr) = curr_expr else { unreachable!() };
                        flattened.expressions.push(expr);
                        let assignment = Assignment {
                            first: Some(flattened.expressions.len() - 1),
                            second: None,
                        };
                        curr_expr = Some(Expr::Assignment(assignment));
                    }
                    Some(Expr::Assignment(mut a)) => {
                        let assignment = Assignment {
                            first: a.second,
                            second: None,
                        };
                        a.second = None;
                        stack.push(Expr::Assignment(a));
                        curr_expr = Some(Expr::Assignment(assignment));
                    }
                    _ => return Err(format!("Expected unary expression")),
                }
                loop {
                    index += 1;
                    if !matches!(
                        tokens.get(index),
                        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                    ) {
                        break;
                    }
                }
                if !matches!(tokens.get(index), Some(primary_tokens!()) | None) {
                    return Err(format!("unexpected operator: {:?}", tokens.get(index)));
                }
            }
            lexer::Token::PUNCT_INCREMENT
            | lexer::Token::PUNCT_DECREMENT
            | lexer::Token::PUNCT_DOT
            | lexer::Token::PUNCT_ARROW => {
                if let Some(unwrapped) = curr_expr {
                    flattened.expressions.push(unwrapped);
                    let expr = match tokens.get(index) {
                        Some(lexer::Token::PUNCT_INCREMENT | lexer::Token::PUNCT_DECREMENT) => {
                            Expr::PostFix(PostFix::WithIncrementDecrement {
                                first: flattened.expressions.len() - 1,
                                op: match tokens.get(index) {
                                    Some(lexer::Token::PUNCT_INCREMENT) => {
                                        PostFixIncrementDecrement::Increment
                                    }
                                    Some(lexer::Token::PUNCT_DECREMENT) => {
                                        PostFixIncrementDecrement::Decrement
                                    }
                                    _ => unreachable!(),
                                },
                            })
                        }
                        Some(lexer::Token::PUNCT_DOT | lexer::Token::PUNCT_ARROW) => {
                            let postfix_type = match tokens.get(index) {
                                Some(lexer::Token::PUNCT_DOT) => PostFix::WithMember {
                                    first: flattened.expressions.len() - 1,
                                    member_ident_key: {
                                        loop {
                                            index += 1;
                                            if !matches!(
                                                tokens.get(index),
                                                Some(
                                                    lexer::Token::WHITESPACE
                                                        | lexer::Token::NEWLINE
                                                )
                                            ) {
                                                break;
                                            }
                                        }
                                        if !matches!(
                                            tokens.get(index),
                                            Some(lexer::Token::IDENT(_))
                                        ) {
                                            return Err(format!(
                                                "expected identifier, got {:?}",
                                                tokens.get(index)
                                            ));
                                        }
                                        let Some(lexer::Token::IDENT(key)) = tokens.get(index) else { unreachable!() };
                                        *key
                                    },
                                },
                                Some(lexer::Token::PUNCT_ARROW) => PostFix::WithPointerToMember {
                                    first: flattened.expressions.len() - 1,
                                    member_ident_key: {
                                        loop {
                                            index += 1;
                                            if !matches!(
                                                tokens.get(index),
                                                Some(
                                                    lexer::Token::WHITESPACE
                                                        | lexer::Token::NEWLINE
                                                )
                                            ) {
                                                break;
                                            }
                                        }
                                        if !matches!(
                                            tokens.get(index),
                                            Some(lexer::Token::IDENT(_))
                                        ) {
                                            return Err(format!(
                                                "expected identifier, got {:?}",
                                                tokens.get(index)
                                            ));
                                        }
                                        let Some(lexer::Token::IDENT(key)) = tokens.get(index) else { unreachable!() };
                                        *key
                                    },
                                },
                                _ => unreachable!(),
                            };
                            Expr::PostFix(postfix_type)
                        }
                        _ => unreachable!(),
                    };
                    flattened.expressions.push(expr);
                    macro_rules! primary_second_assign_swap {
                        ($($e:ident)*) => {
                            match &mut curr_expr {
                                Some(Expr::Primary(_)) => {
                                    curr_expr = Some(expr);
                                }
                                Some(Expr::PostFix(_)) => {
                                    return Err(format!("postfix after postfix is not allowed"));
                                },
                                Some(Expr::Unary(u)) => {
                                    u.first = Some(flattened.expressions.len() - 1);
                                }
                                Some(Expr::Cast(c)) => {
                                    c.cast_expr = Some(flattened.expressions.len() - 1);
                                },
                                $(Some(Expr::$e(i)) => {
                                    i.second = Some(flattened.expressions.len() - 1);
                                })*
                                Some(Expr::Conditional(c)) => {
                                    if c.first.is_none() {
                                        c.first = Some(flattened.expressions.len() - 1);
                                    } else if c.second.is_none() {
                                        c.second = Some(flattened.expressions.len() - 1);
                                    } else if c.third.is_none() {
                                        c.third = Some(flattened.expressions.len() - 1);
                                    }
                                }
                                Some(Expr::Assignment(_a)) => todo!(),
                                _ => return Err(format!("err at index: {}", index)),
                            }
                        };
                    }
                    primary_second_assign_swap!(Multiplicative Additive BitShift Relational Equality BitAND BitXOR BitOR LogicalAND LogicalOR);
                    loop {
                        index += 1;
                        if !matches!(
                            tokens.get(index),
                            Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                        ) {
                            break;
                        }
                    }
                    if !matches!(tokens.get(index), Some(expression_operators!()) | None) {
                        return Err(format!("unexpected operator: {:?}", tokens.get(index)));
                    }
                } else {
                    unreachable!()
                }
            }
            primary_tokens!() => {
                // TODO: we need to check for the case of sizeof and _Alignof
                let token_within = tokens[index];
                let mut temp_index = index;
                loop {
                    temp_index += 1;
                    if !matches!(
                        tokens.get(temp_index),
                        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                    ) {
                        break;
                    }
                }
                let primary = Expr::Primary(Some(PrimaryInner::new_p_token(token_within)?));
                flattened.expressions.push(primary);
                let last_index = flattened.expressions.len() - 1;
                if curr_expr.is_none() {
                    curr_expr = Some(primary);
                } else {
                    macro_rules! primary_second_assign {
                        ($($e:ident)*) => {
                            match &mut curr_expr {
                                Some(Expr::PostFix(_)) => {
                                    return Err(format!("postfix after postfix is not allowed"));
                                },
                                Some(Expr::Unary(u)) => {
                                    assert!(u.first.is_none());
                                    u.first = Some(last_index);
                                }
                                Some(Expr::Cast(c)) => {
                                    c.cast_expr = Some(last_index);
                                },
                                $(Some(Expr::$e(i)) => {
                                    i.second = Some(last_index);
                                })*
                                Some(Expr::Conditional(c)) => {
                                    if c.first.is_none() {
                                        c.first = Some(last_index);
                                    } else if c.second.is_none() {
                                        c.second = Some(last_index);
                                    } else if c.third.is_none() {
                                        c.third = Some(last_index);
                                    }
                                }
                                _ => return Err(format!("err at index: {}", index)),
                            }
                        };
                    }

                    primary_second_assign!(Multiplicative Additive BitShift Relational Equality BitAND BitXOR BitOR LogicalAND LogicalOR Assignment);
                }

                loop {
                    index += 1;
                    if !matches!(
                        tokens.get(index),
                        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                    ) {
                        break;
                    }
                }
                if !matches!(tokens.get(index), Some(expression_operators!()) | None) {
                    return Err(format!("unexpected operator: {:?}", tokens.get(index)));
                }
            }
            lexer::Token::PUNCT_OPEN_PAR => {
                index += 1;
                let starting = index;
                let mut parenth_counter = 1;
                while parenth_counter > 0 {
                    match tokens.get(index) {
                        Some(lexer::Token::PUNCT_OPEN_PAR) => parenth_counter += 1,
                        Some(lexer::Token::PUNCT_CLOSE_PAR) => parenth_counter -= 1,
                        None => return Err(format!("Missing closing parenth")),
                        _ => {}
                    }
                    index += 1;
                }
                while matches!(
                    tokens.get(index),
                    Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                ) && index < tokens.len()
                {
                    index += 1;
                }
                if let Some(expr) = curr_expr {
                    stack.push(expr);
                }
                // if we run into a token that makes everything inside the (...) just a primary
                // expression
                if matches!(tokens.get(index), Some(expression_operators!()) | None) {
                    stack.push(Expr::Primary(None));
                    curr_expr = None;
                    let mut inside_parenth_index = starting;
                    while matches!(
                        tokens.get(inside_parenth_index),
                        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                    ) {
                        inside_parenth_index += 1;
                    }
                    if !matches!(
                        tokens.get(inside_parenth_index),
                        Some(
                            lexer::Token::IDENT(_)
                                | lexer::Token::CONSTANT_DEC_INT { .. }
                                | lexer::Token::CONSTANT_CHAR(_)
                                | lexer::Token::PUNCT_OPEN_PAR
                                | lexer::Token::PUNCT_PLUS
                                | lexer::Token::PUNCT_MINUS
                                | lexer::Token::PUNCT_NOT_BOOL
                                | lexer::Token::PUNCT_TILDE
                                | lexer::Token::PUNCT_INCREMENT
                                | lexer::Token::PUNCT_DECREMENT
                        )
                    ) {
                        return Err(format!(
                            "expected '-', '(', or an identifier/integer constant: {:?}",
                            tokens[inside_parenth_index]
                        ));
                    }
                    index = inside_parenth_index;
                } else {
                    // Typenames
                    let (new_index, type_name) = parser::declarations::parse_type_names(
                        tokens, starting, flattened, str_maps,
                    )?;
                    flattened.type_names.push(type_name);
                    match tokens.get(index) {
                        // Postfix
                        Some(lexer::Token::PUNCT_OPEN_CURLY) => {
                            let (_, i) = parser::declarations::parse_initializer(
                                tokens, index, flattened, str_maps,
                            )?;
                            flattened.initializers.push(i);
                            stack.push(parser::expressions::Expr::PostFix(
                                PostFix::WithTypeNameInitializerList {
                                    type_name: flattened.type_names.len() - 1,
                                    initializer_list: flattened.initializers.len() - 1,
                                },
                            ));
                            curr_expr = None;
                        }
                        // Cast
                        Some(_) => {
                            let cast = Cast {
                                type_name: Some(flattened.type_names.len() - 1),
                                cast_expr: None,
                            };
                            stack.push(Expr::Cast(cast));
                            curr_expr = None;
                        }
                        None => unreachable!(),
                    }
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
                let mut already_popped_primary = false;
                while let Some(mut e) = stack.pop() {
                    match e {
                        Expr::Primary(ref mut p) => {
                            let Some(unwrapped) = curr_expr else { unreachable!() };
                            flattened.expressions.push(unwrapped);
                            *p = Some(PrimaryInner::new_p_expr(flattened.expressions.len() - 1));
                            curr_expr = Some(e);
                            if !matches!(stack.last(), Some(Expr::Unary(_) | Expr::Cast(_))) {
                                break;
                            } else {
                                already_popped_primary = true;
                            }
                        }
                        Expr::PostFix(_) => {
                            unreachable!()
                        }
                        Expr::Unary(ref mut u) => {
                            let Some(unwrapped) = curr_expr else { unreachable!() };
                            flattened.expressions.push(unwrapped);
                            u.first = Some(flattened.expressions.len() - 1);
                            curr_expr = Some(e);
                            if !matches!(stack.last(), Some(Expr::Unary(_) | Expr::Cast(_)))
                                && already_popped_primary
                            {
                                break;
                            }
                        }
                        Expr::Cast(ref mut c) => {
                            let Some(unwrapped) = curr_expr else { unreachable!() };
                            flattened.expressions.push(unwrapped);
                            c.cast_expr = Some(flattened.expressions.len() - 1);
                            curr_expr = Some(e);
                            if !matches!(stack.last(), Some(Expr::Unary(_) | Expr::Cast(_)))
                                && already_popped_primary
                            {
                                break;
                            }
                        }
                        _ => {
                            let Some(unwrapped) = curr_expr else { unreachable!() };
                            assert!(
                                e.priority() <= unwrapped.priority(),
                                "{} {}",
                                e.priority(),
                                unwrapped.priority()
                            );
                            flattened.expressions.push(unwrapped);
                            let unwrapped = Some(flattened.expressions.len() - 1);
                            macro_rules! set_to_unwrapped {
                                ($($e: ident) *) => {
                                    match e {
                                        Expr::Unary(ref mut u) => {
                                            u.first = unwrapped;
                                        }
                                        $(Expr::$e(ref mut i) => {
                                            i.second = unwrapped;
                                        })*
                                        Expr::Conditional(ref mut c) => {
                                            c.third = unwrapped;
                                        }
                                        _ => unreachable!(),
                                    }
                                };
                            }
                            set_to_unwrapped!(Multiplicative Additive BitShift Relational Equality BitAND BitXOR BitOR LogicalAND LogicalOR);
                            curr_expr = Some(e);
                        }
                    }
                }
                loop {
                    index += 1;
                    if !matches!(
                        tokens.get(index),
                        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                    ) {
                        break;
                    }
                }
                if matches!(
                    tokens.get(index),
                    Some(primary_tokens!() | lexer::Token::PUNCT_OPEN_PAR)
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
                macro_rules! left_expression_handle_in_unary_expression {
                    ($($e: ident) *) => {
                        match &left_expression {
                            Some(Expr::Primary(_) | Expr::PostFix(_)) => {
                                // if a '~' or '!' follow a primary expression, that is not allowed.
                                match tokens[index] {
                                    lexer::Token::PUNCT_TILDE | lexer::Token::PUNCT_NOT_BOOL => {
                                        return Err(format!("unexpected {:?} token", tokens[index]));
                                    }
                                    _ => {}
                                }
                                curr_expr = Some(Expr::Additive(Additive {
                                    op: match tokens[index] {
                                        lexer::Token::PUNCT_PLUS => AdditiveOps::Add,
                                        lexer::Token::PUNCT_MINUS => AdditiveOps::Sub,
                                        _ => unreachable!("{:?}", tokens[index]),
                                    },
                                    first: None,
                                    second: None,
                                }));
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
                                    let Some(left_expression_unwrapped) = left_expression else { unreachable!() };
                                    stack.push(left_expression_unwrapped);
                                    left_expression = None;
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
                                    curr_expr = Some(Expr::Additive(Additive {
                                        op: match tokens[index] {
                                            lexer::Token::PUNCT_PLUS => AdditiveOps::Add,
                                            lexer::Token::PUNCT_MINUS => AdditiveOps::Sub,
                                            _ => unreachable!("{:?}", tokens[index]),
                                        },
                                        first: None,
                                        second: None,
                                    }));
                                }
                            }
                            $(Some(Expr::$e(i)) => {
                                case_where_it_could_be_unary_or_additive!(
                                    i,
                                    curr_expr,
                                    tokens[index]
                                );
                            })*
                            _ => unreachable!(),
                        }
                    };
                }
                left_expression_handle_in_unary_expression!(Multiplicative Additive BitShift Relational Equality BitAND BitXOR BitOR LogicalAND LogicalOR);
                loop {
                    index += 1;
                    if !matches!(tokens.get(index), Some(lexer::Token::WHITESPACE)) {
                        break;
                    }
                }
                if !matches!(
                    tokens.get(index),
                    Some(
                        primary_tokens!()
                            | lexer::Token::PUNCT_OPEN_PAR
                            | lexer::Token::PUNCT_PLUS
                            | lexer::Token::PUNCT_MINUS
                            | lexer::Token::PUNCT_NOT_BOOL
                            | lexer::Token::PUNCT_TILDE
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
                curr_expr = Some(Expr::Multiplicative(Multiplicative {
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
                        primary_tokens!()
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
            lexer::Token::PUNCT_BITSHIFT_RIGHT | lexer::Token::PUNCT_BITSHIFT_LEFT => {
                if curr_expr.is_none() {
                    return Err(format!("unexpected token: {:?}", tokens[index]));
                }
                left_expression = curr_expr;
                curr_expr = Some(Expr::BitShift(BitShift {
                    op: match tokens[index] {
                        lexer::Token::PUNCT_BITSHIFT_LEFT => BitShiftOp::Left,
                        lexer::Token::PUNCT_BITSHIFT_RIGHT => BitShiftOp::Right,
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
                        primary_tokens!()
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
                curr_expr = Some(Expr::Relational(Relational {
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
                        primary_tokens!()
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
                curr_expr = Some(Expr::Equality(Equality {
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
                        primary_tokens!()
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
                curr_expr = Some(Expr::BitAND(BitAND {
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
                        primary_tokens!()
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
                curr_expr = Some(Expr::BitXOR(BitXOR {
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
                        primary_tokens!()
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
                curr_expr = Some(Expr::BitOR(BitOR {
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
                        primary_tokens!()
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
                curr_expr = Some(Expr::LogicalAND(LogicalAND {
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
                        primary_tokens!()
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
                curr_expr = Some(Expr::LogicalOR(LogicalOR {
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
                        primary_tokens!()
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
                    flattened.expressions.push(expr);
                    let expr_cond = Expr::Conditional(Conditional {
                        first: Some(flattened.expressions.len() - 1),
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
                    let Some(unwrapped) = curr_expr else { unreachable!() };
                    flattened.expressions.push(unwrapped);
                    let unwrapped = Some(flattened.expressions.len() - 1);
                    macro_rules! set_to_unwrapped {
                        ($($e: ident) *) => {
                            match expr {
                                Expr::Unary(ref mut u) => {
                                    u.first = unwrapped;
                                }
                                $(Expr::$e(ref mut i) => {
                                    i.second = unwrapped;
                                })*
                                Expr::Conditional(ref mut c) => {
                                    c.second = unwrapped;
                                    curr_expr = Some(expr);
                                    break;
                                }
                                _ => unreachable!(),
                            }
                        };
                    }
                    set_to_unwrapped!(Multiplicative Additive BitShift Relational Equality BitAND BitXOR BitOR LogicalAND LogicalOR);
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
        if left_expression.is_some() && curr_expr.is_some() {
            let Some(mut left) = left_expression else { unreachable!() };
            let Some(mut right) = curr_expr else { unreachable!() };
            if left.priority() >= right.priority() {
                assert!(left.priority() >= right.priority());
                flattened.expressions.push(left);
                left_has_higher_eq_priority(flattened.expressions.len() - 1, &mut right);
            } else {
                right_has_higher_priority(&mut left, &mut right);
                stack.push(left);
            }
            curr_expr = Some(right);
            left_expression = None;
        }
    }
    while let Some(mut expr) = stack.pop() {
        if let Some(unwrapped) = curr_expr {
            flattened.expressions.push(unwrapped);
            let unwrapped = Some(flattened.expressions.len() - 1);
            macro_rules! set_to_unwrapped {
                ($($e: ident) *) => {
                    match expr {
                        Expr::PostFix(_) => todo!(),
                        Expr::Unary(ref mut u) => {
                            u.first = unwrapped;
                        }
                        Expr::Cast(ref mut c) => {
                            c.cast_expr = unwrapped;
                        }
                        $(Expr::$e(ref mut i) => {
                            i.second = unwrapped;
                        })*
                        Expr::Conditional(ref mut c) => {
                            assert!(c.first.is_some() && c.second.is_some());
                            c.third = unwrapped;
                        }
                        _ => unreachable!(),
                    }
                };
                    }
            set_to_unwrapped!(Multiplicative Additive BitShift Relational Equality BitAND BitXOR BitOR LogicalAND LogicalOR);
        }
        curr_expr = Some(expr);
    }
    let Some(curr_expr) = curr_expr else { unreachable!() };
    Ok((index, curr_expr))
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
    let mut flattened = parser::Flattened::new();
    let (_, curr_expr) = parse_expressions(tokens, 0, &mut flattened, str_maps)?;
    Ok(recursive_eval(
        &curr_expr,
        str_maps,
        flattened.expressions.as_slice(),
    )?)
}
fn recursive_eval(
    expr: &Expr,
    str_maps: &mut lexer::ByteVecMaps,
    expressions: &[Expr],
) -> Result<i128, String> {
    match expr {
        Expr::Primary(p) => {
            match p {
                Some(PrimaryInner::Expr(e)) => {
                    recursive_eval(&expressions[*e], str_maps, expressions)
                }
                Some(PrimaryInner::Token(t)) => {
                    assert!(matches!(
                        t,
                        lexer::Token::CONSTANT_DEC_INT { .. } | lexer::Token::CONSTANT_CHAR(_)
                    ));
                    match t {
                        lexer::Token::CONSTANT_DEC_INT { value_key, suffix } => {
                            // "For the purposes of this token conversion and evaluation,
                            // all signed integer types and all unsigned integer types act as if they have the same representation
                            // as, respectively, the types intmax_t and uintmax_t defined in the header <stdint.h>."
                            //
                            // We just 'cheat' by using i128 integer types. That way, regardless
                            // whether we get u64 (uintmax_t) or i64 (intmax_t), we can still
                            // compare and not have to do any weird casts.
                            // TODO: add overflow checks...
                            let value = &str_maps.key_to_byte_vec[*value_key];
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
                        lexer::Token::IDENT(_) => {
                            return Err(format!("identifier is not a constant"))
                        }
                        _ => unreachable!(),
                    }
                }
                None => unreachable!(),
            }
        }
        Expr::PostFix(_) => {
            return Err(format!(
                "postfix expressions aren't allowed in preprocessing directives"
            ));
        }
        Expr::Unary(u) => {
            let Some(first) = u.first else { unreachable!() };
            match u.op {
                UnaryOp::Add => Ok(recursive_eval(&expressions[first], str_maps, expressions)?),
                UnaryOp::Sub => Ok(-recursive_eval(&expressions[first], str_maps, expressions)?),
                UnaryOp::BitNOT => Ok(!recursive_eval(&expressions[first], str_maps, expressions)?),
                UnaryOp::LogicalNOT => Ok(
                    if recursive_eval(&expressions[first], str_maps, expressions)? == 0 {
                        1
                    } else {
                        0
                    },
                ),
                UnaryOp::Ampersand
                | UnaryOp::Deref
                | UnaryOp::Increment
                | UnaryOp::Sizeof
                | UnaryOp::AlignOf => {
                    unreachable!()
                }
            }
        }
        Expr::Cast(_) => {
            return Err(format!(
                "cast expressions aren't allowed in preprocessing directives"
            ));
        }
        Expr::Multiplicative(m) => {
            let Some(first) = m.first else { unreachable!() };
            let Some(second) = m.second else { unreachable!() };
            match m.op {
                MultiplicativeOps::Mult => {
                    Ok(recursive_eval(&expressions[first], str_maps, expressions)?
                        * recursive_eval(&expressions[second], str_maps, expressions)?)
                }
                MultiplicativeOps::Div | MultiplicativeOps::Mod => {
                    let right = recursive_eval(&expressions[second], str_maps, expressions)?;
                    if right == 0 {
                        return Err(String::from("cannot divide by zero"));
                    }
                    match m.op {
                        MultiplicativeOps::Div => {
                            Ok(recursive_eval(&expressions[first], str_maps, expressions)? / right)
                        }
                        MultiplicativeOps::Mod => {
                            Ok(recursive_eval(&expressions[first], str_maps, expressions)? % right)
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }
        Expr::Additive(a) => {
            let Some(left) = a.first else { unreachable!() };
            let Some(right) = a.second else { unreachable!() };
            match a.op {
                AdditiveOps::Add => Ok(recursive_eval(&expressions[left], str_maps, expressions)?
                    + recursive_eval(&expressions[right], str_maps, expressions)?),
                AdditiveOps::Sub => Ok(recursive_eval(&expressions[left], str_maps, expressions)?
                    - recursive_eval(&expressions[right], str_maps, expressions)?),
            }
        }
        Expr::BitShift(bs) => {
            let Some(left) = bs.first else { unreachable!() };
            let Some(right) = bs.second else { unreachable!() };
            match bs.op {
                BitShiftOp::Left => Ok(recursive_eval(&expressions[left], str_maps, expressions)?
                    << recursive_eval(&expressions[right], str_maps, expressions)?),
                BitShiftOp::Right => Ok(recursive_eval(&expressions[left], str_maps, expressions)?
                    >> recursive_eval(&expressions[right], str_maps, expressions)?),
            }
        }
        Expr::Relational(r) => {
            let Some(first) = r.first else { unreachable!() };
            let Some(second) = r.second else { unreachable!() };
            match r.op {
                RelationalOp::LessThan => Ok(
                    if recursive_eval(&expressions[first], str_maps, expressions)?
                        < recursive_eval(&expressions[second], str_maps, expressions)?
                    {
                        1
                    } else {
                        0
                    },
                ),
                RelationalOp::LessThanEq => Ok(
                    if recursive_eval(&expressions[first], str_maps, expressions)?
                        <= recursive_eval(&expressions[second], str_maps, expressions)?
                    {
                        1
                    } else {
                        0
                    },
                ),
                RelationalOp::GreaterThan => Ok(
                    if recursive_eval(&expressions[first], str_maps, expressions)?
                        > recursive_eval(&expressions[second], str_maps, expressions)?
                    {
                        1
                    } else {
                        0
                    },
                ),
                RelationalOp::GreaterThanEq => Ok(
                    if recursive_eval(&expressions[first], str_maps, expressions)?
                        >= recursive_eval(&expressions[second], str_maps, expressions)?
                    {
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
                    if recursive_eval(&expressions[first], str_maps, expressions)?
                        == recursive_eval(&expressions[second], str_maps, expressions)?
                    {
                        1
                    } else {
                        0
                    },
                ),
                EqualityOp::NotEqual => Ok(
                    if recursive_eval(&expressions[first], str_maps, expressions)?
                        != recursive_eval(&expressions[second], str_maps, expressions)?
                    {
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
                if (recursive_eval(&expressions[first], str_maps, expressions)?
                    & recursive_eval(&expressions[second], str_maps, expressions)?)
                    != 0
                {
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
                if (recursive_eval(&expressions[first], str_maps, expressions)?
                    ^ recursive_eval(&expressions[second], str_maps, expressions)?)
                    != 0
                {
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
                if (recursive_eval(&expressions[first], str_maps, expressions)?
                    | recursive_eval(&expressions[second], str_maps, expressions)?)
                    != 0
                {
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
                if recursive_eval(&expressions[first], str_maps, expressions)? != 0
                    && recursive_eval(&expressions[second], str_maps, expressions)? != 0
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
                if recursive_eval(&expressions[first], str_maps, expressions)? != 0
                    || recursive_eval(&expressions[second], str_maps, expressions)? != 0
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
            if recursive_eval(&expressions[first], str_maps, expressions)? != 0 {
                Ok(recursive_eval(&expressions[second], str_maps, expressions)?)
            } else {
                Ok(recursive_eval(&expressions[third], str_maps, expressions)?)
            }
        }
        _ => unreachable!(),
    }
}
#[cfg(test)]
mod tests {
    use crate::parser::expressions;
    use crate::{lexer, parser};

    #[test]
    fn eval_expression_test_empty() -> Result<(), String> {
        let src = r##""##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
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
            let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, false, "(1 + 1) * 0");
        }
        {
            let src = r##"1 + (1 * 0)"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, true, "1 + (1 * 0)");
        }
        {
            let src = r##"((1 + 1) * 0)"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, false, "((1 + 1) * 0)");
        }
        {
            let src = r##"((((1))))"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, true, "((((1))))");
        }
        {
            let src = r##"((((1)))))"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps);
            match res {
                Err(_) => {}
                Ok(_) => return Err(String::from("unbalanced parentheses not caught")),
            }
        }
        {
            let src = r##"(((((1))))"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps);
            match res {
                Err(_) => {}
                Ok(_) => return Err(String::from("unbalanced parentheses not caught")),
            }
        }
        {
            let src = r##"0 - (1 + 1)"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, true, "0 - (1 + 1)");
        }
        {
            let src = r##"1"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, true, "1");
        }
        {
            let src = r##"'1'"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, true, "'1'");
        }
        Ok(())
    }
    #[test]
    fn eval_expression_test_unary() -> Result<(), String> {
        let src = r##"!1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false, "!1");
        let src = r##"!0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "!0");
        let src = r##"~0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "~0");
        let src = r##"~~~0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "~~~0");
        let src = r##"~~~~0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false, "~~~~0");
        let src = r##"--------------1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps);
        assert!(
            res.is_err(),
            "'--' operator not caught in cpp constant expression"
        );
        Ok(())
    }
    #[test]
    fn eval_expression_test_multiplicative() -> Result<(), String> {
        let src = r##"1 * 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 * !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 / 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 / 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps);
        match res {
            Err(_) => {}
            Ok(_) => return Err("division by zero not caught".to_string()),
        }
        let src = r##"1 + 1 * 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 * 1 + 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_additive() -> Result<(), String> {
        let src = r##"1 + 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 - 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"0 - 1 + 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false, "0 - 1 + 1");
        let src = r##"0 - 1 + !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "0 - 1 + !1");
        {
            let src = r##"'1' - '1'"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, false, "'1' - '1'");
        }
        {
            let src = r##"'2' - '1'"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, true, "'1' - '1'");
        }
        Ok(())
    }
    #[test]
    fn eval_expression_test_bitshift() -> Result<(), String> {
        let src = r##"1 << 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 >> 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 >> !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        Ok(())
    }
    #[test]
    fn eval_expression_test_relational() -> Result<(), String> {
        let src = r##"1 < 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 < 2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 < !2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 <= 2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"2 <= 2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 > 2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 > 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 >= 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 >= 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        Ok(())
    }
    #[test]
    fn eval_expression_test_equality() -> Result<(), String> {
        let src = r##"1 == 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 != 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 != !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 != 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        Ok(())
    }
    #[test]
    fn eval_expression_test_bit_and() -> Result<(), String> {
        let src = r##"1 & 0 == 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 & 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 & !0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 & 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 == 0 & 1 == 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_bit_xor() -> Result<(), String> {
        let src = r##"1 ^ 0 == 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false, "1 ^ 0 == 0");
        let src = r##"(1 ^ !0) == 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "(1 ^ !0) == 0");
        Ok(())
    }
    #[test]
    fn eval_expression_test_bit_or() -> Result<(), String> {
        let src = r##"1 | 0 == 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 | !0 == 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "1 | !0 == 0");
        Ok(())
    }
    #[test]
    fn eval_expression_test_logical_and() -> Result<(), String> {
        let src = r##"1 && 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 && 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 && !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_logical_or() -> Result<(), String> {
        let src = r##"1 || 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 || 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 || 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_conditional() -> Result<(), String> {
        let src = r##"1 ? 1 : 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"(1 + 1 == 3) ? 1 : 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"~0 ? (1 + 1 == 2) : 0 * 4"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 ? 0 : 1 * 4"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 ? 0 : !(1 * 4)"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        Ok(())
    }
    #[test]
    fn parse_expressions_test_cast() -> Result<(), String> {
        let src = r#"(int)1"#.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
        let mut flattened = parser::Flattened::new();
        let (_, cast_expr) =
            expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
        assert!(matches!(cast_expr, expressions::Expr::Cast(_)));
        let expressions::Expr::Cast(c) = cast_expr else { unreachable!() };
        assert!(matches!(
            flattened.expressions[c.cast_expr.unwrap()],
            expressions::Expr::Primary(_)
        ));
        let expressions::Expr::Primary(Some(expressions::PrimaryInner::Token(t))) =
            flattened.expressions[c.cast_expr.unwrap()] else { unreachable!() };
        assert!(matches!(t, lexer::Token::CONSTANT_DEC_INT { .. }));
        let lexer::Token::CONSTANT_DEC_INT { value_key, suffix }  = t else { unreachable!() };
        assert!(str_maps.key_to_byte_vec[value_key] == *b"1");
        Ok(())
    }
    #[test]
    fn parse_expressions_test_additive_cast() -> Result<(), String> {
        let src = r#"1 + (int)1"#.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
        let mut flattened = parser::Flattened::new();
        let (_, add) = expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
        assert!(matches!(add, expressions::Expr::Additive(_)));
        let expressions::Expr::Additive(a) = add else { unreachable!() };
        let Some(first_idx) = a.first else { unreachable!() };
        let Some(second_idx) = a.second else { unreachable!() };
        assert!(matches!(
            flattened.expressions[first_idx],
            expressions::Expr::Primary(_)
        ));
        assert!(matches!(
            flattened.expressions[second_idx],
            expressions::Expr::Cast(_)
        ));
        let expressions::Expr::Primary(Some(expressions::PrimaryInner::Token(t))) =
            flattened.expressions[first_idx] else { unreachable!() };
        let expressions::Expr::Cast(c) =
            flattened.expressions[second_idx] else { unreachable!() };
        assert!(matches!(t, lexer::Token::CONSTANT_DEC_INT { .. }));
        let lexer::Token::CONSTANT_DEC_INT { value_key, suffix }  = t else { unreachable!() };
        assert!(str_maps.key_to_byte_vec[value_key] == *b"1");
        assert!(matches!(a.op, expressions::AdditiveOps::Add));
        let expressions::Expr::Cast(c) =
            flattened.expressions[second_idx] else { unreachable!() };
        let Some(c_idx) = c.cast_expr else { unreachable!() };
        assert!(matches!(
            flattened.expressions[c_idx],
            expressions::Expr::Primary(_)
        ));
        let expressions::Expr::Primary(Some(expressions::PrimaryInner::Token(t))) =
            flattened.expressions[c_idx] else { unreachable!() };
        assert!(matches!(t, lexer::Token::CONSTANT_DEC_INT { .. }));
        let lexer::Token::CONSTANT_DEC_INT { value_key, suffix } = t else { unreachable!() };
        assert!(str_maps.key_to_byte_vec[value_key] == *b"1");
        Ok(())
    }
    #[test]
    fn parse_expressions_test_additive_unary_cast() -> Result<(), String> {
        let src = r#"1 + !(int)1"#.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
        let mut flattened = parser::Flattened::new();
        let (_, add) = expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
        assert!(matches!(add, expressions::Expr::Additive(_)));
        let expressions::Expr::Additive(a) = add else { unreachable!() };
        let Some(first_idx) = a.first else { unreachable!() };
        assert!(matches!(
            flattened.expressions[first_idx],
            expressions::Expr::Primary(_)
        ));
        let expressions::Expr::Primary(Some(expressions::PrimaryInner::Token(t))) =
            flattened.expressions[first_idx] else { unreachable!() };
        assert!(matches!(t, lexer::Token::CONSTANT_DEC_INT { .. }));
        let lexer::Token::CONSTANT_DEC_INT { value_key, suffix }  = t else { unreachable!() };
        assert!(str_maps.key_to_byte_vec[value_key] == *b"1");
        let Some(second_idx) = a.second else { unreachable!() };
        assert!(matches!(
            flattened.expressions[second_idx],
            expressions::Expr::Unary(_)
        ));
        let expressions::Expr::Unary(u) = flattened.expressions[second_idx] else { unreachable!() };
        assert!(matches!(u.op, expressions::UnaryOp::LogicalNOT));
        let Some(cast_idx) = u.first else { unreachable!() };
        assert!(matches!(
            flattened.expressions[cast_idx],
            expressions::Expr::Cast(_)
        ));
        let expressions::Expr::Cast(c) = flattened.expressions[cast_idx] else { unreachable!() };
        let Some(p_idx) = c.cast_expr else { unreachable!() };
        assert!(matches!(
            flattened.expressions[p_idx],
            expressions::Expr::Primary(_)
        ));
        let expressions::Expr::Primary(Some(expressions::PrimaryInner::Token(t))) =
            flattened.expressions[p_idx] else { unreachable!() };
        assert!(matches!(t, lexer::Token::CONSTANT_DEC_INT { .. }));
        let lexer::Token::CONSTANT_DEC_INT { value_key, suffix } = t else { unreachable!() };
        assert!(str_maps.key_to_byte_vec[value_key] == *b"1");
        Ok(())
    }
    #[test]
    fn parse_expressions_test_postfix() -> Result<(), String> {
        {
            let src = r#"hi-- + 1"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let (_, add) =
                expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(add, expressions::Expr::Additive(_)));
            let expressions::Expr::Additive(a) = add else { unreachable!() };
            assert!(matches!(a.op, expressions::AdditiveOps::Add));

            let Some(left_idx) = a.first else { unreachable!() };
            assert!(matches!(
                flattened.expressions[left_idx],
                expressions::Expr::PostFix(_)
            ));
            let expressions::Expr::PostFix(p) = flattened.expressions[left_idx] else { unreachable!() };
            assert!(matches!(
                p,
                expressions::PostFix::WithIncrementDecrement { .. }
            ));
            let expressions::PostFix::WithIncrementDecrement { first, op } = p else { unreachable!() };
            assert!(matches!(
                flattened.expressions[first],
                expressions::Expr::Primary(_)
            ));
            assert!(matches!(
                op,
                expressions::PostFixIncrementDecrement::Decrement
            ));

            let Some(right_idx) = a.second else { unreachable!() };
            assert!(matches!(
                flattened.expressions[right_idx],
                expressions::Expr::Primary(_)
            ));
        }
        Ok(())
    }
    #[test]
    fn parse_expressions_with_unary_postfix() -> Result<(), String> {
        {
            let src = r#"!(hi * 3)-- + -1"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let (_, add) =
                expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(add, expressions::Expr::Additive(_)),);
            let expressions::Expr::Additive(a) = add else { unreachable!() };
            let Some(first_idx) = a.first else { unreachable!() };
            assert!(matches!(
                flattened.expressions[first_idx],
                expressions::Expr::Unary(_)
            ));
        }
        {
            let src = r#"!hi-- + -1"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let (_, add) =
                expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(add, expressions::Expr::Additive(_)),);
            let expressions::Expr::Additive(a) = add else { unreachable!() };
            let Some(first_idx) = a.first else { unreachable!() };
            assert!(matches!(
                flattened.expressions[first_idx],
                expressions::Expr::Unary(_)
            ));
        }
        Ok(())
    }
    #[test]
    fn parse_expressions_post_pointer_member() -> Result<(), String> {
        {
            let src = r#"hi->hi2"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let (_, post) =
                expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
        }
        Ok(())
    }
    #[test]
    fn parse_expressions_simple_assignment_test() -> Result<(), String> {
        {
            let src = r#"hi = hi2"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let (_, assign) =
                expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(assign, expressions::Expr::Assignment(_)));
            let expressions::Expr::Assignment(a) = assign else { unreachable!() };
            let Some(first_idx) = a.first else { unreachable!() };
            assert!(matches!(
                flattened.expressions[first_idx],
                expressions::Expr::Primary(_)
            ));
            let Some(second_idx) = a.second else { unreachable!() };
            assert!(matches!(
                flattened.expressions[second_idx],
                expressions::Expr::Primary(_)
            ));
        }
        Ok(())
    }
    #[test]
    fn parse_expressions_non_unary_left_assignment_test() -> Result<(), String> {
        {
            let src = r#"1 * 1 = hi2"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let assign = expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps);
            assert!(assign.is_err());
        }
        Ok(())
    }
    //#[test]
    //fn parse_expressions_test() -> Result<(), String> {
    //    let src = r##"++(1 + 1);"##.as_bytes();
    //    let mut expressions = Vec::new();
    //    let mut str_maps = lexer::ByteVecMaps::new();
    //    let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
    //    let (new_index, unary_expr) =
    //        parse_expressions(&tokens, 0, &mut expressions, &mut str_maps)?;
    //    match unary_expr {
    //        expressions::Expr::Unary(u) => match u {
    //            expressions::Unary { op, first } => {
    //                assert_eq!(op, expressions::UnaryOp::Increment);
    //                let Some(key) = first else { unreachable!() };
    //                match expressions[key] {
    //                    expressions::Expr::Primary(p) => {
    //                        let Some(pi) = p else { unreachable!() };
    //                        let expressions::PrimaryInner::Expr(e) = pi else { unreachable!() };
    //                    }
    //                    _ => assert!(false),
    //                }
    //            }
    //        },
    //        _ => assert!(false),
    //    }
    //    Ok(())
    //}
}
