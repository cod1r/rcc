use crate::error::*;
use crate::lexer::*;
use crate::parser::declarations::*;
use crate::parser::statements::*;
use crate::parser::Flattened;
use crate::parser::*;

// The parsing functions written are closely modeled after the syntax groups defined in https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2310.pdf
// It was hard for me to decide whether to write the logic where `parse_conditional_expression` will ALWAYS return conditional expressions or to follow the syntax grouping/precedence but
// I just decided that it's just simpler to write things similar to how the syntax is designed. That way I get precedence without having to write a precedence table.

// The original parsing logic had a stack and left and right expressions which made everything super complicated and not maintainable for me

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

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Ampersand,
    Sub,
    Add,
    Deref,
    BitNOT,
    LogicalNOT,
    Increment,
    Decrement,
    Sizeof,
    AlignOf,
}
#[derive(Copy, Clone)]
pub struct Unary {
    pub op: UnaryOp,
    pub first: ExpressionIndex,
}
#[derive(Copy, Clone)]
pub struct Cast {
    type_name: Option<TypeNameIndex>,
    cast_expr: Option<usize>,
}

pub type ArgumentExprListIndex = usize;
#[derive(Copy, Clone)]
pub enum PostFix {
    WithSubscript {
        first: ExpressionIndex,
        subscript: ExpressionIndex,
    },
    WithFunctionCall {
        first: ExpressionIndex,
        argument_expr_idx: ArgumentExprListIndex,
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
        op: TokenType,
    },
    WithTypeNameInitializerList {
        type_name: TypeNameIndex,
        initializer_list: InitializerListIndex,
    },
}

#[derive(Copy, Clone)]
enum BinaryExprType {
    Comma,
    Assignment,
    LogOR,
    LogAND,
    BitOR,
    BitAND,
    BitXOR,
    Eq,
    NotEq,
    LessThan,
    GreaterThan,
    LessThanEq,
    GreaterThanEq,
    BitShiftLeft,
    BitShiftRight,
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    ArgExprList,
}

#[derive(Copy, Clone)]
pub struct Conditional {
    pub first: ExpressionIndex,
    pub second: ExpressionIndex,
    pub third: ExpressionIndex,
}

enum PrimaryType {
    Token(Token),
    Expr(ExpressionIndex),
}

#[derive(Copy, Clone)]
pub enum Expr {
    Binary {
        r#type: BinaryExprType,
        first: Option<ExpressionIndex>,
        second: Option<ExpressionIndex>,
    },
    Conditional(Conditional),
    Unary(Unary),
    Cast(Cast),
    PostFix(PostFix),
    Primary(PrimaryType),
}

impl Expr {
    pub fn priority(&self) -> u8 {
        match self {
            Expr::Binary(b) => match b.r#type {
                Mult | Div | Mod => u8::MAX - 4,
                Add | Sub => u8::MAX - 5,
                BitShiftLeft | BitShiftRight => u8::MAX - 6,
                LessThan | GreaterThan | LessThanEq | GreaterThanEq => u8::MAX - 7,
                NotEq | Eq => u8::MAX - 8,
                BitAND => u8::MAX - 9,
                BitXOR => u8::MAX - 10,
                BitOR => u8::MAX - 11,
                LogAND => u8::MAX - 12,
                LogOR => u8::MAX - 13,
                Assignment => u8::MAX - 15,
                Comma => u8::MAX - 16,
            },
            Expr::Conditional(_) => u8::MAX - 14,
            Expr::Unary(_) => u8::MAX - 2,
            Expr::PostFix(_) => u8::MAX - 1,
            Expr::Comma(_) => u8::MAX - 16,
            Expr::Primary(_) => u8::MAX,
            Expr::Cast(_) => u8::MAX - 3,
        }
    }
}

fn match_right_and_do_operation(a: Expr) {
    match right {
        // Primary has the highest priority
        Expr::Primary(p) => {
            assert!(p.is_some());
            assert!($a.second.is_none());
        }
        Expr::PostFix(p) => match p {
            PostFix::WithSubscript { first, .. } => {
                *first = a.second;
            }
            PostFix::WithFunctionCall { first, .. } => {
                *first = a.second;
            }
            PostFix::WithMember { first, .. } => {
                *first = a.second;
            }
            PostFix::WithPointerToMember { first, .. } => {
                *first = a.second;
            }
            _ => todo!(),
        },
        Expr::Unary(_u) => {
            assert!(a.second.is_none());
        }
        Expr::Cast(_) => todo!(),
        Expr::Binary { first, second, .. } => {
            assert!(i.first.is_none());
            right.first = a.second;
        }
        _ => unreachable!(),
    }
}
// right expr has a higher priority so it takes the previous expr's
// right operand or if there isn't a right operand, it takes the only operand
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
        Expr::Binary { .. } => {
            match_right_and_do_operation(left);
        }
        Expr::Conditional(c) => {
            assert!(c.first.is_some());
            assert!(c.second.is_some());
            match right {
                Expr::Primary(p) => {
                    assert!(p.is_some());
                    assert!($c.third.is_none());
                }
                Expr::Unary(_u) => {
                    assert!($c.third.is_none());
                }
                Expr::Binary { first, .. } => {
                    assert!(first.is_none());
                    right.first = c.third;
                }
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}

fn left_has_higher_eq_priority(left: usize, right: &mut Expr) {
    let index = Some(left);
    match right {
        Expr::PostFix(p) => match p {
            PostFix::WithSubscript { first, .. } => {
                *first = index;
            }
            PostFix::WithFunctionCall { first, .. } => {
                *first = index;
            }
            PostFix::WithMember { first, .. } => {
                *first = index;
            }
            PostFix::WithPointerToMember { first, .. } => {
                *first = index;
            }
            _ => todo!(),
        },
        Expr::Primary(Some(ref mut p)) => {
            *p = PrimaryInner::new_p_expr(left);
        }
        Expr::Binary { first, second, .. } => {
            *first = index;
        }
        _ => unreachable!(),
    }
}
macro_rules! expression_operators {
    () => {
        TokenType::PUNCT_PLUS
            | TokenType::PUNCT_MINUS
            | TokenType::PUNCT_MULT
            | TokenType::PUNCT_DIV
            | TokenType::PUNCT_MODULO
            | TokenType::PUNCT_BITSHIFT_LEFT
            | TokenType::PUNCT_BITSHIFT_RIGHT
            | TokenType::PUNCT_LESS_THAN
            | TokenType::PUNCT_LESS_THAN_EQ
            | TokenType::PUNCT_GREATER_THAN
            | TokenType::PUNCT_GREATER_THAN_EQ
            | TokenType::PUNCT_EQ_BOOL
            | TokenType::PUNCT_NOT_EQ_BOOL
            | TokenType::PUNCT_AND_BIT
            | TokenType::PUNCT_XOR_BIT
            | TokenType::PUNCT_OR_BIT
            | TokenType::PUNCT_AND_BOOL
            | TokenType::PUNCT_OR_BOOL
            | TokenType::PUNCT_CLOSE_PAR
            | TokenType::PUNCT_QUESTION_MARK
            | TokenType::PUNCT_COLON
            | TokenType::PUNCT_ASSIGNMENT
            | TokenType::PUNCT_MULT_ASSIGN
            | TokenType::PUNCT_DIV_ASSIGN
            | TokenType::PUNCT_MODULO_ASSIGN
            | TokenType::PUNCT_ADD_ASSIGN
            | TokenType::PUNCT_SUB_ASSIGN
            | TokenType::PUNCT_L_SHIFT_BIT_ASSIGN
            | TokenType::PUNCT_R_SHIFT_BIT_ASSIGN
            | TokenType::PUNCT_AND_BIT_ASSIGN
            | TokenType::PUNCT_XOR_BIT_ASSIGN
            | TokenType::PUNCT_OR_BIT_ASSIGN
            | TokenType::PUNCT_INCREMENT
            | TokenType::PUNCT_DECREMENT
            | TokenType::PUNCT_DOT
            | TokenType::PUNCT_ARROW
    };
}

macro_rules! primary_tokens {
    () => {
        TokenType::IDENT { .. }
            | TokenType::StringLiteral { .. }
            | TokenType::CONSTANT_DEC_INT { .. }
            | TokenType::CONSTANT_HEXA_INT { .. }
            | TokenType::CONSTANT_DEC_FLOAT { .. }
            | TokenType::CONSTANT_HEXA_FLOAT { .. }
            | TokenType::CONSTANT_CHAR { .. }
            | TokenType::CONSTANT_OCTAL_INT { .. }
            | TokenType::CONSTANT_ENUM { .. }
    };
}

macro_rules! unary_tokens {
    () => {
        TokenType::PUNCT_INCREMENT
            | TokenType::PUNCT_DECREMENT
            | TokenType::KEYWORD_SIZEOF
            | TokenType::KEYWORD__ALIGNOF
    };
}

macro_rules! unary_ops {
    () => {
        TokenType::PUNCT_AND_BIT
            | TokenType::PUNCT_MULT
            | TokenType::PUNCT_ADD
            | TokenType::PUNCT_MINUS
            | TokenType::PUNCT_TILDE
            | TokenType::PUNCT_NOT_BOOL
    };
}

macro_rules! assignment_ops {
    () => {
        TokenType::PUNCT_ASSIGNMENT
            | TokenType::PUNCT_MODULO_ASSIGN
            | TokenType::PUNCT_DIV_ASSIGN
            | TokenType::PUNCT_MULT_ASSIGN
            | TokenType::PUNCT_ADD_ASSIGN
            | TokenType::PUNCT_SUB_ASSIGN
            | TokenType::PUNCT_L_SHIFT_BIT_ASSIGN
            | TokenType::PUNCT_R_SHIFT_BIT_ASSIGN
            | TokenType::PUNCT_AND_BIT_ASSIGN
            | TokenType::PUNCT_XOR_BIT_ASSIGN
            | TokenType::PUNCT_OR_BIT_ASSIGN
    };
}

fn handle_increment_decrement_binary_expr(
    expr_struct: Expr,
    tokens: &[Token],
    index: usize,
    stack: &mut Vec<Expr>,
    curr_expr: &mut Option<Expr>,
    flattened: &mut Flattened,
) {
    // previous expression has a second operand, which
    // means the second operand is actually a postfix
    // expression, due to the current operator
    // occurring after.
    if let Some(Expr::Binary { .. }) = expr_struct.second {
        flattened
            .expressions
            .push(Expr::PostFix(PostFix::WithIncrementDecrement {
                first: expr_struct,
                op: match tokens.get(*index) {
                    Some(Token {
                        r#type: TokenType::PUNCT_INCREMENT,
                        ..
                    }) => PostFixIncrementDecrement::Increment,
                    Some(Token {
                        r#type: TokenType::PUNCT_DECREMENT,
                        ..
                    }) => PostFixIncrementDecrement::Decrement,
                    _ => unreachable!(),
                },
            }));
        expr_struct.second = Some(flattened.expressions.len() - 1);
    } else {
        // previous expression does not have a second
        // operand which means the second operand is
        // actually a unary expression
        stack.push(expr_struct);
        *curr_expr = Some(Expr::Unary(Unary {
            first: None,
            op: match tokens.get(index) {
                Some(Token {
                    r#type: TokenType::PUNCT_INCREMENT,
                    ..
                }) => UnaryOp::Increment,
                Some(Token {
                    r#type: TokenType::PUNCT_DECREMENT,
                    ..
                }) => UnaryOp::Decrement,
                _ => unreachable!(),
            },
        }));
    }
}

fn parse_increment_decrement(
    tokens: &[Token],
    index: usize,
    curr_expr: &mut Option<Expr>,
    stack: &mut Vec<Expr>,
    flattened: &mut Flattened,
) -> Result<(), String> {
    match curr_expr {
        Some(Expr::Conditional(_)) => todo!(),
        Some(Expr::Cast(ref mut c)) => {
            if let Some(cast_expr_key) = c.cast_expr {
                flattened
                    .expressions
                    .push(Expr::PostFix(PostFix::WithIncrementDecrement {
                        first: cast_expr_key,
                        op: match tokens.get(index) {
                            Some(Token {
                                r#type: TokenType::PUNCT_INCREMENT,
                                ..
                            }) => PostFixIncrementDecrement::Increment,
                            Some(Token {
                                r#type: TokenType::PUNCT_DECREMENT,
                                ..
                            }) => PostFixIncrementDecrement::Decrement,
                            _ => unreachable!(),
                        },
                    }));
                c.cast_expr = Some(flattened.expressions.len() - 1);
            } else {
                // cast expression has a None for it's first
                // expression which means we have a unary
                // expression
                stack.push(curr_expr.unwrap());
                *curr_expr = Some(Expr::Unary(Unary {
                    first: None,
                    op: match tokens.get(index) {
                        Some(Token {
                            r#type: TokenType::PUNCT_INCREMENT,
                            ..
                        }) => UnaryOp::Increment,
                        Some(Token {
                            r#type: TokenType::PUNCT_DECREMENT,
                            ..
                        }) => UnaryOp::Decrement,
                        _ => unreachable!(),
                    },
                }));
            }
        }
        Some(Expr::Unary(ref mut u)) => {
            if let Some(first_key) = u.first {
                flattened
                    .expressions
                    .push(Expr::PostFix(PostFix::WithIncrementDecrement {
                        first: first_key,
                        op: match tokens.get(index) {
                            Some(Token {
                                r#type: TokenType::PUNCT_INCREMENT,
                                ..
                            }) => PostFixIncrementDecrement::Increment,
                            Some(Token {
                                r#type: TokenType::PUNCT_DECREMENT,
                                ..
                            }) => PostFixIncrementDecrement::Decrement,
                            _ => unreachable!(),
                        },
                    }));
                u.first = Some(flattened.expressions.len() - 1);
            } else {
                // technically unary op after a unary op isn't
                // allowed because unary ops can only be applied to
                // modifiable l-values but that's up to the
                // semantic analyzer to handle
                stack.push(curr_expr.unwrap());
                *curr_expr = Some(Expr::Unary(Unary {
                    first: None,
                    op: match tokens.get(index) {
                        Some(Token {
                            r#type: TokenType::PUNCT_INCREMENT,
                            ..
                        }) => UnaryOp::Increment,
                        Some(Token {
                            r#type: TokenType::PUNCT_DECREMENT,
                            ..
                        }) => UnaryOp::Decrement,
                        _ => unreachable!(),
                    },
                }));
            }
        }
        Some(Expr::PostFix(_) | Expr::Primary(_)) => {
            flattened.expressions.push(curr_expr.unwrap());
            *curr_expr = Some(Expr::PostFix(PostFix::WithIncrementDecrement {
                first: flattened.expressions.len() - 1,
                op: match tokens.get(index) {
                    Some(Token {
                        r#type: TokenType::PUNCT_INCREMENT,
                        ..
                    }) => PostFixIncrementDecrement::Increment,
                    Some(Token {
                        r#type: TokenType::PUNCT_DECREMENT,
                        ..
                    }) => PostFixIncrementDecrement::Decrement,
                    _ => unreachable!(),
                },
            }));
        }
        Some(Expr::Binary { .. }) => {
            handle_increment_decrement_binary_expr(
                curr_expr.unwrap(),
                tokens,
                index,
                stack,
                curr_expr,
                flattened,
            );
        }
        None => {
            *curr_expr = Some(Expr::Unary(Unary {
                first: None,
                op: match tokens.get(index) {
                    Some(Token {
                        r#type: TokenType::PUNCT_INCREMENT,
                        ..
                    }) => UnaryOp::Increment,
                    Some(Token {
                        r#type: TokenType::PUNCT_DECREMENT,
                        ..
                    }) => UnaryOp::Decrement,
                    _ => unreachable!(),
                },
            }));
        }
    }
    Ok(())
}

fn parse_primary_expression(tokens: &[Token], index: &mut usize) -> Result<Expr, String> {
    if !matches(tokens.get(*index), Some(primary_tokens!())) {
        match tokens.get(*index) {
            Some(Token { line, column, .. }) => {
                return Err(error("Expected primary token", line, column));
            }
            None => {
                return Err("Expected primary token".to_string());
            }
        }
    }
    Primary(PrimaryType::Token(tokens[*index]))
}

fn parse_argument_expression_list(tokens: &[Token], index: &mut usize) -> Result<Expr, String> {
    todo!();
    match tokens.get(*index) {
        Some(Token {
            r#type: TokenType::PUNCT_COMMA,
            ..
        }) => {}
        _ => break,
    }
    *index += 1;
}

fn parse_postfix_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &ByteVecMaps,
    last_postfix_expression: Option<Expr>,
) -> Result<Expr, String> {
    match tokens.get(*index) {
        Some(Token {
            r#type: TokenType::PUNCT_OPEN_PAR,
            ..
        }) => {
            *index += 1;
            if last_postfix_expression.is_none() {
                // typename and initializer postfix expression
                let type_name = parse_type_names(tokens, index, flattened, str_maps)?;
                consume_whitespace(tokens, index);
                expected_token(tokens, index, str_maps, TokenType::PUNCT_CLOSE_PAR)?;
                consume_whitespace(tokens, index);
                expected_token(tokens, index, str_maps, TokenType::PUNCT_OPEN_CURLY)?;
                let initializer_list = parse_initializer_list(tokens, index, flattened, str_maps)?;
                return Ok(Expr::PostFix(PostFix::WithTypeNameInitializerList {
                    type_name,
                    initializer_list,
                }));
            } else {
                // postfix argument expression list expression
                parse_argument_expression_list(tokens, index)?;
            }
        }
        Some(
            t @ Token {
                r#type:
                    TokenType::PUNCT_INCREMENT
                    | TokenType::PUNCT_DECREMENT
                    | TokenType::PUNCT_ARROW
                    | TokenType::PUNCT_DOT,
                line,
                column,
            },
        ) => {
            *index += 1;
            if last_postfix_expression.is_none() {
                return Err(error("Expected expression before", line, column));
            }
            let Token { r#type, .. } = t;
            flattened.expressions.push(last_postfix_expression.unwrap());
            return Ok(Expr::PostFix(PostFix::WithIncrementDecrement {
                op: r#type,
                first: flattened.expressions.len() - 1,
            }));
        }
        _ => {
            break;
        }
    }
}

fn parse_unary_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &ByteVecMaps,
) -> Result<Expr, String> {
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type: unary_ops!(),
                ..
            },
        ) => {
            let cast_expr = parse_cast_expression(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(cast_expr);
            Ok(Expr::Unary(Unary {
                op: t.r#type,
                first: flattened.expressions.len() - 1,
            }))
        }
    }
}

fn parse_cast_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &ByteVecMaps,
) -> Result<Expr, String> {
    match tokens.get(*index) {
        Some(Token {
            r#type: TokenType::PUNCT_OPEN_PAR,
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            let type_name = parse_type_names(tokens, index, flattened, str_maps)?;
            expected_token(tokens, index, TokenType::PUNCT_CLOSE_PAR)?;
            consume_whitespace(tokens, index);
            let cast_expression = parse_cast_expression(tokens, index, flattened, str_maps)?;
            flattened.type_names.push(type_name);
            flattened.expressions.push(cast_expression);
            Ok(Expr::Cast(Cast {
                type_name: flattened.type_names.len() - 1,
                cast_expression: flattened.expressions.len() - 1,
            }))
        }
        _ => parse_unary_expression(tokens, index, flattened, str_maps)?,
    }
}

fn parse_multiplicative_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &ByteVecMaps,
) -> Result<Expr, String> {
    let cast_expr = parse_cast_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(Token {
            r#type: TokenType::PUNCT_MULT | TokenType::PUNCT_DIV | TokenType::PUNCT_MOD,
            ..
        }) => {
            *index += 1;
            let second_operand = parse_cast_expression(tokens, index, flattenex, str_maps)?;
            flattened.expressions.push(cast_expr);
            let first = flattened.expressions.len() - 1;
            flattened.expressions.push(second_operand);
            let second = flattened.expressions.len() - 1;
            Ok(Expr::Binary {
                r#type: BinaryExprType::Mult,
                first,
                second,
            })
        }
        _ => Ok(cast_expr),
    }
}

fn parse_additive_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &ByteVecMaps,
) -> Result<Expr, String> {
    let mult_expr = parse_multiplicative_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type: TokenType::PUNCT_PLUS | TokenType::PUNCT_MINUS,
                ..
            },
        ) => {
            *index += 1;
            let second_operand =
                parse_multiplicative_expression(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(mult_expr);
            let first = flattened.expressions.len() - 1;
            flattened.expressions.push(second_operand);
            let second = flattened.expressions.len() - 1;
            Ok(Expr::Binary {
                r#type: t.r#type,
                first,
                second,
            })
        }
        _ => Ok(mult_expr),
    }
}

fn parse_shift_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &ByteVecMaps,
) -> Result<Expr, String> {
    let add_expr = parse_additive_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type: TokenType::PUNCT_BITSHIFT_LEFT | TokenType::PUNCT_BITSHIFT_RIGHT,
                ..
            },
        ) => {
            *index += 1;
            let second_operand = parse_additive_expression(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(add_expr);
            let first = flattened.expressions.len() - 1;
            flattened.expressions.push(second_operand);
            let second = flattened.expressions.len() - 1;
            Ok(Expr::Binary {
                r#type: t.r#type,
                first,
                second,
            })
        }
        _ => Ok(add_expr),
    }
}

fn parse_relational_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &ByteVecMaps,
) -> Result<Expr, String> {
    let shift_expr = parse_shift_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type:
                    TokenType::PUNCT_LESS_THAN
                    | TokenType::PUNCT_LESS_THAN_EQ
                    | TokenType::PUNCT_GREATER_THAN
                    | TokenType::PUNCT_GREATER_THAN_EQ,
                ..
            },
        ) => {
            *index += 1;
            let second_operand = parse_shift_expression(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(shift_expr);
            let first = flattened.expressions.len() - 1;
            flattened.expressions.push(second_operand);
            let second = flattened.expressions.len() - 1;
            Ok(Expr::Binary {
                r#type: t.r#type,
                first,
                second,
            })
        }
        _ => Ok(shift_expr),
    }
}

fn parse_equality_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &ByteVecMaps,
) -> Result<Expr, String> {
    let relational_expr = parse_relational_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type: TokenType::PUNCT_NOT_EQ_BOOL | TokenType::PUNCT_EQ_BOOL,
                ..
            },
        ) => {
            *index += 1;
            let second_operand = parse_relational_expression(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(relational_expr);
            let first = flattened.expressions.len() - 1;
            flattened.expressions.push(second_operand);
            let second = flattened.expressions.len() - 1;
            Ok(Expr::Binary {
                r#type: t.r#type,
                first,
                second,
            })
        }
        _ => Ok(relational_expr),
    }
}

fn parse_bitwise_AND_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &ByteVecMaps,
) -> Result<Expr, String> {
    let eq_expr = parse_equality_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type: TokenType::PUNCT_AND_BIT,
                ..
            },
        ) => {
            *index += 1;
            let second_operand = parse_equality_expression(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(eq_expr);
            let first = flattened.expressions.len() - 1;
            flattened.expressions.push(second_operand);
            let second = flattened.expressions.len() - 1;
            Ok(Expr::Binary {
                r#type: t.r#type,
                first,
                second,
            })
        }
        _ => Ok(eq_expr),
    }
}

fn parse_bitwise_XOR_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &ByteVecMaps,
) -> Result<Expr, String> {
    let bit_and_expr = parse_bitwise_AND_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type: TokenType::PUNCT_XOR_BIT,
                ..
            },
        ) => {
            *index += 1;
            let second_operand = parse_bitwise_AND_expression(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(bit_and_expr);
            let first = flattened.expressions.len() - 1;
            flattened.expressions.push(second_operand);
            let second = flattened.expressions.len() - 1;
            Ok(Expr::Binary {
                r#type: t.r#type,
                first,
                second,
            })
        }
        _ => Ok(bit_and_expr),
    }
}

fn parse_bitwise_OR_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &ByteVecMaps,
) -> Result<Expr, String> {
    let bit_xor_expr = parse_bitwise_XOR_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type: TokenType::PUNCT_OR_BIT,
                ..
            },
        ) => {
            *index += 1;
            let second_operand = parse_bitwise_XOR_expression(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(bit_xor_expr);
            let first = flattened.expressions.len() - 1;
            flattened.expressions.push(second_operand);
            let second = flattened.expressions.len() - 1;
            Ok(Expr::Binary {
                r#type: t.r#type,
                first,
                second,
            })
        }
        _ => Ok(bit_xor_expr),
    }
}

fn parse_logical_AND_expression() -> Result<(), String> {
    let bit_or_expr = parse_bitwise_OR_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type: TokenType::PUNCT_OR_BIT,
                ..
            },
        ) => {
            *index += 1;
            let second_operand = parse_bitwise_OR_expression(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(bit_or_expr);
            let first = flattened.expressions.len() - 1;
            flattened.expressions.push(second_operand);
            let second = flattened.expressions.len() - 1;
            Ok(Expr::Binary {
                r#type: t.r#type,
                first,
                second,
            })
        }
        _ => Ok(bit_or_expr),
    }
}

fn parse_logical_OR_expression() -> Result<(), String> {
    let logical_and_expr = parse_logical_AND_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type: TokenType::PUNCT_OR_BIT,
                ..
            },
        ) => {
            *index += 1;
            let second_operand = parse_logical_AND_expression(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(logical_and_expr);
            let first = flattened.expressions.len() - 1;
            flattened.expressions.push(second_operand);
            let second = flattened.expressions.len() - 1;
            Ok(Expr::Binary {
                r#type: t.r#type,
                first,
                second,
            })
        }
        _ => Ok(logical_and_expr),
    }
}

fn parse_conditional_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &ByteVecMaps,
) -> Result<Expr, String> {
    let first_expr = parse_logical_OR_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    if matches!(
        tokens.get(*index),
        Some(Token {
            r#type: TokenType::PUNCT_QUESTION_PARK..
        })
    ) {
        flattened.expressions.push(first_expr);
        let first_idx = flattened.expressions.len() - 1;

        *index += 1;
        consume_whitespace(tokens, index);

        let expression = parse_expressions(tokens, index, flattened, str_maps)?;
        flattened.expressions.push(expression);
        let second_idx = flattened.expressions.len() - 1;

        consume_whitespace(tokens, index);
        expected_token(tokens, index, TokenType::PUNCT_COLON)?;
        consume_whitespace(tokens, index);

        let conditional = parse_conditional_expression(tokens, index, flattened, str_maps)?;
        flattened.expressions.push(conditional);
        let third_idx = flattened.expressions.len() - 1;

        return Ok(Expr::Conditional(Conditional {
            first: first_idx,
            second: second_idx,
            third: third_idx,
        }));
    }

    Ok(first_expr)
}

pub fn parse_assignment_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &ByteVecMaps,
) -> Result<Expr, String> {
    let conditional = parse_conditional_expression(tokens, index, flattened, str_maps)?;
    if matches!(
        conditional,
        Expr::Unary(_) | Expr::PostFix(_) | Expr::Primary(_)
    ) {
        consume_whitespace(tokens, index);
        match tokens.get(*index) {
            Some(
                t @ Token {
                    r#type: assignment_ops!(),
                    ..
                },
            ) => {
                *index += 1;
                // this basically attempts to transform `var += 1` to `var = var + 1`
                let assignment_expr =
                    parse_assignment_expression(tokens, index, flattened, str_maps)?;
                flattened.expressions.push(conditional);
                let first = flattened.expressions.len() - 1;
                flattened.expressions.push(assignment_expr);
                let second = flattened.expressions.len() - 1;
                let transformed = Expr::Binary {
                    r#type: match t.r#type {
                        TokenType::PUNCT_ASSIGNMENT => BinaryExprType::Assignment,
                        TokenType::PUNCT_MODULO_ASSIGN => BinaryExprType::Mod,
                        TokenType::PUNCT_DIV_ASSIGN => BinaryExprType::Div,
                        TokenType::PUNCT_MULT_ASSIGN => BinaryExprType::Mult,
                        TokenType::PUNCT_ADD_ASSIGN => BinaryExprType::Add,
                        TokenType::PUNCT_SUB_ASSIGN => BinaryExprType::Sub,
                        TokenType::PUNCT_L_SHIFT_BIT_ASSIGN => BinaryExprType::BitShiftLeft,
                        TokenType::PUNCT_R_SHIFT_BIT_ASSIGN => BinaryExprType::BitShiftRight,
                        TokenType::PUNCT_AND_BIT_ASSIGN => BinaryExprType::BitAND,
                        TokenType::PUNCT_XOR_BIT_ASSIGN => BinaryExprType::BitXOR,
                        TokenType::PUNCT_OR_BIT_ASSIGN => BinaryExprType::BitOR,
                    },
                    first,
                    second,
                };
                flattened.expressions.push(transformed);
                let second = flattened.expressions.len() - 1;
                return Ok(Expr::Binary {
                    r#type: BinaryExprType::Assignment,
                    first,
                    second,
                });
            }
            Some(Token { line, column, .. }) => {
                return Err(error("Expected assignment operator", line, column))
            }
            _ => return Err("Expected assignment operator".to_string()),
        }
    }
    Ok(conditional)
}

pub fn parse_comma_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &ByteVecMaps,
) -> Result<Expr, String> {
    let assignment_expr = parse_assignment_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type: TokenType::PUNCT_COMMA,
                ..
            },
        ) => {
            *index += 1;
            let second_operand = parse_assignment_expression(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(assignment_expr);
            let first = flattened.expressions.len() - 1;
            flattened.expressions.push(second_operand);
            let second = flattened.expressions.len() - 1;
            Ok(Expr::Binary {
                r#type: t.r#type,
                first,
                second,
            })
        }
        _ => Ok(assignment_expr),
    }
}

pub fn parse_expressions(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Expr, String> {
    // stack is used for expressions that have nested levels
    // -- like ( ( ... ) ) or 5 + 6 * 4 -> 5 + (6 * 4)
    let mut stack = Vec::<Expr>::new();
    // curr_expr is used for expressions with higher priority
    let mut curr_expr: Option<Expr> = None;
    // left_expression is used for expressions that have two operands
    // and priority needs to be set between right vs left
    let mut left_expression: Option<Expr> = None;
    // used to differentiate between contexts where a comma expression is parsed or a postfix
    // expression with an argument expression list is parsed
    let mut parsing_argument_expression_list_in_postfix = Vec::new();
    while *index < tokens.len() {
        let Token {
            r#type,
            location: Some(Location { line, column }),
        } = tokens[*index]
        else {
            unreachable!()
        };
        match r#type {
            //Comma expressions
            TokenType::PUNCT_COMMA => {
                if curr_expr.is_none() {
                    return Err(error("Expected expression before ','", line, column));
                }
                let Some(curr_expr_inside) = curr_expr else {
                    unreachable!()
                };
                if let Some(false) = parsing_argument_expression_list_in_postfix.last() {
                    flattened.expressions.push(curr_expr_inside);
                    curr_expr = Some(Expr::Binary {
                        r#type: BinaryExprType::Comma,
                        first: Some(flattened.expressions.len() - 1),
                        second: None,
                    });
                } else {
                    let Some(recent_arg_list) = flattened.argument_expr_list_list.last_mut() else {
                        unreachable!()
                    };
                    recent_arg_list.push(curr_expr_inside);
                    curr_expr = None;
                }
                *index += 1;
            }
            //Assignment
            TokenType::PUNCT_ASSIGNMENT
            | TokenType::PUNCT_MULT_ASSIGN
            | TokenType::PUNCT_DIV_ASSIGN
            | TokenType::PUNCT_MODULO_ASSIGN
            | TokenType::PUNCT_ADD_ASSIGN
            | TokenType::PUNCT_SUB_ASSIGN
            | TokenType::PUNCT_L_SHIFT_BIT_ASSIGN
            | TokenType::PUNCT_R_SHIFT_BIT_ASSIGN
            | TokenType::PUNCT_AND_BIT_ASSIGN
            | TokenType::PUNCT_XOR_BIT_ASSIGN
            | TokenType::PUNCT_OR_BIT_ASSIGN => {
                *index += 1;
                parse_assignment_expression(
                    curr_expr, tokens, index, flattened, &mut stack, str_maps,
                )?;
            }
            // Postfix but with unary edge cases
            TokenType::PUNCT_INCREMENT | TokenType::PUNCT_DECREMENT => {
                parse_increment_decrement(tokens, *index, &mut curr_expr, &mut stack, flattened)?;
            }
            TokenType::PUNCT_DOT | TokenType::PUNCT_ARROW | TokenType::PUNCT_OPEN_SQR => {
                if curr_expr.is_some() {
                    match r#type {
                        TokenType::PUNCT_DOT | TokenType::PUNCT_ARROW => {
                            let dot_or_arrow = tokens[*index];
                            consume_whitespace(tokens, index);
                            if !matches!(
                                tokens.get(*index),
                                Some(Token {
                                    r#type: TokenType::IDENT { .. }..
                                })
                            ) {
                                return Err(error("Expected identifier", line, column));
                            }
                            let Some(Token {
                                r#type: TokenType::IDENT { str_map_key, .. },
                                ..
                            }) = tokens.get(*index)
                            else {
                                unreachable!()
                            };
                            left_expression = curr_expr;
                            let postfix_type = match dot_or_arrow.r#type {
                                TokenType::PUNCT_DOT => PostFix::WithMember {
                                    first: None,
                                    member_ident_key: *str_map_key,
                                },
                                TokenType::PUNCT_ARROW => PostFix::WithPointerToMember {
                                    first: None,
                                    member_ident_key: *str_map_key,
                                },
                                _ => unreachable!(),
                            };
                            curr_expr = Some(Expr::PostFix(postfix_type));
                            *index += 1;
                        }
                        TokenType::PUNCT_OPEN_SQR => {
                            let Some(mut curr_expr_inside) = curr_expr else {
                                unreachable!()
                            };
                            let mut postfix_subscript = Expr::PostFix(PostFix::WithSubscript {
                                first: None,
                                subscript: None,
                            });
                            if curr_expr_inside.priority() >= postfix_subscript.priority() {
                                flattened.expressions.push(curr_expr_inside);
                                left_has_higher_eq_priority(
                                    flattened.expressions.len() - 1,
                                    &mut postfix_subscript,
                                );
                            } else {
                                right_has_higher_priority(
                                    &mut curr_expr_inside,
                                    &mut postfix_subscript,
                                );
                            }
                            stack.push(postfix_subscript);
                            curr_expr = None;
                            *index += 1;
                        }
                        _ => unreachable!(),
                    }
                } else {
                    let Some(bytes) = tokens[*index].to_byte_vec(str_maps) else {
                        unreachable!()
                    };
                    let msg = format!("Unexpected token {}", String::from_utf8(bytes).unwrap());
                    return Err(error(&msg, line, column));
                }
                // Dont need to check for identifier after because the identifier is already
                // parsed before due to postfix struct requiring that identifiers be consumed
            }
            TokenType::PUNCT_CLOSE_SQR => {
                if curr_expr.is_none() {
                    let Some(bytes) = tokens[*index].to_byte_vec(str_maps) else {
                        unreachable!()
                    };
                    let msg = format!("Unexpected token {}", String::from_utf8(bytes).unwrap());
                    return Err(error(&msg, line, column));
                }
                while let Some(mut e) = stack.pop() {
                    let Some(unwrapped) = curr_expr else {
                        unreachable!()
                    };
                    flattened.expressions.push(unwrapped);
                    match e {
                        Expr::Primary(ref mut p) => {
                            *p = Some(PrimaryInner::new_p_expr(flattened.expressions.len() - 1));
                            curr_expr = Some(e);
                        }
                        Expr::PostFix(ref mut p) => match p {
                            PostFix::WithSubscript { subscript, .. } => {
                                *subscript = Some(flattened.expressions.len() - 1);
                                curr_expr = Some(e);
                                break;
                            }
                            _ => unreachable!(),
                        },
                        Expr::Unary(ref mut u) => {
                            u.first = Some(flattened.expressions.len() - 1);
                            curr_expr = Some(e);
                        }
                        Expr::Cast(ref mut c) => {
                            c.cast_expr = Some(flattened.expressions.len() - 1);
                            curr_expr = Some(e);
                        }
                        _ => {
                            assert!(
                                e.priority() <= unwrapped.priority(),
                                "{} {}",
                                e.priority(),
                                unwrapped.priority()
                            );
                            let unwrapped = Some(flattened.expressions.len() - 1);
                            match e {
                                Expr::Primary(ref mut p) => {
                                    *p = Some(PrimaryInner::Expr(unwrapped.unwrap()));
                                }
                                Expr::Unary(ref mut u) => {
                                    u.first = unwrapped;
                                }
                                Expr::Binary { ref mut second, .. } => {
                                    *second = unwrapped;
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
                *index += 1;
            }
            //Primary expressions
            primary_tokens!() => {
                // TODO: we need to check for the case of sizeof and _Alignof
                // -- Don't think I need to anymore because it's handled by unary exprs
                let token_within: Token = tokens[*index];
                consume_whitespace(tokens, index);
                let pi = PrimaryInner::new_p_token(token_within);
                let Some(token_to_byte_vec) = token_within.to_byte_vec(str_maps) else {
                    unreachable!()
                };
                let Ok(s) = String::from_utf8(token_to_byte_vec) else {
                    unreachable!()
                };
                if pi.is_err() {
                    todo!("ERROR HERE")
                }
                let Ok(PiOk) = pi else { unreachable!() };
                let primary = Expr::Primary(Some(PiOk));
                flattened.expressions.push(primary);
                let last_index = flattened.expressions.len() - 1;
                if curr_expr.is_none() {
                    curr_expr = Some(primary);
                } else {
                    match &mut curr_expr {
                        Some(Expr::PostFix(_)) => {
                            todo!("ERROR HERE")
                        }
                        Some(Expr::Unary(u)) => {
                            assert!(u.first.is_none());
                            u.first = Some(last_index);
                        }
                        Some(Expr::Cast(c)) => {
                            c.cast_expr = Some(last_index);
                        }
                        Some(Expr::Binary { second, .. }) => {
                            *second = Some(last_index);
                        }
                        Some(Expr::Conditional(c)) => {
                            if c.first.is_none() {
                                c.first = Some(last_index);
                            } else if c.second.is_none() {
                                c.second = Some(last_index);
                            } else if c.third.is_none() {
                                c.third = Some(last_index);
                            }
                        }
                        _ => {
                            todo!("ERROR HERE")
                        }
                    }
                }

                consume_whitespace(tokens, index);
            }
            TokenType::PUNCT_OPEN_PAR => {
                if matches!(curr_expr, Some(Expr::Primary(_) | Expr::PostFix(_))) {
                    // moving past open par because it isn't primary but postfix
                    *index += 1;
                    flattened.argument_expr_list_list.push(Vec::new());
                    parsing_argument_expression_list_in_postfix.push(true);
                    // have to do this matching bc postfix is an enum not struct
                    if !matches!(
                        curr_expr,
                        Some(
                            Expr::Primary(_)
                                | Expr::PostFix(_)
                                | Expr::Cast(_)
                                | Expr::Unary(_)
                                | Expr::Conditional(_)
                        )
                    ) {
                        let Some(curr_expr_inside) = curr_expr else {
                            unreachable!()
                        };
                        match curr_expr_inside {
                            Expr::Binary {
                                second,
                                r#type,
                                first,
                            } => {
                                let Some(curr_expr_second_key) = second else {
                                    unreachable!()
                                };
                                stack.push(Expr::Binary {
                                    r#type,
                                    first,
                                    second: None,
                                });
                                stack.push(Expr::PostFix(PostFix::WithFunctionCall {
                                    first: Some(curr_expr_second_key),
                                    argument_expr_idx: flattened.argument_expr_list_list.len() - 1,
                                }));
                                curr_expr = None;
                            }
                            _ => unreachable!(),
                        }
                    } else {
                        let Some(curr_expr_inside) = curr_expr else {
                            unreachable!()
                        };
                        match curr_expr_inside {
                            Expr::Primary(_) | Expr::PostFix(_) => {
                                flattened.expressions.push(curr_expr_inside);
                                stack.push(curr_expr_inside);
                                stack.push(Expr::PostFix(PostFix::WithFunctionCall {
                                    first: Some(flattened.expressions.len() - 1),
                                    argument_expr_idx: flattened.argument_expr_list_list.len() - 1,
                                }));
                            }
                            Expr::Cast(mut c) => {
                                let Some(cast_expr_idx) = c.cast_expr else {
                                    unreachable!()
                                };
                                c.cast_expr = None;
                                stack.push(curr_expr_inside);
                                stack.push(Expr::PostFix(PostFix::WithFunctionCall {
                                    first: Some(cast_expr_idx),
                                    argument_expr_idx: flattened.argument_expr_list_list.len() - 1,
                                }));
                            }
                            Expr::Unary(mut u) => {
                                let Some(unary_expr_idx) = u.first else {
                                    unreachable!()
                                };
                                u.first = None;
                                stack.push(curr_expr_inside);
                                stack.push(Expr::PostFix(PostFix::WithFunctionCall {
                                    first: Some(unary_expr_idx),
                                    argument_expr_idx: flattened.argument_expr_list_list.len() - 1,
                                }));
                            }
                            Expr::Conditional(_) => unreachable!(),
                            _ => unreachable!(),
                        }
                        curr_expr = None;
                    }
                    continue;
                } else {
                    parsing_argument_expression_list_in_postfix.push(false);
                }
                *index += 1;
                expected_token(tokens, str_maps, index, TokenType::PUNCT_CLOSE_PAR);
                consume_whitespace(tokens, index);
                if let Some(expr) = curr_expr {
                    stack.push(expr);
                }
                // if we run into a token that makes everything inside the (...) just a primary
                // expression
                if matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: expression_operators!(),
                        ..
                    }) | None
                ) {
                    stack.push(Expr::Primary(None));
                    curr_expr = None;
                    consume_whitespace(tokens, index);
                    if !matches!(
                        tokens.get(*index),
                        Some(Token {
                            r#type: TokenType::IDENT { .. }
                                | TokenType::CONSTANT_DEC_INT { .. }
                                | TokenType::CONSTANT_CHAR { .. }
                                | TokenType::PUNCT_OPEN_PAR
                                | TokenType::PUNCT_PLUS
                                | TokenType::PUNCT_MINUS
                                | TokenType::PUNCT_NOT_BOOL
                                | TokenType::PUNCT_TILDE
                                | TokenType::PUNCT_INCREMENT
                                | TokenType::PUNCT_DECREMENT,
                            ..
                        })
                    ) {
                        todo!("ERROR HERE")
                    }
                } else {
                    consume_whitespace(tokens, index);
                    // Typenames
                    let type_name = parse_type_names(&tokens, index, flattened, str_maps)?;
                    flattened.type_names.push(type_name);
                    match tokens.get(*index) {
                        // Postfix
                        Some(Token {
                            r#type: TokenType::PUNCT_OPEN_CURLY,
                            ..
                        }) => {
                            let i = parse_initializer(tokens, index, flattened, str_maps)?;
                            flattened.initializers.push(i);
                            stack.push(Expr::PostFix(PostFix::WithTypeNameInitializerList {
                                type_name: flattened.type_names.len() - 1,
                                initializer_list: flattened.initializers.len() - 1,
                            }));
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
            TokenType::PUNCT_CLOSE_PAR => {
                if let Some(true) = parsing_argument_expression_list_in_postfix.pop() {
                    let Some(curr_expr_inside) = curr_expr else {
                        unreachable!()
                    };
                    let Some(arg_vec) = flattened.argument_expr_list_list.last_mut() else {
                        unreachable!()
                    };
                    arg_vec.push(curr_expr_inside);
                    let withfunction = stack.pop();
                    assert!(matches!(
                        withfunction,
                        Some(Expr::PostFix(PostFix::WithFunctionCall { .. }))
                    ));
                    curr_expr = stack.pop();
                    let Some(mut withfunction) = withfunction else {
                        unreachable!()
                    };
                    let Some(mut curr_expr_inner) = curr_expr else {
                        unreachable!()
                    };
                    if curr_expr_inner.priority() >= withfunction.priority() {
                        flattened.expressions.push(curr_expr_inner);
                        left_has_higher_eq_priority(
                            flattened.expressions.len() - 1,
                            &mut withfunction,
                        );
                    } else {
                        right_has_higher_priority(&mut curr_expr_inner, &mut withfunction);
                    }
                    curr_expr = Some(withfunction);
                    *index += 1;
                    continue;
                }
                if curr_expr.is_none() {
                    todo!("ERROR HERE")
                }
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
                    let Some(unwrapped) = curr_expr else {
                        unreachable!()
                    };
                    flattened.expressions.push(unwrapped);
                    match e {
                        Expr::Primary(ref mut p) => {
                            *p = Some(PrimaryInner::new_p_expr(flattened.expressions.len() - 1));
                            curr_expr = Some(e);
                            if !matches!(stack.last(), Some(Expr::Unary(_) | Expr::Cast(_))) {
                                break;
                            } else {
                                already_popped_primary = true;
                            }
                        }
                        Expr::PostFix(_) => unreachable!(),
                        Expr::Unary(ref mut u) => {
                            u.first = Some(flattened.expressions.len() - 1);
                            curr_expr = Some(e);
                            if !matches!(stack.last(), Some(Expr::Unary(_) | Expr::Cast(_)))
                                && already_popped_primary
                            {
                                break;
                            }
                        }
                        Expr::Cast(ref mut c) => {
                            c.cast_expr = Some(flattened.expressions.len() - 1);
                            curr_expr = Some(e);
                            if !matches!(stack.last(), Some(Expr::Unary(_) | Expr::Cast(_)))
                                && already_popped_primary
                            {
                                break;
                            }
                        }
                        _ => {
                            assert!(
                                e.priority() <= unwrapped.priority(),
                                "{} {}",
                                e.priority(),
                                unwrapped.priority()
                            );
                            let unwrapped = Some(flattened.expressions.len() - 1);
                            match &mut e {
                                Expr::Unary(ref mut u) => {
                                    u.first = unwrapped;
                                }
                                Expr::Binary { second, .. } => {
                                    *second = unwrapped;
                                }
                                Expr::Conditional(c) => {
                                    c.third = unwrapped;
                                }
                                _ => unreachable!(),
                            }
                            curr_expr = Some(e);
                        }
                    }
                }
                if matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: primary_tokens!() | TokenType::PUNCT_OPEN_PAR,
                        ..
                    })
                ) {
                    todo!("ERROR HERE")
                }
            }
            //Unary expressions
            TokenType::PUNCT_PLUS
            | TokenType::PUNCT_MINUS
            | TokenType::PUNCT_NOT_BOOL
            | TokenType::PUNCT_TILDE
            | TokenType::KEYWORD_SIZEOF
            | TokenType::KEYWORD__ALIGNOF => {
                left_expression = curr_expr;
                match &left_expression {
                    Some(Expr::Primary(_) | Expr::PostFix(_)) => {
                        // if a '~' or '!' follow a primary expression, that is not allowed.
                        match tokens[*index].r#type {
                            TokenType::PUNCT_TILDE | TokenType::PUNCT_NOT_BOOL => {
                                todo!("ERROR HERE")
                            }
                            _ => {}
                        }
                        curr_expr = Some(Expr::Binary {
                            r#type: match tokens[*index].r#type {
                                TokenType::PUNCT_PLUS => BinaryExprType::Add,
                                TokenType::PUNCT_MINUS => BinaryExprType::Sub,
                                _ => unreachable!("{:?}", tokens[*index]),
                            },
                            first: None,
                            second: None,
                        });
                    }
                    None => {
                        curr_expr = Some(Expr::Unary(Unary {
                            op: match tokens[*index].r#type {
                                TokenType::PUNCT_PLUS => UnaryOp::Add,
                                TokenType::PUNCT_MINUS => UnaryOp::Sub,
                                TokenType::PUNCT_NOT_BOOL => UnaryOp::LogicalNOT,
                                TokenType::PUNCT_TILDE => UnaryOp::BitNOT,
                                TokenType::KEYWORD_SIZEOF => UnaryOp::Sizeof,
                                TokenType::KEYWORD__ALIGNOF => UnaryOp::AlignOf,
                                _ => unreachable!(),
                            },
                            first: None,
                        }));
                    }
                    Some(Expr::Unary(Unary { op: _, first })) => {
                        if first.is_none() {
                            let Some(left_expression_unwrapped) = left_expression else {
                                unreachable!()
                            };
                            stack.push(left_expression_unwrapped);
                            left_expression = None;
                            curr_expr = Some(Expr::Unary(Unary {
                                op: match tokens[*index].r#type {
                                    TokenType::PUNCT_PLUS => UnaryOp::Add,
                                    TokenType::PUNCT_MINUS => UnaryOp::Sub,
                                    TokenType::PUNCT_NOT_BOOL => UnaryOp::LogicalNOT,
                                    TokenType::PUNCT_TILDE => UnaryOp::BitNOT,
                                    TokenType::KEYWORD_SIZEOF => UnaryOp::Sizeof,
                                    TokenType::KEYWORD__ALIGNOF => UnaryOp::AlignOf,
                                    _ => unreachable!(),
                                },
                                first: None,
                            }));
                        } else {
                            // if a '~' or '!' follow a unary expression, that is not allowed.
                            match tokens[*index].r#type {
                                TokenType::PUNCT_TILDE | TokenType::PUNCT_NOT_BOOL => {
                                    todo!("ERROR HERE")
                                }
                                _ => {}
                            }
                            curr_expr = Some(Expr::Binary {
                                r#type: match tokens[*index].r#type {
                                    TokenType::PUNCT_PLUS => BinaryExprType::Add,
                                    TokenType::PUNCT_MINUS => BinaryExprType::Sub,
                                    _ => unreachable!("{:?}", tokens[*index]),
                                },
                                first: None,
                                second: None,
                            });
                        }
                    }
                    Some(Expr::Binary { second, .. }) => {
                        if second.is_none() {
                            curr_expr = Some(Expr::Unary(Unary {
                                op: match tokens[*index].r#type {
                                    TokenType::PUNCT_PLUS { .. } => UnaryOp::Add,
                                    TokenType::PUNCT_MINUS { .. } => UnaryOp::Sub,
                                    TokenType::PUNCT_NOT_BOOL { .. } => UnaryOp::LogicalNOT,
                                    TokenType::PUNCT_TILDE { .. } => UnaryOp::BitNOT,
                                    _ => unreachable!(),
                                },
                                first: None,
                            }));
                        } else {
                            curr_expr = Some(Expr::Binary {
                                r#type: match tokens[*index].r#type {
                                    TokenType::PUNCT_PLUS { .. } => BinaryExprType::Add,
                                    TokenType::PUNCT_MINUS { .. } => BinaryExprType::Sub,
                                    _ => unreachable!(),
                                },
                                first: None,
                                second: None,
                            });
                        }
                    }
                    _ => unreachable!(),
                }
                if let Some(Expr::Unary(u)) = &curr_expr {
                    if matches!(u.op, UnaryOp::AlignOf) {
                        if !matches!(
                            tokens.get(*index),
                            Some(Token {
                                r#type: primary_tokens!() | TokenType::PUNCT_OPEN_PAR,
                                ..
                            })
                        ) {
                            todo!("ERROR HERE")
                        }
                    } else {
                        if !matches!(
                            tokens.get(*index),
                            Some(Token {
                                r#type: primary_tokens!()
                                    | TokenType::PUNCT_OPEN_PAR
                                    | TokenType::PUNCT_PLUS
                                    | TokenType::PUNCT_MINUS
                                    | TokenType::PUNCT_NOT_BOOL
                                    | TokenType::PUNCT_TILDE
                                    | TokenType::PUNCT_MULT
                                    | TokenType::PUNCT_AND_BIT
                                    | TokenType::KEYWORD_SIZEOF
                                    | TokenType::KEYWORD__ALIGNOF,
                                ..
                            })
                        ) {
                            todo!("ERROR HERE")
                        }
                    }
                }
            }
            //Multiplicative expressions with unary edge cases
            TokenType::PUNCT_MULT | TokenType::PUNCT_DIV | TokenType::PUNCT_MODULO => {
                match curr_expr {
                    Some(Expr::Primary(_) | Expr::PostFix(_)) => {
                        left_expression = curr_expr;
                        curr_expr = Some(Expr::Binary {
                            r#type: match tokens[*index].r#type {
                                TokenType::PUNCT_MULT => BinaryExprType::Mult,
                                TokenType::PUNCT_DIV => BinaryExprType::Div,
                                TokenType::PUNCT_MODULO => BinaryExprType::Mod,
                                _ => unreachable!(),
                            },
                            first: None,
                            second: None,
                        });
                    }
                    Some(Expr::Binary { second, .. }) => {
                        if second.is_none() {
                            stack.push(curr_expr.unwrap());
                            curr_expr = Some(Expr::Unary(Unary {
                                op: match tokens[*index].r#type {
                                    TokenType::PUNCT_MULT { .. } => UnaryOp::Deref,
                                    _ => {
                                        todo!("ERROR HERE")
                                    }
                                },
                                first: None,
                            }));
                        } else {
                            left_expression = curr_expr;
                            curr_expr = Some(Expr::Binary {
                                r#type: match tokens[*index].r#type {
                                    TokenType::PUNCT_MULT { .. } => BinaryExprType::Mult,
                                    TokenType::PUNCT_DIV { .. } => BinaryExprType::Div,
                                    TokenType::PUNCT_MODULO { .. } => BinaryExprType::Mod,
                                    _ => unreachable!(),
                                },
                                first: None,
                                second: None,
                            });
                        }
                    }
                    Some(Expr::Cast(_) | Expr::Unary(_)) => {
                        stack.push(curr_expr.unwrap());
                        curr_expr = Some(Expr::Unary(Unary {
                            op: match tokens[*index].r#type {
                                TokenType::PUNCT_MULT { .. } => UnaryOp::Deref,
                                _ => {
                                    todo!("ERROR HERE")
                                }
                            },
                            first: None,
                        }));
                    }
                    Some(Expr::Conditional(_)) => unreachable!(),
                    None => {
                        curr_expr = Some(Expr::Unary(Unary {
                            op: match tokens[*index].r#type {
                                TokenType::PUNCT_MULT => UnaryOp::Deref,
                                _ => {
                                    todo!("ERROR HERE")
                                }
                            },
                            first: None,
                        }));
                    }
                }
                if !matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: primary_tokens!()
                            | TokenType::PUNCT_OPEN_PAR
                            | TokenType::PUNCT_PLUS
                            | TokenType::PUNCT_MINUS
                            | TokenType::PUNCT_NOT_BOOL
                            | TokenType::PUNCT_TILDE
                            | TokenType::PUNCT_MULT
                            | TokenType::PUNCT_AND_BIT
                            | TokenType::KEYWORD_SIZEOF
                            | TokenType::KEYWORD__ALIGNOF,
                        ..
                    })
                ) {
                    todo!("ERROR HERE")
                }
            }
            //Bitshift expressions
            TokenType::PUNCT_BITSHIFT_RIGHT | TokenType::PUNCT_BITSHIFT_LEFT => {
                if curr_expr.is_none() {
                    todo!("ERROR HERE")
                }
                left_expression = curr_expr;
                curr_expr = Some(Expr::Binary {
                    r#type: match tokens[*index].r#type {
                        TokenType::PUNCT_BITSHIFT_LEFT => BinaryExprType::BitShiftLeft,
                        TokenType::PUNCT_BITSHIFT_RIGHT => BinaryExprType::BitShiftRight,
                        _ => unreachable!(),
                    },
                    first: None,
                    second: None,
                });
                if !matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: primary_tokens!()
                            | TokenType::PUNCT_OPEN_PAR
                            | TokenType::PUNCT_PLUS
                            | TokenType::PUNCT_MINUS
                            | TokenType::PUNCT_NOT_BOOL
                            | TokenType::PUNCT_TILDE,
                        ..
                    })
                ) {
                    todo!("ERROR HERE")
                }
            }
            //Relational expressions
            TokenType::PUNCT_LESS_THAN
            | TokenType::PUNCT_LESS_THAN_EQ
            | TokenType::PUNCT_GREATER_THAN
            | TokenType::PUNCT_GREATER_THAN_EQ => {
                if curr_expr.is_none() {
                    todo!("ERROR HERE")
                }
                left_expression = curr_expr;
                curr_expr = Some(Expr::Binary {
                    r#type: match tokens[*index].r#type {
                        TokenType::PUNCT_LESS_THAN => BinaryExprType::LessThan,
                        TokenType::PUNCT_LESS_THAN_EQ => BinaryExprType::LessThanEq,
                        TokenType::PUNCT_GREATER_THAN => BinaryExprType::GreaterThan,
                        TokenType::PUNCT_GREATER_THAN_EQ => BinaryExprType::GreaterThanEq,
                        _ => unreachable!(),
                    },
                    first: None,
                    second: None,
                });
                if !matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: primary_tokens!()
                            | TokenType::PUNCT_OPEN_PAR
                            | TokenType::PUNCT_PLUS
                            | TokenType::PUNCT_MINUS
                            | TokenType::PUNCT_NOT_BOOL
                            | TokenType::PUNCT_TILDE,
                        ..
                    })
                ) {
                    todo!("ERROR HERE")
                }
            }
            //Equality expressions
            TokenType::PUNCT_EQ_BOOL | TokenType::PUNCT_NOT_EQ_BOOL => {
                if curr_expr.is_none() {
                    todo!("ERROR HERE")
                }
                left_expression = curr_expr;
                let op = tokens[*index].r#type;
                curr_expr = Some(Expr::Binary {
                    r#type: match op {
                        TokenType::PUNCT_EQ_BOOL => BinaryExprType::Eq,
                        TokenType::PUNCT_NOT_EQ_BOOL => BinaryExprType::NotEq,
                        _ => unreachable!(),
                    },
                    first: None,
                    second: None,
                });
                if !matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: primary_tokens!()
                            | TokenType::PUNCT_OPEN_PAR
                            | TokenType::PUNCT_PLUS
                            | TokenType::PUNCT_MINUS
                            | TokenType::PUNCT_NOT_BOOL
                            | TokenType::PUNCT_TILDE,
                        ..
                    })
                ) {
                    todo!("ERROR HERE")
                }
            }
            //BitAND expressions with unary edge cases
            TokenType::PUNCT_AND_BIT => {
                match curr_expr {
                    Some(curr_expr_inside) => match curr_expr_inside {
                        Expr::Primary(_) | Expr::PostFix(_) => {
                            left_expression = curr_expr;
                            curr_expr = Some(Expr::Binary {
                                r#type: BinaryExprType::BitAND,
                                first: None,
                                second: None,
                            });
                        }
                        Expr::Binary { second, .. } => {
                            if second.is_none() {
                                stack.push(curr_expr_inside);
                                curr_expr = Some(Expr::Unary(Unary {
                                    op: UnaryOp::Ampersand,
                                    first: None,
                                }));
                            } else {
                                left_expression = curr_expr;
                                curr_expr = Some(Expr::Binary {
                                    r#type: BinaryExprType::BitAND,
                                    first: None,
                                    second: None,
                                });
                            }
                        }
                        Expr::Cast(_) | Expr::Unary(_) => {
                            stack.push(curr_expr_inside);
                            curr_expr = Some(Expr::Unary(Unary {
                                op: UnaryOp::Ampersand,
                                first: None,
                            }));
                        }
                        Expr::Conditional(_) => unreachable!(),
                    },
                    None => {
                        curr_expr = Some(Expr::Unary(Unary {
                            op: UnaryOp::Ampersand,
                            first: None,
                        }));
                    }
                }
                if !matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: primary_tokens!()
                            | TokenType::PUNCT_OPEN_PAR
                            | TokenType::PUNCT_PLUS
                            | TokenType::PUNCT_MINUS
                            | TokenType::PUNCT_NOT_BOOL
                            | TokenType::PUNCT_TILDE
                            | TokenType::PUNCT_MULT
                            | TokenType::PUNCT_AND_BIT
                            | TokenType::KEYWORD_SIZEOF
                            | TokenType::KEYWORD__ALIGNOF,
                        ..
                    })
                ) {
                    todo!("ERROR HERE")
                }
            }
            //BitXOR expressions
            TokenType::PUNCT_XOR_BIT => {
                if curr_expr.is_none() {
                    todo!("ERROR HERE")
                }
                left_expression = curr_expr;
                curr_expr = Some(Expr::Binary {
                    r#type: BinaryExprType::BitXOR,
                    first: None,
                    second: None,
                });
                if !matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: primary_tokens!()
                            | TokenType::PUNCT_OPEN_PAR
                            | TokenType::PUNCT_PLUS
                            | TokenType::PUNCT_MINUS
                            | TokenType::PUNCT_NOT_BOOL
                            | TokenType::PUNCT_TILDE
                            | TokenType::PUNCT_MULT
                            | TokenType::PUNCT_AND_BIT
                            | TokenType::KEYWORD_SIZEOF
                            | TokenType::KEYWORD__ALIGNOF,
                        ..
                    })
                ) {
                    let Some(bytes) = tokens[*index].to_byte_vec(str_maps) else {
                        unreachable!()
                    };
                    let msg = format!("Unexpected {}", String::from_utf8(bytes).unwrap());
                    return Err(error(&msg, line, column));
                }
            }
            //BitOR expressions
            TokenType::PUNCT_OR_BIT => {
                if curr_expr.is_none() {
                    todo!("ERROR HERE")
                }
                left_expression = curr_expr;
                curr_expr = Some(Expr::Binary {
                    r#type: BinaryExprType::BitOR,
                    first: None,
                    second: None,
                });
                if !matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: primary_tokens!()
                            | TokenType::PUNCT_OPEN_PAR
                            | TokenType::PUNCT_PLUS
                            | TokenType::PUNCT_MINUS
                            | TokenType::PUNCT_NOT_BOOL
                            | TokenType::PUNCT_TILDE
                            | TokenType::PUNCT_MULT
                            | TokenType::PUNCT_AND_BIT
                            | TokenType::KEYWORD_SIZEOF
                            | TokenType::KEYWORD__ALIGNOF,
                        ..
                    })
                ) {
                    let Some(bytes) = tokens[*index].to_byte_vec(str_maps) else {
                        unreachable!()
                    };
                    let msg = format!("Unexpected {}", String::from_utf8(bytes).unwrap());
                    return Err(error(&msg, line, column));
                }
            }
            //LogicalAND expressions
            TokenType::PUNCT_AND_BOOL => {
                if curr_expr.is_none() {
                    todo!("ERROR HERE")
                }
                left_expression = curr_expr;
                curr_expr = Some(Expr::Binary {
                    r#type: BinaryExprType::LogAND,
                    first: None,
                    second: None,
                });
                if !matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: primary_tokens!()
                            | TokenType::PUNCT_OPEN_PAR
                            | TokenType::PUNCT_PLUS
                            | TokenType::PUNCT_MINUS
                            | TokenType::PUNCT_NOT_BOOL
                            | TokenType::PUNCT_TILDE
                            | TokenType::PUNCT_MULT
                            | TokenType::PUNCT_AND_BIT
                            | TokenType::KEYWORD_SIZEOF
                            | TokenType::KEYWORD__ALIGNOF,
                        ..
                    })
                ) {
                    let Some(bytes) = tokens[*index].to_byte_vec(str_maps) else {
                        unreachable!()
                    };
                    let msg = format!("Unexpected {}", String::from_utf8(bytes).unwrap());
                    return Err(error(&msg, line, column));
                }
            }
            //LogicalOR expressions
            TokenType::PUNCT_OR_BOOL => {
                if curr_expr.is_none() {
                    todo!("ERROR HERE")
                }
                left_expression = curr_expr;
                curr_expr = Some(Expr::Binary {
                    r#type: BinaryExprType::LogOR,
                    first: None,
                    second: None,
                });
                consume_whitespace(tokens, index);
                if !matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: primary_tokens!()
                            | TokenType::PUNCT_OPEN_PAR
                            | TokenType::PUNCT_PLUS
                            | TokenType::PUNCT_MINUS
                            | TokenType::PUNCT_NOT_BOOL
                            | TokenType::PUNCT_TILDE
                            | TokenType::PUNCT_MULT
                            | TokenType::PUNCT_AND_BIT
                            | TokenType::KEYWORD_SIZEOF
                            | TokenType::KEYWORD__ALIGNOF,
                        ..
                    })
                ) {
                    let Some(bytes) = tokens[*index].to_byte_vec(str_maps) else {
                        unreachable!()
                    };
                    let msg = format!("Unexpected {}", String::from_utf8(bytes).unwrap());
                    return Err(error(&msg, line, column));
                }
            }
            //Conditional expressions
            TokenType::PUNCT_QUESTION_MARK { .. } => {
                if let Some(expr) = curr_expr {
                    flattened.expressions.push(expr);
                    let expr_cond = Expr::Conditional(Conditional {
                        first: Some(flattened.expressions.len() - 1),
                        second: parse_expressions(tokens, index, flattened, str_maps)?,
                        third: todo!(),
                    });
                    stack.push(expr_cond);
                    curr_expr = None;
                } else {
                    todo!("ERROR HERE")
                }
                consume_whitespace(tokens, index);
            }
            TokenType::PUNCT_COLON { .. } => {
                if curr_expr.is_none() {
                    todo!("ERROR HERE")
                }
                while let Some(mut expr) = stack.pop() {
                    let Some(unwrapped) = curr_expr else {
                        unreachable!()
                    };
                    flattened.expressions.push(unwrapped);
                    let unwrapped = Some(flattened.expressions.len() - 1);

                    match &mut expr {
                        Expr::Unary(ref mut u) => {
                            u.first = unwrapped;
                        }
                        Expr::Binary { second, .. } => {
                            *second = unwrapped;
                        }
                        Expr::Conditional(c) => {
                            c.second = unwrapped;
                            curr_expr = Some(expr);
                            break;
                        }
                        _ => unreachable!(),
                    }
                    curr_expr = Some(expr);
                }
                if !matches!(curr_expr, Some(Expr::Conditional(_))) {
                    todo!("ERROR HERE")
                }
                stack.push(curr_expr.unwrap());
                curr_expr = None;
                consume_whitespace(tokens, index);
            }
            TokenType::WHITESPACE { .. } | TokenType::NEWLINE { .. } => {
                *index += 1;
            }
            _ => {
                todo!("ERROR HERE")
            }
        }
        if left_expression.is_some() && curr_expr.is_some() {
            let Some(mut left) = left_expression else {
                unreachable!()
            };
            let Some(mut right) = curr_expr else {
                unreachable!()
            };
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
        if let Some(curr_expr_inside) = curr_expr {
            flattened.expressions.push(curr_expr_inside);
            let unwrapped = Some(flattened.expressions.len() - 1);
            macro_rules! set_to_unwrapped {
                ($($e: ident) *) => {
                    match expr {
                        Expr::Primary(_) => todo!("{} {}", expr.priority(), curr_expr_inside.priority()),
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
                    }
                };
                    }
        }
        curr_expr = Some(expr);
    }
    let Some(curr_expr) = curr_expr else {
        unreachable!()
    };
    Ok(curr_expr)
}
//Notes:
//The expression that controls conditional inclusion shall be an integer constant expression
//Because the controlling constant expression is evaluated during translation phase 4, all identifiers either are or are not macro names — there simply are no keywords, enumeration constants, etc
//All macro identifiers are evaluated as defined or not defined.
// TODO: rewrite this. It works but is WAYY too convoluted.
pub fn eval_constant_expression_integer_when_preprocess(
    tokens: &[Token],
    index: &mut usize,
    str_maps: &mut ByteVecMaps,
) -> Result<i128, String> {
    if let Some(not_allowed_t) = tokens.iter().find(|t| {
        matches!(
            t.r#type,
            TokenType::PUNCT_ASSIGNMENT
                | TokenType::PUNCT_INCREMENT
                | TokenType::PUNCT_DECREMENT
                | TokenType::PUNCT_OPEN_CURLY
                | TokenType::PUNCT_CLOSE_CURLY
                | TokenType::PUNCT_OPEN_SQR
                | TokenType::PUNCT_CLOSE_SQR
                | TokenType::CONSTANT_DEC_FLOAT { .. }
                | TokenType::CONSTANT_HEXA_FLOAT { .. }
                | TokenType::PUNCT_COMMA
                | TokenType::StringLiteral { .. }
                | TokenType::PUNCT_ARROW
                | TokenType::PUNCT_ADD_ASSIGN
                | TokenType::PUNCT_DIV_ASSIGN
                | TokenType::PUNCT_SUB_ASSIGN
                | TokenType::PUNCT_MULT_ASSIGN
                | TokenType::PUNCT_MODULO_ASSIGN
                | TokenType::PUNCT_AND_BIT_ASSIGN
                | TokenType::PUNCT_OR_BIT_ASSIGN
                | TokenType::PUNCT_XOR_BIT_ASSIGN
                | TokenType::PUNCT_L_SHIFT_BIT_ASSIGN
                | TokenType::PUNCT_R_SHIFT_BIT_ASSIGN
        )
    }) {
        let Some(bytes) = not_allowed_t.to_byte_vec(str_maps) else {
            unreachable!()
        };
        let msg = format!("Cannot have {}", String::from_utf8(bytes).unwrap());
        let Some(Location { line, column }) = not_allowed_t.location else {
            unreachable!()
        };
        return Err(error(&msg, line, column));
    }
    // TODO: describe our algorithm in comments below
    // or we will forget how any of this shit works
    let mut flattened = Flattened::new();
    let curr_expr = parse_expressions(tokens, index, &mut flattened, str_maps)?;
    recursive_eval(&curr_expr, str_maps, flattened.expressions.as_slice())
}

fn recursive_eval(
    expr: &Expr,
    str_maps: &mut ByteVecMaps,
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
                        Token {
                            r#type: TokenType::CONSTANT_DEC_INT { .. }
                                | TokenType::CONSTANT_CHAR { .. },
                            ..
                        }
                    ));
                    match t.r#type {
                        TokenType::CONSTANT_DEC_INT { suffix, value_key } => {
                            // "For the purposes of this token conversion and evaluation,
                            // all signed integer types and all unsigned integer types act as if they have the same representation
                            // as, respectively, the types intmax_t and uintmax_t defined in the header <stdint.h>."
                            //
                            // We just 'cheat' by using i128 integer types. That way, regardless
                            // whether we get u64 (uintmax_t) or i64 (intmax_t), we can still
                            // compare and not have to do any weird casts.
                            // TODO: add overflow checks...
                            let value = &str_maps.key_to_byte_vec[value_key];
                            let Ok(to_be_parsed) = String::from_utf8(value.to_vec()) else {
                                unreachable!()
                            };
                            match to_be_parsed.parse::<i128>() {
                                Ok(v) if v <= u64::MAX as i128 && v >= i64::MIN as i128 => Ok(v),
                                _ => {
                                    todo!("ERROR HERE")
                                }
                            }
                        }
                        TokenType::CONSTANT_CHAR { const_char, .. } => {
                            let parsed_val = match const_char.parse_to_value(str_maps) {
                                Ok(pv) => pv as i128,
                                Err(s) => {
                                    todo!("ERROR HERE")
                                }
                            };
                            Ok(parsed_val)
                        }
                        _ => unreachable!(),
                    }
                }
                None => unreachable!(),
            }
        }
        Expr::PostFix(_) => {
            todo!("ERROR HERE")
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
                | UnaryOp::Decrement
                | UnaryOp::Sizeof
                | UnaryOp::AlignOf => {
                    unreachable!("{:?}", u.op)
                }
            }
        }
        Expr::Cast(_) => {
            todo!("ERROR HERE")
        }
        Expr::Binary {
            r#type,
            first,
            second,
        } => {
            use BinaryExprType::*;
            let first = first.expect("There should be an index");
            let second = second.expect("There should be an index");
            match r#type {
                Mult => Ok(recursive_eval(&expressions[first], str_maps, expressions)?
                    * recursive_eval(&expressions[second], str_maps, expressions)?),
                Div | Mod => {
                    let op = r#type;
                    let right = recursive_eval(&expressions[second], str_maps, expressions)?;
                    if right == 0 {
                        todo!("Once we have expressions, the location of the source is lost, so how do I report location of the error");
                    }
                    match op {
                        Div => {
                            Ok(recursive_eval(&expressions[first], str_maps, expressions)? / right)
                        }
                        Mod => {
                            Ok(recursive_eval(&expressions[first], str_maps, expressions)? % right)
                        }
                        _ => unreachable!(),
                    }
                }
                Add => Ok(recursive_eval(&expressions[first], str_maps, expressions)?
                    + recursive_eval(&expressions[second], str_maps, expressions)?),
                Sub => Ok(recursive_eval(&expressions[first], str_maps, expressions)?
                    - recursive_eval(&expressions[second], str_maps, expressions)?),
                BitShiftLeft => Ok(recursive_eval(&expressions[first], str_maps, expressions)?
                    << recursive_eval(&expressions[second], str_maps, expressions)?),
                BitShiftRight => Ok(recursive_eval(&expressions[first], str_maps, expressions)?
                    >> recursive_eval(&expressions[second], str_maps, expressions)?),
                LessThan => Ok(
                    if recursive_eval(&expressions[first], str_maps, expressions)?
                        < recursive_eval(&expressions[second], str_maps, expressions)?
                    {
                        1
                    } else {
                        0
                    },
                ),
                LessThanEq => Ok(
                    if recursive_eval(&expressions[first], str_maps, expressions)?
                        <= recursive_eval(&expressions[second], str_maps, expressions)?
                    {
                        1
                    } else {
                        0
                    },
                ),
                GreaterThan => Ok(
                    if recursive_eval(&expressions[first], str_maps, expressions)?
                        > recursive_eval(&expressions[second], str_maps, expressions)?
                    {
                        1
                    } else {
                        0
                    },
                ),
                GreaterThanEq => Ok(
                    if recursive_eval(&expressions[first], str_maps, expressions)?
                        >= recursive_eval(&expressions[second], str_maps, expressions)?
                    {
                        1
                    } else {
                        0
                    },
                ),
                Eq => Ok(
                    if recursive_eval(&expressions[first], str_maps, expressions)?
                        == recursive_eval(&expressions[second], str_maps, expressions)?
                    {
                        1
                    } else {
                        0
                    },
                ),
                NotEq => Ok(
                    if recursive_eval(&expressions[first], str_maps, expressions)?
                        != recursive_eval(&expressions[second], str_maps, expressions)?
                    {
                        1
                    } else {
                        0
                    },
                ),
                BitAND => Ok(recursive_eval(&expressions[first], str_maps, expressions)?
                    & recursive_eval(&expressions[second], str_maps, expressions)?),
                BitXOR => Ok(recursive_eval(&expressions[first], str_maps, expressions)?
                    ^ recursive_eval(&expressions[second], str_maps, expressions)?),
                BitOR => Ok(recursive_eval(&expressions[first], str_maps, expressions)?
                    | recursive_eval(&expressions[second], str_maps, expressions)?),
                LogAND => Ok(
                    if recursive_eval(&expressions[first], str_maps, expressions)? != 0
                        && recursive_eval(&expressions[second], str_maps, expressions)? != 0
                    {
                        1
                    } else {
                        0
                    },
                ),
                LogOR => Ok(
                    if recursive_eval(&expressions[first], str_maps, expressions)? != 0
                        || recursive_eval(&expressions[second], str_maps, expressions)? != 0
                    {
                        1
                    } else {
                        0
                    },
                ),
                Comma | Assignment => {
                    Err("Comma or Assignment expressions arent allowed".to_string())
                }
            }
        }
        Expr::Conditional(c) => {
            let Some(first) = c.first else { unreachable!() };
            let Some(second) = c.second else {
                unreachable!()
            };
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
    fn eval_expression_temp() -> Result<(), String> {
        let src = r##"0.4 * 0.4"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        println!("{:?}", tokens);
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps);
        match res {
            Err(_) => {}
            Ok(_) => return Err(String::from("empty expression not caught")),
        }
        Ok(())
    }

    #[test]
    fn eval_expression_test_empty() -> Result<(), String> {
        let src = r##""##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps);
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
            let res = expressions::eval_constant_expression_integer_when_preprocess(
                &tokens,
                &mut str_maps,
            )?;
            assert_eq!(res != 0, false, "(1 + 1) * 0");
        }
        {
            let src = r##"1 + (1 * 0)"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer_when_preprocess(
                &tokens,
                &mut str_maps,
            )?;
            assert_eq!(res != 0, true, "1 + (1 * 0)");
        }
        {
            let src = r##"((1 + 1) * 0)"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer_when_preprocess(
                &tokens,
                &mut str_maps,
            )?;
            assert_eq!(res != 0, false, "((1 + 1) * 0)");
        }
        {
            let src = r##"((((1))))"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer_when_preprocess(
                &tokens,
                &mut str_maps,
            )?;
            assert_eq!(res != 0, true, "((((1))))");
        }
        {
            let src = r##"((((1)))))"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer_when_preprocess(
                &tokens,
                &mut str_maps,
            );
            match res {
                Err(_) => {}
                Ok(_) => return Err(String::from("unbalanced parentheses not caught")),
            }
        }
        {
            let src = r##"(((((1))))"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer_when_preprocess(
                &tokens,
                &mut str_maps,
            );
            match res {
                Err(_) => {}
                Ok(_) => return Err(String::from("unbalanced parentheses not caught")),
            }
        }
        {
            let src = r##"0 - (1 + 1)"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer_when_preprocess(
                &tokens,
                &mut str_maps,
            )?;
            assert_eq!(res != 0, true, "0 - (1 + 1)");
        }
        {
            let src = r##"1"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer_when_preprocess(
                &tokens,
                &mut str_maps,
            )?;
            assert_eq!(res != 0, true, "1");
        }
        {
            let src = r##"'1'"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer_when_preprocess(
                &tokens,
                &mut str_maps,
            )?;
            assert_eq!(res != 0, true, "'1'");
        }
        Ok(())
    }
    #[test]
    fn eval_expression_test_unary() -> Result<(), String> {
        let src = r##"!1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false, "!1");
        let src = r##"!0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "!0");
        let src = r##"~0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "~0");
        let src = r##"~~~0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "~~~0");
        let src = r##"~~~~0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false, "~~~~0");
        let src = r##"--------------1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps);
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
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 * !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 / 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 / 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps);
        match res {
            Err(_) => {}
            Ok(_) => return Err("division by zero not caught".to_string()),
        }
        let src = r##"1 + 1 * 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 * 1 + 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_additive() -> Result<(), String> {
        let src = r##"1 + 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 - 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"0 - 1 + 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false, "0 - 1 + 1");
        let src = r##"0 - 1 + !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "0 - 1 + !1");
        {
            let src = r##"'1' - '1'"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer_when_preprocess(
                &tokens,
                &mut str_maps,
            )?;
            assert_eq!(res != 0, false, "'1' - '1'");
        }
        {
            let src = r##"'2' - '1'"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression_integer_when_preprocess(
                &tokens,
                &mut str_maps,
            )?;
            assert_eq!(res != 0, true, "'1' - '1'");
        }
        Ok(())
    }
    #[test]
    fn eval_expression_test_bitshift() -> Result<(), String> {
        let src = r##"1 << 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 >> 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 >> !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        Ok(())
    }
    #[test]
    fn eval_expression_test_relational() -> Result<(), String> {
        let src = r##"1 < 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 < 2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 < !2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 <= 2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"2 <= 2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 > 2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 > 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 >= 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 >= 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        Ok(())
    }
    #[test]
    fn eval_expression_test_equality() -> Result<(), String> {
        let src = r##"1 == 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 != 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 != !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 != 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        Ok(())
    }
    #[test]
    fn eval_expression_test_bit_and() -> Result<(), String> {
        let src = r##"1 & 0 == 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 & 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 & !0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 & 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 == 0 & 1 == 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_bit_xor() -> Result<(), String> {
        let src = r##"1 ^ 0 == 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false, "1 ^ 0 == 0");
        let src = r##"(1 ^ !0) == 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "(1 ^ !0) == 0");
        Ok(())
    }
    #[test]
    fn eval_expression_test_bit_or() -> Result<(), String> {
        let src = r##"1 | 0 == 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 | !0 == 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "1 | !0 == 0");
        Ok(())
    }
    #[test]
    fn eval_expression_test_logical_and() -> Result<(), String> {
        let src = r##"1 && 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 && 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 && !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_logical_or() -> Result<(), String> {
        let src = r##"1 || 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 || 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 || 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_conditional() -> Result<(), String> {
        let src = r##"1 ? 1 : 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"(1 + 1 == 3) ? 1 : 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"~0 ? (1 + 1 == 2) : 0 * 4"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 ? 0 : 1 * 4"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 ? 0 : !(1 * 4)"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res =
            expressions::eval_constant_expression_integer_when_preprocess(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        Ok(())
    }
    #[test]
    fn parse_expressions_test_cast() -> Result<(), String> {
        let src = r#"(int)1"#.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
        let mut flattened = parser::Flattened::new();
        let (_, cast_expr) = parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
        assert!(matches!(cast_expr, expressions::Expr::Cast(_)));
        let expressions::Expr::Cast(c) = cast_expr else {
            unreachable!()
        };
        assert!(matches!(
            flattened.expressions[c.cast_expr.unwrap()],
            expressions::Expr::Primary(_)
        ));
        let expressions::Expr::Primary(Some(expressions::PrimaryInner::Token(t))) =
            flattened.expressions[c.cast_expr.unwrap()]
        else {
            unreachable!()
        };
        assert!(matches!(t, TokenType::CONSTANT_DEC_INT { .. }));
        let TokenType::CONSTANT_DEC_INT { value_key, .. } = t else {
            unreachable!()
        };
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
        let expressions::Expr::Additive(a) = add else {
            unreachable!()
        };
        let Some(first_idx) = a.first else {
            unreachable!()
        };
        let Some(second_idx) = a.second else {
            unreachable!()
        };
        assert!(matches!(
            flattened.expressions[first_idx],
            expressions::Expr::Primary(_)
        ));
        assert!(matches!(
            flattened.expressions[second_idx],
            expressions::Expr::Cast(_)
        ));
        let expressions::Expr::Primary(Some(expressions::PrimaryInner::Token(t))) =
            flattened.expressions[first_idx]
        else {
            unreachable!()
        };
        assert!(matches!(t, TokenType::CONSTANT_DEC_INT { .. }));
        let TokenType::CONSTANT_DEC_INT { value_key, .. } = t else {
            unreachable!()
        };
        assert!(str_maps.key_to_byte_vec[value_key] == *b"1");
        assert!(matches!(a.op, expressions::AdditiveOps::Add));
        let expressions::Expr::Cast(c) = flattened.expressions[second_idx] else {
            unreachable!()
        };
        let Some(c_idx) = c.cast_expr else {
            unreachable!()
        };
        assert!(matches!(
            flattened.expressions[c_idx],
            expressions::Expr::Primary(_)
        ));
        let expressions::Expr::Primary(Some(expressions::PrimaryInner::Token(t))) =
            flattened.expressions[c_idx]
        else {
            unreachable!()
        };
        assert!(matches!(t, TokenType::CONSTANT_DEC_INT { .. }));
        let TokenType::CONSTANT_DEC_INT { value_key, .. } = t else {
            unreachable!()
        };
        assert!(str_maps.key_to_byte_vec[value_key] == *b"1");
        assert!(c.type_name.is_some());
        let Some(type_name_index) = c.type_name else {
            unreachable!()
        };
        assert!(flattened.type_names.get(type_name_index).is_some());
        let Some(type_name) = flattened.type_names.get(type_name_index) else {
            unreachable!()
        };
        assert!(matches!(
            type_name.specifier_qualifier_list.type_specifiers.get(0),
            Some(parser::declarations::TypeSpecifier::Int)
        ));
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
        let expressions::Expr::Additive(a) = add else {
            unreachable!()
        };
        let Some(first_idx) = a.first else {
            unreachable!()
        };
        assert!(matches!(
            flattened.expressions[first_idx],
            expressions::Expr::Primary(_)
        ));
        let expressions::Expr::Primary(Some(expressions::PrimaryInner::Token(t))) =
            flattened.expressions[first_idx]
        else {
            unreachable!()
        };
        assert!(matches!(t, TokenType::CONSTANT_DEC_INT { .. }));
        let TokenType::CONSTANT_DEC_INT {
            value_key,
            suffix,
            pos_in_src,
        } = t
        else {
            unreachable!()
        };
        assert!(str_maps.key_to_byte_vec[value_key] == *b"1");
        let Some(second_idx) = a.second else {
            unreachable!()
        };
        assert!(matches!(
            flattened.expressions[second_idx],
            expressions::Expr::Unary(_)
        ));
        let expressions::Expr::Unary(u) = flattened.expressions[second_idx] else {
            unreachable!()
        };
        assert!(matches!(u.op, expressions::UnaryOp::LogicalNOT));
        let Some(cast_idx) = u.first else {
            unreachable!()
        };
        assert!(matches!(
            flattened.expressions[cast_idx],
            expressions::Expr::Cast(_)
        ));
        let expressions::Expr::Cast(c) = flattened.expressions[cast_idx] else {
            unreachable!()
        };
        let Some(p_idx) = c.cast_expr else {
            unreachable!()
        };
        assert!(matches!(
            flattened.expressions[p_idx],
            expressions::Expr::Primary(_)
        ));
        let expressions::Expr::Primary(Some(expressions::PrimaryInner::Token(t))) =
            flattened.expressions[p_idx]
        else {
            unreachable!()
        };
        assert!(matches!(t, TokenType::CONSTANT_DEC_INT { .. }));
        let Token {
            r#type:
                TokenType::CONSTANT_DEC_INT {
                    value_key,
                    suffix,
                    pos_in_src,
                },
            ..
        } = t
        else {
            unreachable!()
        };
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
            let expressions::Expr::Additive(a) = add else {
                unreachable!()
            };
            assert!(matches!(a.op, expressions::AdditiveOps::Add));

            let Some(left_idx) = a.first else {
                unreachable!()
            };
            assert!(matches!(
                flattened.expressions[left_idx],
                expressions::Expr::PostFix(_)
            ));
            let expressions::Expr::PostFix(p) = flattened.expressions[left_idx] else {
                unreachable!()
            };
            assert!(matches!(
                p,
                expressions::PostFix::WithIncrementDecrement { .. }
            ));
            let expressions::PostFix::WithIncrementDecrement { first, op } = p else {
                unreachable!()
            };
            assert!(matches!(
                flattened.expressions[first],
                expressions::Expr::Primary(_)
            ));
            assert!(matches!(
                op,
                expressions::PostFixIncrementDecrement::Decrement
            ));

            let Some(right_idx) = a.second else {
                unreachable!()
            };
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
            let expressions::Expr::Additive(a) = add else {
                unreachable!()
            };
            let Some(first_idx) = a.first else {
                unreachable!()
            };
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
            let expressions::Expr::Additive(a) = add else {
                unreachable!()
            };
            let Some(first_idx) = a.first else {
                unreachable!()
            };
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
            assert!(matches!(
                post,
                parser::expressions::Expr::PostFix(
                    parser::expressions::PostFix::WithPointerToMember { .. }
                )
            ));
        }
        Ok(())
    }
    #[test]
    fn parse_expressions_postfix_subscript() -> Result<(), String> {
        {
            let src = r#"hi[hi2]"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let (_, post_subscript) =
                expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(
                post_subscript,
                parser::expressions::Expr::PostFix(
                    parser::expressions::PostFix::WithSubscript { .. }
                )
            ));
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
            let expressions::Expr::Assignment(a) = assign else {
                unreachable!()
            };
            let Some(first_idx) = a.first else {
                unreachable!()
            };
            assert!(matches!(
                flattened.expressions[first_idx],
                expressions::Expr::Primary(_)
            ));
            let Some(second_idx) = a.second else {
                unreachable!()
            };
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
    #[test]
    fn parse_expressions_unary_increment_decrement() -> Result<(), String> {
        {
            let src = r#"++variable"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let (_, unary_increment) =
                expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(
                unary_increment,
                parser::expressions::Expr::Unary(_)
            ));
            let parser::expressions::Expr::Unary(u) = unary_increment else {
                unreachable!()
            };
            assert!(matches!(u.op, parser::expressions::UnaryOp::Increment));
            assert!(u.first.is_some() && flattened.expressions.len() > u.first.unwrap());
        }
        {
            let src = r#"--variable"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let (_, unary_decrement) =
                expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(
                unary_decrement,
                parser::expressions::Expr::Unary(_)
            ));
            let parser::expressions::Expr::Unary(u) = unary_decrement else {
                unreachable!()
            };
            assert!(matches!(u.op, parser::expressions::UnaryOp::Decrement));
            assert!(u.first.is_some() && flattened.expressions.len() > u.first.unwrap());
        }
        Ok(())
    }
    #[test]
    fn parse_expressions_unary_ops_test() -> Result<(), String> {
        {
            let src = r#"*hi"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let (_, unary_deref) =
                expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(unary_deref, parser::expressions::Expr::Unary(_)));
            let parser::expressions::Expr::Unary(u) = unary_deref else {
                unreachable!()
            };
            assert!(matches!(u.op, parser::expressions::UnaryOp::Deref));
            assert!(u.first.is_some() && flattened.expressions.len() > u.first.unwrap());
        }
        {
            let src = r#"&hi"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let (_, unary_amper) =
                expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(unary_amper, parser::expressions::Expr::Unary(_)));
            let parser::expressions::Expr::Unary(u) = unary_amper else {
                unreachable!()
            };
            assert!(matches!(u.op, parser::expressions::UnaryOp::Ampersand));
            assert!(u.first.is_some() && flattened.expressions.len() > u.first.unwrap());
        }
        {
            let src = r#"+hi"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let (_, unary_plus) =
                expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(unary_plus, parser::expressions::Expr::Unary(_)));
            let parser::expressions::Expr::Unary(u) = unary_plus else {
                unreachable!()
            };
            assert!(matches!(u.op, parser::expressions::UnaryOp::Add));
            assert!(u.first.is_some() && flattened.expressions.len() > u.first.unwrap());
        }
        {
            let src = r#"-hi"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let (_, unary_minus) =
                expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(unary_minus, parser::expressions::Expr::Unary(_)));
            let parser::expressions::Expr::Unary(u) = unary_minus else {
                unreachable!()
            };
            assert!(matches!(u.op, parser::expressions::UnaryOp::Sub));
            assert!(u.first.is_some() && flattened.expressions.len() > u.first.unwrap());
        }
        {
            let src = r#"~hi"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let (_, unary_tilde) =
                expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(unary_tilde, parser::expressions::Expr::Unary(_)));
            let parser::expressions::Expr::Unary(u) = unary_tilde else {
                unreachable!()
            };
            assert!(matches!(u.op, parser::expressions::UnaryOp::BitNOT));
            assert!(u.first.is_some() && flattened.expressions.len() > u.first.unwrap());
        }
        {
            let src = r#"!hi"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let (_, unary_not) =
                expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(unary_not, parser::expressions::Expr::Unary(_)));
            let parser::expressions::Expr::Unary(u) = unary_not else {
                unreachable!()
            };
            assert!(matches!(u.op, parser::expressions::UnaryOp::LogicalNOT));
            assert!(u.first.is_some() && flattened.expressions.len() > u.first.unwrap());
        }
        {
            let src = r#"sizeof hi"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let (_, unary_sizeof) =
                expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(unary_sizeof, parser::expressions::Expr::Unary(_)));
            let parser::expressions::Expr::Unary(u) = unary_sizeof else {
                unreachable!()
            };
            assert!(matches!(u.op, parser::expressions::UnaryOp::Sizeof));
            assert!(u.first.is_some() && flattened.expressions.len() > u.first.unwrap());
        }
        {
            let src = r#"_Alignof (hi)"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let (_, unary_alignof) =
                expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(unary_alignof, parser::expressions::Expr::Unary(_)));
            let parser::expressions::Expr::Unary(u) = unary_alignof else {
                unreachable!()
            };
            assert!(matches!(u.op, parser::expressions::UnaryOp::AlignOf));
            assert!(u.first.is_some() && flattened.expressions.len() > u.first.unwrap());
        }
        Ok(())
    }
    #[test]
    fn parse_expressions_comma_operator() -> Result<(), String> {
        {
            let src = r#"(1, 5)"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let (_, primary_comma) =
                expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(
                primary_comma,
                parser::expressions::Expr::Primary(_)
            ));
            let parser::expressions::Expr::Primary(Some(parser::expressions::PrimaryInner::Expr(
                c,
            ))) = primary_comma
            else {
                unreachable!()
            };
            assert!(matches!(
                flattened.expressions[c],
                parser::expressions::Expr::Comma(_)
            ));
            let parser::expressions::Expr::Comma(c) = flattened.expressions[c] else {
                unreachable!()
            };
            assert!(c.first.is_some());
            assert!(c.second.is_some());
            let Some(first) = c.first else { unreachable!() };
            let Some(second) = c.second else {
                unreachable!()
            };
            assert!(first < flattened.expressions.len());
            assert!(second < flattened.expressions.len());
            assert!(matches!(
                flattened.expressions[first],
                parser::expressions::Expr::Primary(_)
            ));
            assert!(matches!(
                flattened.expressions[second],
                parser::expressions::Expr::Primary(_)
            ));
        }
        {
            let src = r#"(1, 3, 5)"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let (_, primary_comma_nested) =
                expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(
                primary_comma_nested,
                parser::expressions::Expr::Primary(_)
            ));
            let parser::expressions::Expr::Primary(Some(parser::expressions::PrimaryInner::Expr(
                c,
            ))) = primary_comma_nested
            else {
                unreachable!()
            };
            assert!(matches!(
                flattened.expressions[c],
                parser::expressions::Expr::Comma(_)
            ));
            let parser::expressions::Expr::Comma(c) = flattened.expressions[c] else {
                unreachable!()
            };
            assert!(c.first.is_some());
            assert!(c.second.is_some());
            let Some(first) = c.first else { unreachable!() };
            let Some(second) = c.second else {
                unreachable!()
            };
            assert!(first < flattened.expressions.len());
            assert!(second < flattened.expressions.len());
            assert!(matches!(
                flattened.expressions[first],
                parser::expressions::Expr::Comma(_)
            ));
            assert!(matches!(
                flattened.expressions[second],
                parser::expressions::Expr::Primary(Some(_))
            ));
            let parser::expressions::Expr::Comma(c) = flattened.expressions[first] else {
                unreachable!()
            };
            assert!(c.first.is_some());
            assert!(c.second.is_some());
            let Some(first) = c.first else { unreachable!() };
            let Some(second) = c.second else {
                unreachable!()
            };
            assert!(matches!(
                flattened.expressions[first],
                parser::expressions::Expr::Primary(Some(_))
            ));
            assert!(matches!(
                flattened.expressions[second],
                parser::expressions::Expr::Primary(Some(_))
            ));
        }
        Ok(())
    }
    #[test]
    fn parse_expressions_argument_expression_test() -> Result<(), String> {
        {
            let src = r#"hi(1, 3, 5)"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let (_, postfix_arg_expr_list) =
                expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(
                postfix_arg_expr_list,
                parser::expressions::Expr::PostFix(
                    parser::expressions::PostFix::WithFunctionCall { .. }
                )
            ));
            let parser::expressions::Expr::PostFix(
                parser::expressions::PostFix::WithFunctionCall {
                    argument_expr_idx,
                    first,
                },
            ) = postfix_arg_expr_list
            else {
                unreachable!()
            };
            assert!(flattened.argument_expr_list_list[argument_expr_idx].len() == 3);
            assert!(flattened.expressions.len() > first.unwrap());
            assert!(matches!(
                flattened.expressions[first.unwrap()],
                parser::expressions::Expr::Primary(_)
            ));
        }
        {
            let src = r#"me->hi(1, 3, 5)"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let (_, postfix_arg_expr_list) =
                expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(
                postfix_arg_expr_list,
                parser::expressions::Expr::PostFix(
                    parser::expressions::PostFix::WithFunctionCall { .. }
                )
            ));
            let parser::expressions::Expr::PostFix(
                parser::expressions::PostFix::WithFunctionCall {
                    argument_expr_idx,
                    first,
                },
            ) = postfix_arg_expr_list
            else {
                unreachable!()
            };
            assert!(flattened.argument_expr_list_list[argument_expr_idx].len() == 3);
            assert!(flattened.expressions.len() > first.unwrap());
            assert!(matches!(
                flattened.expressions[first.unwrap()],
                parser::expressions::Expr::PostFix(
                    parser::expressions::PostFix::WithPointerToMember { .. }
                )
            ));
        }
        {
            let src = r#"me->hi.meow(1, 3, 5)"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = parser::Flattened::new();
            let (_, postfix_arg_expr_list) =
                expressions::parse_expressions(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(
                postfix_arg_expr_list,
                parser::expressions::Expr::PostFix(
                    parser::expressions::PostFix::WithFunctionCall { .. }
                )
            ));
            let parser::expressions::Expr::PostFix(
                parser::expressions::PostFix::WithFunctionCall {
                    argument_expr_idx,
                    first,
                },
            ) = postfix_arg_expr_list
            else {
                unreachable!()
            };
            assert!(flattened.argument_expr_list_list[argument_expr_idx].len() == 3);
            assert!(flattened.expressions.len() > first.unwrap());
            assert!(matches!(
                flattened.expressions[first.unwrap()],
                parser::expressions::Expr::PostFix(parser::expressions::PostFix::WithMember { .. })
            ));
            let parser::expressions::Expr::PostFix(parser::expressions::PostFix::WithMember {
                first,
                member_ident_key,
            }) = flattened.expressions[first.unwrap()]
            else {
                unreachable!()
            };
            assert!(matches!(
                flattened.expressions[first.unwrap()],
                parser::expressions::Expr::PostFix(
                    parser::expressions::PostFix::WithPointerToMember { .. }
                )
            ));
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
