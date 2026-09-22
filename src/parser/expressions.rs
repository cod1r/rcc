use crate::error::*;
use crate::lexer::*;
use crate::parser::declarations::*;
use crate::parser::statements::*;
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

#[derive(Debug, Copy, Clone)]
pub enum UnaryType {
    Expr {
        op: TokenType,
        first: ExpressionIndex,
    },
    SizeOfTypeName(TypeNameIndex),
    SizeOfUnaryExpr(ExpressionIndex),
    AlignOf(TypeNameIndex),
}

#[derive(Debug, Copy, Clone)]
pub struct Cast {
    type_name: TypeNameIndex,
    cast_expr: ExpressionIndex,
}

#[derive(Debug, Copy, Clone)]
pub enum PostFix {
    WithSubscript {
        first: ExpressionIndex,
        subscript: ExpressionIndex,
    },
    WithFunctionCall {
        first: ExpressionIndex,
        argument_expr_idx: ExpressionIndex,
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

#[derive(Debug, Copy, Clone)]
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

#[derive(Debug, Copy, Clone)]
pub struct Conditional {
    pub first: ExpressionIndex,
    pub second: ExpressionIndex,
    pub third: ExpressionIndex,
}

#[derive(Debug, Copy, Clone)]
pub enum PrimaryType {
    Token(Token),
    Expr(ExpressionIndex),
}

#[derive(Debug, Copy, Clone)]
pub enum Expr {
    Binary {
        r#type: BinaryExprType,
        first: ExpressionIndex,
        second: ExpressionIndex,
    },
    Conditional(Conditional),
    Unary(UnaryType),
    Cast(Cast),
    PostFix(PostFix),
    Primary(PrimaryType),
}

macro_rules! expression_operators {
    () => {
        TokenType::PLUS
            | TokenType::MINUS
            | TokenType::MULT
            | TokenType::DIV
            | TokenType::MODULO
            | TokenType::BITSHIFT_LEFT
            | TokenType::BITSHIFT_RIGHT
            | TokenType::LESS_THAN
            | TokenType::LESS_THAN_EQ
            | TokenType::GREATER_THAN
            | TokenType::GREATER_THAN_EQ
            | TokenType::EQ_BOOL
            | TokenType::NOT_EQ_BOOL
            | TokenType::AND_BIT
            | TokenType::XOR_BIT
            | TokenType::OR_BIT
            | TokenType::AND_BOOL
            | TokenType::OR_BOOL
            | TokenType::CLOSE_PAR
            | TokenType::QUESTION_MARK
            | TokenType::COLON
            | TokenType::ASSIGNMENT
            | TokenType::MULT_ASSIGN
            | TokenType::DIV_ASSIGN
            | TokenType::MODULO_ASSIGN
            | TokenType::ADD_ASSIGN
            | TokenType::SUB_ASSIGN
            | TokenType::L_SHIFT_BIT_ASSIGN
            | TokenType::R_SHIFT_BIT_ASSIGN
            | TokenType::AND_BIT_ASSIGN
            | TokenType::XOR_BIT_ASSIGN
            | TokenType::OR_BIT_ASSIGN
            | TokenType::INCREMENT
            | TokenType::DECREMENT
            | TokenType::DOT
            | TokenType::ARROW
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
        TokenType::INCREMENT | TokenType::DECREMENT | TokenType::SIZEOF | TokenType::_ALIGNOF
    };
}

macro_rules! unary_ops {
    () => {
        TokenType::AMPERSAND
            | TokenType::ASTERISK
            | TokenType::PLUS
            | TokenType::MINUS
            | TokenType::TILDE
            | TokenType::NOT_BOOL
            | TokenType::INCREMENT
            | TokenType::DECREMENT
    };
}

macro_rules! assignment_ops {
    () => {
        TokenType::ASSIGNMENT
            | TokenType::MODULO_ASSIGN
            | TokenType::DIV_ASSIGN
            | TokenType::MULT_ASSIGN
            | TokenType::ADD_ASSIGN
            | TokenType::SUB_ASSIGN
            | TokenType::L_SHIFT_BIT_ASSIGN
            | TokenType::R_SHIFT_BIT_ASSIGN
            | TokenType::AND_BIT_ASSIGN
            | TokenType::XOR_BIT_ASSIGN
            | TokenType::OR_BIT_ASSIGN
    };
}

macro_rules! postfix_ops {
    () => {
TokenType::OPEN_SQR
                            | TokenType::OPEN_PAR
                            | TokenType::DOT
                            | TokenType::ARROW
                            | TokenType::INCREMENT
                            | TokenType::DECREMENT
    }
}

fn parse_primary_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Expr, String> {
    if matches!(
        tokens.get(*index),
        Some(Token {
            r#type: TokenType::OPEN_PAR,
            ..
        })
    ) {
        *index += 1;
        consume_whitespace(tokens, index);

        let expr = parse_expressions(tokens, index, flattened, str_maps)?;

        consume_whitespace(tokens, index);
        expected_token(tokens, index, TokenType::CLOSE_PAR, "Expected ')'")?;

        flattened.expressions.push(expr);
        return Ok(Expr::Primary(PrimaryType::Expr(
            flattened.expressions.len() - 1,
        )));
    }
    if !matches!(
        tokens.get(*index),
        Some(Token {
            r#type: primary_tokens!(),
            ..
        })
    ) {
        match tokens.get(*index) {
            Some(Token {
                location: Some(Location { line, column }),
                ..
            }) => {
                return Err(error("Expected primary token", *line, *column));
            }
            Some(Token { location: None, .. }) => unreachable!(
                "Inserted tokens from preprocessing should at least have the line number"
            ),
            None => {
                return Err("Expected primary token".to_string());
            }
        }
    }
    let t = tokens[*index];
    *index += 1;
    Ok(Expr::Primary(PrimaryType::Token(t)))
}

fn parse_argument_expression_list(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Expr, String> {
    let assignment_expr = parse_assignment_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);

    let mut comma_expr = None;
    loop {
        // Because I parse left to right, and because the comma operator comes after the expression,
        // I have to loop until there isn't another comma token.
        match tokens.get(*index) {
            Some(
                t @ Token {
                    r#type: TokenType::COMMA,
                    ..
                },
            ) => {
                *index += 1;
                consume_whitespace(tokens, index);
                let second_operand =
                    parse_assignment_expression(tokens, index, flattened, str_maps)?;
                flattened.expressions.push(if comma_expr.is_none() {
                    assignment_expr
                } else {
                    comma_expr.unwrap()
                });
                let first = flattened.expressions.len() - 1;
                flattened.expressions.push(second_operand);
                let second = flattened.expressions.len() - 1;
                comma_expr = Some(Expr::Binary {
                    r#type: BinaryExprType::ArgExprList,
                    first,
                    second,
                })
            }
            _ => return Ok(assignment_expr),
        }
        consume_whitespace(tokens, index);
        if !matches!(
            tokens.get(*index),
            Some(Token {
                r#type: TokenType::COMMA,
                ..
            })
        ) {
            break Ok(comma_expr.expect("This should have something"));
        }
    }
}

fn parse_postfix_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
    last_postfix_expression: Option<Expr>,
) -> Result<Expr, String> {
    match tokens.get(*index) {
        Some(Token {
            r#type: TokenType::OPEN_SQR,
            location: Some(Location { line, column }),
        }) => {
            *index += 1;
            if last_postfix_expression.is_none() {
                return Err(error("Expected expression before", *line, *column));
            }
            flattened.expressions.push(last_postfix_expression.unwrap());
            let first = flattened.expressions.len() - 1;
            consume_whitespace(tokens, index);
            let subscript = parse_expressions(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(subscript);
            let subscript_idx = flattened.expressions.len() - 1;
            consume_whitespace(tokens, index);
            expected_token(tokens, index, TokenType::CLOSE_SQR, "Expected ']'")?;
            return Ok(Expr::PostFix(PostFix::WithSubscript {
                first,
                subscript: subscript_idx,
            }));
        }
        Some(Token {
            r#type: TokenType::OPEN_PAR,
            ..
        }) => {
            let idx_of_open_par = *index;
            *index += 1;
            if check_if_not_declaration_token(tokens, index) && last_postfix_expression.is_none() {
                *index = idx_of_open_par;
                return parse_primary_expression(tokens, index, flattened, str_maps);
            }
            if last_postfix_expression.is_none() {
                // typename and initializer postfix expression
                let type_name = parse_type_names(tokens, index, flattened, str_maps)?;
                flattened.type_names.push(type_name);
                consume_whitespace(tokens, index);
                expected_token(tokens, index, TokenType::CLOSE_PAR, "Expected ')'")?;
                consume_whitespace(tokens, index);
                expected_token(tokens, index, TokenType::OPEN_CURLY, "Expected '{'")?;
                let initializer_list = parse_initializer_list(tokens, index, flattened, str_maps)?;
                flattened.initializer_lists.push(initializer_list);
                return Ok(Expr::PostFix(PostFix::WithTypeNameInitializerList {
                    type_name: flattened.type_names.len() - 1,
                    initializer_list: flattened.initializer_lists.len() - 1,
                }));
            } else {
                // postfix argument expression list expression
                let arg_expr_list =
                    parse_argument_expression_list(tokens, index, flattened, str_maps)?;
                consume_whitespace(tokens, index);
                expected_token(tokens, index, TokenType::CLOSE_PAR, "Expected ')'");
                let Some(p) = last_postfix_expression else {
                    unreachable!()
                };
                flattened.expressions.push(p);
                let first = flattened.expressions.len() - 1;
                flattened.expressions.push(arg_expr_list);
                let argument_expr_idx = flattened.expressions.len() - 1;
                return Ok(Expr::PostFix(PostFix::WithFunctionCall {
                    first,
                    argument_expr_idx,
                }));
            }
        }
        Some(
            t @ Token {
                r#type: TokenType::INCREMENT | TokenType::DECREMENT,
                location: Some(Location { line, column }),
            },
        ) => {
            *index += 1;
            if last_postfix_expression.is_none() {
                return Err(error("Expected expression before", *line, *column));
            }
            let Token { r#type, .. } = t;
            flattened.expressions.push(last_postfix_expression.unwrap());
            return Ok(Expr::PostFix(PostFix::WithIncrementDecrement {
                op: *r#type,
                first: flattened.expressions.len() - 1,
            }));
        }
        Some(
            t @ Token {
                r#type: TokenType::ARROW | TokenType::DOT,
                location: Some(Location { line, column }),
            },
        ) => {
            *index += 1;
            if last_postfix_expression.is_none() {
                return Err(error("Expected expression before", *line, *column));
            }
            consume_whitespace(tokens, index);
            expected_identifier(tokens, str_maps, index)?;
            let Token {
                r#type: TokenType::IDENT { str_map_key },
                ..
            } = tokens[*index - 1]
            else {
                unreachable!()
            };
            flattened.expressions.push(last_postfix_expression.unwrap());
            return Ok(Expr::PostFix(match t.r#type {
                TokenType::ARROW => PostFix::WithPointerToMember {
                    first: flattened.expressions.len() - 1,
                    member_ident_key: str_map_key,
                },
                TokenType::DOT => PostFix::WithMember {
                    first: flattened.expressions.len() - 1,
                    member_ident_key: str_map_key,
                },
                _ => unreachable!(),
            }));
        }
        _ => parse_primary_expression(tokens, index, flattened, str_maps),
    }
}

fn parse_unary_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Expr, String> {
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type: unary_ops!(),
                ..
            },
        ) => {
            *index += 1;
            consume_whitespace(tokens, index);
            let cast_expr = parse_cast_expression(tokens, index, flattened, str_maps)?;
            consume_whitespace(tokens, index);

            // I need to check for a postfix operator after because postfix operators have a higher precedence than unary ops.
            // If I don't then the postfix op gets ignored because of the left to right parsing...
            if matches!(tokens.get(*index), Some(Token{r#type:postfix_ops!(), .. })) {
                let postfix_expr = parse_postfix_expression(tokens, index, flattened, str_maps, Some(cast_expr))?;
                flattened.expressions.push(postfix_expr);
            } else {
                // need this branch because parse_postfix_expression pushes the passed in expression to the flattened structure as well.
                flattened.expressions.push(cast_expr);
            }

            Ok(Expr::Unary(UnaryType::Expr {
                op: t.r#type,
                first: flattened.expressions.len() - 1,
            }))
        }
        Some(Token {
            r#type: TokenType::SIZEOF,
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            match tokens.get(*index) {
                Some(Token {
                    r#type: TokenType::OPEN_PAR,
                    ..
                }) => {
                    *index += 1;
                    consume_whitespace(tokens, index);
                    let type_name = parse_type_names(tokens, index, flattened, str_maps)?;
                    flattened.type_names.push(type_name);
                    Ok(Expr::Unary(UnaryType::SizeOfTypeName(
                        flattened.type_names.len() - 1,
                    )))
                }
                _ => {
                    // I don't increment index here because that token is part of the unary expr
                    consume_whitespace(tokens, index);
                    let unary_expr = parse_unary_expression(tokens, index, flattened, str_maps)?;
                    flattened.expressions.push(unary_expr);
                    Ok(Expr::Unary(UnaryType::SizeOfUnaryExpr (
                        flattened.expressions.len() - 1,
                    )))
                }
            }
        }
        Some(Token {
            r#type: TokenType::_ALIGNOF,
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            expected_token(tokens, index, TokenType::OPEN_PAR, "Expected '('")?;
            consume_whitespace(tokens, index);
            let type_name = parse_type_names(tokens, index, flattened, str_maps)?;
            flattened.type_names.push(type_name);
            Ok(Expr::Unary(UnaryType::AlignOf(
                flattened.type_names.len() - 1,
            )))
        }
        None => return Err("Expected unary op or postfix expr".to_string()),
        _ => {
            // Due to the nature of how I parse from left to right, and how postfix expressions are by nature,
            // it's quite tricky to know if something is a postfix expression until the parsing gets past the primary expression.
            // Therefore, I will keep track of the primary expression in the caller and pass it back into parse_postfix_expression function to
            // get the proper postfix expression.
            let mut postfix_expr_temp = None;
            loop {
                let postfix_expr = parse_postfix_expression(
                    tokens,
                    index,
                    flattened,
                    str_maps,
                    postfix_expr_temp,
                )?;
                consume_whitespace(tokens, index);
                if !matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: postfix_ops!(),
                        ..
                    })
                ) {
                    break Ok(postfix_expr);
                }
                assert!(matches!(postfix_expr, Expr::Primary(_) | Expr::PostFix(_)));
                postfix_expr_temp = Some(postfix_expr);
            }
        }
    }
}

fn check_if_not_declaration_token(tokens: &[Token], index: &usize) -> bool {
    // At this point, I don't know if this will be a primary expression or a cast expression, so I
    // have to check if it's a declaration token, (ex: `int`). If not, then it is a primary expression so
    // I reset the index back to the location of the '('.
    tokens.get(*index).is_some() && !is_declaration_token(tokens[*index])
}

fn parse_cast_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Expr, String> {
    match tokens.get(*index) {
        Some(Token {
            r#type: TokenType::OPEN_PAR,
            ..
        }) => {
            let open_par_idx = *index;
            *index += 1;
            consume_whitespace(tokens, index);

            if check_if_not_declaration_token(tokens, index) {
                *index = open_par_idx;
                return parse_unary_expression(tokens, index, flattened, str_maps);
            }

            let type_name = parse_type_names(tokens, index, flattened, str_maps)?;
            expected_token(tokens, index, TokenType::CLOSE_PAR, "Expected ')'")?;
            consume_whitespace(tokens, index);
            let cast_expression = parse_cast_expression(tokens, index, flattened, str_maps)?;
            flattened.type_names.push(type_name);
            flattened.expressions.push(cast_expression);
            Ok(Expr::Cast(Cast {
                type_name: flattened.type_names.len() - 1,
                cast_expr: flattened.expressions.len() - 1,
            }))
        }
        _ => parse_unary_expression(tokens, index, flattened, str_maps),
    }
}

fn parse_multiplicative_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Expr, String> {
    let cast_expr = parse_cast_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type: TokenType::ASTERISK | TokenType::DIV | TokenType::MODULO,
                ..
            },
        ) => {
            *index += 1;
            consume_whitespace(tokens, index);
            let second_operand = parse_cast_expression(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(cast_expr);
            let first = flattened.expressions.len() - 1;
            flattened.expressions.push(second_operand);
            let second = flattened.expressions.len() - 1;
            let r#type = match t.r#type {
                TokenType::ASTERISK => BinaryExprType::Mult,
                TokenType::DIV => BinaryExprType::Div,
                TokenType::MODULO => BinaryExprType::Mod,
                _ => unreachable!(),
            };
            Ok(Expr::Binary {
                r#type,
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
    str_maps: &mut ByteVecMaps,
) -> Result<Expr, String> {
    let mult_expr = parse_multiplicative_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type: TokenType::PLUS | TokenType::MINUS,
                ..
            },
        ) => {
            *index += 1;
            consume_whitespace(tokens, index);
            let second_operand =
                parse_multiplicative_expression(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(mult_expr);
            let first = flattened.expressions.len() - 1;
            flattened.expressions.push(second_operand);
            let second = flattened.expressions.len() - 1;
            Ok(Expr::Binary {
                r#type: if t.r#type == TokenType::PLUS {
                    BinaryExprType::Add
                } else {
                    BinaryExprType::Sub
                },
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
    str_maps: &mut ByteVecMaps,
) -> Result<Expr, String> {
    let add_expr = parse_additive_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type: TokenType::BITSHIFT_LEFT | TokenType::BITSHIFT_RIGHT,
                ..
            },
        ) => {
            *index += 1;
            let second_operand = parse_additive_expression(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(add_expr);
            let first = flattened.expressions.len() - 1;
            flattened.expressions.push(second_operand);
            let second = flattened.expressions.len() - 1;
            let r#type = match t.r#type {
                TokenType::BITSHIFT_LEFT => BinaryExprType::BitShiftLeft,
                TokenType::BITSHIFT_RIGHT => BinaryExprType::BitShiftRight,
                _ => unreachable!(),
            };
            Ok(Expr::Binary {
                r#type,
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
    str_maps: &mut ByteVecMaps,
) -> Result<Expr, String> {
    let shift_expr = parse_shift_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type:
                    TokenType::LESS_THAN
                    | TokenType::LESS_THAN_EQ
                    | TokenType::GREATER_THAN
                    | TokenType::GREATER_THAN_EQ,
                ..
            },
        ) => {
            *index += 1;
            let second_operand = parse_shift_expression(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(shift_expr);
            let first = flattened.expressions.len() - 1;
            flattened.expressions.push(second_operand);
            let second = flattened.expressions.len() - 1;
            let r#type = match t.r#type {
                TokenType::LESS_THAN => BinaryExprType::LessThan,
                TokenType::LESS_THAN_EQ => BinaryExprType::LessThanEq,
                TokenType::GREATER_THAN => BinaryExprType::LessThan,
                TokenType::GREATER_THAN_EQ => BinaryExprType::LessThanEq,
                _ => unreachable!(),
            };
            Ok(Expr::Binary {
                r#type,
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
    str_maps: &mut ByteVecMaps,
) -> Result<Expr, String> {
    let relational_expr = parse_relational_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type: TokenType::NOT_EQ_BOOL | TokenType::EQ_BOOL,
                ..
            },
        ) => {
            *index += 1;
            consume_whitespace(tokens, index);
            let second_operand = parse_relational_expression(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(relational_expr);
            let first = flattened.expressions.len() - 1;
            flattened.expressions.push(second_operand);
            let second = flattened.expressions.len() - 1;
            Ok(Expr::Binary {
                r#type: if t.r#type == TokenType::NOT_EQ_BOOL {
                    BinaryExprType::NotEq
                } else {
                    BinaryExprType::Eq
                },
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
    str_maps: &mut ByteVecMaps,
) -> Result<Expr, String> {
    let eq_expr = parse_equality_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type: TokenType::AMPERSAND,
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
                r#type: BinaryExprType::BitAND,
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
    str_maps: &mut ByteVecMaps,
) -> Result<Expr, String> {
    let bit_and_expr = parse_bitwise_AND_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type: TokenType::XOR_BIT,
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
                r#type: BinaryExprType::BitXOR,
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
    str_maps: &mut ByteVecMaps,
) -> Result<Expr, String> {
    let bit_xor_expr = parse_bitwise_XOR_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type: TokenType::OR_BIT,
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
                r#type: BinaryExprType::BitOR,
                first,
                second,
            })
        }
        _ => Ok(bit_xor_expr),
    }
}

fn parse_logical_AND_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Expr, String> {
    let bit_or_expr = parse_bitwise_OR_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type: TokenType::AND_BOOL,
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
                r#type: BinaryExprType::LogAND,
                first,
                second,
            })
        }
        _ => Ok(bit_or_expr),
    }
}

fn parse_logical_OR_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Expr, String> {
    let logical_and_expr = parse_logical_AND_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type: TokenType::OR_BOOL,
                ..
            },
        ) => {
            *index += 1;
            consume_whitespace(tokens, index);
            let second_operand = parse_logical_AND_expression(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(logical_and_expr);
            let first = flattened.expressions.len() - 1;
            flattened.expressions.push(second_operand);
            let second = flattened.expressions.len() - 1;
            Ok(Expr::Binary {
                r#type: BinaryExprType::LogOR,
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
    str_maps: &mut ByteVecMaps,
) -> Result<Expr, String> {
    let first_expr = parse_logical_OR_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    if matches!(
        tokens.get(*index),
        Some(Token {
            r#type: TokenType::QUESTION_MARK,
            ..
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
        expected_token(tokens, index, TokenType::COLON, "Expected ':'")?;
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
    str_maps: &mut ByteVecMaps,
) -> Result<Expr, String> {
    let conditional = parse_conditional_expression(tokens, index, flattened, str_maps)?;
    if *index < tokens.len() {
        consume_whitespace(tokens, index);
        match tokens.get(*index) {
            Some(
                t @ Token {
                    r#type: assignment_ops!(),
                    location: Some(Location{line,column})
                },
            ) => {
                if !matches!(
                    conditional,
                    Expr::Unary(_) | Expr::PostFix(_) | Expr::Primary(_)
                ) {
                    return Err(error("Expected unary expression before assignment", *line, *column));
                }
                *index += 1;
                consume_whitespace(tokens, index);

                flattened.expressions.push(conditional);
                let first = flattened.expressions.len() - 1;

                // this basically attempts to transform `var += 1` to `var = var + 1`
                let assignment_expr =
                    parse_assignment_expression(tokens, index, flattened, str_maps)?;
                flattened.expressions.push(assignment_expr);
                let mut second = flattened.expressions.len() - 1;

                if t.r#type != TokenType::ASSIGNMENT {
                    let transformed = Expr::Binary {
                        r#type: match t.r#type {
                            TokenType::MODULO_ASSIGN => BinaryExprType::Mod,
                            TokenType::DIV_ASSIGN => BinaryExprType::Div,
                            TokenType::MULT_ASSIGN => BinaryExprType::Mult,
                            TokenType::ADD_ASSIGN => BinaryExprType::Add,
                            TokenType::SUB_ASSIGN => BinaryExprType::Sub,
                            TokenType::L_SHIFT_BIT_ASSIGN => BinaryExprType::BitShiftLeft,
                            TokenType::R_SHIFT_BIT_ASSIGN => BinaryExprType::BitShiftRight,
                            TokenType::AND_BIT_ASSIGN => BinaryExprType::BitAND,
                            TokenType::XOR_BIT_ASSIGN => BinaryExprType::BitXOR,
                            TokenType::OR_BIT_ASSIGN => BinaryExprType::BitOR,
                            _ => unreachable!(),
                        },
                        first,
                        second,
                    };
                    flattened.expressions.push(transformed);
                    second = flattened.expressions.len() - 1;
                }

                return Ok(Expr::Binary {
                    r#type: BinaryExprType::Assignment,
                    first,
                    second,
                });
            }
            Some(Token {
                location: Some(Location { line, column }),
                ..
            }) => return Ok(conditional),
            _ => return Err("Expected assignment operator".to_string()),
        }
    }
    Ok(conditional)
}

pub fn parse_comma_expression(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Expr, String> {
    let assignment_expr = parse_assignment_expression(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);

    let mut comma_expr = None;
    loop {
        // Because I parse left to right, and because the comma operator comes after the expression,
        // I have to loop until there isn't another comma token.
        match tokens.get(*index) {
            Some(
                t @ Token {
                    r#type: TokenType::COMMA,
                    ..
                },
            ) => {
                *index += 1;
                consume_whitespace(tokens, index);
                let second_operand =
                    parse_assignment_expression(tokens, index, flattened, str_maps)?;
                flattened.expressions.push(if comma_expr.is_none() {
                    assignment_expr
                } else {
                    comma_expr.unwrap()
                });
                let first = flattened.expressions.len() - 1;
                flattened.expressions.push(second_operand);
                let second = flattened.expressions.len() - 1;
                comma_expr = Some(Expr::Binary {
                    r#type: BinaryExprType::Comma,
                    first,
                    second,
                })
            }
            _ => return Ok(assignment_expr),
        }
        consume_whitespace(tokens, index);
        if !matches!(
            tokens.get(*index),
            Some(Token {
                r#type: TokenType::COMMA,
                ..
            })
        ) {
            break Ok(comma_expr.expect("This should have something"));
        }
    }
}

pub fn parse_expressions(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Expr, String> {
    parse_comma_expression(tokens, index, flattened, str_maps)
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
            TokenType::ASSIGNMENT
                | TokenType::INCREMENT
                | TokenType::DECREMENT
                | TokenType::OPEN_CURLY
                | TokenType::CLOSE_CURLY
                | TokenType::OPEN_SQR
                | TokenType::CLOSE_SQR
                | TokenType::CONSTANT_DEC_FLOAT { .. }
                | TokenType::CONSTANT_HEXA_FLOAT { .. }
                | TokenType::COMMA
                | TokenType::StringLiteral { .. }
                | TokenType::ARROW
                | TokenType::ADD_ASSIGN
                | TokenType::DIV_ASSIGN
                | TokenType::SUB_ASSIGN
                | TokenType::MULT_ASSIGN
                | TokenType::MODULO_ASSIGN
                | TokenType::AND_BIT_ASSIGN
                | TokenType::OR_BIT_ASSIGN
                | TokenType::XOR_BIT_ASSIGN
                | TokenType::L_SHIFT_BIT_ASSIGN
                | TokenType::R_SHIFT_BIT_ASSIGN
        )
    }) {
        let Some(bytes) = not_allowed_t.to_byte_vec(str_maps) else {
            unreachable!()
        };
        let msg = format!(
            "Cannot have {} in constant expression",
            String::from_utf8(bytes).unwrap()
        );
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
                PrimaryType::Expr(e) => recursive_eval(&expressions[*e], str_maps, expressions),
                PrimaryType::Token(t) => {
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
            }
        }
        Expr::PostFix(_) => {
            todo!("ERROR HERE")
        }
        Expr::Unary(u) => {
            let UnaryType::Expr { op, first } = u else {
                unreachable!()
            };
            let first = *first;
            let op = *op;
            match op {
                TokenType::PLUS => Ok(recursive_eval(&expressions[first], str_maps, expressions)?),
                TokenType::MINUS => {
                    Ok(-recursive_eval(&expressions[first], str_maps, expressions)?)
                }
                TokenType::TILDE => {
                    Ok(!recursive_eval(&expressions[first], str_maps, expressions)?)
                }
                TokenType::NOT_BOOL => Ok(
                    if recursive_eval(&expressions[first], str_maps, expressions)? == 0 {
                        1
                    } else {
                        0
                    },
                ),
                _ => {
                    unreachable!()
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
            let first = *first;
            let second = *second;
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
                _ => todo!("Unhandled binary op"),
            }
        }
        Expr::Conditional(c) => {
            let first = c.first else { unreachable!() };
            let second = c.second else { unreachable!() };
            let third = c.third else { unreachable!() };
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
    use crate::lexer::*;
    use crate::parser::expressions::*;
    use crate::parser::*;

    #[test]
    fn eval_expression_temp() -> Result<(), String> {
        let src = r##"1 + 2 * 4"##.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression_integer_when_preprocess(&tokens, &mut 0, &mut str_maps);
        assert!(res == Ok(9));
        Ok(())
    }

    #[test]
    fn eval_expression_test_primary() -> Result<(), String> {
        {
            let src = r##"((((1))))"##.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
            let res =
                eval_constant_expression_integer_when_preprocess(&tokens, &mut 0, &mut str_maps)?;
            assert_eq!(res != 0, true, "((((1))))");
        }
        {
            let src = r##"(((((1))))"##.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
            let res =
                eval_constant_expression_integer_when_preprocess(&tokens, &mut 0, &mut str_maps);
            match res {
                Err(_) => {}
                Ok(_) => return Err(String::from("unbalanced parentheses not caught")),
            }
        }
        {
            let src = r##"0 - (1 + 1)"##.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
            let res =
                eval_constant_expression_integer_when_preprocess(&tokens, &mut 0, &mut str_maps)?;
            assert_eq!(res != 0, true, "0 - (1 + 1)");
        }
        Ok(())
    }
    #[test]
    fn eval_expression_test_unary() -> Result<(), String> {
        let src = r##"!1"##.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression_integer_when_preprocess(&tokens, &mut 0, &mut str_maps)?;
        assert_eq!(res == 0, true, "!1");
        let src = r##"--------------1"##.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression_integer_when_preprocess(&tokens, &mut 0, &mut str_maps);
        match res {
            Err(ref e) => println!("{}", e),
            Ok(_) => {}
        }
        assert!(
            res.is_err(),
            "'--' operator not caught in cpp constant expression"
        );
        Ok(())
    }
    #[test]
    fn eval_expression_test_logical_or() -> Result<(), String> {
        let src = r##"1 || 1"##.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression_integer_when_preprocess(&tokens, &mut 0, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 || 1"##.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression_integer_when_preprocess(&tokens, &mut 0, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 || 0"##.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression_integer_when_preprocess(&tokens, &mut 0, &mut str_maps)?;
        assert_eq!(res != 0, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_conditional() -> Result<(), String> {
        let src = r##"1 ? 1 : 0"##.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression_integer_when_preprocess(&tokens, &mut 0, &mut str_maps)?;
        assert!(res != 0);
        let src = r##"(1 + 1 == 3) ? 1 : 0"##.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression_integer_when_preprocess(&tokens, &mut 0, &mut str_maps)?;
        assert!(res == 0);
        let src = r##"~0 ? (1 + 1 == 2) : 0 * 4"##.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression_integer_when_preprocess(&tokens, &mut 0, &mut str_maps)?;
        assert!(res == 1);
        let src = r##"0 ? 0 : 1 * 4"##.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression_integer_when_preprocess(&tokens, &mut 0, &mut str_maps)?;
        assert!(res == 4);
        let src = r##"0 ? 0 : !(1 * 4)"##.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression_integer_when_preprocess(&tokens, &mut 0, &mut str_maps)?;
        assert!(res == 0);
        Ok(())
    }
    #[test]
    fn parse_expressions_test_cast() -> Result<(), String> {
        let src = r#"(int)1"#.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let tokens = lexer(&src.to_vec(), false, &mut str_maps)?;
        let mut flattened = Flattened::new();
        let cast_expr = parse_expressions(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
        assert!(matches!(cast_expr, Expr::Cast(_)));
        let Expr::Cast(c) = cast_expr else {
            unreachable!()
        };
        assert!(matches!(
            flattened.expressions[c.cast_expr],
            Expr::Primary(_)
        ));
        let Expr::Primary(PrimaryType::Token(t)) = flattened.expressions[c.cast_expr] else {
            unreachable!()
        };
        assert!(matches!(
            t,
            Token {
                r#type: TokenType::CONSTANT_DEC_INT { .. },
                ..
            }
        ));
        let Token {
            r#type: TokenType::CONSTANT_DEC_INT { value_key, .. },
            ..
        } = t
        else {
            unreachable!()
        };
        assert!(str_maps.key_to_byte_vec[value_key] == *b"1");
        Ok(())
    }
    #[test]
    fn parse_expressions_test_additive_cast() -> Result<(), String> {
        let src = r#"1 + (int)1"#.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let tokens = lexer(&src.to_vec(), false, &mut str_maps)?;
        let mut flattened = Flattened::new();
        let add = parse_expressions(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
        assert!(matches!(
            add,
            Expr::Binary {
                r#type: BinaryExprType::Add,
                ..
            }
        ));
        let Expr::Binary { first, second, .. } = add else {
            unreachable!()
        };
        assert!(matches!(flattened.expressions[first], Expr::Primary(_)));
        assert!(matches!(flattened.expressions[second], Expr::Cast(_)));
        let Expr::Primary(PrimaryType::Token(t)) = flattened.expressions[first] else {
            unreachable!()
        };
        assert!(matches!(
            t,
            Token {
                r#type: TokenType::CONSTANT_DEC_INT { .. },
                ..
            }
        ));
        let Token {
            r#type: TokenType::CONSTANT_DEC_INT { value_key, .. },
            ..
        } = t
        else {
            unreachable!()
        };
        assert!(str_maps.key_to_byte_vec[value_key] == *b"1");
        let Expr::Cast(c) = flattened.expressions[second] else {
            unreachable!()
        };
        let c_idx = c.cast_expr else { unreachable!() };
        assert!(matches!(flattened.expressions[c_idx], Expr::Primary(_)));
        let Expr::Primary(PrimaryType::Token(t)) = flattened.expressions[c_idx] else {
            unreachable!()
        };
        assert!(matches!(
            t,
            Token {
                r#type: TokenType::CONSTANT_DEC_INT { .. },
                ..
            }
        ));
        let Token {
            r#type: TokenType::CONSTANT_DEC_INT { value_key, .. },
            ..
        } = t
        else {
            unreachable!()
        };
        assert!(str_maps.key_to_byte_vec[value_key] == *b"1");
        let type_name_index = c.type_name else {
            unreachable!()
        };
        assert!(flattened.type_names.get(type_name_index).is_some());
        let Some(type_name) = flattened.type_names.get(type_name_index) else {
            unreachable!()
        };
        assert!(matches!(
            type_name.specifier_qualifier_list.type_specifiers.get(0),
            Some(TypeSpecifier::Int)
        ));
        Ok(())
    }
    #[test]
    fn parse_expressions_test_additive_unary_cast() -> Result<(), String> {
        let src = r#"1 + !(int)1"#.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let tokens = lexer(&src.to_vec(), false, &mut str_maps)?;
        let mut flattened = Flattened::new();
        let add = parse_expressions(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
        assert!(matches!(
            add,
            Expr::Binary {
                r#type: BinaryExprType::Add,
                ..
            }
        ));
        let Expr::Binary { first, second, .. } = add else {
            unreachable!()
        };
        assert!(matches!(flattened.expressions[first], Expr::Primary(_)));
        let Expr::Primary(PrimaryType::Token(t)) = flattened.expressions[first] else {
            unreachable!()
        };
        assert!(matches!(
            t,
            Token {
                r#type: TokenType::CONSTANT_DEC_INT { .. },
                ..
            }
        ));
        let Token {
            r#type: TokenType::CONSTANT_DEC_INT { value_key, suffix },
            ..
        } = t
        else {
            unreachable!()
        };
        assert!(str_maps.key_to_byte_vec[value_key] == *b"1");
        assert!(matches!(flattened.expressions[second], Expr::Unary(_)));
        let Expr::Unary(UnaryType::Expr { op, first }) = flattened.expressions[second] else {
            unreachable!()
        };
        assert!(matches!(op, TokenType::NOT_BOOL));
        let cast_idx = first;
        assert!(matches!(flattened.expressions[cast_idx], Expr::Cast(_)));
        let Expr::Cast(c) = flattened.expressions[cast_idx] else {
            unreachable!()
        };
        let p_idx = c.cast_expr;
        assert!(matches!(flattened.expressions[p_idx], Expr::Primary(_)));
        let Expr::Primary(PrimaryType::Token(t)) = flattened.expressions[p_idx] else {
            unreachable!()
        };
        assert!(matches!(
            t,
            Token {
                r#type: TokenType::CONSTANT_DEC_INT { .. },
                ..
            }
        ));
        let Token {
            r#type: TokenType::CONSTANT_DEC_INT { value_key, suffix },
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
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = Flattened::new();
            let add = parse_expressions(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(
                add,
                Expr::Binary {
                    r#type: BinaryExprType::Add,
                    ..
                }
            ));

            let Expr::Binary { first, .. } = add else {
                unreachable!()
            };
            assert!(matches!(flattened.expressions[first], Expr::PostFix(_)));
            let Expr::PostFix(p) = flattened.expressions[first] else {
                unreachable!()
            };
            assert!(matches!(p, PostFix::WithIncrementDecrement { .. }));
            let PostFix::WithIncrementDecrement { first, op } = p else {
                unreachable!()
            };
            assert!(matches!(flattened.expressions[first], Expr::Primary(_)));
            assert!(matches!(op, TokenType::DECREMENT));

            let Expr::Binary { second, .. } = add else {
                unreachable!()
            };
            assert!(matches!(flattened.expressions[second], Expr::Primary(_)));
        }
        Ok(())
    }
    #[test]
    fn parse_expressions_with_unary_postfix() -> Result<(), String> {
        {
            let src = r#"!(hi * 3)-- + -1"#.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = Flattened::new();
            let add = parse_expressions(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(
                add,
                Expr::Binary {
                    r#type: BinaryExprType::Add,
                    ..
                }
            ));
            let Expr::Binary { first, second,.. } = add else {
                unreachable!()
            };
            assert!(matches!(flattened.expressions[first], Expr::Unary(_)));
            assert!(matches!(flattened.expressions[second], Expr::Unary(_)));
        }
        {
            let src = r#"!hi-- + -1"#.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = Flattened::new();
            let add = parse_expressions(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(
                add,
                Expr::Binary {
                    r#type: BinaryExprType::Add,
                    ..
                }
            ));
            let Expr::Binary { first, second, .. } = add else {
                unreachable!()
            };
            assert!(matches!(flattened.expressions[first], Expr::Unary(_)));
            assert!(matches!(flattened.expressions[second], Expr::Unary(_)));
        }
        Ok(())
    }
    #[test]
    fn parse_expressions_post_pointer_member() -> Result<(), String> {
        {
            let src = r#"hi->hi2"#.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = Flattened::new();
            let post = parse_expressions(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(
                post,
                Expr::PostFix(PostFix::WithPointerToMember { .. })
            ));
        }
        Ok(())
    }
    #[test]
    fn parse_expressions_postfix_subscript() -> Result<(), String> {
        {
            let src = r#"hi[hi2]"#.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = Flattened::new();
            let post_subscript = parse_expressions(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(
                post_subscript,
                Expr::PostFix(PostFix::WithSubscript { .. })
            ));
        }
        Ok(())
    }
    #[test]
    fn parse_expressions_simple_assignment_test() -> Result<(), String> {
        {
            let src = r#"hi = hi2"#.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = Flattened::new();
            let assign = parse_expressions(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(
                assign,
                Expr::Binary {
                    r#type: BinaryExprType::Assignment,
                    ..
                }
            ));
            let Expr::Binary { first, second, .. } = assign else {
                unreachable!()
            };
            assert!(matches!(flattened.expressions[first], Expr::Primary(_)));
            assert!(matches!(flattened.expressions[second], Expr::Primary(_)));
        }
        Ok(())
    }
    #[test]
    fn parse_expressions_non_unary_left_assignment_test() -> Result<(), String> {
        {
            let src = r#"1 * 1 = hi2"#.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = Flattened::new();
            let assign = parse_expressions(&tokens, &mut 0, &mut flattened, &mut str_maps);
            assert!(assign.is_err());
        }
        Ok(())
    }
    #[test]
    fn parse_expressions_unary_increment_decrement() -> Result<(), String> {
        {
            let src = r#"--variable"#.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = Flattened::new();
            let unary_decrement =
                parse_expressions(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(
                unary_decrement,
                Expr::Unary(UnaryType::Expr { .. })
            ));
            let Expr::Unary(UnaryType::Expr { op, first }) = unary_decrement else {
                unreachable!()
            };
            assert!(matches!(op, TokenType::DECREMENT));
            assert!(flattened.expressions.len() > first);
        }
        Ok(())
    }
    #[test]
    fn parse_expressions_unary_ops_test() -> Result<(), String> {
        {
            let src = r#"!hi"#.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = Flattened::new();
            let unary_not = parse_expressions(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(unary_not, Expr::Unary(UnaryType::Expr { .. })));
            let Expr::Unary(UnaryType::Expr { op, first }) = unary_not else {
                unreachable!()
            };
            assert!(matches!(op, TokenType::NOT_BOOL));
            assert!(flattened.expressions.len() > first);
        }
        {
            let src = r#"sizeof hi"#.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = Flattened::new();
            let unary_sizeof = parse_expressions(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(unary_sizeof, Expr::Unary(UnaryType::SizeOfUnaryExpr(_))));
            let Expr::Unary(UnaryType::SizeOfUnaryExpr(expr_idx)) = unary_sizeof else {
                unreachable!()
            };
            assert!(matches!(flattened.expressions[expr_idx], Expr::Primary(_)));
        }
        {
            let src = r#"sizeof(int)"#.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = Flattened::new();
            let unary_sizeof = parse_expressions(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(unary_sizeof, Expr::Unary(UnaryType::SizeOfTypeName(_))));
            let Expr::Unary(UnaryType::SizeOfTypeName(type_name_idx)) = unary_sizeof else {
                unreachable!()
            };
            assert!(matches!(flattened.type_names[type_name_idx], TypeName { .. }));
        }
        {
            let src = r#"_Alignof (hi)"#.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = Flattened::new();
            let unary_alignof = parse_expressions(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(unary_alignof, Expr::Unary(_)));
            let Expr::Unary(UnaryType::AlignOf(type_name_idx)) = unary_alignof else {
                unreachable!()
            };
            assert!(flattened.type_names.get(type_name_idx).is_some());
        }
        Ok(())
    }
    #[test]
    fn parse_expressions_comma_operator() -> Result<(), String> {
        {
            let src = r#"(1, 3, 5)"#.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = Flattened::new();
            let primary_comma_nested =
                parse_expressions(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(primary_comma_nested, Expr::Primary(_)));
            let Expr::Primary(PrimaryType::Expr(c)) = primary_comma_nested else {
                unreachable!()
            };
            assert!(matches!(
                flattened.expressions[c],
                Expr::Binary {
                    r#type: BinaryExprType::Comma,
                    ..
                }
            ));
            let Expr::Binary {
                r#type: BinaryExprType::Comma,
                first,
                second,
            } = flattened.expressions[c]
            else {
                unreachable!()
            };
            assert!(matches!(
                flattened.expressions[first],
                Expr::Binary {
                    r#type: BinaryExprType::Comma,
                    ..
                }
            ));
            assert!(matches!(flattened.expressions[second], Expr::Primary(_)));
            let Expr::Binary {
                r#type: BinaryExprType::Comma,
                first,
                second,
            } = flattened.expressions[first]
            else {
                unreachable!()
            };
            assert!(matches!(flattened.expressions[first], Expr::Primary(_)));
            assert!(matches!(flattened.expressions[second], Expr::Primary(_)));
        }
        Ok(())
    }
    #[test]
    fn parse_expressions_argument_expression_test() -> Result<(), String> {
        {
            let src = r#"hi(1, 3, 5)"#.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = Flattened::new();
            let postfix_arg_expr_list =
                parse_expressions(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(
                postfix_arg_expr_list,
                Expr::PostFix(PostFix::WithFunctionCall { .. })
            ));
            let Expr::PostFix(PostFix::WithFunctionCall {
                argument_expr_idx,
                first,
            }) = postfix_arg_expr_list
            else {
                unreachable!()
            };
            assert!(matches!(flattened.expressions[first], Expr::Primary(_)));
        }
        {
            let src = r#"me->hi(1, 3, 5)"#.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(&src.to_vec(), false, &mut str_maps)?;
            let mut flattened = Flattened::new();
            let postfix_arg_expr_list =
                parse_expressions(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(
                postfix_arg_expr_list,
                Expr::PostFix(PostFix::WithFunctionCall { .. })
            ));
            let Expr::PostFix(PostFix::WithFunctionCall {
                argument_expr_idx,
                first,
            }) = postfix_arg_expr_list
            else {
                unreachable!()
            };
            assert!(matches!(
                flattened.expressions[first],
                Expr::PostFix(PostFix::WithPointerToMember { .. })
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
