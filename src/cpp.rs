use std::collections::HashMap;

use crate::lexer::{self};
use crate::parser::{self};

macro_rules! case_where_it_could_be_unary_or_additive {
    ($parserTypeVar:ident, $rightExprVar:ident, $token:expr) => {
        if $parserTypeVar.second.is_none() {
            $rightExprVar = Some(parser::Expr::Unary(parser::Unary {
                op: match $token {
                    lexer::Token::PUNCT_PLUS => parser::UnaryOp::Add,
                    lexer::Token::PUNCT_MINUS => parser::UnaryOp::Sub,
                    lexer::Token::PUNCT_NOT_BOOL => parser::UnaryOp::LogicalNOT,
                    lexer::Token::PUNCT_TILDE => parser::UnaryOp::BitNOT,
                    _ => unreachable!(),
                },
                first: None,
            }));
        } else {
            $rightExprVar = Some(parser::Expr::Additive(parser::Additive {
                op: match $token {
                    lexer::Token::PUNCT_PLUS => parser::AdditiveOps::Add,
                    lexer::Token::PUNCT_MINUS => parser::AdditiveOps::Sub,
                    _ => unreachable!(),
                },
                first: None,
                second: None,
            }));
        }
    };
}

#[derive(PartialEq, Debug, Clone)]
pub struct Define {
    pub parameters: Option<Vec<usize>>,
    pub var_arg: bool,
    pub replacement_list: Vec<lexer::Token>,
}
#[derive(Debug)]
struct MacroInterval {
    name_key: usize,
    start: usize,
    len: usize,
}

fn comments(bytes: &[u8]) -> Result<Vec<u8>, String> {
    let mut byte_index = 0;
    let mut comments_removed = Vec::new();
    while byte_index < bytes.len() {
        if bytes[byte_index] == b'\'' || bytes[byte_index] == b'\"' {
            comments_removed.push(bytes[byte_index]);
            let start = bytes[byte_index];
            byte_index += 1;
            while byte_index < bytes.len() && bytes[byte_index] != start {
                comments_removed.push(bytes[byte_index]);
                if byte_index + 1 < bytes.len() && bytes[byte_index] == b'\\' {
                    comments_removed.push(bytes[byte_index + 1]);
                    byte_index += 1;
                }
                byte_index += 1;
            }
            if byte_index < bytes.len() && bytes[byte_index] == start {
                comments_removed.push(bytes[byte_index]);
                byte_index += 1;
            } else {
                return Err(format!("no matching ending quote"));
            }
        } else if byte_index + 1 < bytes.len() {
            if bytes[byte_index] == b'/' && bytes[byte_index + 1] == b'/' {
                comments_removed.push(b' ');
                while byte_index < bytes.len() && bytes[byte_index] != b'\n' {
                    byte_index += 1;
                }
            } else if bytes[byte_index] == b'/' && bytes[byte_index + 1] == b'*' {
                comments_removed.push(b' ');
                while byte_index + 1 < bytes.len()
                    && (bytes[byte_index] != b'*' || bytes[byte_index + 1] != b'/')
                {
                    byte_index += 1;
                }
                if byte_index + 1 < bytes.len()
                    && bytes[byte_index] == b'*'
                    && bytes[byte_index + 1] == b'/'
                {
                    byte_index += 2;
                } else {
                    return Err(format!("no ending */ for block comment"));
                }
            }
        }
        if byte_index < bytes.len() {
            comments_removed.push(bytes[byte_index]);
        }
        byte_index += 1;
    }
    Ok(comments_removed)
}

fn include_directive(
    tokens: &[lexer::Token],
    index: usize,
    include_paths: &[&str],
    defines: &mut HashMap<usize, Define>,
    str_maps: &mut lexer::ByteVecMaps,
    final_tokens: &mut Vec<lexer::Token>,
) -> Result<usize, String> {
    let mut newline_index = index;
    while !matches!(tokens.get(newline_index), Some(lexer::Token::NEWLINE))
        && newline_index < tokens.len()
    {
        newline_index += 1;
    }
    if !matches!(tokens.get(newline_index), Some(lexer::Token::NEWLINE)) {
        return Err(format!("no newline after include directive"));
    }
    let mut include_index = index + 1;
    if matches!(tokens.get(include_index), Some(lexer::Token::WHITESPACE)) {
        include_index += 1;
    }
    let mut file_name = None;
    if matches!(tokens.get(include_index), Some(lexer::Token::IDENT(_))) {
        include_index += 1;
        if matches!(tokens.get(include_index), Some(lexer::Token::WHITESPACE)) {
            include_index += 1;
        }
        match tokens.get(include_index) {
            Some(lexer::Token::PUNCT_LESS_THAN) => {
                include_index += 1;
                let mut punct_greater_than_index = include_index;
                while !matches!(
                    tokens.get(punct_greater_than_index),
                    Some(lexer::Token::PUNCT_GREATER_THAN)
                ) && punct_greater_than_index < tokens.len()
                {
                    punct_greater_than_index += 1;
                }
                if !matches!(
                    tokens.get(punct_greater_than_index),
                    Some(lexer::Token::PUNCT_GREATER_THAN)
                ) {
                    return Err(format!("No '>' for opening '<' in include directive"));
                }
                let file_path_tokens = tokens[include_index..punct_greater_than_index]
                    .iter()
                    .map(|t| t.to_byte_vec(str_maps));
                if file_path_tokens.clone().any(|t_opt| t_opt.is_none()) {
                    return Err(format!(
                        "include directive contains tokens that cannot be stringified"
                    ));
                }
                let string_from_u8 = String::from_utf8_lossy(
                    file_path_tokens
                        .fold(Vec::new(), |mut s: Vec<u8>, t| {
                            s.extend_from_slice(&t.unwrap());
                            s
                        })
                        .as_slice(),
                )
                .to_string();
                file_name = Some(string_from_u8);
            }
            Some(lexer::Token::StringLiteral {
                prefix_key: _,
                sequence_key,
            }) => {
                let sequence = &str_maps.key_to_byte_vec[*sequence_key];
                file_name = Some(String::from_utf8_lossy(sequence).to_string());
            }
            _ => {}
        }
    }
    if let Some(fname) = file_name {
        for path in include_paths {
            let full_path_file = path.to_string() + "/" + &fname;
            match std::fs::read(full_path_file.as_str()) {
                Ok(file_contents) => {
                    let mut tokens_from_file = cpp(file_contents, include_paths, defines, str_maps)?;
                    final_tokens.extend_from_slice(&tokens_from_file);
                    return Ok(newline_index + 1);
                }
                Err(_) => {
                    //eprintln!("fs::read failed for path: {}", full_path_file);
                }
            }
        }
        eprintln!(
            "{fname} not found relative to any paths in {:?}",
            include_paths
        );
    }
    Err(String::from("file not found"))
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
fn right_has_higher_priority(left: &mut parser::Expr, right: &mut parser::Expr) {
    assert!(right.priority() > left.priority());
    match left {
        parser::Expr::Unary(u) => match right {
            parser::Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(u.first.is_none());
            }
            _ => unreachable!(),
        },
        parser::Expr::Multiplicative(m) => match right {
            parser::Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(m.second.is_none());
            }
            parser::Expr::Unary(_) => {
                assert!(m.second.is_none());
            }
            _ => unreachable!(),
        },
        parser::Expr::Additive(a) => match right {
            parser::Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(a.second.is_none());
            }
            parser::Expr::Unary(_) => {
                assert!(a.second.is_none());
            }
            parser::Expr::Multiplicative(m) => {
                assert!(m.first.is_none());
                m.first = a.second.clone();
            }
            _ => unreachable!(),
        },
        parser::Expr::BitShift(bs) => match right {
            parser::Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(bs.second.is_none());
            }
            parser::Expr::Unary(_) => {
                assert!(bs.second.is_none());
            }
            parser::Expr::Multiplicative(m) => {
                assert!(m.first.is_none());
                m.first = bs.second.clone();
            }
            parser::Expr::Additive(a) => {
                assert!(a.first.is_none());
                a.first = bs.second.clone();
            }
            _ => unreachable!(),
        },
        parser::Expr::Relational(r) => match right {
            parser::Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(r.second.is_none());
            }
            parser::Expr::Unary(_) => {
                assert!(r.second.is_none());
            }
            parser::Expr::Multiplicative(m) => {
                assert!(m.first.is_none());
                m.first = r.second.clone();
            }
            parser::Expr::Additive(a) => {
                assert!(a.first.is_none());
                a.first = r.second.clone();
            }
            parser::Expr::BitShift(bs) => {
                assert!(bs.first.is_none());
                bs.first = r.second.clone();
            }
            _ => unreachable!(),
        },
        parser::Expr::Equality(e) => match right {
            parser::Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(e.second.is_none());
            }
            parser::Expr::Unary(_) => {
                assert!(e.second.is_none());
            }
            parser::Expr::Multiplicative(m) => {
                assert!(m.first.is_none());
                m.first = e.second.clone();
            }
            parser::Expr::Additive(a) => {
                assert!(a.first.is_none());
                a.first = e.second.clone();
            }
            parser::Expr::BitShift(bs) => {
                assert!(bs.first.is_none());
                bs.first = e.second.clone();
            }
            parser::Expr::Relational(r) => {
                assert!(r.first.is_none());
                r.first = e.second.clone();
            }
            _ => unreachable!(),
        },
        parser::Expr::BitAND(ba) => match right {
            parser::Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(ba.second.is_none());
            }
            parser::Expr::Unary(_) => {
                assert!(ba.second.is_none());
            }
            parser::Expr::Multiplicative(m) => {
                assert!(m.first.is_none());
                m.first = ba.second.clone();
            }
            parser::Expr::Additive(a) => {
                assert!(a.first.is_none());
                a.first = ba.second.clone();
            }
            parser::Expr::BitShift(bs) => {
                assert!(bs.first.is_none());
                bs.first = ba.second.clone();
            }
            parser::Expr::Relational(r) => {
                assert!(r.first.is_none());
                r.first = ba.second.clone();
            }
            parser::Expr::Equality(e) => {
                assert!(e.first.is_none());
                e.first = ba.second.clone();
            }
            _ => unreachable!(),
        },
        parser::Expr::BitXOR(bx) => match right {
            parser::Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(bx.second.is_none());
            }
            parser::Expr::Unary(_) => {
                assert!(bx.second.is_none());
            }
            parser::Expr::Multiplicative(m) => {
                assert!(m.first.is_none());
                m.first = bx.second.clone();
            }
            parser::Expr::Additive(a) => {
                assert!(a.first.is_none());
                a.first = bx.second.clone();
            }
            parser::Expr::BitShift(bs) => {
                assert!(bs.first.is_none());
                bs.first = bx.second.clone();
            }
            parser::Expr::Relational(r) => {
                assert!(r.first.is_none());
                r.first = bx.second.clone();
            }
            parser::Expr::Equality(e) => {
                assert!(e.first.is_none());
                e.first = bx.second.clone();
            }
            parser::Expr::BitAND(ba) => {
                assert!(ba.first.is_none());
                ba.first = bx.second.clone();
            }
            _ => unreachable!(),
        },
        parser::Expr::BitOR(bo) => match right {
            parser::Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(bo.second.is_none());
            }
            parser::Expr::Unary(_) => {
                assert!(bo.second.is_none());
            }
            parser::Expr::Multiplicative(m) => {
                assert!(m.first.is_none());
                m.first = bo.second.clone();
            }
            parser::Expr::Additive(a) => {
                assert!(a.first.is_none());
                a.first = bo.second.clone();
            }
            parser::Expr::BitShift(bs) => {
                assert!(bs.first.is_none());
                bs.first = bo.second.clone();
            }
            parser::Expr::Relational(r) => {
                assert!(r.first.is_none());
                r.first = bo.second.clone();
            }
            parser::Expr::Equality(e) => {
                assert!(e.first.is_none());
                e.first = bo.second.clone();
            }
            parser::Expr::BitAND(ba) => {
                assert!(ba.first.is_none());
                ba.first = bo.second.clone();
            }
            parser::Expr::BitXOR(bx) => {
                assert!(bx.first.is_none());
                bx.first = bo.second.clone();
            }
            _ => unreachable!(),
        },
        parser::Expr::LogicalAND(la) => match right {
            parser::Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(la.second.is_none());
            }
            parser::Expr::Unary(_) => {
                assert!(la.second.is_none());
            }
            parser::Expr::Multiplicative(m) => {
                assert!(m.first.is_none());
                m.first = la.second.clone();
            }
            parser::Expr::Additive(a) => {
                assert!(a.first.is_none());
                a.first = la.second.clone();
            }
            parser::Expr::BitShift(bs) => {
                assert!(bs.first.is_none());
                bs.first = la.second.clone();
            }
            parser::Expr::Relational(r) => {
                assert!(r.first.is_none());
                r.first = la.second.clone();
            }
            parser::Expr::Equality(e) => {
                assert!(e.first.is_none());
                e.first = la.second.clone();
            }
            parser::Expr::BitAND(ba) => {
                assert!(ba.first.is_none());
                ba.first = la.second.clone();
            }
            parser::Expr::BitXOR(bx) => {
                assert!(bx.first.is_none());
                bx.first = la.second.clone();
            }
            parser::Expr::BitOR(bo) => {
                assert!(bo.first.is_none());
                bo.first = la.second.clone();
            }
            _ => unreachable!(),
        },
        parser::Expr::LogicalOR(lo) => match right {
            parser::Expr::Primary(p) => {
                assert!(p.is_some());
                assert!(lo.second.is_none());
            }
            parser::Expr::Unary(_) => {
                assert!(lo.second.is_none());
            }
            parser::Expr::Multiplicative(m) => {
                assert!(m.first.is_none());
                m.first = lo.second.clone();
            }
            parser::Expr::Additive(a) => {
                assert!(a.first.is_none());
                a.first = lo.second.clone();
            }
            parser::Expr::BitShift(bs) => {
                assert!(bs.first.is_none());
                bs.first = lo.second.clone();
            }
            parser::Expr::Relational(r) => {
                assert!(r.first.is_none());
                r.first = lo.second.clone();
            }
            parser::Expr::Equality(e) => {
                assert!(e.first.is_none());
                e.first = lo.second.clone();
            }
            parser::Expr::BitAND(ba) => {
                assert!(ba.first.is_none());
                ba.first = lo.second.clone();
            }
            parser::Expr::BitXOR(bx) => {
                assert!(bx.first.is_none());
                bx.first = lo.second.clone();
            }
            parser::Expr::BitOR(bo) => {
                assert!(bo.first.is_none());
                bo.first = lo.second.clone();
            }
            parser::Expr::LogicalAND(la) => {
                assert!(la.first.is_none());
                la.first = lo.second.clone();
            }
            _ => unreachable!(),
        },
        parser::Expr::Conditional(c) => {
            assert!(c.first.is_some());
            assert!(c.second.is_some());
            match right {
                parser::Expr::Primary(p) => {
                    assert!(p.is_some());
                    assert!(c.third.is_none());
                }
                parser::Expr::Unary(u) => {
                    assert!(c.third.is_none());
                }
                parser::Expr::Multiplicative(m) => {
                    assert!(m.first.is_none());
                    m.first = c.third.clone();
                }
                parser::Expr::Additive(a) => {
                    assert!(a.first.is_none());
                    a.first = c.third.clone();
                }
                parser::Expr::BitShift(bs) => {
                    assert!(bs.first.is_none());
                    bs.first = c.third.clone();
                }
                parser::Expr::Relational(r) => {
                    assert!(r.first.is_none());
                    r.first = c.third.clone();
                }
                parser::Expr::Equality(e) => {
                    assert!(e.first.is_none());
                    e.first = c.third.clone();
                }
                parser::Expr::BitAND(ba) => {
                    assert!(ba.first.is_none());
                    ba.first = c.third.clone();
                }
                parser::Expr::BitXOR(bx) => {
                    assert!(bx.first.is_none());
                    bx.first = c.third.clone();
                }
                parser::Expr::BitOR(bo) => {
                    assert!(bo.first.is_none());
                    bo.first = c.third.clone();
                }
                parser::Expr::LogicalAND(la) => {
                    assert!(la.first.is_none());
                    la.first = c.third.clone();
                }
                parser::Expr::LogicalOR(lo) => {
                    assert!(lo.first.is_none());
                    lo.first = c.third.clone();
                }
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}

fn left_has_higher_eq_priority(left: &mut parser::Expr, right: &mut parser::Expr) {
    assert!(left.priority() >= right.priority());
    let boxed = Some(Box::new(left.clone()));
    match right {
        parser::Expr::Multiplicative(m) => {
            m.first = boxed;
        }
        parser::Expr::Additive(a) => {
            a.first = boxed;
        }
        parser::Expr::BitShift(bs) => {
            bs.first = boxed;
        }
        parser::Expr::Relational(r) => {
            r.first = boxed;
        }
        parser::Expr::Equality(e) => {
            e.first = boxed;
        }
        parser::Expr::BitAND(ba) => {
            ba.first = boxed;
        }
        parser::Expr::BitXOR(bx) => {
            bx.first = boxed;
        }
        parser::Expr::BitOR(bo) => {
            bo.first = boxed;
        }
        parser::Expr::LogicalAND(la) => {
            la.first = boxed;
        }
        parser::Expr::LogicalOR(lo) => {
            lo.first = boxed;
        }
        parser::Expr::Conditional(c) => {
            unreachable!()
        }
        _ => unreachable!(),
    }
}

//Notes:
//The expression that controls conditional inclusion shall be an integer constant expression
//Because the controlling constant expression is evaluated during translation phase 4, all identifiers either are or are not macro names — there simply are no keywords, enumeration constants, etc
//All macro identifiers are evaluated as defined or not defined.
// TODO: rewrite this. It works but is WAYY too convoluted.
fn eval_constant_expression(
    tokens: &[lexer::Token],
    defines: &HashMap<usize, Define>,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<bool, String> {
    //let START_TIMER = std::time::Instant::now();
    let mut eval_vec_index = 0;
    let eval_vec = &mut tokens.to_vec();
    while eval_vec_index < eval_vec.len() {
        if let lexer::Token::IDENT(curr_id_key) = &eval_vec[eval_vec_index] {
            let curr_id = &str_maps.key_to_byte_vec[*curr_id_key];
            if *curr_id != *b"defined" {
                let mut eval_vec_index_copy = eval_vec_index;
                expand_macro(eval_vec, &mut eval_vec_index_copy, defines, str_maps)?;
                if eval_vec_index_copy != eval_vec_index {
                    continue;
                }
            } else {
                eval_vec_index += 1;
                if matches!(eval_vec.get(eval_vec_index), Some(lexer::Token::WHITESPACE)) {
                    eval_vec_index += 1;
                }
                match eval_vec.get(eval_vec_index) {
                    Some(lexer::Token::PUNCT_OPEN_PAR) => {
                        eval_vec_index += 1;
                        if matches!(eval_vec.get(eval_vec_index), Some(lexer::Token::WHITESPACE)) {
                            eval_vec_index += 1;
                        }
                        if matches!(eval_vec.get(eval_vec_index), Some(lexer::Token::IDENT(_))) {
                            eval_vec_index += 1;
                            if matches!(
                                eval_vec.get(eval_vec_index),
                                Some(lexer::Token::WHITESPACE)
                            ) {
                                eval_vec_index += 1;
                            }
                            if matches!(
                                eval_vec.get(eval_vec_index),
                                Some(lexer::Token::PUNCT_CLOSE_PAR)
                            ) {
                                eval_vec_index += 1;
                            }
                        }
                    }
                    Some(lexer::Token::IDENT(_)) => {
                        eval_vec_index += 1;
                    }
                    _ => {}
                }
            }
        }
        eval_vec_index += 1;
    }
    let mut tokens = eval_vec.as_slice();
    if tokens
        .iter()
        .filter(|t| !matches!(t, lexer::Token::WHITESPACE))
        .count()
        == 0
    {
        return Err(format!("empty expression given"));
    }
    if tokens.iter().any(|t| {
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
            "'=', '++', '--', ',' operators are not allowed and non integer types are not allowed"
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
    let mut stack = Vec::<parser::Expr>::new();
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
    let mut curr_expr: Option<parser::Expr> = None;
    let mut left_expression: Option<parser::Expr> = None;
    let mut right_expression: Option<parser::Expr> = None;
    let mut index = 0;
    while index < tokens.len() {
        match &tokens[index] {
            lexer::Token::IDENT(_) | lexer::Token::CONSTANT_DEC_INT { .. } => {
                let mut token_within = tokens[index];
                if let lexer::Token::IDENT(ident_key) = &tokens[index] {
                    let ident = &str_maps.key_to_byte_vec[*ident_key];
                    if *ident == *b"defined" {
                        let mut defined_index = index + 1;
                        if let Some(lexer::Token::WHITESPACE | lexer::Token::PUNCT_OPEN_PAR) =
                            tokens.get(defined_index)
                        {
                            defined_index += 1;
                            if let Some(lexer::Token::WHITESPACE) = tokens.get(defined_index) {
                                defined_index += 1;
                            }
                            if let Some(lexer::Token::PUNCT_OPEN_PAR) = tokens.get(defined_index) {
                                defined_index += 1;
                            }
                            if let Some(lexer::Token::WHITESPACE) = tokens.get(defined_index) {
                                defined_index += 1;
                            }
                            if let Some(lexer::Token::IDENT(identifier_name_key)) =
                                tokens.get(defined_index)
                            {
                                if defines.contains_key(&identifier_name_key) {
                                    token_within = lexer::Token::CONSTANT_DEC_INT {
                                        value_key: str_maps.add_byte_vec(&[b'1']),
                                        suffix_key: None,
                                    };
                                    index = defined_index;
                                } else {
                                    token_within = lexer::Token::CONSTANT_DEC_INT {
                                        value_key: str_maps.add_byte_vec(&[b'0']),
                                        suffix_key: None,
                                    };
                                    index = defined_index;
                                }
                            } else {
                                return Err(format!(
                                    "unexpected token: {:?}",
                                    tokens[defined_index]
                                ));
                            }
                        } else {
                            return Err(format!("unexpected token: {:?}", tokens[defined_index]));
                        }
                    } else {
                        assert!(!defines.contains_key(&ident_key));
                        token_within = lexer::Token::CONSTANT_DEC_INT {
                            value_key: str_maps.add_byte_vec(&[b'0']),
                            suffix_key: None,
                        };
                    }
                }
                let primary =
                    parser::Expr::Primary(Some(parser::PrimaryInner::new_p_token(token_within)?));
                if curr_expr.is_none() {
                    curr_expr = Some(primary);
                } else {
                    match &mut curr_expr {
                        Some(parser::Expr::Additive(a)) => {
                            assert!(a.first.is_some());
                            a.second = Some(Box::new(primary));
                        }
                        Some(parser::Expr::Unary(u)) => {
                            assert!(u.first.is_none());
                            u.first = Some(Box::new(primary));
                        }
                        Some(parser::Expr::LogicalOR(lo)) => {
                            assert!(lo.first.is_some());
                            lo.second = Some(Box::new(primary));
                        }
                        Some(parser::Expr::LogicalAND(la)) => {
                            assert!(la.first.is_some());
                            la.second = Some(Box::new(primary));
                        }
                        Some(parser::Expr::BitOR(bo)) => {
                            assert!(bo.first.is_some());
                            bo.second = Some(Box::new(primary));
                        }
                        Some(parser::Expr::BitXOR(bx)) => {
                            assert!(bx.first.is_some());
                            bx.second = Some(Box::new(primary));
                        }
                        Some(parser::Expr::BitAND(ba)) => {
                            assert!(ba.first.is_some());
                            ba.second = Some(Box::new(primary));
                        }
                        Some(parser::Expr::Equality(e)) => {
                            assert!(e.first.is_some());
                            e.second = Some(Box::new(primary));
                        }
                        Some(parser::Expr::Relational(r)) => {
                            assert!(r.first.is_some());
                            r.second = Some(Box::new(primary));
                        }
                        Some(parser::Expr::BitShift(bs)) => {
                            assert!(bs.first.is_some());
                            bs.second = Some(Box::new(primary));
                        }
                        Some(parser::Expr::Multiplicative(m)) => {
                            assert!(m.first.is_some());
                            m.second = Some(Box::new(primary));
                        }
                        Some(parser::Expr::Conditional(c)) => {
                            if c.first.is_none() {
                                c.first = Some(Box::new(primary));
                            } else if c.second.is_none() {
                                c.second = Some(Box::new(primary));
                            } else if c.third.is_none() {
                                c.third = Some(Box::new(primary));
                            }
                        }
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
                stack.push(parser::Expr::Primary(None));
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
                        parser::Expr::Primary(ref mut p) => {
                            *p = Some(parser::PrimaryInner::new_p_expr(curr_expr.unwrap()));
                            curr_expr = Some(e);
                            if !matches!(stack.last(), Some(parser::Expr::Unary(_))) {
                                break;
                            } else {
                                popped_opening_parenth_already = true;
                            }
                        }
                        parser::Expr::Unary(ref mut u) => {
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
                                parser::Expr::Unary(ref mut u) => {
                                    u.first = unwrapped;
                                }
                                parser::Expr::Multiplicative(ref mut m) => {
                                    m.second = unwrapped;
                                }
                                parser::Expr::Additive(ref mut a) => {
                                    a.second = unwrapped;
                                }
                                parser::Expr::BitShift(ref mut bs) => {
                                    bs.second = unwrapped;
                                }
                                parser::Expr::Relational(ref mut r) => {
                                    r.second = unwrapped;
                                }
                                parser::Expr::Equality(ref mut e) => {
                                    e.second = unwrapped;
                                }
                                parser::Expr::BitAND(ref mut ba) => {
                                    ba.second = unwrapped;
                                }
                                parser::Expr::BitXOR(ref mut bx) => {
                                    bx.second = unwrapped;
                                }
                                parser::Expr::BitOR(ref mut bo) => {
                                    bo.second = unwrapped;
                                }
                                parser::Expr::LogicalAND(ref mut la) => {
                                    la.second = unwrapped;
                                }
                                parser::Expr::LogicalOR(ref mut lo) => {
                                    lo.second = unwrapped;
                                }
                                parser::Expr::Conditional(ref mut c) => {
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
                    Some(parser::Expr::Primary(p)) => {
                        // if a '~' or '!' follow a primary expression, that is not allowed.
                        match tokens[index] {
                            lexer::Token::PUNCT_TILDE | lexer::Token::PUNCT_NOT_BOOL => {
                                return Err(format!("unexpected {:?} token", tokens[index]));
                            }
                            _ => {}
                        }
                        right_expression = Some(parser::Expr::Additive(parser::Additive {
                            op: match tokens[index] {
                                lexer::Token::PUNCT_PLUS => parser::AdditiveOps::Add,
                                lexer::Token::PUNCT_MINUS => parser::AdditiveOps::Sub,
                                _ => unreachable!("{:?}", tokens[index]),
                            },
                            first: None,
                            second: None,
                        }));
                        curr_expr = None;
                    }
                    None => {
                        curr_expr = Some(parser::Expr::Unary(parser::Unary {
                            op: match tokens[index] {
                                lexer::Token::PUNCT_PLUS => parser::UnaryOp::Add,
                                lexer::Token::PUNCT_MINUS => parser::UnaryOp::Sub,
                                lexer::Token::PUNCT_NOT_BOOL => parser::UnaryOp::LogicalNOT,
                                lexer::Token::PUNCT_TILDE => parser::UnaryOp::BitNOT,
                                _ => unreachable!(),
                            },
                            first: None,
                        }));
                    }
                    Some(parser::Expr::Unary(parser::Unary { op: _, first })) => {
                        if first.is_none() {
                            stack.push(left_expression.clone().unwrap());
                            curr_expr = Some(parser::Expr::Unary(parser::Unary {
                                op: match tokens[index] {
                                    lexer::Token::PUNCT_PLUS => parser::UnaryOp::Add,
                                    lexer::Token::PUNCT_MINUS => parser::UnaryOp::Sub,
                                    lexer::Token::PUNCT_NOT_BOOL => parser::UnaryOp::LogicalNOT,
                                    lexer::Token::PUNCT_TILDE => parser::UnaryOp::BitNOT,
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
                            right_expression = Some(parser::Expr::Additive(parser::Additive {
                                op: match tokens[index] {
                                    lexer::Token::PUNCT_PLUS => parser::AdditiveOps::Add,
                                    lexer::Token::PUNCT_MINUS => parser::AdditiveOps::Sub,
                                    _ => unreachable!("{:?}", tokens[index]),
                                },
                                first: None,
                                second: None,
                            }));
                            curr_expr = None;
                        }
                    }
                    Some(parser::Expr::Multiplicative(m)) => {
                        case_where_it_could_be_unary_or_additive!(
                            m,
                            right_expression,
                            tokens[index]
                        );
                        curr_expr = None;
                    }
                    Some(parser::Expr::Additive(a)) => {
                        case_where_it_could_be_unary_or_additive!(
                            a,
                            right_expression,
                            tokens[index]
                        );
                        curr_expr = None;
                    }
                    Some(parser::Expr::BitShift(bs)) => {
                        case_where_it_could_be_unary_or_additive!(
                            bs,
                            right_expression,
                            tokens[index]
                        );
                        curr_expr = None;
                    }
                    Some(parser::Expr::Relational(r)) => {
                        case_where_it_could_be_unary_or_additive!(
                            r,
                            right_expression,
                            tokens[index]
                        );
                        curr_expr = None;
                    }
                    Some(parser::Expr::Equality(e)) => {
                        case_where_it_could_be_unary_or_additive!(
                            e,
                            right_expression,
                            tokens[index]
                        );
                        curr_expr = None;
                    }
                    Some(parser::Expr::BitAND(ba)) => {
                        case_where_it_could_be_unary_or_additive!(
                            ba,
                            right_expression,
                            tokens[index]
                        );
                        curr_expr = None;
                    }
                    Some(parser::Expr::BitXOR(bx)) => {
                        case_where_it_could_be_unary_or_additive!(
                            bx,
                            right_expression,
                            tokens[index]
                        );
                        curr_expr = None;
                    }
                    Some(parser::Expr::BitOR(bo)) => {
                        case_where_it_could_be_unary_or_additive!(
                            bo,
                            right_expression,
                            tokens[index]
                        );
                        curr_expr = None;
                    }
                    Some(parser::Expr::LogicalAND(la)) => {
                        case_where_it_could_be_unary_or_additive!(
                            la,
                            right_expression,
                            tokens[index]
                        );
                        curr_expr = None;
                    }
                    Some(parser::Expr::LogicalOR(lo)) => {
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
                right_expression = Some(parser::Expr::Multiplicative(parser::Multiplicative {
                    op: match tokens[index] {
                        lexer::Token::PUNCT_MULT => parser::MultiplicativeOps::Mult,
                        lexer::Token::PUNCT_DIV => parser::MultiplicativeOps::Div,
                        lexer::Token::PUNCT_MODULO => parser::MultiplicativeOps::Mod,
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
                right_expression = Some(parser::Expr::BitShift(parser::BitShift {
                    op: match tokens[index] {
                        lexer::Token::PUNCT_BITSHFT_LEFT => parser::BitShiftOp::Left,
                        lexer::Token::PUNCT_BITSHFT_RIGHT => parser::BitShiftOp::Right,
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
                right_expression = Some(parser::Expr::Relational(parser::Relational {
                    op: match tokens[index] {
                        lexer::Token::PUNCT_LESS_THAN => parser::RelationalOp::LessThan,
                        lexer::Token::PUNCT_LESS_THAN_EQ => parser::RelationalOp::LessThanEq,
                        lexer::Token::PUNCT_GREATER_THAN => parser::RelationalOp::GreaterThan,
                        lexer::Token::PUNCT_GREATER_THAN_EQ => parser::RelationalOp::GreaterThanEq,
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
                right_expression = Some(parser::Expr::Equality(parser::Equality {
                    op: match tokens[index] {
                        lexer::Token::PUNCT_EQ_BOOL => parser::EqualityOp::Equal,
                        lexer::Token::PUNCT_NOT_EQ_BOOL => parser::EqualityOp::NotEqual,
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
                right_expression = Some(parser::Expr::BitAND(parser::BitAND {
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
                right_expression = Some(parser::Expr::BitXOR(parser::BitXOR {
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
                right_expression = Some(parser::Expr::BitOR(parser::BitOR {
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
                right_expression = Some(parser::Expr::LogicalAND(parser::LogicalAND {
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
                right_expression = Some(parser::Expr::LogicalOR(parser::LogicalOR {
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
                        "not allowed token after operator '||', {:?}",
                        tokens[index]
                    ));
                }
            }
            lexer::Token::PUNCT_QUESTION_MARK => {
                if let Some(expr) = curr_expr {
                    let expr_cond = parser::Expr::Conditional(parser::Conditional {
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
                        parser::Expr::Unary(ref mut u) => {
                            u.first = unwrapped;
                        }
                        parser::Expr::Multiplicative(ref mut m) => {
                            m.second = unwrapped;
                        }
                        parser::Expr::Additive(ref mut a) => {
                            a.second = unwrapped;
                        }
                        parser::Expr::BitShift(ref mut bs) => {
                            bs.second = unwrapped;
                        }
                        parser::Expr::Relational(ref mut r) => {
                            r.second = unwrapped;
                        }
                        parser::Expr::Equality(ref mut e) => {
                            e.second = unwrapped;
                        }
                        parser::Expr::BitAND(ref mut ba) => {
                            ba.second = unwrapped;
                        }
                        parser::Expr::BitXOR(ref mut bx) => {
                            bx.second = unwrapped;
                        }
                        parser::Expr::BitOR(ref mut bo) => {
                            bo.second = unwrapped;
                        }
                        parser::Expr::LogicalAND(ref mut la) => {
                            la.second = unwrapped;
                        }
                        parser::Expr::LogicalOR(ref mut lo) => {
                            lo.second = unwrapped;
                        }
                        parser::Expr::Conditional(ref mut c) => {
                            c.second = unwrapped;
                            curr_expr = Some(expr);
                            break;
                        }
                        _ => unreachable!(),
                    }
                    curr_expr = Some(expr);
                }
                if !matches!(curr_expr, Some(parser::Expr::Conditional(_))) {
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
            let mut left = left_expression.unwrap();
            let mut right = right_expression.unwrap();
            if left.priority() >= right.priority() {
                left_has_higher_eq_priority(&mut left, &mut right);
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
            parser::Expr::Unary(ref mut u) => {
                u.first = unwrapped;
            }
            parser::Expr::Multiplicative(ref mut m) => {
                m.second = unwrapped;
            }
            parser::Expr::Additive(ref mut a) => {
                a.second = unwrapped;
            }
            parser::Expr::BitShift(ref mut bs) => {
                bs.second = unwrapped;
            }
            parser::Expr::Relational(ref mut r) => {
                r.second = unwrapped;
            }
            parser::Expr::Equality(ref mut e) => {
                e.second = unwrapped;
            }
            parser::Expr::BitAND(ref mut ba) => {
                ba.second = unwrapped;
            }
            parser::Expr::BitXOR(ref mut bx) => {
                bx.second = unwrapped;
            }
            parser::Expr::BitOR(ref mut bo) => {
                bo.second = unwrapped;
            }
            parser::Expr::LogicalAND(ref mut la) => {
                la.second = unwrapped;
            }
            parser::Expr::LogicalOR(ref mut lo) => {
                lo.second = unwrapped;
            }
            parser::Expr::Conditional(ref mut c) => {
                assert!(c.first.is_some() && c.second.is_some());
                c.third = unwrapped;
            }
            _ => unreachable!("{}", expr.priority()),
        }
        curr_expr = Some(expr);
    }
    // where we start evaluating the expression tree
    // TODO: remove duplicate structure of code.
    let mut eval_stack = Vec::<parser::Expr>::new();
    let mut primary_stack = Vec::<i128>::new();
    if curr_expr.is_some() {
        eval_stack.push(curr_expr.unwrap());
        while let Some(mut top_expr) = eval_stack.pop() {
            match top_expr {
                parser::Expr::Primary(ref mut p) => match p.clone().unwrap() {
                    parser::PrimaryInner::Expr(e) => {
                        eval_stack.push(*e);
                    }
                    parser::PrimaryInner::Token(t) => {
                        assert!(matches!(t, lexer::Token::CONSTANT_DEC_INT { .. }));
                        if let lexer::Token::CONSTANT_DEC_INT {
                            value_key,
                            suffix_key,
                        } = t
                        {
                            // "For the purposes of this token conversion and evaluation,
                            // all signed integer types and all unsigned integer types act as if they have the same representation
                            // as, respectively, the types intmax_t and uintmax_t defined in the header <stdint.h>."
                            //
                            // We just 'cheat' by using i128 integer types. That way, regardless
                            // whether we get u64 (uintmax_t) or i64 (intmax_t), we can still
                            // compare and not have to do any weird casts.
                            let value = &str_maps.key_to_byte_vec[value_key];
                            match String::from_utf8_lossy(value).to_string().parse::<i128>() {
                                Ok(v) if v <= u64::MAX as i128 && v >= i64::MIN as i128 => {
                                    primary_stack.push(v);
                                }
                                _ => {
                                    return Err(format!(
                                        "{} cannot be represented as i64 or u64",
                                        String::from_utf8_lossy(value).to_string()
                                    ));
                                }
                            }
                        }
                    }
                },
                parser::Expr::Unary(ref mut u) => {
                    if let Some(first) = &mut u.first {
                        let first_clone = *first.clone();
                        u.first = None;
                        eval_stack.push(top_expr.clone());
                        eval_stack.push(first_clone);
                    } else {
                        assert!(!primary_stack.is_empty());
                        let one = primary_stack.pop().unwrap();
                        match u.op {
                            parser::UnaryOp::Add => {
                                primary_stack.push(one);
                            }
                            parser::UnaryOp::Sub => {
                                primary_stack.push(-one);
                            }
                            parser::UnaryOp::BitNOT => {
                                primary_stack.push(!one);
                            }
                            parser::UnaryOp::LogicalNOT => {
                                primary_stack.push(if one == 0 { 1 } else { 0 });
                            }
                            parser::UnaryOp::Ampersand | parser::UnaryOp::Deref => unreachable!(),
                        }
                    }
                }
                parser::Expr::Multiplicative(ref mut m) => {
                    if let Some(left) = &mut m.first {
                        let left_clone = *left.clone();
                        m.first = None;
                        eval_stack.push(top_expr);
                        eval_stack.push(left_clone);
                    } else if let Some(right) = &mut m.second {
                        let right_clone = *right.clone();
                        m.second = None;
                        eval_stack.push(top_expr);
                        eval_stack.push(right_clone);
                    } else {
                        assert!(primary_stack.len() >= 2);
                        let right = primary_stack.pop().unwrap();
                        let left = primary_stack.pop().unwrap();
                        match m.op {
                            parser::MultiplicativeOps::Mult => {
                                primary_stack.push(left * right);
                            }
                            parser::MultiplicativeOps::Div | parser::MultiplicativeOps::Mod => {
                                if right == 0 {
                                    return Err(String::from("cannot divide by zero"));
                                }
                                match m.op {
                                    parser::MultiplicativeOps::Div => {
                                        primary_stack.push(left / right);
                                    }
                                    parser::MultiplicativeOps::Mod => {
                                        primary_stack.push(left % right);
                                    }
                                    _ => unreachable!(),
                                }
                            }
                        }
                    }
                }
                parser::Expr::Additive(ref mut a) => {
                    if let Some(left) = &mut a.first {
                        let left_clone = *left.clone();
                        a.first = None;
                        eval_stack.push(top_expr);
                        eval_stack.push(left_clone);
                    } else if let Some(right) = &mut a.second {
                        let right_clone = *right.clone();
                        a.second = None;
                        eval_stack.push(top_expr);
                        eval_stack.push(right_clone);
                    } else {
                        assert!(primary_stack.len() >= 2);
                        let right = primary_stack.pop().unwrap();
                        let left = primary_stack.pop().unwrap();
                        match a.op {
                            parser::AdditiveOps::Add => {
                                primary_stack.push(left + right);
                            }
                            parser::AdditiveOps::Sub => {
                                primary_stack.push(left - right);
                            }
                        }
                    }
                }
                parser::Expr::BitShift(ref mut bs) => {
                    if let Some(left) = &mut bs.first {
                        let left_clone = *left.clone();
                        bs.first = None;
                        eval_stack.push(top_expr);
                        eval_stack.push(left_clone);
                    } else if let Some(right) = &mut bs.second {
                        let right_clone = *right.clone();
                        bs.second = None;
                        eval_stack.push(top_expr);
                        eval_stack.push(right_clone);
                    } else {
                        assert!(primary_stack.len() >= 2);
                        let right = primary_stack.pop().unwrap();
                        let left = primary_stack.pop().unwrap();
                        match bs.op {
                            parser::BitShiftOp::Left => {
                                primary_stack.push(left << right);
                            }
                            parser::BitShiftOp::Right => {
                                primary_stack.push(left >> right);
                            }
                        }
                    }
                }
                parser::Expr::Relational(ref mut r) => {
                    if let Some(left) = &mut r.first {
                        let left_clone = *left.clone();
                        r.first = None;
                        eval_stack.push(top_expr);
                        eval_stack.push(left_clone);
                    } else if let Some(right) = &mut r.second {
                        let right_clone = *right.clone();
                        r.second = None;
                        eval_stack.push(top_expr);
                        eval_stack.push(right_clone);
                    } else {
                        assert!(primary_stack.len() >= 2);
                        let right = primary_stack.pop().unwrap();
                        let left = primary_stack.pop().unwrap();
                        match r.op {
                            parser::RelationalOp::LessThan => {
                                primary_stack.push(if left < right { 1 } else { 0 });
                            }
                            parser::RelationalOp::LessThanEq => {
                                primary_stack.push(if left <= right { 1 } else { 0 });
                            }
                            parser::RelationalOp::GreaterThan => {
                                primary_stack.push(if left > right { 1 } else { 0 });
                            }
                            parser::RelationalOp::GreaterThanEq => {
                                primary_stack.push(if left >= right { 1 } else { 0 });
                            }
                        }
                    }
                }
                parser::Expr::Equality(ref mut e) => {
                    if let Some(left) = &mut e.first {
                        let left_clone = *left.clone();
                        e.first = None;
                        eval_stack.push(top_expr);
                        eval_stack.push(left_clone);
                    } else if let Some(right) = &mut e.second {
                        let right_clone = *right.clone();
                        e.second = None;
                        eval_stack.push(top_expr);
                        eval_stack.push(right_clone);
                    } else {
                        assert!(primary_stack.len() >= 2);
                        let right = primary_stack.pop().unwrap();
                        let left = primary_stack.pop().unwrap();
                        match e.op {
                            parser::EqualityOp::Equal => {
                                primary_stack.push(if left == right { 1 } else { 0 });
                            }
                            parser::EqualityOp::NotEqual => {
                                primary_stack.push(if left != right { 1 } else { 0 });
                            }
                        }
                    }
                }
                parser::Expr::BitAND(ref mut ba) => {
                    if let Some(left) = &mut ba.first {
                        let left_clone = *left.clone();
                        ba.first = None;
                        eval_stack.push(top_expr);
                        eval_stack.push(left_clone);
                    } else if let Some(right) = &mut ba.second {
                        let right_clone = *right.clone();
                        ba.second = None;
                        eval_stack.push(top_expr);
                        eval_stack.push(right_clone);
                    } else {
                        assert!(primary_stack.len() >= 2);
                        let right = primary_stack.pop().unwrap();
                        let left = primary_stack.pop().unwrap();
                        primary_stack.push(if (left & right) != 0 { 1 } else { 0 });
                    }
                }
                parser::Expr::BitXOR(ref mut bx) => {
                    if let Some(left) = &mut bx.first {
                        let left_clone = *left.clone();
                        bx.first = None;
                        eval_stack.push(top_expr);
                        eval_stack.push(left_clone);
                    } else if let Some(right) = &mut bx.second {
                        let right_clone = *right.clone();
                        bx.second = None;
                        eval_stack.push(top_expr);
                        eval_stack.push(right_clone);
                    } else {
                        assert!(primary_stack.len() >= 2);
                        let right = primary_stack.pop().unwrap();
                        let left = primary_stack.pop().unwrap();
                        primary_stack.push(if (left ^ right) != 0 { 1 } else { 0 });
                    }
                }
                parser::Expr::BitOR(ref mut bo) => {
                    if let Some(left) = &mut bo.first {
                        let left_clone = *left.clone();
                        bo.first = None;
                        eval_stack.push(top_expr);
                        eval_stack.push(left_clone);
                    } else if let Some(right) = &mut bo.second {
                        let right_clone = *right.clone();
                        bo.second = None;
                        eval_stack.push(top_expr);
                        eval_stack.push(right_clone);
                    } else {
                        assert!(primary_stack.len() >= 2);
                        let right = primary_stack.pop().unwrap();
                        let left = primary_stack.pop().unwrap();
                        primary_stack.push(if (left | right) != 0 { 1 } else { 0 });
                    }
                }
                parser::Expr::LogicalAND(ref mut la) => {
                    if let Some(left) = &mut la.first {
                        let left_clone = *left.clone();
                        la.first = None;
                        eval_stack.push(top_expr);
                        eval_stack.push(left_clone);
                    } else if let Some(right) = &mut la.second {
                        let right_clone = *right.clone();
                        la.second = None;
                        eval_stack.push(top_expr);
                        eval_stack.push(right_clone);
                    } else {
                        assert!(primary_stack.len() >= 2);
                        let right = primary_stack.pop().unwrap();
                        let left = primary_stack.pop().unwrap();
                        primary_stack.push(if left != 0 && right != 0 { 1 } else { 0 });
                    }
                }
                parser::Expr::LogicalOR(ref mut lo) => {
                    if let Some(left) = &mut lo.first {
                        let left_clone = *left.clone();
                        lo.first = None;
                        eval_stack.push(top_expr);
                        eval_stack.push(left_clone);
                    } else if let Some(right) = &mut lo.second {
                        let right_clone = *right.clone();
                        lo.second = None;
                        eval_stack.push(top_expr);
                        eval_stack.push(right_clone);
                    } else {
                        assert!(primary_stack.len() >= 2);
                        let right = primary_stack.pop().unwrap();
                        let left = primary_stack.pop().unwrap();
                        primary_stack.push(if left != 0 || right != 0 { 1 } else { 0 });
                    }
                }
                parser::Expr::Conditional(ref mut c) => {
                    if let Some(first) = &mut c.first {
                        let first_clone = *first.clone();
                        c.first = None;
                        eval_stack.push(top_expr);
                        eval_stack.push(first_clone);
                    } else {
                        assert!(!primary_stack.is_empty());
                        let top = primary_stack.pop().unwrap();
                        if top != 0 {
                            let second = c.second.clone().unwrap();
                            eval_stack.push(*second);
                        } else {
                            let third = c.third.clone().unwrap();
                            eval_stack.push(*third);
                        }
                    }
                }
                _ => unreachable!(),
            }
        }
    }
    //eprintln!("END TIME: {}", (std::time::Instant::now() - START_TIMER).as_nanos());
    assert!(primary_stack.len() == 1);
    Ok(*primary_stack.last().unwrap() != 0)
}
fn if_directive(
    tokens: &mut [lexer::Token],
    index: usize,
    defines: &HashMap<usize, Define>,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(), String> {
    let mut balance_index = index;
    let mut if_endif_counter = 0;
    let mut if_elif_else_structure_index: Vec<(Vec<u8>, usize, usize)> = Vec::new();
    'outer: loop {
        match tokens.get(balance_index) {
            Some(lexer::Token::PUNCT_HASH) => {
                let punct_hash_index = balance_index;
                let mut checks_follows_whitespace_nothing_newline_index = balance_index;
                let follows_whitespace_nothing_newline = loop {
                    if checks_follows_whitespace_nothing_newline_index > 0 {
                        checks_follows_whitespace_nothing_newline_index -= 1;
                    } else {
                        break true;
                    }

                    match tokens.get(checks_follows_whitespace_nothing_newline_index) {
                        Some(lexer::Token::WHITESPACE) => {}
                        Some(lexer::Token::NEWLINE) => break true,
                        _ => break false,
                    }
                };
                if follows_whitespace_nothing_newline {
                    balance_index += 1;
                    if matches!(tokens.get(balance_index), Some(lexer::Token::WHITESPACE)) {
                        balance_index += 1;
                    }
                    match tokens.get(balance_index) {
                        Some(lexer::Token::IDENT(id_key)) => {
                            let id = str_maps.key_to_byte_vec[*id_key].clone();
                            match id.as_slice() {
                                b"endif" => loop {
                                    balance_index += 1;
                                    match tokens.get(balance_index) {
                                        Some(lexer::Token::NEWLINE) => {
                                            if_endif_counter -= 1;
                                            if if_endif_counter == 0 {
                                                if_elif_else_structure_index.push((
                                                    id,
                                                    punct_hash_index,
                                                    balance_index,
                                                ));
                                                break 'outer;
                                            }
                                            balance_index += 1;
                                            break;
                                        }
                                        Some(lexer::Token::WHITESPACE) => {}
                                        Some(_) => {
                                            return Err(format!(
                                                "unexpected token after endif directive: {:?}",
                                                tokens[balance_index]
                                            ))
                                        }
                                        None => {
                                            return Err(format!(
                                                "missing newline after endif directive"
                                            ))
                                        }
                                    }
                                },
                                b"if" | b"ifdef" | b"ifndef" => loop {
                                    balance_index += 1;
                                    match tokens.get(balance_index) {
                                        Some(lexer::Token::NEWLINE) => {
                                            if_endif_counter += 1;
                                            if if_endif_counter == 1 {
                                                if_elif_else_structure_index.push((
                                                    id,
                                                    punct_hash_index,
                                                    balance_index,
                                                ));
                                            }
                                            balance_index += 1;
                                            break;
                                        }
                                        None => {
                                            return Err(format!(
                                                "missing newline after if{{def, ndef}} directive"
                                            ))
                                        }
                                        _ => {}
                                    }
                                },
                                b"elif" if if_endif_counter == 1 => loop {
                                    balance_index += 1;
                                    match tokens.get(balance_index) {
                                        Some(lexer::Token::NEWLINE) => {
                                            if if_endif_counter == 1 {
                                                if_elif_else_structure_index.push((
                                                    id,
                                                    punct_hash_index,
                                                    balance_index,
                                                ));
                                            }
                                            balance_index += 1;
                                            break;
                                        }
                                        None => {
                                            return Err(format!(
                                                "missing newline after elif directive"
                                            ))
                                        }
                                        _ => {}
                                    }
                                },
                                b"else" if if_endif_counter == 1 => loop {
                                    balance_index += 1;
                                    match tokens.get(balance_index) {
                                        Some(lexer::Token::NEWLINE) => {
                                            if if_endif_counter == 1 {
                                                if_elif_else_structure_index.push((
                                                    id,
                                                    punct_hash_index,
                                                    balance_index,
                                                ));
                                            }
                                            balance_index += 1;
                                            break;
                                        }
                                        Some(lexer::Token::WHITESPACE) => {}
                                        Some(_) => {
                                            return Err(format!(
                                                "unexpected token after else directive: {:?}",
                                                tokens[balance_index]
                                            ))
                                        }
                                        None => {
                                            return Err(format!(
                                                "missing newline after else directive"
                                            ))
                                        }
                                    }
                                },
                                _ => {
                                    balance_index += 1;
                                }
                            }
                        }
                        None => break,
                        _ => {
                            balance_index += 1;
                        }
                    }
                } else {
                    balance_index += 1;
                }
            }
            Some(_) => {
                balance_index += 1;
            }
            None => break,
        }
    }
    if if_endif_counter != 0 {
        return Err(String::from(
            "missing endif directive for if{{def, ndef}} directive",
        ));
    }
    let mut seen_elif = false;
    let mut seen_else = false;
    for index_for_structure_index in 0..if_elif_else_structure_index.len() {
        let (macro_id_bytes, start, end) = &if_elif_else_structure_index[index_for_structure_index];
        match macro_id_bytes.as_slice() {
            b"if" | b"ifdef" | b"ifndef" => {
                if seen_elif || seen_else {
                    return Err(format!("cannot have elif or else before if{{def, ndef}}"));
                }
            }
            b"elif" => {
                if seen_else {
                    return Err(format!("cannot have else before elif"));
                }
                seen_elif = true;
            }
            b"else" => {
                seen_else = true;
            }
            b"endif" => {}
            _ => unreachable!(),
        }
    }
    for index_for_structure_index in 0..if_elif_else_structure_index.len() {
        let (macro_id, start, end) = &if_elif_else_structure_index[index_for_structure_index];
        let mut start_looking = *start;
        while !matches!(tokens.get(start_looking), Some(lexer::Token::IDENT(_)))
            && start_looking < tokens.len()
        {
            start_looking += 1;
        }
        assert!(matches!(
            tokens.get(start_looking),
            Some(lexer::Token::IDENT(_))
        ));
        start_looking += 1;
        let eval_vec = &tokens[start_looking..*end];
        let truthy = match macro_id.as_slice() {
            b"if" | b"elif" => eval_constant_expression(eval_vec, defines, str_maps)?,
            b"ifdef" => {
                if eval_vec
                    .iter()
                    .any(|t| !matches!(t, lexer::Token::IDENT(_) | lexer::Token::WHITESPACE))
                {
                    return Err(format!(
                        "expected only identifier within ifdef directive: {:?}",
                        eval_vec
                    ));
                }
                let Some(lexer::Token::IDENT(ident_key)) = eval_vec.iter().find(|t| matches!(t, lexer::Token::IDENT(_))) else { unreachable!() };
                defines.contains_key(ident_key)
            }
            b"ifndef" => {
                if eval_vec
                    .iter()
                    .any(|t| !matches!(t, lexer::Token::IDENT(_) | lexer::Token::WHITESPACE))
                {
                    return Err(format!(
                        "expected only identifier within ifndef directive: {:?}",
                        eval_vec
                    ));
                }
                let Some(lexer::Token::IDENT(ident_key)) = eval_vec.iter().find(|t| matches!(t, lexer::Token::IDENT(_))) else { unreachable!() };
                !defines.contains_key(ident_key)
            }
            b"else" => true,
            b"endif" => break,
            _ => unreachable!(),
        };
        if truthy {
            assert!(index_for_structure_index + 1 < if_elif_else_structure_index.len());
            let next_start = if_elif_else_structure_index[index_for_structure_index + 1].1;
            let mut index_overwrite = if_elif_else_structure_index[0].1;
            let mut index_looking = *end + 1;
            while index_looking < next_start {
                tokens[index_overwrite] = tokens[index_looking];
                index_overwrite += 1;
                index_looking += 1;
            }
            while index_overwrite < if_elif_else_structure_index.last().unwrap().2 {
                match tokens[index_overwrite] {
                    lexer::Token::NEWLINE => {}
                    _ => {
                        tokens[index_overwrite] = lexer::Token::WHITESPACE;
                    }
                }
                index_overwrite += 1;
            }
            return Ok(());
        }
    }
    let mut index_overwrite = if_elif_else_structure_index[0].1;
    while index_overwrite < if_elif_else_structure_index.last().unwrap().2 {
        match tokens[index_overwrite] {
            lexer::Token::NEWLINE => {}
            _ => {
                tokens[index_overwrite] = lexer::Token::WHITESPACE;
            }
        }
        index_overwrite += 1;
    }
    Ok(())
}
fn define_directive(
    tokens: &[lexer::Token],
    index: usize,
    defines: &mut HashMap<usize, Define>,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<usize, String> {
    let mut index_of_identifier = index + 1;
    let mut end = index_of_identifier;
    while !matches!(tokens.get(end), Some(lexer::Token::NEWLINE)) && end < tokens.len() {
        end += 1;
    }
    if !matches!(tokens.get(end), Some(lexer::Token::NEWLINE)) {
        return Err(format!("no newline at end of directive"));
    }
    while index_of_identifier < end {
        match &tokens[index_of_identifier] {
            lexer::Token::WHITESPACE => {}
            lexer::Token::IDENT(id_key) => {
                let id = &str_maps.key_to_byte_vec[*id_key];
                if *id != *b"define" {
                    break;
                }
            }
            _ => {
                return Err(format!("unknown token after define directive"));
            }
        }
        index_of_identifier += 1;
    }
    let mut def_data = Define {
        parameters: None,
        var_arg: false,
        replacement_list: Vec::new(),
    };
    let mut identifier_of_macro_key = 0;
    if let Some([lexer::Token::IDENT(id_key), lexer::Token::PUNCT_OPEN_PAR]) =
        tokens.get(index_of_identifier..index_of_identifier + 2)
    {
        let id = &str_maps.key_to_byte_vec[*id_key];
        def_data.parameters = Some(Vec::new());
        identifier_of_macro_key = *id_key;
        let mut fn_like_macro_index = index_of_identifier + 2;
        while matches!(
            tokens.get(fn_like_macro_index),
            Some(lexer::Token::IDENT(_) | lexer::Token::PUNCT_COMMA | lexer::Token::WHITESPACE)
        ) {
            if let Some(lexer::Token::IDENT(arg_key)) = tokens.get(fn_like_macro_index) {
                if let Some(ref mut v) = def_data.parameters {
                    let arg = &str_maps.key_to_byte_vec[*arg_key];
                    if !v.contains(arg_key) {
                        if *arg == *b"__VA_ARGS__" {
                            return Err(format!("__VA_ARGS__ cannot be used as a parameter name"));
                        }
                        v.push(*arg_key);
                    } else {
                        return Err(format!("duplicate argument name found in define directive"));
                    }
                }
            }
            fn_like_macro_index += 1;
        }
        if matches!(
            tokens.get(fn_like_macro_index..fn_like_macro_index + 2),
            Some([lexer::Token::PUNCT_ELLIPSIS, lexer::Token::PUNCT_CLOSE_PAR])
        ) {
            def_data.var_arg = true;
            fn_like_macro_index += 1;
        }
        def_data
            .replacement_list
            .extend_from_slice(&tokens[fn_like_macro_index + 1..end]);
        defines.insert(*id_key, def_data);
    } else if let Some(lexer::Token::IDENT(id_key)) = tokens.get(index_of_identifier) {
        identifier_of_macro_key = *id_key;
        def_data
            .replacement_list
            .extend_from_slice(&tokens[index_of_identifier + 1..end]);
        defines.insert(*id_key, def_data);
    }
    if defines.contains_key(&identifier_of_macro_key) {
        let identifier_of_macro = &str_maps.key_to_byte_vec[identifier_of_macro_key];
        if *identifier_of_macro == *b"defined"
            || *identifier_of_macro == *b"__LINE__"
            || *identifier_of_macro == *b"__FILE__"
            || *identifier_of_macro == *b"__DATE__"
            || *identifier_of_macro == *b"__STDC__"
            || *identifier_of_macro == *b"__STDC_HOSTED__"
            || *identifier_of_macro == *b"__STDC_VERSION__"
            || *identifier_of_macro == *b"__TIME__"
        {
            return Err(format!(
                "cannot define '{}' as it is a cpp keyword",
                String::from_utf8_lossy(identifier_of_macro).to_string()
            ));
        }
        if let Some(ref mut dd) = defines.get_mut(&identifier_of_macro_key) {
            if dd.parameters.is_some() {
                for t_index in 0..dd.replacement_list.len() {
                    if matches!(
                        dd.replacement_list.get(t_index),
                        Some(lexer::Token::PUNCT_HASH)
                    ) && !matches!(
                        dd.replacement_list.get(t_index + 1),
                        Some(lexer::Token::IDENT(_))
                    ) && !matches!(
                        dd.replacement_list.get(t_index + 1..t_index + 3),
                        Some([lexer::Token::WHITESPACE, lexer::Token::IDENT(_)])
                    ) {
                        return Err(format!("'#' does not immediately precede an argument name"));
                    }
                }
            }
            if matches!(
                dd.replacement_list.first(),
                Some(lexer::Token::PUNCT_HASH_HASH)
            ) || matches!(
                dd.replacement_list.last(),
                Some(lexer::Token::PUNCT_HASH_HASH)
            ) {
                return Err(format!(
                    "'##' cannot be at the beginning or end of a replacement list"
                ));
            }
            if let Some(lexer::Token::WHITESPACE) = dd.replacement_list.first() {
                dd.replacement_list.remove(0);
            }
            if let Some(lexer::Token::WHITESPACE) = dd.replacement_list.last() {
                dd.replacement_list.remove(0);
            }
            return Ok(end + 1);
        } else {
            let lexer::Token::IDENT(id_key) = &tokens[index_of_identifier] else { unreachable!() };
            let id = &str_maps.key_to_byte_vec[*id_key];
            return Err(format!("define directive properly formed but we don't have {} in the hashmap. index_of_identifier gives {}", String::from_utf8_lossy(identifier_of_macro).to_string(), String::from_utf8_lossy(id).to_string()));
        }
    }
    Err(format!(
        "define directive not properly formed at {}",
        index_of_identifier
    ))
}
fn error_directive(tokens: &mut Vec<lexer::Token>) {
    todo!()
}
fn line_directive(tokens: &mut Vec<lexer::Token>, index: usize, end: usize) -> Result<(), String> {
    todo!()
}
fn undef_directive(
    tokens: &[lexer::Token],
    index: usize,
    defines: &mut HashMap<usize, Define>,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<usize, String> {
    let mut index_of_identifier = index + 1;
    let mut newline_index = index + 1;
    while !matches!(tokens.get(newline_index), Some(lexer::Token::NEWLINE)) {
        newline_index += 1;
        if matches!(tokens.get(newline_index), None) {
            return Err(String::from("missing newline for undef directive"));
        }
    }
    if matches!(
        tokens.get(index_of_identifier),
        Some(lexer::Token::WHITESPACE)
    ) {
        index_of_identifier += 1;
    }
    index_of_identifier += 1;
    if matches!(
        tokens.get(index_of_identifier),
        Some(lexer::Token::WHITESPACE)
    ) {
        index_of_identifier += 1;
        if let Some(lexer::Token::IDENT(identifier_to_be_undef_key)) =
            tokens.get(index_of_identifier)
        {
            defines.remove(identifier_to_be_undef_key);
            return Ok(newline_index + 1);
        }
    }
    Err(format!("undef directive not formed correctly"))
}
// TODO: we might need to reconsider the having the 'index' as an mutable reference
// because it's not entirely clear whether or not the 'index' might change.
// If we make the 'index' a returned value, the scope of the caller is encouraged to check its value to
// see if it changed.
fn expand_macro(
    tokens: &mut Vec<lexer::Token>,
    index: &mut usize,
    defines: &HashMap<usize, Define>,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(), String> {
    let index_copy = *index;
    let macro_id_key = match tokens[index_copy] {
        lexer::Token::IDENT(id_key) => id_key,
        _ => return Err(format!("non identifier token given")),
    };
    let macro_id = &str_maps.key_to_byte_vec[macro_id_key];
    if !defines.contains_key(&macro_id_key) {
        return Ok(());
    }
    let mut already_replaced_macro_names_key: Vec<usize> = Vec::new();
    let mut macros_to_replace: Vec<MacroInterval> = Vec::new();
    macros_to_replace.push(MacroInterval {
        name_key: macro_id_key,
        start: index_copy,
        len: 1,
    });
    let mut where_index_should_be_after_we_are_done = index_copy;
    loop {
        if macros_to_replace.is_empty() {
            break;
        }
        if let Some(last_macro_interval) = &macros_to_replace.last() {
            let Some(def_data) = defines.get(&last_macro_interval.name_key) else { unreachable!() };
            let mut replacement_list_copy = def_data.replacement_list.clone();
            if let Some(args) = &def_data.parameters {
                let mut fn_like_macro_index = macros_to_replace.last().unwrap().start + 1;
                while matches!(
                    tokens.get(fn_like_macro_index),
                    Some(lexer::Token::WHITESPACE) | Some(lexer::Token::NEWLINE)
                ) {
                    fn_like_macro_index += 1;
                }
                if let Some(lexer::Token::PUNCT_OPEN_PAR) = tokens.get(fn_like_macro_index) {
                    let start_of_arguments = fn_like_macro_index + 1;
                    fn_like_macro_index += 1;
                    let mut parenth_balance_counter = 1;
                    loop {
                        match tokens.get(fn_like_macro_index) {
                            Some(lexer::Token::PUNCT_OPEN_PAR) => {
                                parenth_balance_counter += 1;
                            }
                            Some(lexer::Token::PUNCT_CLOSE_PAR) => {
                                parenth_balance_counter -= 1;
                            }
                            None => break,
                            _ => {}
                        }
                        if parenth_balance_counter == 0 {
                            break;
                        }
                        fn_like_macro_index += 1;
                    }
                    if !matches!(
                        tokens.get(fn_like_macro_index),
                        Some(lexer::Token::PUNCT_CLOSE_PAR)
                    ) {
                        return Err(format!(
                            "no closing parenth for fn like macro invoc: {}",
                            String::from_utf8_lossy(
                                &str_maps.key_to_byte_vec[last_macro_interval.name_key]
                            )
                            .to_string()
                        ));
                    }
                    macros_to_replace.last_mut().unwrap().len =
                        fn_like_macro_index - macros_to_replace.last().unwrap().start + 1;

                    let mut seen_args = Vec::new();
                    {
                        let mut beginning_of_current_argument = start_of_arguments;
                        let mut argument_temp_index = start_of_arguments;
                        while argument_temp_index <= fn_like_macro_index {
                            match &tokens[argument_temp_index] {
                                lexer::Token::PUNCT_OPEN_PAR => {
                                    let mut parenth_balance_counter = 0;
                                    loop {
                                        match tokens.get(argument_temp_index) {
                                            Some(lexer::Token::PUNCT_OPEN_PAR) => {
                                                parenth_balance_counter += 1;
                                            }
                                            Some(lexer::Token::PUNCT_CLOSE_PAR) => {
                                                parenth_balance_counter -= 1;
                                            }
                                            None => unreachable!(),
                                            _ => {}
                                        }
                                        if parenth_balance_counter == 0 {
                                            break;
                                        }
                                        argument_temp_index += 1;
                                    }
                                }
                                lexer::Token::PUNCT_COMMA | lexer::Token::PUNCT_CLOSE_PAR => {
                                    if seen_args.len() < args.len()
                                        || matches!(
                                            tokens[argument_temp_index],
                                            lexer::Token::PUNCT_CLOSE_PAR
                                        )
                                    {
                                        if let Some(slice) = tokens
                                            .get(beginning_of_current_argument..argument_temp_index)
                                        {
                                            let mut slice_vec = slice.to_vec();
                                            while matches!(
                                                slice_vec.first(),
                                                Some(lexer::Token::WHITESPACE)
                                            ) {
                                                slice_vec.remove(0);
                                            }
                                            while matches!(
                                                slice_vec.last(),
                                                Some(lexer::Token::WHITESPACE)
                                            ) {
                                                slice_vec.pop();
                                            }
                                            seen_args.push(slice_vec);
                                            beginning_of_current_argument = argument_temp_index + 1;
                                        }
                                    }
                                }
                                _ => {}
                            }
                            argument_temp_index += 1;
                        }
                    }

                    if seen_args.len() < args.len()
                        || (seen_args.len() == args.len() && def_data.var_arg)
                    {
                        return Err(
                                format!(
                                    "wrong number of macro arguments given for macro: {}, seen_args len: {}, args len: {}, seen_args: {:?}, tokens: {:?}",
                                    String::from_utf8_lossy(&str_maps.key_to_byte_vec[macros_to_replace.last().unwrap().name_key]).to_string(),
                                    seen_args.len(),
                                    args.len(),
                                    seen_args,
                                    tokens,
                                ));
                    }

                    {
                        let mut replacement_list_index = 0;
                        while replacement_list_index < replacement_list_copy.len() {
                            if let Some(lexer::Token::IDENT(id_name_key)) =
                                replacement_list_copy.get(replacement_list_index)
                            {
                                let id_name = &str_maps.key_to_byte_vec[*id_name_key];
                                if args.contains(id_name_key) {
                                    let mut seen_arg_index = 0;
                                    for arg_index in 0..args.len() {
                                        if *str_maps.key_to_byte_vec[args[arg_index]] == *id_name {
                                            seen_arg_index = arg_index;
                                            break;
                                        }
                                    }
                                    let condition_one = replacement_list_index > 0
                                        && matches!(
                                            replacement_list_copy.get(replacement_list_index - 1),
                                            Some(lexer::Token::PUNCT_HASH)
                                        );
                                    let condition_two = replacement_list_index >= 2
                                        && matches!(
                                            replacement_list_copy.get(
                                                replacement_list_index - 2..replacement_list_index
                                            ),
                                            Some([
                                                lexer::Token::PUNCT_HASH,
                                                lexer::Token::WHITESPACE
                                            ])
                                        );
                                    if condition_one || condition_two {
                                        let mut removal_index = replacement_list_index;
                                        while !matches!(
                                            replacement_list_copy.get(removal_index),
                                            Some(lexer::Token::PUNCT_HASH)
                                        ) {
                                            removal_index -= 1;
                                        }
                                        for _ in removal_index..replacement_list_index + 1 {
                                            replacement_list_copy.remove(removal_index);
                                        }
                                        let mut string_literal_token =
                                            lexer::Token::StringLiteral {
                                                prefix_key: None,
                                                sequence_key: 0,
                                            };
                                        let argument = &seen_args[seen_arg_index];
                                        let lexer::Token::StringLiteral { prefix_key: _, sequence_key } =
                                        &mut string_literal_token else { panic!("WHAT IN THE FUCK") };
                                        let mut sequence = Vec::new();
                                        for t in argument {
                                            match t {
                                                lexer::Token::NEWLINE => {
                                                    sequence.push(b' ');
                                                }
                                                _ => {
                                                    if let Some(mut byte_vec_ified_token) =
                                                        t.to_byte_vec(str_maps)
                                                    {
                                                        if byte_vec_ified_token.contains(&b'\"')
                                                            || byte_vec_ified_token.contains(&b'\\')
                                                        {
                                                            let mut escaped_string = Vec::new();
                                                            for c in byte_vec_ified_token {
                                                                match c {
                                                                    b'\"' | b'\\' => {
                                                                        escaped_string.push(b'\\')
                                                                    }
                                                                    _ => {}
                                                                }
                                                                escaped_string.push(c);
                                                            }
                                                            byte_vec_ified_token = escaped_string;
                                                        }
                                                        sequence.extend_from_slice(
                                                            &byte_vec_ified_token,
                                                        );
                                                    } else {
                                                        return Err(format!("tried to stringify token that cannot be stringified"));
                                                    }
                                                }
                                            }
                                        }
                                        *sequence_key = str_maps.add_byte_vec(&sequence);
                                        replacement_list_copy
                                            .insert(removal_index, string_literal_token);
                                        replacement_list_index = removal_index + 1;
                                        continue;
                                    } else {
                                        //A parameter in the replacement list, UNLESS PRECEDED BY A # OR ## PREPROCESSING TOKEN OR
                                        //FOLLOWED BY A ## PREPROCESSING TOKEN (see below), is replaced by the corresponding argument
                                        //after all macros contained therein have been expanded
                                        let argument = &mut seen_args[seen_arg_index];
                                        let mut has_hash_or_hash_hash_before_or_after = false;
                                        if replacement_list_index > 0 {
                                            let mut left_check_index = replacement_list_index - 1;
                                            if matches!(
                                                replacement_list_copy.get(left_check_index),
                                                Some(lexer::Token::WHITESPACE)
                                            ) {
                                                left_check_index -= 1;
                                            }
                                            if matches!(
                                                replacement_list_copy.get(left_check_index),
                                                Some(lexer::Token::PUNCT_HASH_HASH)
                                            ) {
                                                has_hash_or_hash_hash_before_or_after = true;
                                            }
                                        }
                                        let mut right_check_index = replacement_list_index + 1;
                                        if matches!(
                                            replacement_list_copy.get(right_check_index),
                                            Some(lexer::Token::WHITESPACE)
                                        ) {
                                            right_check_index += 1;
                                        }
                                        if matches!(
                                            replacement_list_copy.get(replacement_list_index),
                                            Some(lexer::Token::PUNCT_HASH_HASH)
                                        ) {
                                            has_hash_or_hash_hash_before_or_after = true;
                                        }
                                        if !has_hash_or_hash_hash_before_or_after {
                                            let mut argument_index = 0;
                                            while argument_index < argument.len() {
                                                let t = argument[argument_index];
                                                if let lexer::Token::IDENT(
                                                    identifier_maybe_defined_key,
                                                ) = t
                                                {
                                                    let identifier_maybe_defined = str_maps
                                                        .key_to_byte_vec
                                                        .get(identifier_maybe_defined_key)
                                                        .unwrap();
                                                    if defines
                                                        .contains_key(&identifier_maybe_defined_key)
                                                    {
                                                        //TODO: we use recursion here because if we did it
                                                        //iteratively, the macros would be expanded but in some
                                                        //cases where the macro would expand into punctuators, it
                                                        //would be hard to distinguish where each argument/slice of
                                                        //tokens began and ended.
                                                        //
                                                        //Ideally, we would want to remove recursion completely
                                                        //One solution would be to insert some implementation
                                                        //defined token to distinguish where arguments are
                                                        //separated but that also seems scuffed as fuck.
                                                        expand_macro(
                                                            argument,
                                                            &mut argument_index,
                                                            defines,
                                                            str_maps,
                                                        )?;
                                                        continue;
                                                    }
                                                }
                                                argument_index += 1;
                                            }
                                        }
                                        let mut replacement_list_index_copy =
                                            replacement_list_index;
                                        let count_of_non_whitespace_tokens = argument
                                            .iter()
                                            .filter(|t| !matches!(t, lexer::Token::WHITESPACE))
                                            .count();
                                        replacement_list_copy.remove(replacement_list_index);
                                        if count_of_non_whitespace_tokens > 0 {
                                            for t in argument {
                                                replacement_list_copy
                                                    .insert(replacement_list_index_copy, *t);
                                                replacement_list_index_copy += 1;
                                            }
                                        } else if has_hash_or_hash_hash_before_or_after {
                                            replacement_list_copy.insert(
                                                replacement_list_index_copy,
                                                lexer::Token::PLACEMARKER,
                                            );
                                        }
                                        replacement_list_index = replacement_list_index_copy;
                                        continue;
                                    }
                                } else if *id_name == *"__VA_ARGS__".as_bytes() {
                                    let va_args_slice = seen_args[args.len()].clone();
                                    let condition_one = replacement_list_index > 0
                                        && matches!(
                                            replacement_list_copy.get(replacement_list_index - 1),
                                            Some(lexer::Token::PUNCT_HASH)
                                        );
                                    let condition_two = replacement_list_index >= 2
                                        && matches!(
                                            replacement_list_copy.get(
                                                replacement_list_index - 2..replacement_list_index
                                            ),
                                            Some([
                                                lexer::Token::PUNCT_HASH,
                                                lexer::Token::WHITESPACE
                                            ])
                                        );
                                    if condition_one || condition_two {
                                        let mut punct_hash_index = replacement_list_index;
                                        while !matches!(
                                            replacement_list_copy.get(punct_hash_index),
                                            Some(lexer::Token::PUNCT_HASH)
                                        ) {
                                            punct_hash_index -= 1;
                                        }
                                        for _ in punct_hash_index..replacement_list_index + 1 {
                                            replacement_list_copy.remove(punct_hash_index);
                                        }
                                        let mut string_literal_token =
                                            lexer::Token::StringLiteral {
                                                prefix_key: None,
                                                sequence_key: 0,
                                            };
                                        let lexer::Token::StringLiteral { prefix_key: _, sequence_key } =
                                        &mut string_literal_token else { panic!("WHAT IN THE FUCK") };
                                        let mut sequence = Vec::new();
                                        for t in va_args_slice {
                                            match t {
                                                lexer::Token::NEWLINE => {
                                                    sequence.push(b' ');
                                                }
                                                _ => {
                                                    if let Some(mut t_str) = t.to_byte_vec(str_maps)
                                                    {
                                                        if t_str.contains(&b'\"')
                                                            || t_str.contains(&b'\\')
                                                        {
                                                            let mut escaped_string = Vec::new();
                                                            for c in t_str {
                                                                match c {
                                                                    b'\"' | b'\\' => {
                                                                        escaped_string.push(b'\\')
                                                                    }
                                                                    _ => {}
                                                                }
                                                                escaped_string.push(c);
                                                            }
                                                            t_str = escaped_string;
                                                        }
                                                        sequence.extend_from_slice(&t_str);
                                                    } else {
                                                        return Err(format!("tried to stringify token that cannot be stringified"));
                                                    }
                                                }
                                            }
                                        }
                                        *sequence_key = str_maps.add_byte_vec(&sequence);
                                        replacement_list_copy
                                            .insert(punct_hash_index, string_literal_token);
                                    } else {
                                        assert!(args.len() < seen_args.len());
                                        replacement_list_copy.remove(replacement_list_index);
                                        let mut replacement_list_index_incremented =
                                            replacement_list_index;
                                        for t in va_args_slice {
                                            replacement_list_copy
                                                .insert(replacement_list_index_incremented, t);
                                            replacement_list_index_incremented += 1;
                                        }
                                    }
                                }
                            }
                            replacement_list_index += 1;
                        }
                    }
                } else {
                    return Ok(());
                }
            }
            /*
                For both object-like and function-like macro invocations,
                before the replacement list is reexamined for more macro names to replace,
                each instance of a ## preprocessing token in the replacement list
                not from an argument) is deleted and the preceding preprocessing token is concatenated with the
                following preprocessing token. Placemarker preprocessing tokens are handled specially: concatena-
                tion of two placemarkers results in a single placemarker preprocessing token, and concatenation
                of a placemarker with a non-placemarker preprocessing token results in the non-placemarker pre-
                processing token. If the result is not a valid preprocessing token, the behavior is undefined. The
                resulting token is available for further macro replacement. The order of evaluation of ## operators is
                unspecified.
            */
            let mut punct_hash_hash_index = 0;
            while punct_hash_hash_index < replacement_list_copy.len() {
                if let Some(lexer::Token::PUNCT_HASH_HASH) =
                    replacement_list_copy.get(punct_hash_hash_index)
                {
                    let mut start_removal = punct_hash_hash_index - 1;
                    if !matches!(
                        replacement_list_copy.get(start_removal),
                        Some(lexer::Token::WHITESPACE)
                    ) {
                        start_removal += 1;
                    }
                    while matches!(
                        replacement_list_copy.get(start_removal),
                        Some(lexer::Token::WHITESPACE | lexer::Token::PUNCT_HASH_HASH)
                    ) {
                        replacement_list_copy.remove(start_removal);
                    }
                    match replacement_list_copy.get(start_removal - 1..start_removal + 1) {
                        Some([lexer::Token::PLACEMARKER, lexer::Token::PLACEMARKER])
                        | Some([_, lexer::Token::PLACEMARKER]) => {
                            replacement_list_copy.remove(start_removal);
                        }
                        Some([lexer::Token::PLACEMARKER, _]) => {
                            replacement_list_copy.remove(start_removal - 1);
                        }
                        _ => {
                            punct_hash_hash_index += 1;
                        }
                    }
                    continue;
                }
                punct_hash_hash_index += 1;
            }

            {
                // the only things that can be macro names are identifiers which only have
                // token types IDENT and CONSTANT_DEC_INT
                // So we only look for those to combine in order to rescan and replace.
                let mut byte_vec_ified_tokens = Vec::new();
                let mut concat_ident_index = 0;
                while concat_ident_index < replacement_list_copy.len() {
                    while let Some(lexer::Token::IDENT(_) | lexer::Token::CONSTANT_DEC_INT { .. }) =
                        replacement_list_copy.get(concat_ident_index)
                    {
                        let concat_id = replacement_list_copy[concat_ident_index]
                            .to_byte_vec(str_maps)
                            .expect("a token that can be stringified");
                        if byte_vec_ified_tokens.is_empty()
                            && matches!(
                                replacement_list_copy.get(concat_ident_index),
                                Some(lexer::Token::CONSTANT_DEC_INT { .. })
                            )
                        {
                            byte_vec_ified_tokens.extend_from_slice(&concat_id);
                            replacement_list_copy.remove(concat_ident_index);
                            break;
                        }
                        byte_vec_ified_tokens.extend_from_slice(&concat_id);
                        replacement_list_copy.remove(concat_ident_index);
                    }
                    if !byte_vec_ified_tokens.is_empty() {
                        let new_ident_token_vec =
                            lexer::lexer(&byte_vec_ified_tokens, true, str_maps)?;
                        assert!(new_ident_token_vec.len() == 1);
                        replacement_list_copy.insert(concat_ident_index, new_ident_token_vec[0]);
                        byte_vec_ified_tokens.clear();
                    }
                    concat_ident_index += 1;
                }
            }

            {
                already_replaced_macro_names_key.push(macros_to_replace.last().unwrap().name_key);
                let mut index_overwrite = macros_to_replace.last().unwrap().start;
                let mut tokens_copy_start =
                    macros_to_replace.last().unwrap().start + macros_to_replace.last().unwrap().len;
                let mut tokens_copy = tokens[tokens_copy_start..].to_vec();
                tokens.resize(
                    tokens.len() - macros_to_replace.last().unwrap().len
                        + replacement_list_copy.len(),
                    lexer::Token::WHITESPACE,
                );
                macros_to_replace.last_mut().unwrap().len = replacement_list_copy.len();
                for t in replacement_list_copy {
                    tokens[index_overwrite] = t;
                    index_overwrite += 1;
                }
                for t in tokens_copy {
                    tokens[index_overwrite] = t;
                    index_overwrite += 1;
                }
            }

            let top_macro_interval = macros_to_replace.pop().unwrap();
            let beginning = top_macro_interval.start;
            let end = top_macro_interval.start + top_macro_interval.len;
            for looking_for_moar_macro_index in beginning..end {
                if let Some(lexer::Token::IDENT(key_for_identifier_that_could_be_macro)) =
                    tokens.get(looking_for_moar_macro_index)
                {
                    if defines.contains_key(key_for_identifier_that_could_be_macro)
                        && !already_replaced_macro_names_key
                            .contains(key_for_identifier_that_could_be_macro)
                    {
                        macros_to_replace.push(MacroInterval {
                            name_key: *key_for_identifier_that_could_be_macro,
                            start: looking_for_moar_macro_index,
                            len: 1,
                        });
                    }
                }
            }

            if macros_to_replace.is_empty() {
                where_index_should_be_after_we_are_done = end;
            }
        }
    }
    *index = where_index_should_be_after_we_are_done;
    Ok(())
}
fn preprocessing_directives(
    tokens: &mut Vec<lexer::Token>,
    include_paths: &[&str],
    defines: &mut HashMap<usize, Define>,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(), String> {
    // the C standard talks about "grouping" where the operands are grouped with the operators
    //
    // if <condition>; the condition is an integer constant expression except that all identifiers
    // are treated like they are either macro names or not.
    // The punctuators that are allowed in the condition expression are the ones under the
    // expression section in the C spec.
    // The constant-expression section in the c17 spec sort of states why...i guess.
    // An integer constant expression shall have integer type and shall only have operands that are integer
    // constants, enumeration constants, character constants
    let mut index: usize = 0;
    let mut final_tokens = Vec::new();
    while index < tokens.len() {
        match &tokens[index] {
            lexer::Token::PUNCT_HASH => {
                let mut index_copy = index;
                let preceded_only_by_whitespace_nothing_or_newline = loop {
                    if index_copy > 0 {
                        index_copy -= 1;
                    } else {
                        break true;
                    }
                    match tokens[index_copy] {
                        lexer::Token::WHITESPACE => {}
                        lexer::Token::NEWLINE => {
                            break true;
                        }
                        _ => break false,
                    }
                };
                if preceded_only_by_whitespace_nothing_or_newline {
                    let mut index_of_directive = index + 1;
                    if let Some(lexer::Token::WHITESPACE) = tokens.get(index_of_directive) {
                        index_of_directive += 1;
                    }
                    if let Some(lexer::Token::IDENT(s)) = tokens.get(index_of_directive) {
                        match str_maps.key_to_byte_vec[*s].as_slice() {
                            b"include" => {
                                index = include_directive(
                                    tokens,
                                    index,
                                    include_paths,
                                    defines,
                                    str_maps,
                                    &mut final_tokens,
                                )?;
                            }
                            b"if" | b"ifdef" | b"ifndef" => {
                                if_directive(tokens, index, defines, str_maps)?;
                            }
                            b"define" => {
                                index = define_directive(tokens, index, defines, str_maps)?;
                            }
                            b"undef" => {
                                index = undef_directive(tokens, index, defines, str_maps)?;
                            }
                            b"endif" => {
                                return Err(format!("missing if directive for endif directive"));
                            }
                            b"error" => todo!(),
                            b"line" => todo!(),
                            b"pragma" => todo!(),
                            b"\n" => {
                                index += 1;
                            }
                            _ => return Err(format!("unknown preprocessing directive: {}", s)),
                        }
                    }
                    continue;
                }
            }
            lexer::Token::IDENT(_) => {
                let old_index = index;
                expand_macro(tokens, &mut index, defines, str_maps)?;
                if old_index != index {
                    for add_index in old_index..index {
                        final_tokens.push(tokens[add_index]);
                    }
                    continue;
                }
            }
            _ => {}
        }
        final_tokens.push(tokens[index]);
        index += 1;
    }
    if index >= tokens.len() {
        *tokens = final_tokens;
        return Ok(());
    }
    Err(String::from("unable to preprocess"))
}
pub fn output_tokens_stdout(tokens: &Vec<lexer::Token>, str_maps: &lexer::ByteVecMaps) {
    print!(
        "{}",
        String::from_utf8_lossy(
            tokens
                .iter()
                .map(|t| t.to_byte_vec(str_maps).unwrap())
                .fold(Vec::new(), |mut a: Vec<u8>, e| {
                    a.extend_from_slice(&e);
                    a
                })
                .as_slice()
        )
        .to_string()
    );
}
// TODO: add flag options so that the user could specify if they wanted to only preprocess
// TODO: implement some kind of warning system
pub fn cpp(
    program_str: Vec<u8>,
    include_paths: &[&str],
    defines: &mut HashMap<usize, Define>,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<Vec<lexer::Token>, String> {
    // step 2 in the translation phase
    let mut backslash_newline_spliced = Vec::with_capacity(program_str.len());
    let mut add_index = 0;
    while add_index < program_str.len() {
        if program_str[add_index] == b'\\'
            && add_index + 1 < program_str.len()
            && program_str[add_index + 1] == b'\n'
        {
            add_index += 2;
            continue;
        }
        backslash_newline_spliced.push(program_str[add_index]);
        add_index += 1;
    }
    // step 3 in the translation phase
    let comments_removed = comments(backslash_newline_spliced.as_slice())?;
    let mut lexed_tokens = lexer::lexer(&comments_removed, true, str_maps)?;
    // step 4 in the translation phase
    preprocessing_directives(&mut lexed_tokens, include_paths, defines, str_maps)?;
    Ok(lexed_tokens)
}

#[cfg(test)]
mod tests {

    use crate::lexer;
    use std::collections::HashMap;

    use super::{
        comments, cpp, define_directive, eval_constant_expression, expand_macro, if_directive,
        include_directive, preprocessing_directives, Define,
    };
    #[test]
    fn comments_removal_outside_quotes() -> Result<(), String> {
        let src = "int main() {\n\"hi\"; // this is me\n}\n";
        let src_bytes = src.as_bytes();
        let removed = comments(src_bytes)?;
        let stringed = String::from_utf8_lossy(&removed).to_string();
        assert_eq!(stringed, "int main() {\n\"hi\";  \n}\n");
        Ok(())
    }
    #[test]
    fn comments_removal_inside_single_quotes() -> Result<(), String> {
        let src = "int main() {\n\"hi\"; '// this is me';\n}\n";
        let src_bytes = src.as_bytes();
        let removed = comments(src_bytes)?;
        let stringed = String::from_utf8_lossy(&removed).to_string();
        assert_eq!(stringed, "int main() {\n\"hi\"; '// this is me';\n}\n");
        Ok(())
    }
    #[test]
    fn comments_removal_inside_double_quotes() -> Result<(), String> {
        let src = "int main() {\n\"hi\"; \"// this is me\";\n}\n";
        let src_bytes = src.as_bytes();
        let removed = comments(src_bytes)?;
        let stringed = String::from_utf8_lossy(&removed).to_string();
        assert_eq!(stringed, "int main() {\n\"hi\"; \"// this is me\";\n}\n");
        Ok(())
    }
    #[test]
    fn block_comment_removal() -> Result<(), String> {
        let src = r##"/*
        HI THIS IS JASON HAR HAR HAR
            */"##;
        let src_bytes = src.as_bytes();
        let removed = comments(src_bytes)?;
        let stringed = String::from_utf8_lossy(&removed).to_string();
        assert_eq!(stringed, " ");
        Ok(())
    }
    #[test]
    fn include_test() -> Result<(), String> {
        let src = r##"#include "hi.h"
int main() {
}"##
        .as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut final_tokens = Vec::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        include_directive(&mut tokens, 0, &["./test_c_files/"], &mut defines, &mut str_maps, &mut final_tokens)?;
        let assert_tokens = [
            lexer::Token::IDENT(str_maps.add_byte_vec("int".as_bytes())),
            lexer::Token::WHITESPACE,
            lexer::Token::IDENT(str_maps.add_byte_vec("main".as_bytes())),
            lexer::Token::PUNCT_OPEN_PAR,
            lexer::Token::PUNCT_CLOSE_PAR,
            lexer::Token::WHITESPACE,
            lexer::Token::PUNCT_OPEN_CURLY,
            lexer::Token::NEWLINE,
            lexer::Token::PUNCT_CLOSE_CURLY,
        ]
        .to_vec();
        assert_eq!(assert_tokens, tokens);
        Ok(())
    }
    #[test]
    fn preprocess_test() -> Result<(), String> {
        let src = r##"#include "hi2.h"
int main() {
hi;
}"##
        .as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        preprocessing_directives(
            &mut tokens,
            &["./test_c_files"],
            &mut defines,
            &mut str_maps,
        )?;
        let assert_tokens = vec![
            lexer::Token::IDENT(2),
            lexer::Token::WHITESPACE,
            lexer::Token::IDENT(3),
            lexer::Token::PUNCT_OPEN_PAR,
            lexer::Token::PUNCT_CLOSE_PAR,
            lexer::Token::WHITESPACE,
            lexer::Token::PUNCT_OPEN_CURLY,
            lexer::Token::NEWLINE,
            lexer::Token::CONSTANT_DEC_INT {
                value_key: 6,
                suffix_key: None,
            },
            lexer::Token::PUNCT_SEMI_COLON,
            lexer::Token::NEWLINE,
            lexer::Token::PUNCT_CLOSE_CURLY,
        ];
        assert_eq!(assert_tokens, tokens);
        Ok(())
    }
    #[test]
    fn expand_macro_hash_operator() -> Result<(), String> {
        let src = r##"#define HI(a) #a
HI(5 5);"##.as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        expand_macro(&mut tokens, &mut 0, &defines, &mut str_maps)?;
        assert_eq!(
            vec![
            lexer::Token::PUNCT_HASH,
            lexer::Token::IDENT(str_maps.add_byte_vec("define".as_bytes())),
            lexer::Token::WHITESPACE,
            lexer::Token::IDENT(str_maps.add_byte_vec("HI".as_bytes())),
            lexer::Token::PUNCT_OPEN_PAR,
            lexer::Token::IDENT(str_maps.add_byte_vec("a".as_bytes())),
                lexer::Token::StringLiteral {
                    prefix_key: None,
                    sequence_key: str_maps.add_byte_vec("5 5".as_bytes())
                },
                lexer::Token::PUNCT_SEMI_COLON
            ],
            tokens
        );
        Ok(())
    }
    #[test]
    fn expand_macro_test_complex() -> Result<(), String> {
        let src = r##"#define hash_hash # ## #
#define mkstr(a) # a
#define in_between(a) mkstr(a)
#define join(c, d) in_between(c hash_hash d)
char p[] = join(x, y);"##;
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.as_bytes().to_vec(), true, &mut str_maps)?;
        let mut index = 0;
        let mut defines = HashMap::new();
        define_directive(&mut tokens, index, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens, index, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens, index, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens, index, &mut defines, &mut str_maps)?;
        while index < tokens.len() {
            if let Some(
                [lexer::Token::IDENT(first), lexer::Token::PUNCT_OPEN_PAR, lexer::Token::IDENT(second)],
            ) = tokens.get(index..index + 3)
            {
                if *first == str_maps.add_byte_vec("join".as_bytes()) && *second == str_maps.add_byte_vec("x".as_bytes()) {
                    break;
                }
            }
            index += 1;
        }
        expand_macro(&mut tokens, &mut index, &defines, &mut str_maps)?;
        assert_eq!(
            vec![
                lexer::Token::IDENT(str_maps.add_byte_vec("char".as_bytes())),
                lexer::Token::WHITESPACE,
                lexer::Token::IDENT(str_maps.add_byte_vec("p".as_bytes())),
                lexer::Token::PUNCT_OPEN_SQR,
                lexer::Token::PUNCT_CLOSE_SQR,
                lexer::Token::WHITESPACE,
                lexer::Token::PUNCT_ASSIGNMENT,
                lexer::Token::WHITESPACE,
                lexer::Token::StringLiteral {
                    prefix_key: None,
                    sequence_key: str_maps.add_byte_vec("x ## y".as_bytes())
                },
                lexer::Token::PUNCT_SEMI_COLON
            ],
            tokens
        );
        Ok(())
    }
    #[test]
    fn test_define_small() -> Result<(), String> {
        let src = r##"#define PP_STRINGIZE_ALL(...) #__VA_ARGS__
PP_STRINGIZE_ALL( hello       /* */ world) /* "hello world" */
"##
        .as_bytes()
        .to_vec();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut defines = HashMap::new();
        let mut tokens = cpp(src, &["./test_c_files"], &mut defines, &mut str_maps)?;
        assert_eq!(
            vec![
                lexer::Token::StringLiteral {
                    prefix_key: None,
                    sequence_key: str_maps.add_byte_vec("hello world".as_bytes())
                },
                lexer::Token::WHITESPACE,
                lexer::Token::NEWLINE
            ],
            tokens
        );
        Ok(())
    }
    #[test]
    fn test_define_directive() -> Result<(), String> {
        let mut str_maps = lexer::ByteVecMaps::new();
        let src = "#define hash_hash # ## #\n";
        let src2 = "#define mkstr(a) # a\n";
        let src3 = "#define in_between(a) mkstr(a)\n";
        let src4 = "#define join(c, d) in_between(c hash_hash d)\n";
        let mut tokens = lexer::lexer(&src.as_bytes().to_vec(), true, &mut str_maps)?;
        let mut tokens2 = lexer::lexer(&src2.as_bytes().to_vec(), true, &mut str_maps)?;
        let mut tokens3 = lexer::lexer(&src3.as_bytes().to_vec(), true, &mut str_maps)?;
        let mut tokens4 = lexer::lexer(&src4.as_bytes().to_vec(), true, &mut str_maps)?;
        let mut defines = HashMap::new();
        define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens2, 0, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens3, 0, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens4, 0, &mut defines, &mut str_maps)?;
        assert_eq!(defines.len(), 4);
        assert!(defines.contains_key(&str_maps.add_byte_vec("hash_hash".as_bytes())));
        assert!(defines.contains_key(&str_maps.add_byte_vec("mkstr".as_bytes())));
        assert!(defines.contains_key(&str_maps.add_byte_vec("in_between".as_bytes())));
        assert!(defines.contains_key(&str_maps.add_byte_vec("join".as_bytes())));
        assert_eq!(
            Define {
                parameters: None,
                var_arg: false,
                replacement_list: vec![
                    lexer::Token::PUNCT_HASH,
                    lexer::Token::WHITESPACE,
                    lexer::Token::PUNCT_HASH_HASH,
                    lexer::Token::WHITESPACE,
                    lexer::Token::PUNCT_HASH,
                ]
            },
            *defines.get(&str_maps.add_byte_vec("hash_hash".as_bytes())).unwrap()
        );
        assert_eq!(
            Define {
                parameters: Some(vec![str_maps.add_byte_vec("a".as_bytes())]),
                var_arg: false,
                replacement_list: vec![
                    lexer::Token::PUNCT_HASH,
                    lexer::Token::WHITESPACE,
                    lexer::Token::IDENT(str_maps.add_byte_vec("a".as_bytes())),
                ]
            },
            *defines.get(&str_maps.add_byte_vec("mkstr".as_bytes())).unwrap()
        );
        assert_eq!(
            Define {
                parameters: Some(vec![str_maps.add_byte_vec("a".as_bytes())]),
                var_arg: false,
                replacement_list: vec![
                    lexer::Token::IDENT(str_maps.add_byte_vec("mkstr".as_bytes())),
                    lexer::Token::PUNCT_OPEN_PAR,
                    lexer::Token::IDENT(str_maps.add_byte_vec("a".as_bytes())),
                    lexer::Token::PUNCT_CLOSE_PAR,
                ]
            },
            *defines
                .get(&str_maps.add_byte_vec("in_between".as_bytes()))
                .unwrap()
        );
        assert_eq!(
            Define {
                parameters: Some(vec![
                    str_maps.add_byte_vec("c".as_bytes()),
                    str_maps.add_byte_vec("d".as_bytes())
                ]),
                var_arg: false,
                replacement_list: vec![
                    lexer::Token::IDENT(str_maps.add_byte_vec("in_between".as_bytes())),
                    lexer::Token::PUNCT_OPEN_PAR,
                    lexer::Token::IDENT(str_maps.add_byte_vec("c".as_bytes())),
                    lexer::Token::WHITESPACE,
                    lexer::Token::IDENT(str_maps.add_byte_vec("hash_hash".as_bytes())),
                    lexer::Token::WHITESPACE,
                    lexer::Token::IDENT(str_maps.add_byte_vec("d".as_bytes())),
                    lexer::Token::PUNCT_CLOSE_PAR,
                ]
            },
            *defines
                .get(&str_maps.add_byte_vec("join".as_bytes()))
                .unwrap()
        );
        Ok(())
    }
    #[test]
    fn expand_macro_test_testing_object_macro_expansion_nested() -> Result<(), String> {
        let src = r##"#define HEHE(a) a
#define A HEHE(4)
A"##
        .as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        expand_macro(&mut tokens, &mut 0, &defines, &mut str_maps)?;
        assert_eq!(
            vec![lexer::Token::CONSTANT_DEC_INT {
                value_key: 7,
                suffix_key: None
            }],
            tokens
        );
        Ok(())
    }
    #[test]
    fn expand_macro_test_testing_object_macro_expansion_nested_2() -> Result<(), String> {
        let src = r##"#define HEHE(a) a
#define A HEHE(4) HEHE(5) HEHE(6)
A"##
        .as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        expand_macro(&mut tokens, &mut 0, &defines, &mut str_maps)?;
        assert_eq!(
            vec![
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: 7,
                    suffix_key: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: 9,
                    suffix_key: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: 11,
                    suffix_key: None
                }
            ],
            tokens
        );
        Ok(())
    }
    #[test]
    fn expand_macro_parentheses_argument() -> Result<(), String> {
        let src = r##"#define HI(a,b) a,b
HI((,),(,))"##
            .as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        expand_macro(&mut tokens, &mut 0, &defines, &mut str_maps)?;
        assert_eq!(
            vec![
                lexer::Token::PUNCT_OPEN_PAR,
                lexer::Token::PUNCT_COMMA,
                lexer::Token::PUNCT_CLOSE_PAR,
                lexer::Token::PUNCT_COMMA,
                lexer::Token::PUNCT_OPEN_PAR,
                lexer::Token::PUNCT_COMMA,
                lexer::Token::PUNCT_CLOSE_PAR,
            ],
            tokens
        );
        Ok(())
    }
    #[test]
    fn expand_macro_test_testing_fn_macro_expansion_nested() -> Result<(), String> {
        let src = r##"#define HEHE(a,b) a b
HEHE(HEHE(1,2),HEHE(3,4))"##
            .as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        expand_macro(&mut tokens, &mut 0, &defines, &mut str_maps)?;
        assert_eq!(
            vec![
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: 8,
                    suffix_key: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: 9,
                    suffix_key: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: 11,
                    suffix_key: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: 12,
                    suffix_key: None
                },
            ],
            tokens
        );
        Ok(())
    }
    #[test]
    fn expand_macro_test_fn_macro_with_arg_that_expands_to_comma() -> Result<(), String> {
        let src = r##"#define HAHA(a,b) a + b
#define C ,
HAHA(C,4)"##
            .as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        expand_macro(&mut tokens, &mut 0, &defines, &mut str_maps)?;
        assert_eq!(
            vec![
                lexer::Token::PUNCT_COMMA,
                lexer::Token::WHITESPACE,
                lexer::Token::PUNCT_PLUS,
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: 10,
                    suffix_key: None
                },
            ],
            tokens
        );
        Ok(())
    }
    #[test]
    fn expand_macro_test_fn_macro_not_clear() -> Result<(), String> {
        let src = r##"#define f(a) a*g
#define g(a) f(a)
f(2)(9)"##
            .as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        expand_macro(&mut tokens, &mut 0, &defines, &mut str_maps)?;
        assert_eq!(
            vec![
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: 11,
                    suffix_key: None
                },
                lexer::Token::PUNCT_MULT,
                lexer::Token::IDENT(10),
                lexer::Token::PUNCT_OPEN_PAR,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: 12,
                    suffix_key: None
                },
                lexer::Token::PUNCT_CLOSE_PAR,
            ],
            tokens
        );
        Ok(())
    }
    #[test]
    fn expand_macro_test_rescan() -> Result<(), String> {
        let src = r##"#define FOOBAR(a, b) printf(#a #b)
#define INVOKE(a, b) a##b(a, b)
INVOKE(FOO,BAR)"##
            .as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        expand_macro(&mut tokens, &mut 0, &defines, &mut str_maps)?;
        assert_eq!(
            vec![
                lexer::Token::IDENT(4),
                lexer::Token::PUNCT_OPEN_PAR,
                lexer::Token::StringLiteral {
                    prefix_key: None,
                    sequence_key: 16
                },
                lexer::Token::WHITESPACE,
                lexer::Token::StringLiteral {
                    prefix_key: None,
                    sequence_key: 17
                },
                lexer::Token::PUNCT_CLOSE_PAR,
            ],
            tokens
        );
        Ok(())
    }
    #[test]
    #[allow(non_snake_case)]
    fn __va_args___test() -> Result<(), String> {
        let src = r##"#define CHICKEN(...) __VA_ARGS__
CHICKEN(1 2,3 4)"##
            .as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        expand_macro(&mut tokens, &mut 8, &defines, &mut str_maps)?;
        assert_eq!(
            vec![
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: 4,
                    suffix_key: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: 5,
                    suffix_key: None
                },
                lexer::Token::PUNCT_COMMA,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: 6,
                    suffix_key: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: 7,
                    suffix_key: None
                },
            ],
            tokens
        );
        Ok(())
    }
    #[test]
    fn expand_macro_not_defined() -> Result<(), String> {
        let src = r##"HI"##.as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        expand_macro(&mut tokens, &mut 0, &mut defines, &mut str_maps)?;
        assert_eq!(vec![lexer::Token::IDENT(0),], tokens);
        Ok(())
    }
    #[test]
    fn expand_macro_side_by_side() -> Result<(), String> {
        let src = r##"#define PP(a, b) a ## b
#define PP2(a, b) a/**/b
PP(/,*)PP2(*,/)"##
            .as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        assert_eq!(
            vec![
                lexer::Token::PUNCT_DIV,
                lexer::Token::PUNCT_MULT,
                lexer::Token::PUNCT_MULT,
                lexer::Token::WHITESPACE,
                lexer::Token::PUNCT_DIV
            ],
            tokens
        );
        Ok(())
    }
    #[test]
    fn eval_expression_test_primary() -> Result<(), String> {
        {
            let src = r##"(1 + 1) * 0"##.as_bytes();
            let defines = HashMap::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
            assert_eq!(res, false, "(1 + 1) * 0");
        }
        {
            let src = r##"1 + (1 * 0)"##.as_bytes();
            let defines = HashMap::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
            assert_eq!(res, true, "1 + (1 * 0)");
        }
        {
            let src = r##"((1 + 1) * 0)"##.as_bytes();
            let defines = HashMap::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
            assert_eq!(res, false, "((1 + 1) * 0)");
        }
        {
            let src = r##"((((1))))"##.as_bytes();
            let defines = HashMap::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
            assert_eq!(res, true, "((((1))))");
        }
        {
            let src = r##"((((1)))))"##.as_bytes();
            let defines = HashMap::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = eval_constant_expression(&tokens, &defines, &mut str_maps);
            match res {
                Err(_) => {}
                Ok(_) => return Err(String::from("unbalanced parentheses not caught")),
            }
        }
        {
            let src = r##"(((((1))))"##.as_bytes();
            let defines = HashMap::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = eval_constant_expression(&tokens, &defines, &mut str_maps);
            match res {
                Err(_) => {}
                Ok(_) => return Err(String::from("unbalanced parentheses not caught")),
            }
        }
        {
            let src = r##"0 - (1 + 1)"##.as_bytes();
            let defines = HashMap::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
            assert_eq!(res, true, "0 - (1 + 1)");
        }
        {
            let src = r##"1"##.as_bytes();
            let defines = HashMap::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
            assert_eq!(res, true, "1");
        }
        Ok(())
    }
    #[test]
    fn eval_expression_test_unary() -> Result<(), String> {
        let src = r##"!1"##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, false, "!1");
        let src = r##"!0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true, "!0");
        let src = r##"~0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true, "~0");
        let src = r##"~~~0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true, "~~~0");
        let src = r##"~~~~0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, false, "~~~~0");
        let src = r##"--------------1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps);
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
        let defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"1 * !1"##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, false);
        let src = r##"1 / 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"1 / 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps);
        match res {
            Err(_) => {}
            Ok(_) => return Err("division by zero not caught".to_string()),
        }
        let src = r##"1 + 1 * 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"0 * 1 + 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_additive() -> Result<(), String> {
        let src = r##"1 + 1"##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"1 - 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, false);
        let src = r##"0 - 1 + 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, false, "0 - 1 + 1");
        let src = r##"0 - 1 + !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true, "0 - 1 + !1");
        Ok(())
    }
    #[test]
    fn eval_expression_test_bitshift() -> Result<(), String> {
        let src = r##"1 << 1"##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"1 >> 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, false);
        let src = r##"1 >> !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        Ok(())
    }
    #[test]
    fn eval_expression_test_relational() -> Result<(), String> {
        let src = r##"1 < 1"##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, false);
        let src = r##"1 < 2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"1 < !2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, false);
        let src = r##"1 <= 2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"2 <= 2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"1 > 2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, false);
        let src = r##"1 > 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"1 >= 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"1 >= 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        Ok(())
    }
    #[test]
    fn eval_expression_test_equality() -> Result<(), String> {
        let src = r##"1 == 1"##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"1 != 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, false);
        let src = r##"1 != !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"1 != 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        Ok(())
    }
    #[test]
    fn eval_expression_test_bit_and() -> Result<(), String> {
        let src = r##"1 & 0 == 0"##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"1 & 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, false);
        let src = r##"1 & !0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"1 & 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"1 == 0 & 1 == 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_bit_xor() -> Result<(), String> {
        let src = r##"1 ^ 0 == 0"##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, false);
        let src = r##"(1 ^ !0) == 0"##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        Ok(())
    }
    #[test]
    fn eval_expression_test_bit_or() -> Result<(), String> {
        let src = r##"1 | 0 == 0"##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"1 | !0 == 0"##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true, "1 | !0 == 0");
        Ok(())
    }
    #[test]
    fn eval_expression_test_logical_and() -> Result<(), String> {
        let src = r##"1 && 1"##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"0 && 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, false);
        let src = r##"1 && !1"##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_logical_or() -> Result<(), String> {
        let src = r##"1 || 1"##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"0 || 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"0 || 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_conditional() -> Result<(), String> {
        let src = r##"1 ? 1 : 0"##.as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"(1 + 1 == 3) ? 1 : 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, false);
        let src = r##"~0 ? (1 + 1 == 2) : 0 * 4"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"0 ? 0 : 1 * 4"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, true);
        let src = r##"0 ? 0 : !(1 * 4)"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_defined() -> Result<(), String> {
        let src = r##"defined(HI)"##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, false);
        let src = r##"defined HI "##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps)?;
        assert_eq!(res, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_empty() -> Result<(), String> {
        let src = r##""##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = eval_constant_expression(&tokens, &defines, &mut str_maps);
        match res {
            Err(_) => {}
            Ok(_) => return Err(String::from("empty expression not caught")),
        }
        Ok(())
    }
    #[test]
    fn if_directive_test() -> Result<(), String> {
        {
            let src = r##"#if 1
4
#endif
"##
            .as_bytes();
            let defines = HashMap::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            if_directive(&mut tokens, 0, &defines, &mut str_maps)?;

            assert_eq!(
                vec![
                    lexer::Token::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("4".as_bytes()),
                        suffix_key: None
                    },
                    lexer::Token::NEWLINE,
                ],
                tokens,
                "failed for 1 inner test"
            );
        }
        {
            let src = r##"#if 0
4
#endif
"##
            .as_bytes();
            let defines = HashMap::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            if_directive(&mut tokens, 0, &defines, &mut str_maps)?;
            assert_eq!(
                vec![] as Vec<lexer::Token>,
                tokens,
                "failed for 2 inner test"
            );
        }
        {
            let src = r##"#if 1 + 1 > 0
4
#endif
"##
            .as_bytes();
            let defines = HashMap::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            if_directive(&mut tokens, 0, &defines, &mut str_maps)?;

            assert_eq!(
                vec![
                    lexer::Token::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("4".as_bytes()),
                        suffix_key: None
                    },
                    lexer::Token::NEWLINE,
                ],
                tokens,
                "failed for 3 inner test"
            );
        }
        {
            let src = r##"#if 1 + 1 > 2
4
#endif
"##
            .as_bytes();
            let defines = HashMap::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            if_directive(&mut tokens, 0, &defines, &mut str_maps)?;
            assert_eq!(
                vec![] as Vec<lexer::Token>,
                tokens,
                "failed for 4 inner test"
            );
        }
        {
            let src = r##"#ifndef hi
4
#endif
"##
            .as_bytes();
            let defines = HashMap::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            if_directive(&mut tokens, 0, &defines, &mut str_maps)?;

            assert_eq!(
                vec![
                    lexer::Token::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("4".as_bytes()),
                        suffix_key: None
                    },
                    lexer::Token::NEWLINE,
                ],
                tokens,
                "failed for 5 inner test"
            );
        }
        {
            let src = r##"#ifdef hi
4
#else
5
#endif
"##
            .as_bytes();
            let defines = HashMap::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            if_directive(&mut tokens, 0, &defines, &mut str_maps)?;
            assert_eq!(
                vec![
                    lexer::Token::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("5".as_bytes()),
                        suffix_key: None
                    },
                    lexer::Token::NEWLINE,
                ],
                tokens,
                "failed 6"
            );
        }
        {
            let src = r##"#define add(a,b) a + b
#if add(4,4) < 8
4
#elif add(1,4) > 0
5
#endif
"##
            .as_bytes();
            let mut defines = HashMap::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = cpp(
                src.to_vec(),
                &["./test_c_files"],
                &mut defines,
                &mut str_maps,
            )?;
            assert_eq!(
                vec![
                    lexer::Token::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("5".as_bytes()),
                        suffix_key: None
                    },
                    lexer::Token::NEWLINE,
                ],
                tokens,
                "failed 7"
            );
        }
        //        {
        //            let src = r##"#define add(a,b) a + b
        //#if add(4,4) < '8'
        //4
        //#elif add(1,4) > '0'
        //5
        //#endif
        //"##
        //            .as_bytes();
        //            let mut defines = HashMap::new();
        //            let mut tokens = cpp(src.to_vec(), &["./test_c_files"], &mut defines)?;
        //            assert_eq!(
        //                vec![
        //                    lexer::Token::CONSTANT_DEC_INT {
        //                        value: "5".to_string(),
        //                        suffix: None
        //                    },
        //                    lexer::Token::NEWLINE,
        //                ],
        //                tokens,
        //                "failed 8"
        //            );
        //        }
        {
            let src = r##"#if HI && 1
4
#else
5
#endif
"##
            .as_bytes();
            let mut defines = HashMap::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = cpp(
                src.to_vec(),
                &["./test_c_files"],
                &mut defines,
                &mut str_maps,
            )?;
            assert_eq!(
                vec![
                    lexer::Token::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("5".as_bytes()),
                        suffix_key: None
                    },
                    lexer::Token::NEWLINE,
                ],
                tokens,
                "failed 9"
            );
        }
        {
            let src = r##"#define HI
#if defined(HI) && 1
4
#else
5
#endif
"##
            .as_bytes();
            let mut defines = HashMap::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = cpp(
                src.to_vec(),
                &["./test_c_files"],
                &mut defines,
                &mut str_maps,
            )?;
            assert_eq!(
                vec![
                    lexer::Token::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("4".as_bytes()),
                        suffix_key: None
                    },
                    lexer::Token::NEWLINE,
                ],
                tokens,
                "failed 10"
            );
        }
        Ok(())
    }
    #[test]
    fn cpp_test_with_complicated_header() -> Result<(), String> {
        let src = r##"#include "pp-acid-test.h"
"##;
        let mut defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = cpp(
            src.as_bytes().to_vec(),
            &["./test_c_files"],
            &mut defines,
            &mut str_maps,
        )?;
        let assert_tokens: Vec<lexer::Token> = vec![];
        let s = tokens
            .iter()
            .map(|t| t.to_byte_vec(&str_maps).unwrap())
            .fold(Vec::new(), |mut a: Vec<u8>, e| {
                a.extend_from_slice(&e);
                a
            });
        println!("{s:?}");
        Ok(())
    }
}
