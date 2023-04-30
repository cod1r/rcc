use std::collections::HashMap;

use crate::lexer::{self};
use crate::parser::{self};

#[derive(PartialEq, Debug, Clone)]
struct Define {
    identifier: String,
    parameters: Option<Vec<String>>,
    var_arg: bool,
    replacement_list: Vec<lexer::Token>,
}
#[derive(Debug)]
struct MacroInterval {
    name: String,
    start: usize,
    end: usize,
}

fn comments(bytes: &[u8]) -> Result<Vec<u8>, String> {
    let mut byte_index = 0;
    let mut within_quotes = false;
    let mut comments_removed = Vec::new();
    while byte_index < bytes.len() {
        if bytes[byte_index] == b'\'' || bytes[byte_index] == b'\"' {
            within_quotes = true;
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
                within_quotes = false;
                comments_removed.push(bytes[byte_index]);
                byte_index += 1;
            } else {
                return Err(format!("no matching ending quote"));
            }
        } else if byte_index + 1 < bytes.len() && !within_quotes {
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
fn get_header_name_from_tokens(tokens: &[lexer::Token]) -> Option<String> {
    if let (Some(lexer::Token::PUNCT_LESS_THAN), Some(lexer::Token::PUNCT_GREATER_THAN)) =
        (tokens.first(), tokens.last())
    {
        let mut stringified = tokens[1..tokens.len() - 1].iter().map(|t| t.to_string());
        if stringified.any(|t_opt| t_opt.is_none()) {
            return None;
        }
        return Some(stringified.fold(String::new(), |mut acc, e| {
            acc += &e.unwrap();
            acc
        }));
    }
    None
}
fn include_directive(
    tokens: &mut Vec<lexer::Token>,
    mut index: usize,
    end: usize,
    include_paths: &[&str],
    defines: &HashMap<String, Define>,
) -> Result<(), String> {
    let mut index_header_file = index + 2;
    while index_header_file < tokens.len() {
        if matches!(
            tokens.get(index_header_file),
            Some(
                lexer::Token::PUNCT_LESS_THAN
                    | lexer::Token::IDENT(_)
                    | lexer::Token::StringLiteral { .. }
            )
        ) {
            break;
        }
        index_header_file += 1;
    }
    let mut file_name = String::new();
    match &tokens[index_header_file] {
        lexer::Token::PUNCT_LESS_THAN => {
            if let Some(new_file_name) =
                get_header_name_from_tokens(&tokens[index_header_file..end])
            {
                file_name = new_file_name;
            } else {
                return Err(format!(
                    "unknown token in include directive: {:?}",
                    tokens[index_header_file]
                ));
            }
        }
        lexer::Token::IDENT(identifier) => {
            if let Some(def_data) = defines.get(identifier) {
                if let Some(new_file_name) = get_header_name_from_tokens(&def_data.replacement_list)
                {
                    file_name = new_file_name;
                } else {
                    return Err(format!(
                        "unknown token in include directive: {:?}",
                        tokens[index_header_file]
                    ));
                }
            } else {
                return Err(format!(
                    "unknown identifier in include directive: {}",
                    identifier
                ));
            }
        }
        lexer::Token::StringLiteral { prefix, sequence } => {
            if prefix.is_none() {
                file_name = sequence.to_string();
            } else {
                return Err(format!(
                    "unknown token in include directive: {:?}",
                    tokens[index_header_file]
                ));
            }
        }
        _ => {
            return Err(format!(
                "unknown token in include directive: {:?}",
                tokens[index_header_file]
            ))
        }
    }
    for path in include_paths {
        if let Ok(mut ei) = std::fs::read_dir(path) {
            for entry in ei.by_ref().flatten() {
                let name = entry.file_name().to_string_lossy().to_string();
                if name == file_name {
                    if let Ok(file_contents) = std::fs::read(path.to_string() + "/" + &name) {
                        if let Ok(tokens_from_file) = lexer::lexer(file_contents, true) {
                            loop {
                                if let lexer::Token::NEWLINE = tokens[index] {
                                    tokens.remove(index);
                                    break;
                                }
                                tokens.remove(index);
                            }
                            for t in tokens_from_file {
                                tokens.insert(index, t);
                                index += 1;
                            }
                            return Ok(());
                        }
                    }
                }
            }
        }
    }
    Err(String::from("file not found"))
}

/*
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
*/
fn right_has_higher_priority(left: &mut parser::Expr, right: &mut parser::Expr) {
    assert!(right.priority() > left.priority());
    match left {
        parser::Expr::Unary(u) => {
            match right {
                parser::Expr::Primary(p) => {
                    assert!(p.is_some());
                    assert!(u.first.is_none());
                }
                _ => unreachable!(),
            }
            u.first = Some(Box::new(right.clone()));
        }
        parser::Expr::Multiplicative(m) => {
            match right {
                parser::Expr::Primary(p) => {
                    assert!(p.is_some());
                    assert!(m.second.is_none());
                }
                parser::Expr::Unary(_) => {
                    assert!(m.second.is_none());
                }
                _ => unreachable!(),
            }
            m.second = Some(Box::new(right.clone()));
        }
        parser::Expr::Additive(a) => {
            match right {
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
            }
            a.second = Some(Box::new(right.clone()));
        }
        parser::Expr::BitShift(bs) => {
            match right {
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
            }
            bs.second = Some(Box::new(right.clone()));
        }
        parser::Expr::Relational(r) => {
            match right {
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
            }
            r.second = Some(Box::new(right.clone()));
        }
        parser::Expr::Equality(e) => {
            match right {
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
            }
            e.second = Some(Box::new(right.clone()));
        }
        parser::Expr::BitAND(ba) => {
            match right {
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
            }
            ba.second = Some(Box::new(right.clone()));
        }
        parser::Expr::BitXOR(bx) => {
            match right {
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
            }
            bx.second = Some(Box::new(right.clone()));
        }
        parser::Expr::BitOR(bo) => {
            match right {
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
            }
            bo.second = Some(Box::new(right.clone()));
        }
        parser::Expr::LogicalAND(la) => {
            match right {
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
            }
            la.second = Some(Box::new(right.clone()));
        }
        parser::Expr::LogicalOR(lo) => {
            match right {
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
            }
            lo.second = Some(Box::new(right.clone()));
        }
        parser::Expr::Conditional(c) => {
            todo!("we gotta see how we construct conditional expressions first");
            match right {
                parser::Expr::Primary(_) => {}
                parser::Expr::Unary(_) => {}
                parser::Expr::Multiplicative(_) => {}
                parser::Expr::Additive(_) => {}
                parser::Expr::BitShift(_) => {}
                parser::Expr::Relational(_) => {}
                parser::Expr::Equality(_) => {}
                parser::Expr::BitAND(_) => {}
                parser::Expr::BitXOR(_) => {}
                parser::Expr::BitOR(_) => {}
                parser::Expr::LogicalAND(_) => {}
                parser::Expr::LogicalOR(_) => {}
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}

fn left_has_higher_eq_priority(left: &mut parser::Expr, right: &mut parser::Expr) {
    assert!(left.priority() >= right.priority());
    match right {
        parser::Expr::Multiplicative(m) => {}
        parser::Expr::Additive(a) => {}
        parser::Expr::BitShift(bs) => {}
        parser::Expr::Relational(r) => {}
        parser::Expr::Equality(e) => {}
        parser::Expr::BitAND(ba) => {}
        parser::Expr::BitXOR(bx) => {}
        parser::Expr::BitOR(bo) => {}
        parser::Expr::LogicalAND(la) => {}
        parser::Expr::LogicalOR(lo) => {}
        parser::Expr::Conditional(c) => {}
        _ => unreachable!(),
    }
}

//Notes:
//The expression that controls conditional inclusion shall be an integer constant expression
//Because the controlling constant expression is evaluated during translation phase 4, all identifiers either are or are not macro names — there simply are no keywords, enumeration constants, etc
//All macro identifiers are evaluated as defined or not defined.
fn eval_constant_expression(tokens: &[lexer::Token]) -> Result<bool, String> {
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
                parenth_balance.push(tokens[par_bal_index].clone());
            }
            lexer::Token::PUNCT_CLOSE_PAR => {
                if let Some(lexer::Token::PUNCT_OPEN_PAR) = parenth_balance.last() {
                    parenth_balance.pop();
                }
            }
            _ => {}
        }
    }
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
    let mut index = 0;
    while index < tokens.len() {
        match &tokens[index] {
            // we unwrap for some operators because there should always be a left operand before.
            lexer::Token::PUNCT_QUESTION_MARK => {
                if let Some(_) = curr_expr {
                    let mut cond = parser::Conditional {
                        first: Some(Box::new(curr_expr.unwrap())),
                        second: None,
                        third: None,
                    };
                    let expr_cond = parser::Expr::Conditional(cond);
                    curr_expr = Some(expr_cond);
                } else {
                    return Err(String::from(
                        "missing first operator for conditional expression",
                    ));
                }
            }
            lexer::Token::PUNCT_COLON => {
                assert!(matches!(stack.last(), Some(parser::Expr::Conditional(_))));
            }
            lexer::Token::PUNCT_XOR_BIT => {
                let mut expression_unwrapped = curr_expr.unwrap();
                let mut new_expression = parser::Expr::BitXOR(parser::BitXOR {
                    first: None,
                    second: None,
                });
                if expression_unwrapped.priority() >= new_expression.priority() {
                    left_has_higher_eq_priority(&mut expression_unwrapped, &mut new_expression);
                    curr_expr = Some(new_expression);
                } else {
                    right_has_higher_priority(&mut expression_unwrapped, &mut new_expression);
                    curr_expr = Some(expression_unwrapped);
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
                            | lexer::Token::PUNCT_MINUS
                    )
                ) {
                    return Err(String::from("not allowed token after operator"));
                }
            }
            lexer::Token::PUNCT_CLOSE_PAR => {
                assert!(matches!(stack.last(), Some(parser::Expr::Primary(None))));
                let mut stack_expr = stack.pop().unwrap();
                let parser::Expr::Primary(p) = &mut stack_expr else { unreachable!() };
                *p = Some(parser::PrimaryInner::new_p_expr(curr_expr.unwrap()));
                curr_expr = Some(stack_expr);

                // we have a primary expression in curr_expr which means everything else on the
                // stack has equal or lower priority for the first iteration
                while let Some(mut e) = stack.pop() {
                    if let Some(ref mut expr_in_curr) = curr_expr {
                        if e.priority() > expr_in_curr.priority() {
                            left_has_higher_eq_priority(&mut e, expr_in_curr);
                            curr_expr = Some(expr_in_curr);
                        } else {
                            right_has_higher_priority(&mut e, expr_in_curr);
                            curr_expr = Some(e);
                        }
                    } else {
                        unreachable!()
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
                    Some(lexer::Token::IDENT(_) | lexer::Token::PUNCT_OPEN_PAR)
                ) {
                    return Err(String::from("not allowed token after operator"));
                }
            }
            lexer::Token::PUNCT_PLUS => {
                let expression_unwrapped = curr_expr.unwrap();
                let mut new_expression = parser::Expr::Additive(parser::Additive {
                    first: None,
                    second: None,
                });
                if expression_unwrapped.priority() > new_expression.priority() {
                } else {
                }
                loop {
                    index += 1;
                    if !matches!(tokens.get(index), Some(lexer::Token::WHITESPACE)) {
                        break;
                    }
                }
                if matches!(
                    tokens.get(index),
                    Some(lexer::Token::IDENT(_) | lexer::Token::PUNCT_OPEN_PAR)
                ) {
                    return Err(String::from("not allowed token after operator"));
                }
            }
            lexer::Token::PUNCT_OR_BOOL => {
                let expression_unwrapped = curr_expr.unwrap();
                let mut new_expression = parser::Expr::LogicalOR(parser::LogicalOR {
                    first: None,
                    second: None,
                });
                if expression_unwrapped.priority() > new_expression.priority() {
                } else {
                }
            }
            lexer::Token::PUNCT_AND_BOOL => {
                let expression_unwrapped = curr_expr.unwrap();
                let mut new_expression = parser::Expr::LogicalAND(parser::LogicalAND {
                    first: None,
                    second: None,
                });
                if expression_unwrapped.priority() > new_expression.priority() {
                    let parser::Expr::LogicalAND(logand) = &mut new_expression else { unreachable!() };
                    logand.first = Some(Box::new(expression_unwrapped));
                    curr_expr = Some(new_expression);
                } else {
                }
            }
            lexer::Token::PUNCT_MULT => {}
            lexer::Token::PUNCT_OPEN_PAR => {
                stack.push(parser::Expr::Primary(None));
                curr_expr = None;
                loop {
                    index += 1;
                    if !matches!(tokens.get(index), Some(lexer::Token::WHITESPACE)) {
                        break;
                    }
                }
            }
            lexer::Token::IDENT(_) => {
                let inner_primary = parser::PrimaryInner::new_p_token(tokens[index].clone())?;
                if curr_expr.is_none() {
                    curr_expr = Some(parser::Expr::Primary(Some(inner_primary)));
                } else {
                    match &mut curr_expr {
                        Some(parser::Expr::Conditional(conditional)) => {
                            if conditional.first.is_none() {
                                conditional.first =
                                    Some(Box::new(parser::Expr::Primary(Some(inner_primary))));
                            } else if conditional.second.is_none() {
                                conditional.second =
                                    Some(Box::new(parser::Expr::Primary(Some(inner_primary))));
                            } else if conditional.third.is_none() {
                                conditional.third =
                                    Some(Box::new(parser::Expr::Primary(Some(inner_primary))));
                            } else {
                            }
                        }
                        Some(parser::Expr::LogicalOR(logicalor)) => {}
                        Some(parser::Expr::LogicalAND(logicaland)) => {}
                        Some(parser::Expr::BitOR(bitor)) => {}
                        Some(parser::Expr::BitXOR(bitxor)) => {}
                        Some(parser::Expr::BitAND(bitand)) => {}
                        Some(parser::Expr::Equality(equality)) => {}
                        Some(parser::Expr::Relational(relational)) => {}
                        Some(parser::Expr::BitShift(bitshift)) => {}
                        Some(parser::Expr::Additive(additive)) => {}
                        Some(parser::Expr::Multiplicative(multiplicative)) => {}
                        Some(parser::Expr::Unary(unary)) => {}
                        Some(parser::Expr::Primary(primaryinner)) => {}
                        _ => unreachable!(),
                    }
                }
            }
            lexer::Token::WHITESPACE => {}
            _ => unreachable!(),
        }
        todo!()
    }
    Ok(false)
}
fn if_directive(
    tokens: &mut Vec<lexer::Token>,
    index: usize,
    defines: &HashMap<String, Vec<lexer::Token>>,
) -> Result<(), String> {
    todo!()
}
fn define_directive(
    tokens: &mut Vec<lexer::Token>,
    index: usize,
    defines: &mut HashMap<String, Define>,
) -> Result<(), String> {
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
            lexer::Token::IDENT(id) => {
                if id != "define" {
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
        identifier: String::new(),
        parameters: None,
        var_arg: false,
        replacement_list: Vec::new(),
    };
    let mut matches_one_of_conditions = false;
    let mut identifier_of_macro = String::new();
    if let Some([lexer::Token::IDENT(id), lexer::Token::PUNCT_OPEN_PAR]) =
        tokens.get(index_of_identifier..index_of_identifier + 2)
    {
        def_data.parameters = Some(Vec::new());
        def_data.identifier = id.to_string();
        identifier_of_macro = id.to_string();
        let mut fn_like_macro_index = index_of_identifier + 2;
        while matches!(
            tokens.get(fn_like_macro_index),
            Some(lexer::Token::IDENT(_))
                | Some(lexer::Token::PUNCT_COMMA)
                | Some(lexer::Token::WHITESPACE)
        ) {
            if let Some(lexer::Token::IDENT(arg)) = tokens.get(fn_like_macro_index) {
                if let Some(ref mut v) = def_data.parameters {
                    if !v.contains(arg) {
                        if arg == "__VA_ARGS__" {
                            return Err(format!("__VA_ARGS__ cannot be used as a parameter name"));
                        }
                        v.push(arg.to_string());
                    } else {
                        return Err(format!("duplicate argument name found in define directive"));
                    }
                }
            }
            fn_like_macro_index += 1;
        }
        if matches!(
            tokens.get(fn_like_macro_index),
            Some(lexer::Token::PUNCT_CLOSE_PAR)
        ) {
            def_data
                .replacement_list
                .extend_from_slice(&tokens[fn_like_macro_index + 1..end]);
            defines.insert(def_data.identifier.clone(), def_data);
            matches_one_of_conditions = true;
        } else if matches!(
            tokens.get(fn_like_macro_index..fn_like_macro_index + 2),
            Some([lexer::Token::PUNCT_ELLIPSIS, lexer::Token::PUNCT_CLOSE_PAR])
        ) {
            def_data.var_arg = true;
            def_data
                .replacement_list
                .extend_from_slice(&tokens[fn_like_macro_index + 2..end]);
            defines.insert(def_data.identifier.clone(), def_data);
            matches_one_of_conditions = true;
        }
    } else if let Some(lexer::Token::IDENT(id)) = tokens.get(index_of_identifier) {
        identifier_of_macro = id.to_string();
        def_data.identifier = id.to_string();
        def_data
            .replacement_list
            .extend_from_slice(&tokens[index_of_identifier + 1..end]);
        defines.insert(def_data.identifier.clone(), def_data);
        matches_one_of_conditions = true;
    }
    if matches_one_of_conditions {
        if identifier_of_macro == "defined" {
            return Err(format!("cannot define 'defined' as it is a cpp keyword"));
        }
        if let Some(ref mut dd) = defines.get_mut(&identifier_of_macro) {
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
            let mut length = end - index + 1;
            while length > 0 {
                tokens.remove(index);
                length -= 1;
            }
            return Ok(());
        } else {
            let lexer::Token::IDENT(id) = &tokens[index_of_identifier] else { unreachable!() };
            return Err(format!("define directive properly formed but we don't have {} in the hashmap. index_of_identifier gives {}", identifier_of_macro, id));
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
    tokens: &mut Vec<lexer::Token>,
    index: usize,
    defines: &mut HashMap<String, Define>,
) -> Result<(), String> {
    let index_of_identifier = index + 2;
    if let Some(lexer::Token::IDENT(identifier_to_be_undef)) = tokens.get(index_of_identifier) {
        defines.remove(identifier_to_be_undef);
        return Ok(());
    }
    Err(format!("unknown token in undef directive"))
}
fn expand_macro(
    tokens: &mut Vec<lexer::Token>,
    index: &mut usize,
    defines: &HashMap<String, Define>,
) -> Result<(), String> {
    let index_copy = *index;
    let macro_id = match tokens[index_copy].clone() {
        lexer::Token::IDENT(id) => id,
        _ => return Err(format!("non identifier token given")),
    };
    if !defines.contains_key(&macro_id) {
        return Ok(());
    }
    let mut already_replaced_macro_names: Vec<String> = Vec::new();
    let mut macros_to_replace: Vec<MacroInterval> = Vec::new();
    macros_to_replace.push(MacroInterval {
        name: macro_id,
        start: index_copy,
        end: index_copy + 1,
    });
    let mut where_index_should_be_after_we_are_done = index_copy;
    loop {
        if macros_to_replace.is_empty() {
            break;
        }
        if let Some(last_macro_interval) = &macros_to_replace.last() {
            if let Some(def_data) = defines.get(&last_macro_interval.name) {
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
                                last_macro_interval.name
                            ));
                        }
                        macros_to_replace.last_mut().unwrap().end = fn_like_macro_index + 1;

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
                                    macros_to_replace.last().unwrap().name,
                                    seen_args.len(),
                                    args.len(),
                                    seen_args,
                                    tokens,
                                ));
                        }

                        for arg in &mut seen_args {
                            let mut index = 0;
                            while index < arg.len() {
                                if let lexer::Token::IDENT(inside_id) = &arg[index] {
                                    if defines.contains_key(inside_id)
                                        && !already_replaced_macro_names.contains(inside_id)
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
                                        expand_macro(arg, &mut index, defines)?;
                                    }
                                }
                                index += 1;
                            }
                        }
                        already_replaced_macro_names
                            .push(macros_to_replace.last().unwrap().name.clone());
                        {
                            let mut replacement_list_index = 0;
                            while replacement_list_index < replacement_list_copy.len() {
                                if let Some(lexer::Token::IDENT(id_name)) =
                                    replacement_list_copy.get(replacement_list_index)
                                {
                                    if args.contains(id_name) {
                                        let mut seen_arg_index = 0;
                                        for arg_index in 0..args.len() {
                                            if args[arg_index] == *id_name {
                                                seen_arg_index = arg_index;
                                                break;
                                            }
                                        }
                                        if (replacement_list_index > 0
                                            && matches!(
                                                replacement_list_copy
                                                    .get(replacement_list_index - 1),
                                                Some(lexer::Token::PUNCT_HASH)
                                            ))
                                            || (replacement_list_index > 1
                                                && matches!(
                                                    replacement_list_copy.get(
                                                        replacement_list_index - 2
                                                            ..replacement_list_index
                                                    ),
                                                    Some([
                                                        lexer::Token::PUNCT_HASH,
                                                        lexer::Token::WHITESPACE,
                                                    ])
                                                ))
                                        {
                                            let mut removal_index = replacement_list_index;
                                            while !matches!(
                                                replacement_list_copy.get(removal_index),
                                                Some(lexer::Token::PUNCT_HASH)
                                            ) {
                                                removal_index -= 1;
                                            }
                                            let id_name_clone = id_name.clone();
                                            loop {
                                                if let Some(lexer::Token::IDENT(remove_id)) =
                                                    replacement_list_copy.get(removal_index)
                                                {
                                                    if *remove_id == id_name_clone {
                                                        replacement_list_copy.remove(removal_index);
                                                        break;
                                                    }
                                                }
                                                replacement_list_copy.remove(removal_index);
                                            }
                                            let mut string_literal_token =
                                                lexer::Token::StringLiteral {
                                                    prefix: None,
                                                    sequence: String::new(),
                                                };
                                            let argument = &seen_args[seen_arg_index];
                                            let lexer::Token::StringLiteral { prefix: _, sequence } =
                                        &mut string_literal_token else { panic!("WHAT IN THE FUCK") };
                                            for argument_index in 0..argument.len() {
                                                if let Some(stringified_token) =
                                                    argument[argument_index].to_string()
                                                {
                                                    sequence.push_str(&stringified_token);
                                                } else {
                                                    return Err(format!("tried to stringify token that cannot be stringified"));
                                                }
                                            }
                                            replacement_list_copy
                                                .insert(removal_index, string_literal_token);
                                            continue;
                                        } else {
                                            replacement_list_copy.remove(replacement_list_index);
                                            let argument = &seen_args[seen_arg_index];
                                            let mut replacement_list_index_copy =
                                                replacement_list_index;
                                            let count_of_non_whitespace_tokens = argument
                                                .iter()
                                                .filter(|t| !matches!(t, lexer::Token::WHITESPACE))
                                                .count();
                                            if count_of_non_whitespace_tokens > 0 {
                                                for t in argument {
                                                    replacement_list_copy.insert(
                                                        replacement_list_index_copy,
                                                        t.clone(),
                                                    );
                                                    replacement_list_index_copy += 1;
                                                }
                                            } else if (replacement_list_index > 0
                                                && matches!(
                                                    replacement_list_copy
                                                        .get(replacement_list_index - 1),
                                                    Some(lexer::Token::PUNCT_HASH_HASH)
                                                ))
                                                || (matches!(
                                                    replacement_list_copy
                                                        .get(replacement_list_index + 1),
                                                    Some(lexer::Token::PUNCT_HASH_HASH)
                                                ))
                                            {
                                                replacement_list_copy.insert(
                                                    replacement_list_index_copy,
                                                    lexer::Token::PLACEMARKER,
                                                );
                                            }
                                            continue;
                                        }
                                    } else if id_name == "__VA_ARGS__" {
                                        replacement_list_copy.remove(replacement_list_index);
                                        assert!(args.len() < seen_args.len());
                                        let mut replacement_list_index_incremented =
                                            replacement_list_index;
                                        for seen_arg_index in args.len()..seen_args.len() {
                                            let argument_slice = &seen_args[seen_arg_index];
                                            for t in argument_slice {
                                                replacement_list_copy.insert(
                                                    replacement_list_index_incremented,
                                                    t.clone(),
                                                );
                                                replacement_list_index_incremented += 1;
                                            }
                                            if seen_arg_index < seen_args.len() - 1 {
                                                replacement_list_copy.insert(
                                                    replacement_list_index_incremented,
                                                    lexer::Token::PUNCT_COMMA,
                                                );
                                            }
                                            replacement_list_index_incremented += 1;
                                        }
                                    }
                                }
                                replacement_list_index += 1;
                            }
                        }
                    } else {
                        return Err(format!(
                            "no args given for function macro: {} {:?}",
                            last_macro_interval.name, tokens
                        ));
                    }
                }
                let mut length =
                    macros_to_replace.last().unwrap().end - macros_to_replace.last().unwrap().start;
                while length > 0 {
                    tokens.remove(macros_to_replace.last().unwrap().start);
                    macros_to_replace.last_mut().unwrap().end -= 1;
                    length -= 1;
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
                            Some(lexer::Token::WHITESPACE) | Some(lexer::Token::PUNCT_HASH_HASH)
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
                            _ => {}
                        }
                        continue;
                    }
                    punct_hash_hash_index += 1;
                }

                {
                    let mut stringified_tokens = String::new();
                    let mut concat_ident_index = 0;
                    while concat_ident_index < replacement_list_copy.len() {
                        while let Some(lexer::Token::IDENT(concat_id)) =
                            replacement_list_copy.get(concat_ident_index)
                        {
                            stringified_tokens.push_str(&concat_id);
                            replacement_list_copy.remove(concat_ident_index);
                        }
                        if stringified_tokens.len() > 0 {
                            let new_ident_token = lexer::Token::IDENT(stringified_tokens.clone());
                            replacement_list_copy.insert(concat_ident_index, new_ident_token);
                            stringified_tokens.clear();
                        }
                        concat_ident_index += 1;
                    }
                }

                let mut insert_index = macros_to_replace.last().unwrap().start;
                for t in replacement_list_copy {
                    tokens.insert(insert_index, t);
                    macros_to_replace.last_mut().unwrap().end += 1;
                    insert_index += 1;
                }

                let top_macro_interval = macros_to_replace.pop().unwrap();
                let beginning = top_macro_interval.start;
                let end = top_macro_interval.end;
                for looking_for_moar_macro_index in beginning..end {
                    if let Some(lexer::Token::IDENT(identifier_that_could_be_macro)) =
                        tokens.get(looking_for_moar_macro_index)
                    {
                        if defines.contains_key(identifier_that_could_be_macro)
                            && !already_replaced_macro_names
                                .contains(identifier_that_could_be_macro)
                        {
                            macros_to_replace.push(MacroInterval {
                                name: identifier_that_could_be_macro.to_string(),
                                start: looking_for_moar_macro_index,
                                end: looking_for_moar_macro_index + 1,
                            });
                        }
                    }
                }

                if macros_to_replace.len() == 0 {
                    where_index_should_be_after_we_are_done = end + 1;
                }
            } else {
                break;
            }
        }
    }
    *index = where_index_should_be_after_we_are_done;
    Ok(())
}
fn preprocessing_directives(
    tokens: &mut Vec<lexer::Token>,
    include_paths: &[&str],
) -> Result<(), String> {
    // the C standard talks about "grouping" where the operands are grouped with the operators
    //
    // if <condition>; the condition is an integer constant expression except that all identifiers
    // are treated like they are either macro names or not.
    // Right now, I'm confused as to where in the spec does it talk about what punctuators are
    // allowed in the expressions following #if preprocessor directives
    // The constant-expression section in the c17 spec sort of states why...i guess.
    // An integer constant expression shall have integer type and shall only have operands that are integer
    // constants, enumeration constants, character constants
    let mut defines = HashMap::new();
    let mut index: usize = 0;
    while index < tokens.len() {
        match &tokens[index] {
            lexer::Token::PUNCT_HASH => {
                let mut newline = index + 2;
                while !matches!(tokens.get(newline), Some(lexer::Token::NEWLINE))
                    && newline < tokens.len()
                {
                    newline += 1;
                }
                if let Some(lexer::Token::NEWLINE) = tokens.get(newline) {
                    let mut index_of_directive = index + 1;
                    if let Some(lexer::Token::WHITESPACE) = tokens.get(index_of_directive) {
                        index_of_directive += 1;
                    }
                    if let Some(lexer::Token::IDENT(s)) = tokens.get(index_of_directive) {
                        match s.as_str() {
                            "include" => {
                                include_directive(tokens, index, newline, include_paths, &defines)?;
                            }
                            "if" | "ifdef" | "ifndef" => todo!("if directives"),
                            "define" => {
                                define_directive(tokens, index, &mut defines)?;
                            }
                            "defined" => todo!(),
                            "undef" => {
                                undef_directive(tokens, index, &mut defines)?;
                            }
                            "error" => todo!(),
                            "line" => todo!(),
                            "pragma" => todo!(),
                            _ => {}
                        }
                    }
                    continue;
                } else {
                    return Err(format!("no newline at the end of preprocessing directive"));
                }
            }
            lexer::Token::IDENT(_) => {
                expand_macro(tokens, &mut index, &defines)?;
            }
            _ => {}
        }
        index += 1;
    }
    if index >= tokens.len() {
        return Ok(());
    }
    Err(String::from("unable to preprocess"))
}
// TODO: add flag options so that the user could specify if they wanted to only preprocess
pub fn cpp(program_str: Vec<u8>) -> Result<Vec<lexer::Token>, String> {
    let backslash_newline_spliced = program_str
        .iter()
        .map(|b| *b as char)
        .collect::<String>()
        .replace("\\\n", "");
    let backslash_newline_spliced = backslash_newline_spliced.as_bytes();
    let comments_removed = comments(backslash_newline_spliced)?;
    let lexed_tokens = lexer::lexer(comments_removed, true);
    todo!()
}

#[cfg(test)]
mod tests {

    use crate::lexer;
    use std::collections::HashMap;

    use super::{
        comments, define_directive, expand_macro, include_directive, preprocessing_directives,
        Define,
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
}"##;
        let mut tokens = lexer::lexer(src.as_bytes().to_vec(), true)?;
        let mut defines = HashMap::new();
        include_directive(&mut tokens, 0, 18, &["./test_c_files/"], &mut defines)?;
        let assert_tokens = [
            lexer::Token::IDENT(String::from("int")),
            lexer::Token::WHITESPACE,
            lexer::Token::IDENT(String::from("main")),
            lexer::Token::PUNCT_OPEN_PAR,
            lexer::Token::PUNCT_CLOSE_PAR,
            lexer::Token::WHITESPACE,
            lexer::Token::PUNCT_OPEN_CURLY,
            lexer::Token::NEWLINE,
            lexer::Token::PUNCT_CLOSE_CURLY,
        ];
        assert_eq!(tokens, assert_tokens);
        Ok(())
    }
    #[test]
    fn preprocess_test() -> Result<(), String> {
        let src = r##"#include "hi2.h"
int main() {
hi;
}"##;
        let mut tokens = lexer::lexer(src.as_bytes().to_vec(), true)?;
        preprocessing_directives(&mut tokens, &["./test_c_files"])?;
        let assert_tokens = vec![
            lexer::Token::IDENT(String::from("int")),
            lexer::Token::WHITESPACE,
            lexer::Token::IDENT(String::from("main")),
            lexer::Token::PUNCT_OPEN_PAR,
            lexer::Token::PUNCT_CLOSE_PAR,
            lexer::Token::WHITESPACE,
            lexer::Token::PUNCT_OPEN_CURLY,
            lexer::Token::NEWLINE,
            lexer::Token::CONSTANT_DEC_INT {
                value: 5.to_string(),
                suffix: None,
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
HI(5 5);"##;
        let mut tokens = lexer::lexer(src.as_bytes().to_vec(), true)?;
        let mut defines = HashMap::new();
        define_directive(&mut tokens, 0, &mut defines)?;
        expand_macro(&mut tokens, &mut 0, &defines)?;
        assert_eq!(
            vec![
                lexer::Token::StringLiteral {
                    prefix: None,
                    sequence: "5 5".to_string()
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
        let mut tokens = lexer::lexer(src.as_bytes().to_vec(), true)?;
        let mut index = 0;
        let mut defines = HashMap::new();
        define_directive(&mut tokens, index, &mut defines)?;
        define_directive(&mut tokens, index, &mut defines)?;
        define_directive(&mut tokens, index, &mut defines)?;
        define_directive(&mut tokens, index, &mut defines)?;
        while index < tokens.len() {
            if let Some(
                [lexer::Token::IDENT(first), lexer::Token::PUNCT_OPEN_PAR, lexer::Token::IDENT(second)],
            ) = tokens.get(index..index + 3)
            {
                if first == "join" && second == "x" {
                    break;
                }
            }
            index += 1;
        }
        expand_macro(&mut tokens, &mut index, &defines)?;
        assert_eq!(
            vec![
                lexer::Token::IDENT("char".to_string()),
                lexer::Token::WHITESPACE,
                lexer::Token::IDENT("p".to_string()),
                lexer::Token::PUNCT_OPEN_SQR,
                lexer::Token::PUNCT_CLOSE_SQR,
                lexer::Token::WHITESPACE,
                lexer::Token::PUNCT_ASSIGNMENT,
                lexer::Token::WHITESPACE,
                lexer::Token::StringLiteral {
                    prefix: None,
                    sequence: "x ## y".to_string()
                },
                lexer::Token::PUNCT_SEMI_COLON
            ],
            tokens
        );
        Ok(())
    }
    #[test]
    fn test_define_directive() -> Result<(), String> {
        let src = "#define hash_hash # ## #\n";
        let src2 = "#define mkstr(a) # a\n";
        let src3 = "#define in_between(a) mkstr(a)\n";
        let src4 = "#define join(c, d) in_between(c hash_hash d)\n";
        let mut tokens = lexer::lexer(src.as_bytes().to_vec(), true)?;
        let mut tokens2 = lexer::lexer(src2.as_bytes().to_vec(), true)?;
        let mut tokens3 = lexer::lexer(src3.as_bytes().to_vec(), true)?;
        let mut tokens4 = lexer::lexer(src4.as_bytes().to_vec(), true)?;
        let mut defines = HashMap::new();
        define_directive(&mut tokens, 0, &mut defines)?;
        define_directive(&mut tokens2, 0, &mut defines)?;
        define_directive(&mut tokens3, 0, &mut defines)?;
        define_directive(&mut tokens4, 0, &mut defines)?;
        assert_eq!(defines.len(), 4);
        assert!(defines.contains_key("hash_hash"));
        assert!(defines.contains_key("mkstr"));
        assert!(defines.contains_key("in_between"));
        assert!(defines.contains_key("join"));
        assert_eq!(
            Define {
                identifier: "hash_hash".to_string(),
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
            *defines.get("hash_hash").unwrap()
        );
        assert_eq!(
            Define {
                identifier: "mkstr".to_string(),
                parameters: Some(vec!["a".to_string()]),
                var_arg: false,
                replacement_list: vec![
                    lexer::Token::PUNCT_HASH,
                    lexer::Token::WHITESPACE,
                    lexer::Token::IDENT("a".to_string()),
                ]
            },
            *defines.get("mkstr").unwrap()
        );
        assert_eq!(
            Define {
                identifier: "in_between".to_string(),
                parameters: Some(vec!["a".to_string()]),
                var_arg: false,
                replacement_list: vec![
                    lexer::Token::IDENT("mkstr".to_string()),
                    lexer::Token::PUNCT_OPEN_PAR,
                    lexer::Token::IDENT("a".to_string()),
                    lexer::Token::PUNCT_CLOSE_PAR,
                ]
            },
            *defines.get("in_between").unwrap()
        );
        assert_eq!(
            Define {
                identifier: "join".to_string(),
                parameters: Some(vec!["c".to_string(), "d".to_string()]),
                var_arg: false,
                replacement_list: vec![
                    lexer::Token::IDENT("in_between".to_string()),
                    lexer::Token::PUNCT_OPEN_PAR,
                    lexer::Token::IDENT("c".to_string()),
                    lexer::Token::WHITESPACE,
                    lexer::Token::IDENT("hash_hash".to_string()),
                    lexer::Token::WHITESPACE,
                    lexer::Token::IDENT("d".to_string()),
                    lexer::Token::PUNCT_CLOSE_PAR,
                ]
            },
            *defines.get("join").unwrap()
        );
        Ok(())
    }
    #[test]
    fn expand_macro_test_testing_object_macro_expansion_nested() -> Result<(), String> {
        let src = r##"#define HEHE(a) a
#define A HEHE(4)
A"##;
        let src_bytes = src.as_bytes();
        let mut tokens = lexer::lexer(src_bytes.to_vec(), true)?;
        let mut defines = HashMap::new();
        define_directive(&mut tokens, 0, &mut defines)?;
        define_directive(&mut tokens, 0, &mut defines)?;
        expand_macro(&mut tokens, &mut 0, &defines)?;
        assert_eq!(
            vec![lexer::Token::CONSTANT_DEC_INT {
                value: 4.to_string(),
                suffix: None
            }],
            tokens
        );
        Ok(())
    }
    #[test]
    fn expand_macro_test_testing_object_macro_expansion_nested_2() -> Result<(), String> {
        let src = r##"#define HEHE(a) a
#define A HEHE(4) HEHE(5) HEHE(6)
A"##;
        let src_bytes = src.as_bytes();
        let mut tokens = lexer::lexer(src_bytes.to_vec(), true)?;
        let mut defines = HashMap::new();
        define_directive(&mut tokens, 0, &mut defines)?;
        define_directive(&mut tokens, 0, &mut defines)?;
        expand_macro(&mut tokens, &mut 0, &defines)?;
        assert_eq!(
            vec![
                lexer::Token::CONSTANT_DEC_INT {
                    value: 4.to_string(),
                    suffix: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value: 5.to_string(),
                    suffix: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value: 6.to_string(),
                    suffix: None
                }
            ],
            tokens
        );
        Ok(())
    }
    #[test]
    fn expand_macro_parentheses_argument() -> Result<(), String> {
        let src = r##"#define HI(a,b) a,b
HI((,),(,))"##;
        let src_bytes = src.as_bytes();
        let mut tokens = lexer::lexer(src_bytes.to_vec(), true)?;
        let mut defines = HashMap::new();
        define_directive(&mut tokens, 0, &mut defines)?;
        expand_macro(&mut tokens, &mut 0, &defines)?;
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
HEHE(HEHE(1,2),HEHE(3,4))"##;
        let src_bytes = src.as_bytes();
        let mut tokens = lexer::lexer(src_bytes.to_vec(), true)?;
        let mut defines = HashMap::new();
        define_directive(&mut tokens, 0, &mut defines)?;
        expand_macro(&mut tokens, &mut 0, &defines)?;
        assert_eq!(
            vec![
                lexer::Token::CONSTANT_DEC_INT {
                    value: 1.to_string(),
                    suffix: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value: 2.to_string(),
                    suffix: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value: 3.to_string(),
                    suffix: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value: 4.to_string(),
                    suffix: None
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
HAHA(C,4)"##;
        let src_bytes = src.as_bytes();
        let mut tokens = lexer::lexer(src_bytes.to_vec(), true)?;
        let mut defines = HashMap::new();
        define_directive(&mut tokens, 0, &mut defines)?;
        define_directive(&mut tokens, 0, &mut defines)?;
        expand_macro(&mut tokens, &mut 0, &defines)?;
        assert_eq!(
            vec![
                lexer::Token::PUNCT_COMMA,
                lexer::Token::WHITESPACE,
                lexer::Token::PUNCT_PLUS,
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value: 4.to_string(),
                    suffix: None
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
f(2)(9)"##;
        let src_bytes = src.as_bytes();
        let mut tokens = lexer::lexer(src_bytes.to_vec(), true)?;
        let mut defines = HashMap::new();
        define_directive(&mut tokens, 0, &mut defines)?;
        define_directive(&mut tokens, 0, &mut defines)?;
        expand_macro(&mut tokens, &mut 0, &defines)?;
        assert_eq!(
            vec![
                lexer::Token::CONSTANT_DEC_INT {
                    value: 2.to_string(),
                    suffix: None
                },
                lexer::Token::PUNCT_MULT,
                lexer::Token::IDENT("f".to_string()),
                lexer::Token::PUNCT_OPEN_PAR,
                lexer::Token::CONSTANT_DEC_INT {
                    value: "9".to_string(),
                    suffix: None
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
INVOKE(FOO,BAR)"##;
        let src_bytes = src.as_bytes();
        let mut tokens = lexer::lexer(src_bytes.to_vec(), true)?;
        let mut defines = HashMap::new();
        define_directive(&mut tokens, 0, &mut defines)?;
        define_directive(&mut tokens, 0, &mut defines)?;
        expand_macro(&mut tokens, &mut 0, &defines)?;
        assert_eq!(
            vec![
                lexer::Token::IDENT("printf".to_string()),
                lexer::Token::PUNCT_OPEN_PAR,
                lexer::Token::StringLiteral {
                    prefix: None,
                    sequence: "FOO".to_string()
                },
                lexer::Token::WHITESPACE,
                lexer::Token::StringLiteral {
                    prefix: None,
                    sequence: "BAR".to_string()
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
CHICKEN(1 2,3 4)"##;
        let src_bytes = src.as_bytes();
        let mut tokens = lexer::lexer(src_bytes.to_vec(), true)?;
        let mut defines = HashMap::new();
        define_directive(&mut tokens, 0, &mut defines)?;
        expand_macro(&mut tokens, &mut 0, &defines)?;
        assert_eq!(
            vec![
                lexer::Token::CONSTANT_DEC_INT {
                    value: 1.to_string(),
                    suffix: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value: 2.to_string(),
                    suffix: None
                },
                lexer::Token::PUNCT_COMMA,
                lexer::Token::CONSTANT_DEC_INT {
                    value: 3.to_string(),
                    suffix: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value: 4.to_string(),
                    suffix: None
                },
            ],
            tokens
        );
        Ok(())
    }
    #[test]
    fn expand_macro_not_defined() -> Result<(), String> {
        let src = r##"HI"##;
        let src_bytes = src.as_bytes();
        let mut tokens = lexer::lexer(src_bytes.to_vec(), true)?;
        let mut defines = HashMap::new();
        expand_macro(&mut tokens, &mut 0, &mut defines)?;
        assert_eq!(vec![lexer::Token::IDENT("HI".to_string()),], tokens);
        Ok(())
    }
}
