use std::collections::HashMap;

use crate::error::*;
use crate::lexer::*;
use crate::parser::expressions::*;
use crate::parser::*;

#[derive(PartialEq, Debug, Clone)]
pub struct Define {
    // Definitely is a better way to see if a macro is a function-like macro or not,
    // but 'parameters' here will be Some(...) if it is a function-like macro and None
    // if it isn't.
    pub parameters: Option<Vec<usize>>,
    pub var_arg: bool,
    pub replacement_list: Vec<Token>,
}

// Depth is for replacing multiple macros of the same name in the same replacement list
struct Macro {
    macro_key: usize,
    // inclusive start and end
    start: usize,
    end: usize,
    depth: usize,
    arguments: Option<Vec<Vec<Token>>>,
}

fn concat_adjacent_strings(
    tokens: &[Token],
    str_maps: &mut ByteVecMaps,
) -> Result<Vec<Token>, String> {
    let mut adjacent_strings_concated = Vec::new();
    let mut token_string_concated_index = 0;
    while token_string_concated_index < tokens.len() {
        if let Some(Token {
            r#type:
                TokenType::StringLiteral {
                    str_lit: first_string_lit,
                    ..
                },
            ..
        }) = tokens.get(token_string_concated_index)
        {
            let mut prev_prefix = first_string_lit.prefix_key;
            let mut first_byte_vec =
                str_maps.key_to_byte_vec[first_string_lit.sequence_key].clone();
            let mut adjacent_string_lit_index = token_string_concated_index + 1;
            while matches!(
                tokens.get(adjacent_string_lit_index),
                Some(Token {
                    r#type: TokenType::WHITESPACE
                        | TokenType::NEWLINE
                        | TokenType::StringLiteral { .. },
                    ..
                })
            ) && adjacent_string_lit_index < tokens.len()
            {
                if let Some(Token {
                    r#type:
                        TokenType::StringLiteral {
                            str_lit: second_string_lit,
                            ..
                        },
                    ..
                }) = tokens.get(adjacent_string_lit_index)
                {
                    match (prev_prefix, second_string_lit.prefix_key) {
                        (Some(prev_key), Some(second_prefix_key)) => {
                            let first_prefix = str_maps.key_to_byte_vec[prev_key].as_slice();
                            let second_prefix =
                                str_maps.key_to_byte_vec[second_prefix_key].as_slice();
                            if *first_prefix != *second_prefix {
                                return Err(format!(
                                    "Cannot concatenate string literals with differing prefixes"
                                ));
                            }
                        }
                        _ => {}
                    }
                    let second_byte_vec = &str_maps.key_to_byte_vec[second_string_lit.sequence_key];
                    first_byte_vec.extend_from_slice(second_byte_vec);
                    prev_prefix = second_string_lit.prefix_key;
                }
                adjacent_string_lit_index += 1;
            }
            adjacent_strings_concated.push(Token {
                r#type: TokenType::StringLiteral {
                    str_lit: StringLiteral {
                        prefix_key: prev_prefix,
                        sequence_key: str_maps.add_byte_vec(first_byte_vec.as_slice()),
                    },
                },
                location: None,
            });
            // If there is a StringLiteral at adjacent_string_lit_index, then we
            // set token_string_concated_index = adjacent_string_lit_index.
            // else we just increment token_string_concated_index so that it moves on from
            // the current StringLiteral
            if matches!(
                tokens.get(adjacent_string_lit_index),
                Some(Token {
                    r#type: TokenType::StringLiteral { .. },
                    ..
                })
            ) {
                token_string_concated_index = adjacent_string_lit_index;
            } else {
                token_string_concated_index += 1;
            }
            continue;
        }
        if token_string_concated_index < tokens.len() {
            adjacent_strings_concated.push(tokens[token_string_concated_index]);
        }
        token_string_concated_index += 1;
    }
    Ok(adjacent_strings_concated)
}

fn process_comments(bytes: &[u8]) -> Result<Vec<u8>, String> {
    let mut byte_index = 0;
    let mut comments_removed = Vec::new();
    while byte_index < bytes.len() {
        if bytes[byte_index] == b'\'' || bytes[byte_index] == b'\"' {
            comments_removed.push(bytes[byte_index]);
            let start = bytes[byte_index];
            byte_index += 1;
            while byte_index < bytes.len() && bytes[byte_index] != start {
                comments_removed.push(bytes[byte_index]);
                // for escaped ' and " and any other escaped character
                if byte_index + 1 < bytes.len() && bytes[byte_index] == b'\\' {
                    comments_removed.push(bytes[byte_index + 1]);
                    byte_index += 1;
                }
                byte_index += 1;
            }
            if byte_index < bytes.len() && bytes[byte_index] == start {
                comments_removed.push(bytes[byte_index]);
                byte_index += 1;
                continue;
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
    tokens: &[Token],
    index: &mut usize,
    curr_path: &str,
    include_paths: &[&str],
    defines: &mut HashMap<usize, Define>,
    str_maps: &mut ByteVecMaps,
) -> Result<(), String> {
    let mut newline_index = *index;
    while !matches!(
        tokens.get(newline_index),
        Some(Token {
            r#type: TokenType::NEWLINE,
            ..
        })
    ) && newline_index < tokens.len()
    {
        newline_index += 1;
    }
    if !matches!(
        tokens.get(newline_index),
        Some(Token {
            r#type: TokenType::NEWLINE,
            ..
        })
    ) {
        return Err(format!("no newline after include directive"));
    }
    let mut include_tokens = Vec::new();
    let mut expand_macro_index = *index + 1;
    while expand_macro_index < newline_index {
        if let Some(Token {
            r#type: TokenType::IDENT { str_map_key, .. },
            ..
        }) = tokens.get(expand_macro_index)
        {
            if defines.contains_key(str_map_key) {
                expand_macro(
                    tokens,
                    &mut expand_macro_index,
                    defines,
                    str_maps,
                    &mut include_tokens,
                )?;
                continue;
            }
        }
        if expand_macro_index < newline_index {
            include_tokens.push(tokens[expand_macro_index]);
        }
        expand_macro_index += 1;
    }
    let tokens = include_tokens;
    let mut include_index = 0;
    if matches!(
        tokens.get(include_index),
        Some(Token {
            r#type: TokenType::WHITESPACE,
            ..
        })
    ) {
        include_index += 1;
    }
    let mut file_name = None;
    let mut look_at_current_dir = false;
    let mut end_of_file_path_index = 0;
    if matches!(
        tokens.get(include_index),
        Some(Token {
            r#type: TokenType::IDENT { .. },
            ..
        })
    ) {
        include_index += 1;

        if matches!(
            tokens.get(include_index),
            Some(Token {
                r#type: TokenType::WHITESPACE,
                ..
            })
        ) {
            include_index += 1;
        }
        match tokens.get(include_index) {
            Some(Token {
                r#type: TokenType::LESS_THAN,
                ..
            }) => {
                include_index += 1;
                let mut punct_greater_than_index = include_index;
                while !matches!(
                    tokens.get(punct_greater_than_index),
                    Some(Token {
                        r#type: TokenType::GREATER_THAN,
                        ..
                    })
                ) && punct_greater_than_index < tokens.len()
                {
                    punct_greater_than_index += 1;
                }
                if !matches!(
                    tokens.get(punct_greater_than_index),
                    Some(Token {
                        r#type: TokenType::GREATER_THAN,
                        ..
                    })
                ) {
                    return Err(format!("No '>' for opening '<' in include directive"));
                }
                let mut file_path_bytes = Vec::new();
                for index in include_index..punct_greater_than_index {
                    if let Some(bv) = tokens[index].to_byte_vec(str_maps) {
                        file_path_bytes.extend_from_slice(bv.as_slice());
                    } else {
                        return Err(format!("{:?} cannot be to_byte_vec-fied", tokens[index]));
                    }
                }
                match String::from_utf8(file_path_bytes) {
                    Ok(s) => {
                        file_name = Some(s);
                    }
                    Err(_) => {
                        return Err(format!("Include directive contains invalid utf8"));
                    }
                }
                end_of_file_path_index = punct_greater_than_index + 1;
            }
            Some(Token {
                r#type:
                    TokenType::StringLiteral {
                        str_lit:
                            StringLiteral {
                                prefix_key: _,
                                sequence_key,
                            },
                    },
                ..
            }) => {
                look_at_current_dir = true;
                let sequence = &str_maps.key_to_byte_vec[*sequence_key];
                match String::from_utf8(sequence.to_vec()) {
                    Ok(s) => file_name = Some(s),
                    Err(_) => {
                        return Err(format!("Include directive contains invalid utf8"));
                    }
                }
                end_of_file_path_index = include_index + 1;
            }
            _ => {}
        }
    }
    if let Some(slice) = tokens.get(end_of_file_path_index + 1..) {
        if slice
            .iter()
            .filter(|t| {
                !matches!(
                    t,
                    Token {
                        r#type: TokenType::WHITESPACE,
                        ..
                    }
                )
            })
            .count()
            > 0
        {
            let Some(t) = tokens.get(include_index) else {
                unreachable!()
            };
            let Some(bv) = t.to_byte_vec(str_maps) else {
                unreachable!()
            };
            let Ok(s) = String::from_utf8(bv) else {
                unreachable!()
            };
            eprintln!("Warning: Tokens after {s} are skipped",);
        }
    }
    if let Some(fname) = file_name {
        if look_at_current_dir {
            let curr_dir = {
                let mut split_index = 0;
                for (i, c) in curr_path.char_indices().rev() {
                    if c == '/' {
                        split_index = i;
                        break;
                    }
                }
                curr_path.split_at(split_index).0
            };
            let full_path_file = curr_dir.to_string() + "/" + &fname;
            match std::fs::read(full_path_file.as_str()) {
                Ok(file_contents) => {
                    let tokens_from_file = cpp(
                        file_contents,
                        full_path_file.as_str(),
                        include_paths,
                        defines,
                        str_maps,
                    )?;
                    return Ok(());
                }
                Err(_) => {
                    eprintln!("fs::read failed for path: {}", full_path_file);
                }
            }
        } else {
            for path_index in 0..include_paths.len() {
                let path = include_paths[path_index];
                let full_path_file = path.to_string() + "/" + &fname;
                match std::fs::read(full_path_file.as_str()) {
                    Ok(file_contents) => {
                        let tokens_from_file =
                            cpp(file_contents, curr_path, include_paths, defines, str_maps)?;
                        return Ok(());
                    }
                    Err(_) => {
                        eprintln!("fs::read failed for path: {}", full_path_file);
                    }
                }
            }
        }
    }
    Err(String::from("file not found"))
}

fn parse_defined_in_if_directive(
    tokens: &[Token],
    index: usize,
    final_eval_tokens: &mut Vec<Token>,
    defines: &HashMap<usize, Define>,
    str_maps: &mut ByteVecMaps,
) -> Result<(), String> {
    let mut defined_index = index + 1;
    if let Some(Token {
        r#type: TokenType::WHITESPACE | TokenType::OPEN_PAR,
        ..
    }) = tokens.get(defined_index)
    {
        let start = defined_index;
        defined_index += 1;
        if let Some(Token {
            r#type: TokenType::WHITESPACE,
            ..
        }) = tokens.get(defined_index)
        {
            defined_index += 1;
        }
        if let Some(Token {
            r#type: TokenType::OPEN_PAR,
            ..
        }) = tokens.get(defined_index)
        {
            defined_index += 1;
        }
        if let Some(Token {
            r#type: TokenType::WHITESPACE,
            ..
        }) = tokens.get(defined_index)
        {
            defined_index += 1;
        }
        if let Some(Token {
            r#type:
                TokenType::IDENT {
                    str_map_key: identifier_name_key,
                },
            ..
        }) = tokens.get(defined_index)
        {
            defined_index += 1;
            if defines.contains_key(&identifier_name_key) {
                final_eval_tokens.push(Token {
                    r#type: TokenType::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec(&[b'1']),
                        suffix: None,
                    },
                    location: None,
                });
            } else {
                final_eval_tokens.push(Token {
                    r#type: TokenType::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec(&[b'0']),
                        suffix: None,
                    },
                    location: None,
                });
            }
            if matches!(
                tokens.get(start),
                Some(Token {
                    r#type: TokenType::OPEN_PAR,
                    ..
                })
            ) {
                while !matches!(
                    tokens.get(defined_index),
                    Some(Token {
                        r#type: TokenType::CLOSE_PAR,
                        ..
                    })
                ) && defined_index < tokens.len()
                {
                    defined_index += 1;
                }
                if defined_index == tokens.len() {
                    return Err(format!("Missing closing parenthesis for defined at: TODO!"));
                }
                return Ok(());
            }
        } else {
            return Err(format!("unexpected token: {:?}", tokens[defined_index]));
        }
    } else {
        return Err(format!("unexpected token: {:?}", tokens[defined_index]));
    }
    Ok(())
}

fn if_directive(
    tokens: &mut [Token],
    index: &mut usize,
    defines: &HashMap<usize, Define>,
    str_maps: &mut ByteVecMaps,
) -> Result<(), String> {
    let mut balance_index = *index;
    let mut if_endif_counter = 0;
    let mut if_elif_else_structure_index: Vec<(Vec<u8>, usize, usize)> = Vec::new();
    'outer: loop {
        match tokens.get(balance_index) {
            Some(Token {
                r#type: TokenType::HASH,
                ..
            }) => {
                let punct_hash_index = balance_index;
                let mut checks_follows_whitespace_nothing_newline_index = balance_index;
                let follows_whitespace_nothing_newline = loop {
                    if checks_follows_whitespace_nothing_newline_index > 0 {
                        checks_follows_whitespace_nothing_newline_index -= 1;
                    } else {
                        break true;
                    }

                    match tokens.get(checks_follows_whitespace_nothing_newline_index) {
                        Some(Token {
                            r#type: TokenType::WHITESPACE,
                            ..
                        }) => {}
                        Some(Token {
                            r#type: TokenType::NEWLINE,
                            ..
                        }) => break true,
                        _ => break false,
                    }
                };
                if follows_whitespace_nothing_newline {
                    balance_index += 1;
                    if matches!(
                        tokens.get(balance_index),
                        Some(Token {
                            r#type: TokenType::WHITESPACE,
                            ..
                        })
                    ) {
                        balance_index += 1;
                    }
                    match tokens.get(balance_index) {
                        Some(Token {
                            r#type:
                                TokenType::IDENT {
                                    str_map_key: id_key,
                                    ..
                                },
                            ..
                        }) => {
                            let id = str_maps.key_to_byte_vec[*id_key].clone();
                            match id.as_slice() {
                                b"endif" => loop {
                                    balance_index += 1;
                                    match tokens.get(balance_index) {
                                        Some(Token {
                                            r#type: TokenType::NEWLINE,
                                            ..
                                        }) => {
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
                                        Some(Token {
                                            r#type: TokenType::WHITESPACE,
                                            ..
                                        }) => {}
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
                                        Some(Token {
                                            r#type: TokenType::NEWLINE,
                                            ..
                                        }) => {
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
                                        Some(Token {
                                            r#type: TokenType::NEWLINE,
                                            ..
                                        }) => {
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
                                        Some(Token {
                                            r#type: TokenType::NEWLINE,
                                            ..
                                        }) => {
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
                                        Some(Token {
                                            r#type: TokenType::WHITESPACE,
                                            ..
                                        }) => {}
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
        let (macro_id_bytes, _, _) = &if_elif_else_structure_index[index_for_structure_index];
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
        while !matches!(
            tokens.get(start_looking),
            Some(Token {
                r#type: TokenType::IDENT { .. },
                ..
            })
        ) && start_looking < tokens.len()
        {
            start_looking += 1;
        }
        assert!(matches!(
            tokens.get(start_looking),
            Some(Token {
                r#type: TokenType::IDENT { .. },
                ..
            })
        ));
        start_looking += 1;
        let eval_vec = &tokens[start_looking..*end];

        let truthy = match macro_id.as_slice() {
            b"if" | b"elif" => {
                let mut eval_vec_index = 0;
                let mut final_eval_tokens = Vec::new();
                while eval_vec_index < eval_vec.len() {
                    if let Token {
                        r#type:
                            TokenType::IDENT {
                                str_map_key: curr_id_key,
                            },
                        ..
                    } = &eval_vec[eval_vec_index]
                    {
                        let curr_id = str_maps.key_to_byte_vec[*curr_id_key].clone();
                        if curr_id != *b"defined" {
                            if !defines.contains_key(curr_id_key) {
                                final_eval_tokens.push(Token {
                                    r#type: TokenType::CONSTANT_DEC_INT {
                                        value_key: str_maps.add_byte_vec(&[b'0']),
                                        suffix: None,
                                    },
                                    location: None,
                                });
                                eval_vec_index += 1;
                            } else {
                                expand_macro(
                                    &eval_vec,
                                    &mut eval_vec_index,
                                    defines,
                                    str_maps,
                                    &mut final_eval_tokens,
                                )?;
                            }
                        } else {
                            parse_defined_in_if_directive(
                                eval_vec,
                                eval_vec_index,
                                &mut final_eval_tokens,
                                defines,
                                str_maps,
                            )?;
                        }
                        continue;
                    }
                    if eval_vec_index < eval_vec.len() {
                        final_eval_tokens.push(eval_vec[eval_vec_index]);
                    }
                    eval_vec_index += 1;
                }
                let eval_vec = final_eval_tokens;
                expressions::eval_constant_expression_integer_when_preprocess(
                    eval_vec.as_slice(),
                    index,
                    str_maps,
                )? != 0
            }
            b"ifdef" => {
                if eval_vec.iter().any(|t| {
                    !matches!(
                        t,
                        Token {
                            r#type: TokenType::IDENT { .. } | TokenType::WHITESPACE,
                            ..
                        }
                    )
                }) {
                    return Err(format!(
                        "expected only identifier within ifdef directive: {:?}",
                        eval_vec
                    ));
                }
                let Some(Token {
                    r#type:
                        TokenType::IDENT {
                            str_map_key: ident_key,
                            ..
                        },
                    ..
                }) = eval_vec.iter().find(|t| {
                    matches!(
                        t,
                        Token {
                            r#type: TokenType::IDENT { .. },
                            ..
                        }
                    )
                })
                else {
                    unreachable!()
                };
                defines.contains_key(ident_key)
            }
            b"ifndef" => {
                if eval_vec.iter().any(|t| {
                    !matches!(
                        t,
                        Token {
                            r#type: TokenType::IDENT { .. } | TokenType::WHITESPACE,
                            ..
                        }
                    )
                }) {
                    return Err(format!(
                        "expected only identifier within ifndef directive: {:?}",
                        eval_vec
                    ));
                }
                let Some(Token {
                    r#type:
                        TokenType::IDENT {
                            str_map_key: ident_key,
                            ..
                        },
                    ..
                }) = eval_vec.iter().find(|t| {
                    matches!(
                        t,
                        Token {
                            r#type: TokenType::IDENT { .. },
                            ..
                        }
                    )
                })
                else {
                    unreachable!()
                };
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
                match tokens[index_overwrite].r#type {
                    TokenType::NEWLINE => {}
                    _ => {
                        tokens[index_overwrite] = Token {
                            r#type: TokenType::WHITESPACE,
                            location: None,
                        };
                    }
                }
                index_overwrite += 1;
            }
            return Ok(());
        }
    }
    let mut index_overwrite = if_elif_else_structure_index[0].1;
    while index_overwrite < if_elif_else_structure_index.last().unwrap().2 {
        match tokens[index_overwrite].r#type {
            TokenType::NEWLINE => {}
            _ => {
                tokens[index_overwrite] = Token {
                    r#type: TokenType::WHITESPACE,
                    location: None,
                };
            }
        }
        index_overwrite += 1;
    }
    Ok(())
}

fn parse_identifier_list(
    tokens: &[Token],
    index: &mut usize,
    str_maps: &ByteVecMaps,
) -> Result<Vec<usize>, String> {
    let mut identifier_keys = Vec::new();
    loop {
        consume_specifically_spaces(tokens, index);
        if let Some(Token {
            r#type: TokenType::IDENT { str_map_key },
            location: Some(Location { line, column }),
        }) = tokens.get(*index)
        {
            *index += 1;
            let arg = &str_maps.key_to_byte_vec[*str_map_key];
            if identifier_keys.contains(str_map_key) {
                // A parameter identifier in a function-like macro shall be uniquely declared within its scope.
                return Err(error("duplicate parameter name", *line, *column));
            }
            if *arg == *b"__VA_ARGS__" {
                return Err(error(
                    "__VA_ARGS__ cannot be used as a parameter name",
                    *line,
                    *column,
                ));
            }
            identifier_keys.push(*str_map_key);
        } else {
            if let Some(Token {
                location: Some(Location { line, column }),
                ..
            }) = tokens.get(*index)
            {
                return Err(error("Expected identifier token", *line, *column));
            }
            return Err("Expected identifier token".to_string());
        }
        consume_specifically_spaces(tokens, index);
        if !matches!(
            tokens.get(*index),
            Some(Token {
                r#type: TokenType::COMMA,
                ..
            })
        ) {
            break Ok(identifier_keys);
        }
        *index += 1;
        if matches!(
            tokens.get(*index),
            Some(Token {
                r#type: TokenType::ELLIPSIS,
                ..
            })
        ) {
            break Ok(identifier_keys);
        }
    }
}

fn define_directive(
    tokens: &[Token],
    index: &mut usize,
    defines: &mut HashMap<usize, Define>,
    str_maps: &ByteVecMaps,
) -> Result<(), String> {
    let mut def_data = Define {
        parameters: None,
        var_arg: false,
        replacement_list: Vec::new(),
    };
    let Some(Token {
        r#type:
            TokenType::IDENT {
                str_map_key: identifier_of_macro_key,
                ..
            },
        ..
    }) = tokens.get(*index)
    else {
        unreachable!()
    };
    *index += 1;
    let Some(position_of_newline) = tokens[*index..]
        .iter()
        .position(|t| t.r#type == TokenType::NEWLINE)
    else {
        unreachable!()
    };
    // There shall be white space between the identifier and the replacement list in the definition of an object-like macro.
    if let Some(Token {
        r#type: TokenType::OPEN_PAR,
        ..
    }) = tokens.get(*index)
    {
        def_data.parameters = Some(parse_identifier_list(tokens, index, str_maps)?);
        if matches!(
            tokens.get(*index),
            Some(Token {
                r#type: TokenType::ELLIPSIS,
                ..
            })
        ) {
            *index += 1;
            def_data.var_arg = true;
        }
        expected_token(tokens, index, TokenType::CLOSE_PAR, "Expected ')'")?;
    }
    consume_specifically_spaces(tokens, index);
    def_data
        .replacement_list
        .extend_from_slice(&tokens[*index..position_of_newline]);
    defines.insert(*identifier_of_macro_key, def_data);
    let identifier_of_macro = &str_maps.key_to_byte_vec[*identifier_of_macro_key];
    let Some(ref mut dd) = defines.get_mut(&identifier_of_macro_key) else {
        unreachable!()
    };
    while let Some(Token {
        r#type: TokenType::WHITESPACE,
        ..
    }) = dd.replacement_list.last()
    {
        dd.replacement_list.pop();
    }
    if let Some(Token {
        r#type: TokenType::HASH_HASH,
        location: Some(Location { line, column }),
    }) = dd.replacement_list.first()
    {
        return Err(error(
            "'##' cannot be at the beginning or end of a replacement list",
            *line,
            *column,
        ));
    }
    if let Some(Token {
        r#type: TokenType::HASH_HASH,
        location: Some(Location { line, column }),
    }) = dd.replacement_list.last()
    {
        return Err(error(
            "'##' cannot be at the beginning or end of a replacement list",
            *line,
            *column,
        ));
    }
    return Ok(());
}
fn error_directive(_tokens: &mut Vec<TokenType>) {
    todo!()
}
fn line_directive(_tokens: &mut Vec<TokenType>, _index: usize, _end: usize) -> Result<(), String> {
    todo!()
}
fn undef_directive(
    tokens: &[Token],
    index: &mut usize,
    defines: &mut HashMap<usize, Define>,
    _str_maps: &mut ByteVecMaps,
) -> Result<(), String> {
    let mut index_of_identifier = *index + 1;
    if matches!(
        tokens.get(index_of_identifier),
        Some(Token {
            r#type: TokenType::WHITESPACE,
            ..
        })
    ) {
        index_of_identifier += 1;
    }
    index_of_identifier += 1;
    if matches!(
        tokens.get(index_of_identifier),
        Some(Token {
            r#type: TokenType::WHITESPACE,
            ..
        })
    ) {
        index_of_identifier += 1;
        if let Some(Token {
            r#type:
                TokenType::IDENT {
                    str_map_key: identifier_to_be_undef_key,
                    ..
                },
            ..
        }) = tokens.get(index_of_identifier)
        {
            defines.remove(identifier_to_be_undef_key);
            let mut newline_index = index_of_identifier + 1;
            while !matches!(
                tokens.get(newline_index),
                Some(Token {
                    r#type: TokenType::NEWLINE,
                    ..
                })
            ) {
                newline_index += 1;
                if matches!(tokens.get(newline_index), None) {
                    return Err(String::from("missing newline for undef directive"));
                }
            }
            return Ok(());
        }
    }
    Err(format!("undef directive not formed correctly"))
}
fn hash_hash_deletion_and_concat_tokens(
    replacement_list: &mut Vec<Token>,
    hash_hash_from_args: &[usize],
) {
    let mut hash_hash_process_index = 0;
    while hash_hash_process_index < replacement_list.len() {
        let token = replacement_list[hash_hash_process_index];
        if matches!(token.r#type, TokenType::HASH_HASH)
            && !hash_hash_from_args.contains(&hash_hash_process_index)
        {
            let mut left_index = hash_hash_process_index - 1;
            // left_index should never be less than zero because in the define_directive
            // function, we check if ## is at the beginning or end and we trim whitespace.
            // Same thing for right_index.
            while matches!(
                replacement_list.get(left_index),
                Some(Token {
                    r#type: TokenType::WHITESPACE | TokenType::NEWLINE,
                    ..
                })
            ) {
                left_index -= 1;
            }
            let mut right_index = hash_hash_process_index + 1;
            while matches!(
                replacement_list.get(right_index),
                Some(Token {
                    r#type: TokenType::WHITESPACE | TokenType::NEWLINE,
                    ..
                })
            ) {
                right_index += 1;
            }
            let replacement_list_clone = replacement_list.clone();
            let left_token = replacement_list_clone.get(left_index);
            let right_token = replacement_list_clone.get(right_index);
            //Placemarker preprocessing tokens are handled specially: concatena-
            //tion of two placemarkers results in a single placemarker preprocessing token, and concatenation
            //of a placemarker with a non-placemarker preprocessing token results in the non-placemarker pre-
            //processing token
            match (left_token, right_token) {
                (
                    Some(Token {
                        r#type: TokenType::PLACEMARKER,
                        ..
                    }),
                    Some(Token {
                        r#type: TokenType::PLACEMARKER,
                        ..
                    }),
                ) => {
                    for _ in left_index..=right_index {
                        replacement_list.remove(left_index);
                    }
                    replacement_list.insert(
                        left_index,
                        Token {
                            r#type: TokenType::PLACEMARKER,
                            location: None,
                        },
                    );
                }
                (
                    Some(_),
                    Some(Token {
                        r#type: TokenType::PLACEMARKER,
                        ..
                    }),
                ) => {
                    for _ in left_index + 1..=right_index {
                        replacement_list.remove(left_index + 1);
                    }
                }
                (
                    Some(Token {
                        r#type: TokenType::PLACEMARKER,
                        ..
                    }),
                    Some(_),
                ) => {
                    for _ in left_index..right_index {
                        replacement_list.remove(left_index);
                    }
                }
                (Some(_), Some(_)) => {
                    for _ in left_index + 1..right_index {
                        replacement_list.remove(left_index + 1);
                    }
                }
                _ => unreachable!(),
            }
            hash_hash_process_index = left_index;
            continue;
        }
        hash_hash_process_index += 1;
    }
}

fn parse_macro_and_replace(
    defines: &HashMap<usize, Define>,
    macro_stack: &mut Vec<Macro>,
    replacement_list: &mut Vec<Token>,
    str_maps: &mut ByteVecMaps,
    already_replaced_macros: &mut Vec<(usize, usize)>,
) -> Result<(), String> {
    let Some(curr_macro) = macro_stack.pop() else {
        unreachable!()
    };
    let Some(defines_data) = defines.get(&curr_macro.macro_key) else {
        unreachable!()
    };
    // actual_replacement_list is the current replacement_list for the current macro replacement
    // the replacement_list in the fn args is the overall replacement_list
    let mut actual_replacement_list = defines_data.replacement_list.clone();
    let mut hash_hash_from_args = Vec::new();
    if let Some(parameters) = &defines_data.parameters {
        let Some(arguments) = curr_macro.arguments else {
            unreachable!(
                "{}",
                String::from_utf8(str_maps.key_to_byte_vec[curr_macro.macro_key].clone()).unwrap()
            )
        };
        let mut token_index = 0;
        while token_index < actual_replacement_list.len() {
            let token = actual_replacement_list[token_index];
            match token.r#type {
                TokenType::IDENT {
                    str_map_key: id_key,
                    ..
                } => {
                    if parameters.contains(&id_key)
                        || str_maps.key_to_byte_vec[id_key] == b"__VA_ARGS__"
                    {
                        // can never go out of bounds
                        let mut p_index = 0;
                        let seen_arg_index = loop {
                            if p_index == parameters.len() || parameters[p_index] == id_key {
                                break p_index;
                            }
                            p_index += 1;
                        };
                        let argument = if seen_arg_index >= arguments.len() {
                            Vec::<Token>::new()
                        } else {
                            arguments[seen_arg_index].clone()
                        };
                        let first_condition = token_index > 0
                            && matches!(
                                actual_replacement_list.get(token_index - 1),
                                Some(Token {
                                    r#type: TokenType::HASH,
                                    ..
                                })
                            );
                        let second_condition = token_index > 1
                            && matches!(
                                actual_replacement_list.get(token_index - 2),
                                Some(Token {
                                    r#type: TokenType::HASH,
                                    ..
                                })
                            )
                            && matches!(
                                actual_replacement_list.get(token_index - 1),
                                Some(Token {
                                    r#type: TokenType::WHITESPACE,
                                    ..
                                })
                            );
                        if first_condition || second_condition {
                            // stringification of argument token sequence
                            let mut sequence = Vec::new();
                            let start_remove_index = if first_condition {
                                token_index - 1
                            } else {
                                token_index - 2
                            };
                            for _ in start_remove_index..token_index + 1 {
                                actual_replacement_list.remove(start_remove_index);
                            }
                            for t in argument {
                                match t.r#type {
                                    TokenType::NEWLINE => {
                                        sequence.push(b' ');
                                    }
                                    _ => {
                                        if let Some(mut bv) = t.to_byte_vec(str_maps) {
                                            if bv.contains(&b'\\') || bv.contains(&b'"') {
                                                for bv_index in 0..bv.len() {
                                                    if bv[bv_index] == b'\\' || bv[bv_index] == b'"'
                                                    {
                                                        bv.insert(bv_index, b'\\');
                                                    }
                                                }
                                            }
                                            sequence.extend_from_slice(&bv);
                                        } else {
                                            return Err(format!(
                                                    "tried to stringify token that cannot be stringified"
                                            ));
                                        }
                                    }
                                }
                            }
                            actual_replacement_list.insert(
                                start_remove_index,
                                Token {
                                    r#type: TokenType::StringLiteral {
                                        str_lit: StringLiteral {
                                            prefix_key: None,
                                            sequence_key: str_maps.add_byte_vec(&sequence),
                                        },
                                    },
                                    location: None,
                                },
                            );
                        } else {
                            actual_replacement_list.remove(token_index);
                            let mut insert_index = token_index;
                            let count_of_non_whitespace = argument
                                .iter()
                                .filter(|t| {
                                    !matches!(
                                        t,
                                        Token {
                                            r#type: TokenType::WHITESPACE | TokenType::NEWLINE,
                                            ..
                                        }
                                    )
                                })
                                .count();
                            if count_of_non_whitespace > 0 {
                                for t in argument {
                                    if matches!(
                                        t,
                                        Token {
                                            r#type: TokenType::HASH_HASH,
                                            ..
                                        }
                                    ) {
                                        hash_hash_from_args.push(insert_index);
                                    }
                                    actual_replacement_list.insert(insert_index, t);
                                    insert_index += 1;
                                }
                                token_index = insert_index;
                            } else {
                                // Only add placemarker if parameter is preceded or followed by ##
                                if (token_index > 1
                                    && matches!(
                                        actual_replacement_list.get(token_index - 2),
                                        Some(Token {
                                            r#type: TokenType::HASH_HASH,
                                            ..
                                        })
                                    )
                                    && matches!(
                                        actual_replacement_list.get(token_index - 1),
                                        Some(Token {
                                            r#type: TokenType::WHITESPACE,
                                            ..
                                        })
                                    ))
                                    || (token_index > 0
                                        && matches!(
                                            actual_replacement_list.get(token_index - 1),
                                            Some(Token {
                                                r#type: TokenType::HASH_HASH,
                                                ..
                                            })
                                        ))
                                    || matches!(
                                        actual_replacement_list.get(token_index + 1),
                                        Some(Token {
                                            r#type: TokenType::HASH_HASH,
                                            ..
                                        })
                                    )
                                    || (matches!(
                                        actual_replacement_list.get(token_index + 1),
                                        Some(Token {
                                            r#type: TokenType::WHITESPACE,
                                            ..
                                        })
                                    ) && matches!(
                                        actual_replacement_list.get(token_index + 2),
                                        Some(Token {
                                            r#type: TokenType::HASH_HASH,
                                            ..
                                        })
                                    ))
                                {
                                    actual_replacement_list.insert(
                                        insert_index,
                                        Token {
                                            r#type: TokenType::PLACEMARKER,
                                            location: None,
                                        },
                                    );
                                    token_index += 1;
                                }
                            }
                        }
                        continue;
                    }
                }
                _ => {}
            }
            token_index += 1;
        }
    }
    hash_hash_deletion_and_concat_tokens(&mut actual_replacement_list, &hash_hash_from_args);
    let mut placemarker_removal_index = 0;
    while placemarker_removal_index < actual_replacement_list.len() {
        if let Token {
            r#type: TokenType::PLACEMARKER,
            ..
        } = actual_replacement_list[placemarker_removal_index]
        {
            actual_replacement_list.remove(placemarker_removal_index);
            continue;
        }
        placemarker_removal_index += 1;
    }
    let mut byte_vec = Vec::new();
    for t in actual_replacement_list {
        let Some(inner_byte_vec) = t.to_byte_vec(str_maps) else {
            unreachable!()
        };
        byte_vec.extend_from_slice(inner_byte_vec.as_slice());
    }
    let actual_replacement_list = lexer(byte_vec.as_slice(), true, str_maps)?;
    for _ in curr_macro.start..curr_macro.end + 1 {
        replacement_list.remove(curr_macro.start);
    }
    let mut insert_index = curr_macro.start;
    for t in &actual_replacement_list {
        replacement_list.insert(insert_index, *t);
        insert_index += 1;
    }
    already_replaced_macros.push((curr_macro.macro_key, curr_macro.depth));
    // rescanning for further replacement
    let mut moar_macros_index = curr_macro.start;
    'outer: while moar_macros_index < replacement_list.len() {
        if let Some(Token {
            r#type: TokenType::IDENT {
                str_map_key: key, ..
            },
            ..
        }) = replacement_list.get(moar_macros_index)
        {
            if defines.contains_key(key) {
                for already_replaced_macros_index in 0..already_replaced_macros.len() {
                    let (macro_key, depth) = already_replaced_macros[already_replaced_macros_index];
                    if *key == macro_key && depth < curr_macro.depth {
                        moar_macros_index += 1;
                        continue 'outer;
                    }
                }
                let Some(define_data) = defines.get(key) else {
                    unreachable!()
                };
                if let Some(parameters) = &define_data.parameters {
                    if let Some(mut next_macro) = parse_function_macro(
                        replacement_list,
                        &[],
                        moar_macros_index,
                        defines,
                        *key,
                    ) {
                        let Some(args) = &mut next_macro.arguments else {
                            unreachable!()
                        };
                        if args.len() < parameters.len()
                            || (args.len() > parameters.len() && !define_data.var_arg)
                        {
                            moar_macros_index += 1;
                            continue 'outer;
                        }
                        for arg in args {
                            expand_arguments(arg, defines, str_maps)?;
                        }
                        next_macro.depth = curr_macro.depth + 1;
                        moar_macros_index = next_macro.end + 1;
                        macro_stack.push(next_macro);
                        continue 'outer;
                    }
                } else {
                    macro_stack.push(Macro {
                        macro_key: *key,
                        start: moar_macros_index,
                        end: moar_macros_index,
                        depth: curr_macro.depth + 1,
                        arguments: None,
                    });
                }
            }
        }
        moar_macros_index += 1;
    }
    Ok(())
}
// This function takes in either a starting (
// or a starting identifier. Reason being is that
// final_tokens could have a function macro identifier somewhere
// on the end of it but the invocation is in the tokens vector.
//
// This function also takes in two slices, because when the 'tokens' parameter slice ends,
// the extended_tokens could have actual tokens that, when combined with the actual tokens in the
// 'tokens' parameter, become an actual macro function invocation.
fn parse_function_macro(
    tokens: &[Token],
    extended_tokens: &[Token],
    start_index: usize,
    defines: &HashMap<usize, Define>,
    // have to pass macro_key in because some macro could expand and have a macro name at the end,
    // inside of final_tokens
    macro_key: usize,
) -> Option<Macro> {
    let mut fn_macro_index = start_index;
    if matches!(
        tokens.get(fn_macro_index),
        Some(Token {
            r#type: TokenType::IDENT { .. },
            ..
        })
    ) {
        fn_macro_index += 1;
    }
    while matches!(
        tokens.get(fn_macro_index),
        Some(Token {
            r#type: TokenType::WHITESPACE | TokenType::NEWLINE,
            ..
        })
    ) {
        fn_macro_index += 1;
    }
    if !matches!(
        tokens.get(fn_macro_index),
        Some(Token {
            r#type: TokenType::OPEN_PAR,
            ..
        })
    ) && !matches!(
        extended_tokens.get(
            if !extended_tokens.is_empty() && fn_macro_index >= tokens.len() {
                fn_macro_index - tokens.len()
            } else {
                0
            },
        ),
        Some(Token {
            r#type: TokenType::OPEN_PAR,
            ..
        })
    ) {
        return None;
    }
    if defines.get(&macro_key).is_none() {
        return None;
    }
    let Some(def_data) = defines.get(&macro_key) else {
        unreachable!()
    };
    let Some(parameters) = &def_data.parameters else {
        unreachable!()
    };
    let open_par_index = fn_macro_index;
    let mut parenth_stack = vec![(TokenType::OPEN_PAR, fn_macro_index)];
    fn_macro_index += 1;
    let mut comma_indices = Vec::<usize>::new();
    while !parenth_stack.is_empty()
        && (tokens.get(fn_macro_index).is_some()
            || if !extended_tokens.is_empty() && fn_macro_index >= tokens.len() {
                extended_tokens.get(fn_macro_index - tokens.len()).is_some()
            } else {
                false
            })
    {
        let temp_list = [
            tokens.get(fn_macro_index),
            extended_tokens.get(
                if !extended_tokens.is_empty() && fn_macro_index >= tokens.len() {
                    fn_macro_index - tokens.len()
                } else {
                    0
                },
            ),
        ];
        let Some(ot) = temp_list.iter().find(|ot| ot.is_some()) else {
            unreachable!()
        };
        if let Some(t) = ot {
            match t.r#type {
                TokenType::COMMA => {
                    if comma_indices.len() < parameters.len() {
                        comma_indices.push(fn_macro_index);
                    }
                }
                TokenType::OPEN_PAR => {
                    parenth_stack.push((TokenType::OPEN_PAR, fn_macro_index));
                }
                TokenType::CLOSE_PAR => {
                    if let (TokenType::OPEN_PAR, par_index) = parenth_stack[parenth_stack.len() - 1]
                    {
                        parenth_stack.pop();
                        if let Some(comma_index) = comma_indices.last() {
                            if *comma_index > par_index
                                && *comma_index < fn_macro_index
                                && !parenth_stack.is_empty()
                            {
                                comma_indices.pop();
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        fn_macro_index += 1;
    }
    if parenth_stack.is_empty() {
        let close_par_index = fn_macro_index - 1;
        let mut prev_comma_index = open_par_index + 1;
        let mut macro_obj = Macro {
            macro_key,
            start: start_index,
            end: close_par_index,
            depth: 1,
            arguments: Some(Vec::new()),
        };
        let Some(v) = &mut macro_obj.arguments else {
            unreachable!()
        };
        for comma_idx in comma_indices {
            if prev_comma_index < tokens.len() {
                v.push(tokens[prev_comma_index..comma_idx].to_vec());
            } else {
                v.push(
                    extended_tokens[prev_comma_index - tokens.len()..comma_idx - tokens.len()]
                        .to_vec(),
                );
            }
            prev_comma_index = comma_idx + 1;
        }
        if prev_comma_index < tokens.len() {
            v.push(tokens[prev_comma_index..close_par_index].to_vec());
        } else {
            v.push(
                extended_tokens[prev_comma_index - tokens.len()..close_par_index - tokens.len()]
                    .to_vec(),
            );
        }
        return Some(macro_obj);
    }
    None
}
//Before being substituted, each
//argument’s preprocessing tokens are completely macro replaced as if they formed the rest of the
//preprocessing file; no other preprocessing tokens are available
//-- in isolation basically
fn expand_arguments(
    argument: &mut Vec<Token>,
    defines: &HashMap<usize, Define>,
    str_maps: &mut ByteVecMaps,
) -> Result<(), String> {
    let mut already_replaced_macros = Vec::<(usize, usize)>::new();
    let mut current_depth = 1;
    loop {
        let mut moar_macros_index = 0;
        let mut macro_stack = Vec::<Macro>::new();
        'outer: while moar_macros_index < argument.len() {
            if let Some(Token {
                r#type:
                    TokenType::IDENT {
                        str_map_key: key, ..
                    },
                ..
            }) = argument.get(moar_macros_index)
            {
                if defines.contains_key(key) {
                    for already_replaced_macros_index in 0..already_replaced_macros.len() {
                        let (macro_key, depth) =
                            already_replaced_macros[already_replaced_macros_index];
                        if *key == macro_key && depth < current_depth {
                            moar_macros_index += 1;
                            continue 'outer;
                        }
                    }
                    let Some(macro_define) = defines.get(&key) else {
                        unreachable!()
                    };
                    if macro_define.parameters.is_some() {
                        let macro_obj =
                            parse_function_macro(argument, &[], moar_macros_index, defines, *key);
                        if let Some(mut m) = macro_obj {
                            let end = m.end + 1;
                            m.depth = current_depth;
                            macro_stack.push(m);
                            moar_macros_index = end;
                        } else {
                            moar_macros_index += 1;
                            continue;
                        }
                    } else {
                        macro_stack.push(Macro {
                            macro_key: *key,
                            start: moar_macros_index,
                            end: moar_macros_index,
                            depth: current_depth,
                            arguments: None,
                        });
                        moar_macros_index += 1;
                    }
                    already_replaced_macros.push((*key, 1));
                } else {
                    moar_macros_index += 1;
                }
                continue;
            }
            moar_macros_index += 1;
        }
        if macro_stack.is_empty() {
            break;
        }
        while !macro_stack.is_empty() {
            parse_macro_and_replace(
                defines,
                &mut macro_stack,
                argument,
                str_maps,
                &mut already_replaced_macros,
            )?;
        }
        current_depth += 1;
    }
    Ok(())
}
fn expand_macro(
    tokens: &[Token],
    index: &mut usize,
    defines: &HashMap<usize, Define>,
    str_maps: &mut ByteVecMaps,
    final_tokens: &mut Vec<Token>,
) -> Result<(), String> {
    let mut accumulated_replacements = Vec::<Token>::new();
    let mut current_token = tokens[*index];
    let mut macro_index = index;
    // vector of (macro_key, depth)
    let mut already_replaced_macros: Vec<(usize, usize)> = Vec::new();
    let mut rechecking = false;
    let mut rechecking_idx = 0;
    'recheck: loop {
        let TokenType::IDENT {
            str_map_key: macro_id_key,
            ..
        } = current_token.r#type
        else {
            unreachable!("{:?}", current_token)
        };
        let Some(def_data) = defines.get(&macro_id_key) else {
            unreachable!()
        };
        let mut first_macro = if def_data.parameters.is_some() || def_data.var_arg {
            let parsed = if !rechecking {
                parse_function_macro(tokens, &[], *macro_index, defines, macro_id_key)
            } else {
                parse_function_macro(
                    &accumulated_replacements,
                    &tokens[*macro_index..],
                    rechecking_idx,
                    defines,
                    macro_id_key,
                )
            };
            if let Some(mut m) = parsed {
                let Some(args) = &mut m.arguments else {
                    unreachable!()
                };
                for arg in args {
                    expand_arguments(arg, defines, str_maps)?;
                }
                m.depth = already_replaced_macros
                    .iter()
                    .fold(0, |a, (_, depth)| a.max(*depth))
                    + 1;
                m
            } else {
                if !rechecking {
                    accumulated_replacements.push(tokens[*macro_index]);
                    *macro_index += 1;
                }
                break 'recheck;
            }
        } else {
            Macro {
                macro_key: macro_id_key,
                start: *macro_index,
                end: *macro_index,
                depth: 1,
                arguments: None,
            }
        };
        let mut original_macro = if !rechecking {
            tokens[first_macro.start..first_macro.end + 1].to_vec()
        } else {
            let mut first_part = accumulated_replacements[first_macro.start..].to_vec();
            let length_of_macro = first_macro.end + 1 - first_macro.start;
            first_part.extend_from_slice(
                &tokens[*macro_index
                    ..*macro_index + length_of_macro
                        - (accumulated_replacements.len() - first_macro.start)],
            );
            accumulated_replacements.truncate(rechecking_idx);
            first_part
        };
        // only change macro_index if the current macro ends past the current macro_index
        // FOR THE CASE OF WHEN A MACRO IS INVOKED WITHIN ACCUMULATED_REPLACEMENTS AND THE REST OF
        // THE MACRO INVOCATION IS IN 'tokens', 'macro_index' is only ever used to index 'tokens'
        // so macro_index shouldn't be changed if first_macro.end + 1 is less than macro_index
        if !rechecking {
            *macro_index = first_macro.end + 1;
        } else {
            *macro_index = *macro_index
                + (first_macro.end - (accumulated_replacements.len() - first_macro.start))
                + 1;
        }
        // set to zero because the original_macro/replacement_list starts at the first_macro
        first_macro.start = 0;
        // set to length of original_macro/replacement_list because the previous value was an
        // offset in 'tokens'
        first_macro.end = original_macro.len() - 1;
        let mut macro_stack: Vec<Macro> = vec![first_macro];
        while !macro_stack.is_empty() {
            parse_macro_and_replace(
                defines,
                &mut macro_stack,
                &mut original_macro,
                str_maps,
                &mut already_replaced_macros,
            )?;
        }
        // have to check the last non whitespace token to see if it is a fn like macro just in case
        // if the replacement has a fn like macro at the end and the invocation itself is not in
        // the replacement but in tokens like 'f(2)(9)' -> '2*g'. g is fn like macro where the
        // invocation is (9) but 2*g is in final_tokens and (9) is in tokens.
        'outer: for rescan_idx in 0..original_macro.len() {
            if let Some(Token {
                r#type:
                    TokenType::IDENT {
                        str_map_key: key, ..
                    },
                ..
            }) = original_macro.get(rescan_idx)
            {
                if defines.contains_key(key) {
                    for (macro_key, _) in &already_replaced_macros {
                        if *macro_key == *key {
                            continue 'outer;
                        }
                    }
                    current_token = original_macro[rescan_idx];
                    rechecking = true;
                    rechecking_idx = accumulated_replacements.len() + rescan_idx;
                    accumulated_replacements.extend_from_slice(&original_macro);
                    //already_replaced_macros.clear();
                    already_replaced_macros.push((
                        *key,
                        already_replaced_macros
                            .iter()
                            .fold(0, |a, (_, depth)| a.max(*depth))
                            + 1,
                    ));
                    continue 'recheck;
                }
            }
        }
        accumulated_replacements.extend_from_slice(&original_macro);
        break 'recheck;
    }
    final_tokens.extend_from_slice(&accumulated_replacements);
    Ok(())
}

fn get_newline_location(tokens: &[Token], index: &usize) -> usize {
    *index
        + tokens[*index..]
            .iter()
            .position(|t| t.r#type == TokenType::NEWLINE)
            .expect("There should be a newline on the same line as the preprocessing directive")
}

fn look_for_next_preprocessing_directive(tokens: &[Token], index: &mut usize) -> bool {
    let mut preceded_only_by_whitespace_or_nothing_or_newline = true;
    while *index < tokens.len() {
        match tokens.get(*index) {
            Some(Token {
                r#type: TokenType::NEWLINE,
                ..
            }) => {
                preceded_only_by_whitespace_or_nothing_or_newline = true;
            }
            Some(Token {
                r#type: TokenType::WHITESPACE,
                ..
            }) => {}
            Some(Token {
                r#type: TokenType::HASH,
                ..
            }) if preceded_only_by_whitespace_or_nothing_or_newline => {
                consume_specifically_spaces(tokens, index);
                // There's no need to check for an identifier token that matches one of the directive names
                // because of the existence of `non-directives` that are actually directives but have undefined behavior.
                // I handle directives outside of this function
                let newline_comes_after = check_valid_directive(tokens, *index);
                if newline_comes_after {
                    return true;
                }
            }
            Some(Token { .. }) => preceded_only_by_whitespace_or_nothing_or_newline = false,
            None => unreachable!(),
        }
        *index += 1;
    }
    false
}

fn parse_control_line(
    tokens: &mut [Token],
    index: &mut usize,
    curr_path: &str,
    include_paths: &[&str],
    defines: &mut HashMap<usize, Define>,
    str_maps: &mut ByteVecMaps,
) -> Result<(), String> {
    let Some(Token {
        r#type: TokenType::IDENT { str_map_key: s },
        location: Some(Location { line, column }),
    }) = tokens.get(*index)
    else {
        unreachable!()
    };
    match str_maps.key_to_byte_vec[*s].as_slice() {
        b"include" => {
            *index += 1;
            include_directive(tokens, index, curr_path, include_paths, defines, str_maps)?;
        }
        b"define" => {
            *index += 1;
            consume_specifically_spaces(tokens, index);
            define_directive(tokens, index, defines, str_maps)?;
        }
        b"error" => todo!(),
        b"line" => todo!(),
        b"pragma" => todo!(),
        _ => {
            return Err(error(
                "Unknown control line preprocessing directive",
                *line,
                *column,
            ))
        }
    }
    Ok(())
}
fn parse_endif_line(
    tokens: &[Token],
    index: &mut usize,
    str_maps: &ByteVecMaps,
    location_of_if_directive: Location,
) -> Result<(), String> {
    let found = look_for_next_preprocessing_directive(tokens, index);
    if found {
        let Token {
            r#type: TokenType::IDENT { str_map_key },
            ..
        } = tokens[*index]
        else {
            unreachable!()
        };
        if *str_maps.key_to_byte_vec[str_map_key] == *b"endif" {
            return Ok(());
        }
    }
    let Location { line, column } = location_of_if_directive;
    Err(error(
        "Expected 'endif' for corresponding 'if' directive",
        line,
        column,
    ))
}
fn parse_else_group() {}
fn parse_elif_group(
    tokens: &mut [Token],
    index: &mut usize,
    str_maps: &mut ByteVecMaps,

    curr_path: &str,
    include_paths: &[&str],
    defines: &mut HashMap<usize, Define>,
) -> Result<(), String> {
    let newline_location = get_newline_location(tokens, index);
    let res = eval_constant_expression_integer_when_preprocess(
        &tokens[*index..newline_location],
        index,
        str_maps,
    )?;
    parse_group(tokens, index, str_maps, curr_path, include_paths, defines)?;
    Ok(())
}
fn parse_elif_groups(
    tokens: &mut [Token],
    index: &mut usize,
    str_maps: &mut ByteVecMaps,
    curr_path: &str,
    include_paths: &[&str],
    defines: &mut HashMap<usize, Define>,
) {
    loop {
        let found = look_for_next_preprocessing_directive(tokens, index);
        if found {
            if let Some(Token {
                r#type: TokenType::IDENT { str_map_key },
                ..
            }) = tokens.get(*index)
            {
                let name = &str_maps.key_to_byte_vec[*str_map_key];
                if *name == *b"elif" {
                    *index += 1;
                    consume_specifically_spaces(tokens, index);
                    parse_elif_group(tokens, index, str_maps, curr_path, include_paths, defines);
                }
            } else {
                break;
            }
        }
    }
}

// My current thoughts on handling conditional inclusion with preprocessing, is that I'll use recursion and recursive descent parsing in order
// to handle nested if sections correctly without having to track depth.
// With if sections handled correctly, I can process if, elif and else correctly as well
fn parse_if_group(
    tokens: &mut [Token],
    index: &mut usize,
    str_maps: &mut ByteVecMaps,
    if_directive_type: &[u8],
    curr_path: &str,
    include_paths: &[&str],
    defines: &mut HashMap<usize, Define>,
) -> Result<(), String> {
    let Token {
        location: Some(Location { line, column }),
        ..
    } = tokens[*index - 1]
    else {
        unreachable!()
    };
    match if_directive_type {
        b"if" => {}
        b"ifdef" => {}
        b"ifndef" => {}
        _ => unreachable!(),
    }
    let start_of_group_idx = get_newline_location(tokens, index) + 1;
    parse_group(tokens, index, str_maps, curr_path, include_paths, defines)?;
    parse_elif_groups(tokens, index, str_maps, curr_path, include_paths, defines);
    parse_endif_line(tokens, index, str_maps, Location { line, column })?;
    Ok(())
}

fn parse_if_section(
    tokens: &mut [Token],
    index: &mut usize,
    str_maps: &mut ByteVecMaps,
    if_directive_type: &[u8],
    curr_path: &str,
    include_paths: &[&str],
    defines: &mut HashMap<usize, Define>,
) -> Result<(), String> {
    parse_if_group(
        tokens,
        index,
        str_maps,
        if_directive_type,
        curr_path,
        include_paths,
        defines,
    )?;
    Ok(())
}

fn check_valid_directive(tokens: &[Token], index: usize) -> bool {
    let mut newline_comes_after_idx = index + 1;
    while newline_comes_after_idx < tokens.len()
        && !matches!(
            tokens.get(newline_comes_after_idx),
            Some(Token {
                r#type: TokenType::NEWLINE,
                ..
            }),
        )
    {
        newline_comes_after_idx += 1;
    }
    newline_comes_after_idx < tokens.len()
}

fn handle_null_directive_or_non_directive(tokens: &mut [Token], index: &mut usize) {
    // at this point, index should be at the newline token
    while !matches!(
        tokens.get(*index),
        Some(Token {
            r#type: TokenType::HASH,
            ..
        })
    ) {
        *index -= 1;
    }
    tokens[*index].r#type = TokenType::WHITESPACE
}

fn parse_group(
    tokens: &mut [Token],
    index: &mut usize,
    str_maps: &mut ByteVecMaps,
    curr_path: &str,
    include_paths: &[&str],
    defines: &mut HashMap<usize, Define>,
) -> Result<(), String> {
    let found = look_for_next_preprocessing_directive(tokens, index);
    if found {
        if let Some(Token {
            r#type: TokenType::NEWLINE,
            ..
        }) = tokens.get(*index)
        {
            handle_null_directive_or_non_directive(tokens, index);
            return Ok(());
        }
        if !matches!(
            tokens.get(*index),
            Some(Token {
                r#type: TokenType::IDENT { .. },
                ..
            })
        ) {
            handle_null_directive_or_non_directive(tokens, index);
            return Ok(());
        }
        let Some(Token {
            r#type: TokenType::IDENT { str_map_key },
            location: Some(Location { line, column }),
        }) = tokens.get(*index)
        else {
            unreachable!()
        };
        match str_maps.key_to_byte_vec[*str_map_key].as_slice() {
            b"if" | b"ifdef" | b"ifndef" => {
                *index += 1;
                consume_specifically_spaces(tokens, index);
                parse_if_section(
                    tokens,
                    index,
                    str_maps,
                    &str_maps.key_to_byte_vec[*str_map_key].clone(),
                    curr_path,
                    include_paths,
                    defines,
                )?;
            }
            b"include" | b"define" | b"error" | b"line" | b"undef" | b"pragma" => {
                *index += 1;
                consume_specifically_spaces(tokens, index);
                parse_control_line(tokens, index, curr_path, include_paths, defines, str_maps)?;
            }
            d @ (b"else" | b"elif" | b"endif") => {
                return Err(error(
                    &format!(
                        "Unexpected '{}' directive",
                        String::from_utf8(d.to_vec()).unwrap()
                    ),
                    *line,
                    *column,
                ));
            }
            _ => {}
        }
    }
    Ok(())
}
fn preprocessing_directives(
    tokens: &mut Vec<Token>,
    curr_path: &str,
    include_paths: &[&str],
    defines: &mut HashMap<usize, Define>,
    str_maps: &mut ByteVecMaps,
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
    while index < tokens.len() {}
    Err(String::from("unable to preprocess"))
}
pub fn output_tokens_stdout(tokens: &[Token], str_maps: &ByteVecMaps) {
    let vec_bytes = tokens
        .iter()
        .map(|t| t.to_byte_vec(str_maps).unwrap())
        .fold(Vec::new(), |mut a: Vec<u8>, e: Vec<u8>| {
            a.extend_from_slice(&e);
            a
        });
    print!("{}", String::from_utf8(vec_bytes).unwrap());
}

fn process_trigraphs(program_str: Vec<u8>) -> Vec<u8> {
    let mut trigraphs_processed = Vec::new();
    for index in 0..program_str.len() {
        trigraphs_processed.push(if index + 3 < program_str.len() {
            match program_str[index..index + 4] {
                [b'?', b'?', b'='] => {
                    eprintln!("WARNING: ??= trigraph changed to #");
                    b'#'
                }
                [b'?', b'?', b'('] => {
                    eprintln!("WARNING: ??( trigraph changed to [");
                    b'['
                }
                [b'?', b'?', b'/'] => {
                    eprintln!("WARNING: ??/ trigraph changed to \\");
                    b'\\'
                }
                [b'?', b'?', b')'] => {
                    eprintln!("WARNING: ??) trigraph changed to ]");
                    b']'
                }
                [b'?', b'?', b'`'] => {
                    eprintln!("WARNING: ??` trigraph changed to ^");
                    b'^'
                }
                [b'?', b'?', b'<'] => {
                    eprintln!("WARNING: ??< trigraph changed to {{");
                    b'{'
                }
                [b'?', b'?', b'!'] => {
                    eprintln!("WARNING: ??! trigraph changed to |");
                    b'|'
                }
                [b'?', b'?', b'>'] => {
                    eprintln!("WARNING: ??> trigraph changed to }}");
                    b'}'
                }
                [b'?', b'?', b'-'] => {
                    eprintln!("WARNING: ??- trigraph changed to ~");
                    b'~'
                }
                _ => program_str[index],
            }
        } else {
            program_str[index]
        })
    }
    trigraphs_processed
}
fn process_line_continuation(program_str: Vec<u8>) -> Vec<u8> {
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
    backslash_newline_spliced
}
// TODO: add flag options so that the user could specify if they wanted to only preprocess
// TODO: implement some kind of warning system
pub fn cpp(
    program_str: Vec<u8>,
    curr_path: &str,
    include_paths: &[&str],
    defines: &mut HashMap<usize, Define>,
    str_maps: &mut ByteVecMaps,
) -> Result<Vec<Token>, String> {
    // trigraphs (part of step 1 in the translation phase)
    let program_str = process_trigraphs(program_str);
    // step 2 in the translation phase
    let program_str = process_line_continuation(program_str);
    // step 3 in the translation phase
    let program_str = process_comments(program_str.as_slice())?;
    let mut lexed_tokens = lexer(&program_str, true, str_maps)?;
    // step 4 in the translation phase
    preprocessing_directives(
        &mut lexed_tokens,
        curr_path,
        include_paths,
        defines,
        str_maps,
    )?;
    // concatenating adjacent string literals together
    let tokens = concat_adjacent_strings(lexed_tokens.as_slice(), str_maps)?;
    Ok(tokens)
}

#[cfg(test)]
mod tests {

    use crate::lexer::*;
    use crate::parser::expressions::*;
    use crate::parser::*;
    use std::collections::HashMap;

    use super::{
        cpp, define_directive, expand_macro, if_directive, parse_defined_in_if_directive,
        preprocessing_directives, process_comments, Define,
    };
    #[test]
    fn comments_removal_outside_quotes() -> Result<(), String> {
        let src = "int main() {\n\"hi\"; // this is me\n}\n";
        let src_bytes = src.as_bytes();
        let removed = process_comments(src_bytes)?;
        let stringed = String::from_utf8(removed).unwrap();
        assert_eq!(stringed, "int main() {\n\"hi\";  \n}\n");
        Ok(())
    }
    #[test]
    fn comments_removal_inside_single_quotes() -> Result<(), String> {
        let src = "int main() {\n\"hi\"; '// this is me';\n}\n";
        let src_bytes = src.as_bytes();
        let removed = process_comments(src_bytes)?;
        let stringed = String::from_utf8(removed).unwrap();
        assert_eq!(stringed, "int main() {\n\"hi\"; '// this is me';\n}\n");
        Ok(())
    }
    #[test]
    fn comments_removal_inside_double_quotes() -> Result<(), String> {
        let src = "int main() {\n\"hi\"; \"// this is me\";\n}\n";
        let src_bytes = src.as_bytes();
        let removed = process_comments(src_bytes)?;
        let stringed = String::from_utf8(removed).unwrap();
        assert_eq!(stringed, "int main() {\n\"hi\"; \"// this is me\";\n}\n");
        Ok(())
    }
    #[test]
    fn block_comment_removal() -> Result<(), String> {
        let src = r##"/*
        HI THIS IS JASON HAR HAR HAR
            */"##;
        let src_bytes = src.as_bytes();
        let removed = process_comments(src_bytes)?;
        let stringed = String::from_utf8(removed).unwrap();
        assert_eq!(stringed, " ");
        Ok(())
    }
    #[test]
    fn include_test() -> Result<(), String> {
        {
            let src = r##"#include "hi.h"
int main() {
}"##
            .as_bytes();
            let mut defines = HashMap::new();
            let mut str_maps = ByteVecMaps::new();
            let _final_tokens = Vec::<TokenType>::new();
            let tokens = cpp(
                src.to_vec(),
                "./test_c_files/hi.h",
                &["./test_c_files"],
                &mut defines,
                &mut str_maps,
            )?;
            let assert_tokens = [
                TokenType::IDENT {
                    str_map_key: str_maps.add_byte_vec("int".as_bytes()),
                },
                TokenType::WHITESPACE,
                TokenType::IDENT {
                    str_map_key: str_maps.add_byte_vec("main".as_bytes()),
                },
                TokenType::OPEN_PAR,
                TokenType::CLOSE_PAR,
                TokenType::WHITESPACE,
                TokenType::OPEN_CURLY,
                TokenType::NEWLINE,
                TokenType::CLOSE_CURLY,
            ]
            .to_vec();
            assert_eq!(
                assert_tokens,
                tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>()
            );
        }
        {
            let src = r##"#define FILE "hi.h"
#include FILE
int main() {
}"##
            .as_bytes();
            let mut defines = HashMap::new();
            let mut str_maps = ByteVecMaps::new();
            let _final_tokens = Vec::<TokenType>::new();
            let tokens = cpp(
                src.to_vec(),
                "./test_c_files/hi.h",
                &["./test_c_files"],
                &mut defines,
                &mut str_maps,
            )?;
            let assert_tokens = [
                TokenType::IDENT {
                    str_map_key: str_maps.add_byte_vec("int".as_bytes()),
                },
                TokenType::WHITESPACE,
                TokenType::IDENT {
                    str_map_key: str_maps.add_byte_vec("main".as_bytes()),
                },
                TokenType::OPEN_PAR,
                TokenType::CLOSE_PAR,
                TokenType::WHITESPACE,
                TokenType::OPEN_CURLY,
                TokenType::NEWLINE,
                TokenType::CLOSE_CURLY,
            ]
            .to_vec();
            assert_eq!(
                assert_tokens,
                tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>()
            );
        }
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
        let mut str_maps = ByteVecMaps::new();
        let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        preprocessing_directives(
            &mut tokens,
            "./test_c_files/hi2.h",
            &["./test_c_files"],
            &mut defines,
            &mut str_maps,
        )?;
        let assert_tokens = vec![
            TokenType::IDENT { str_map_key: 2 },
            TokenType::WHITESPACE,
            TokenType::IDENT { str_map_key: 3 },
            TokenType::OPEN_PAR,
            TokenType::CLOSE_PAR,
            TokenType::WHITESPACE,
            TokenType::OPEN_CURLY,
            TokenType::NEWLINE,
            TokenType::CONSTANT_DEC_INT {
                value_key: 6,
                suffix: None,
            },
            TokenType::SEMI_COLON,
            TokenType::NEWLINE,
            TokenType::CLOSE_CURLY,
        ];
        assert_eq!(
            assert_tokens,
            tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>()
        );
        Ok(())
    }
    #[test]
    fn expand_macro_hash_operator() -> Result<(), String> {
        let src = r##"#define HI(a) #a
HI(5 5);"##
            .as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = ByteVecMaps::new();
        let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        let mut index = 0;
        define_directive(&mut tokens, &mut index, &mut defines, &mut str_maps)?;
        expand_macro(
            &mut tokens,
            &mut index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![TokenType::StringLiteral {
                str_lit: StringLiteral {
                    prefix_key: None,
                    sequence_key: str_maps.add_byte_vec("5 5".as_bytes())
                },
            }],
            final_tokens
                .iter()
                .map(|t| t.r#type)
                .collect::<Vec<TokenType>>()
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
        let mut str_maps = ByteVecMaps::new();
        let mut tokens = lexer(&src.as_bytes().to_vec(), true, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        let mut index = 0;
        let mut defines = HashMap::new();
        define_directive(&mut tokens, &mut index, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens, &mut index, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens, &mut index, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens, &mut index, &mut defines, &mut str_maps)?;
        while index < tokens.len() {
            if let Some(
                [Token {
                    r#type:
                        TokenType::IDENT {
                            str_map_key: first, ..
                        },
                    ..
                }, Token {
                    r#type: TokenType::OPEN_PAR,
                    ..
                }, Token {
                    r#type:
                        TokenType::IDENT {
                            str_map_key: second,
                            ..
                        },
                    ..
                }],
            ) = tokens.get(index..index + 3)
            {
                if *first == str_maps.add_byte_vec("join".as_bytes())
                    && *second == str_maps.add_byte_vec("x".as_bytes())
                {
                    break;
                }
            }
            index += 1;
        }
        expand_macro(
            &mut tokens,
            &mut index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![TokenType::StringLiteral {
                str_lit: StringLiteral {
                    prefix_key: None,
                    sequence_key: str_maps.add_byte_vec("x ## y".as_bytes())
                },
            }],
            final_tokens
                .iter()
                .map(|t| t.r#type)
                .collect::<Vec<TokenType>>()
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
        let mut str_maps = ByteVecMaps::new();
        let mut defines = HashMap::new();
        let tokens = cpp(src, "", &["./test_c_files"], &mut defines, &mut str_maps)?;
        assert_eq!(
            vec![
                TokenType::StringLiteral {
                    str_lit: StringLiteral {
                        prefix_key: None,
                        sequence_key: str_maps.add_byte_vec(" hello world".as_bytes())
                    },
                },
                TokenType::WHITESPACE,
                TokenType::NEWLINE
            ],
            tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>()
        );
        Ok(())
    }
    #[test]
    fn test_define_directive() -> Result<(), String> {
        let mut str_maps = ByteVecMaps::new();
        let src = "#define hash_hash # ## #\n";
        let src2 = "#define mkstr(a) # a\n";
        let src3 = "#define in_between(a) mkstr(a)\n";
        let src4 = "#define join(c, d) in_between(c hash_hash d)\n";
        let mut tokens = lexer(&src.as_bytes().to_vec(), true, &mut str_maps)?;
        let mut tokens2 = lexer(&src2.as_bytes().to_vec(), true, &mut str_maps)?;
        let mut tokens3 = lexer(&src3.as_bytes().to_vec(), true, &mut str_maps)?;
        let mut tokens4 = lexer(&src4.as_bytes().to_vec(), true, &mut str_maps)?;
        let mut defines = HashMap::new();
        define_directive(&mut tokens, &mut 0, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens2, &mut 0, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens3, &mut 0, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens4, &mut 0, &mut defines, &mut str_maps)?;
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
                    TokenType::HASH,
                    TokenType::WHITESPACE,
                    TokenType::HASH_HASH,
                    TokenType::WHITESPACE,
                    TokenType::HASH,
                ]
                .iter()
                .map(|t| Token {
                    r#type: *t,
                    location: None
                })
                .collect::<Vec<Token>>()
            },
            *defines
                .get(&str_maps.add_byte_vec("hash_hash".as_bytes()))
                .unwrap()
        );
        assert_eq!(
            Define {
                parameters: Some(vec![str_maps.add_byte_vec("a".as_bytes())]),
                var_arg: false,
                replacement_list: vec![
                    TokenType::HASH,
                    TokenType::WHITESPACE,
                    TokenType::IDENT {
                        str_map_key: str_maps.add_byte_vec("a".as_bytes()),
                    },
                ]
                .iter()
                .map(|t| Token {
                    r#type: *t,
                    location: None
                })
                .collect::<Vec<Token>>()
            },
            *defines
                .get(&str_maps.add_byte_vec("mkstr".as_bytes()))
                .unwrap()
        );
        assert_eq!(
            Define {
                parameters: Some(vec![str_maps.add_byte_vec("a".as_bytes())]),
                var_arg: false,
                replacement_list: vec![
                    TokenType::IDENT {
                        str_map_key: str_maps.add_byte_vec("mkstr".as_bytes())
                    },
                    TokenType::OPEN_PAR,
                    TokenType::IDENT {
                        str_map_key: str_maps.add_byte_vec("a".as_bytes())
                    },
                    TokenType::CLOSE_PAR,
                ]
                .iter()
                .map(|t| Token {
                    r#type: *t,
                    location: None
                })
                .collect::<Vec<Token>>()
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
                    TokenType::IDENT {
                        str_map_key: str_maps.add_byte_vec("in_between".as_bytes())
                    },
                    TokenType::OPEN_PAR,
                    TokenType::IDENT {
                        str_map_key: str_maps.add_byte_vec("c".as_bytes())
                    },
                    TokenType::WHITESPACE,
                    TokenType::IDENT {
                        str_map_key: str_maps.add_byte_vec("hash_hash".as_bytes())
                    },
                    TokenType::WHITESPACE,
                    TokenType::IDENT {
                        str_map_key: str_maps.add_byte_vec("d".as_bytes())
                    },
                    TokenType::CLOSE_PAR,
                ]
                .iter()
                .map(|t| Token {
                    r#type: *t,
                    location: None
                })
                .collect::<Vec<Token>>()
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
        let mut str_maps = ByteVecMaps::new();
        let mut tokens = lexer(src, true, &mut str_maps)?;
        let mut index = 0;
        define_directive(&mut tokens, &mut index, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens, &mut index, &mut defines, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        expand_macro(
            &mut tokens,
            &mut index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![TokenType::CONSTANT_DEC_INT {
                value_key: str_maps.add_byte_vec("4".as_bytes()),
                suffix: None,
            }],
            final_tokens
                .iter()
                .map(|t| t.r#type)
                .collect::<Vec<TokenType>>()
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
        let mut str_maps = ByteVecMaps::new();
        let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let mut index = 0;
        define_directive(&mut tokens, &mut index, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens, &mut index, &mut defines, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        expand_macro(
            &mut tokens,
            &mut index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![
                TokenType::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("4".as_bytes()),
                    suffix: None,
                },
                TokenType::WHITESPACE,
                TokenType::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("5".as_bytes()),
                    suffix: None,
                },
                TokenType::WHITESPACE,
                TokenType::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("6".as_bytes()),
                    suffix: None,
                }
            ],
            final_tokens
                .iter()
                .map(|t| t.r#type)
                .collect::<Vec<TokenType>>()
        );
        Ok(())
    }
    #[test]
    fn expand_macro_parentheses_argument() -> Result<(), String> {
        let src = r##"#define HI(a,b) a,b
HI((,),(,))"##
            .as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = ByteVecMaps::new();
        let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let mut index = 0;
        define_directive(&mut tokens, &mut index, &mut defines, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        expand_macro(
            &mut tokens,
            &mut index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![
                TokenType::OPEN_PAR,
                TokenType::COMMA,
                TokenType::CLOSE_PAR,
                TokenType::COMMA,
                TokenType::OPEN_PAR,
                TokenType::COMMA,
                TokenType::CLOSE_PAR,
            ],
            final_tokens
                .iter()
                .map(|t| t.r#type)
                .collect::<Vec<TokenType>>()
        );
        Ok(())
    }
    #[test]
    fn expand_macro_recursive() -> Result<(), String> {
        let src = r##"#define HEHE(a,b) HEHE(a, b)
HEHE(HEHE(1,2),HEHE(3,4))"##
            .as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = ByteVecMaps::new();
        let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let mut index = 0;
        define_directive(&mut tokens, &mut index, &mut defines, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        expand_macro(
            &mut tokens,
            &mut index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![
                TokenType::IDENT {
                    str_map_key: str_maps.add_byte_vec("HEHE".as_bytes())
                },
                TokenType::OPEN_PAR,
                TokenType::IDENT {
                    str_map_key: str_maps.add_byte_vec("HEHE".as_bytes())
                },
                TokenType::OPEN_PAR,
                TokenType::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("1".as_bytes()),
                    suffix: None,
                },
                TokenType::COMMA,
                TokenType::WHITESPACE,
                TokenType::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("2".as_bytes()),
                    suffix: None,
                },
                TokenType::CLOSE_PAR,
                TokenType::COMMA,
                TokenType::WHITESPACE,
                TokenType::IDENT {
                    str_map_key: str_maps.add_byte_vec("HEHE".as_bytes())
                },
                TokenType::OPEN_PAR,
                TokenType::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("3".as_bytes()),
                    suffix: None,
                },
                TokenType::COMMA,
                TokenType::WHITESPACE,
                TokenType::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("4".as_bytes()),
                    suffix: None,
                },
                TokenType::CLOSE_PAR,
                TokenType::CLOSE_PAR,
            ],
            final_tokens
                .iter()
                .map(|t| t.r#type)
                .collect::<Vec<TokenType>>()
        );
        Ok(())
    }
    #[test]
    fn expand_macro_test_testing_fn_macro_expansion_nested() -> Result<(), String> {
        let src = r##"#define HEHE(a,b) a b
HEHE(HEHE(1,2),HEHE(3,4))"##
            .as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = ByteVecMaps::new();
        let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let mut index = 0;
        define_directive(&mut tokens, &mut index, &mut defines, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        expand_macro(
            &mut tokens,
            &mut index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![
                TokenType::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("1".as_bytes()),
                    suffix: None,
                },
                TokenType::WHITESPACE,
                TokenType::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("2".as_bytes()),
                    suffix: None,
                },
                TokenType::WHITESPACE,
                TokenType::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("3".as_bytes()),
                    suffix: None,
                },
                TokenType::WHITESPACE,
                TokenType::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("4".as_bytes()),
                    suffix: None,
                },
            ],
            final_tokens
                .iter()
                .map(|t| t.r#type)
                .collect::<Vec<TokenType>>()
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
        let mut str_maps = ByteVecMaps::new();
        let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let mut index = 0;
        define_directive(&mut tokens, &mut index, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens, &mut index, &mut defines, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        expand_macro(
            &mut tokens,
            &mut index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![
                TokenType::COMMA,
                TokenType::WHITESPACE,
                TokenType::PLUS,
                TokenType::WHITESPACE,
                TokenType::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("4".as_bytes()),
                    suffix: None,
                },
            ],
            final_tokens
                .iter()
                .map(|t| t.r#type)
                .collect::<Vec<TokenType>>()
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
        let mut str_maps = ByteVecMaps::new();
        let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let mut index = 0;
        define_directive(&mut tokens, &mut index, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens, &mut index, &mut defines, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        expand_macro(
            &mut tokens,
            &mut index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![
                TokenType::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("2".as_bytes()),
                    suffix: None,
                },
                TokenType::ASTERISK,
                TokenType::IDENT {
                    str_map_key: str_maps.add_byte_vec("f".as_bytes())
                },
                TokenType::OPEN_PAR,
                TokenType::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("9".as_bytes()),
                    suffix: None,
                },
                TokenType::CLOSE_PAR,
            ],
            final_tokens
                .iter()
                .map(|t| t.r#type)
                .collect::<Vec<TokenType>>()
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
        let mut str_maps = ByteVecMaps::new();
        let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let mut index = 0;
        define_directive(&mut tokens, &mut index, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens, &mut index, &mut defines, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        expand_macro(
            &mut tokens,
            &mut index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![
                TokenType::IDENT {
                    str_map_key: str_maps.add_byte_vec("printf".as_bytes())
                },
                TokenType::OPEN_PAR,
                TokenType::StringLiteral {
                    str_lit: StringLiteral {
                        prefix_key: None,
                        sequence_key: str_maps.add_byte_vec("FOO".as_bytes())
                    },
                },
                TokenType::WHITESPACE,
                TokenType::StringLiteral {
                    str_lit: StringLiteral {
                        prefix_key: None,
                        sequence_key: str_maps.add_byte_vec(" BAR".as_bytes())
                    },
                },
                TokenType::CLOSE_PAR,
            ],
            final_tokens
                .iter()
                .map(|t| t.r#type)
                .collect::<Vec<TokenType>>()
        );
        Ok(())
    }
    #[test]
    fn __va_args___test() -> Result<(), String> {
        let src = r##"#define CHICKEN(...) __VA_ARGS__
CHICKEN(1 2,3 4)"##
            .as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = ByteVecMaps::new();
        let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let mut index = 0;
        define_directive(&mut tokens, &mut index, &mut defines, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        expand_macro(
            &mut tokens,
            &mut index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![
                TokenType::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("1".as_bytes()),
                    suffix: None,
                },
                TokenType::WHITESPACE,
                TokenType::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("2".as_bytes()),
                    suffix: None,
                },
                TokenType::COMMA,
                TokenType::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("3".as_bytes()),
                    suffix: None,
                },
                TokenType::WHITESPACE,
                TokenType::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("4".as_bytes()),
                    suffix: None,
                },
            ],
            final_tokens
                .iter()
                .map(|t| t.r#type)
                .collect::<Vec<TokenType>>()
        );
        Ok(())
    }
    #[test]
    fn expand_macro_side_by_side() -> Result<(), String> {
        let src = r##"#define PP(a, b) a ## b
#define PP2(a, b) a/**/b
PP(/,*)PP2(*,/)"##
            .as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let src = process_comments(src)?;
        let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let mut defines = HashMap::new();
        let mut index = 0;
        define_directive(&mut tokens, &mut index, &mut defines, &mut str_maps)?;
        define_directive(&mut tokens, &mut index, &mut defines, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        expand_macro(
            &tokens,
            &mut index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        expand_macro(
            &tokens,
            &mut index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![
                TokenType::DIV,
                TokenType::ASTERISK,
                TokenType::ASTERISK,
                TokenType::WHITESPACE,
                TokenType::DIV,
            ],
            final_tokens
                .iter()
                .map(|t| t.r#type)
                .collect::<Vec<TokenType>>()
        );
        Ok(())
    }
    #[test]
    fn eval_expression_test_defined() -> Result<(), String> {
        let src = r##"defined(HI)"##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = ByteVecMaps::new();
        let mut final_tokens = Vec::new();
        let tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        parse_defined_in_if_directive(
            tokens.as_slice(),
            0,
            &mut final_tokens,
            &defines,
            &mut str_maps,
        )?;
        let res = expressions::eval_constant_expression_integer_when_preprocess(
            &final_tokens,
            &mut 0,
            &mut str_maps,
        )?;
        assert_eq!(res != 0, false, "failed 1");
        let src = r##"defined HI "##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = ByteVecMaps::new();
        let mut final_tokens = Vec::new();
        let tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        parse_defined_in_if_directive(
            tokens.as_slice(),
            0,
            &mut final_tokens,
            &defines,
            &mut str_maps,
        )?;
        let res = expressions::eval_constant_expression_integer_when_preprocess(
            &final_tokens,
            &mut 0,
            &mut str_maps,
        )?;
        assert_eq!(res != 0, false, "failed 2");
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
            let mut str_maps = ByteVecMaps::new();
            let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
            if_directive(&mut tokens, &mut 0, &defines, &mut str_maps)?;

            assert_eq!(
                vec![
                    TokenType::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("4".as_bytes()),
                        suffix: None,
                    },
                    TokenType::NEWLINE,
                ],
                tokens[0..2]
                    .iter()
                    .map(|t| t.r#type)
                    .collect::<Vec<TokenType>>(),
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
            let mut str_maps = ByteVecMaps::new();
            let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
            if_directive(&mut tokens, &mut 0, &defines, &mut str_maps)?;
            assert_eq!(
                tokens
                    .iter()
                    .map(|t| t.r#type)
                    .filter(|t| !matches!(
                        t,
                        TokenType::WHITESPACE { .. } | TokenType::NEWLINE { .. }
                    ))
                    .count(),
                0,
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
            let mut str_maps = ByteVecMaps::new();
            let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
            if_directive(&mut tokens, &mut 0, &defines, &mut str_maps)?;

            assert_eq!(
                vec![
                    TokenType::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("4".as_bytes()),
                        suffix: None,
                    },
                    TokenType::NEWLINE,
                ],
                tokens[0..2]
                    .iter()
                    .map(|t| t.r#type)
                    .collect::<Vec<TokenType>>(),
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
            let mut str_maps = ByteVecMaps::new();
            let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
            if_directive(&mut tokens, &mut 0, &defines, &mut str_maps)?;
            assert_eq!(
                tokens
                    .iter()
                    .map(|t| t.r#type)
                    .filter(|t| !matches!(
                        t,
                        TokenType::WHITESPACE { .. } | TokenType::NEWLINE { .. }
                    ))
                    .count(),
                0,
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
            let mut str_maps = ByteVecMaps::new();
            let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
            if_directive(&mut tokens, &mut 0, &defines, &mut str_maps)?;

            assert_eq!(
                vec![
                    TokenType::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("4".as_bytes()),
                        suffix: None,
                    },
                    TokenType::NEWLINE,
                ],
                tokens[0..2]
                    .iter()
                    .map(|t| t.r#type)
                    .collect::<Vec<TokenType>>(),
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
            let mut str_maps = ByteVecMaps::new();
            let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
            if_directive(&mut tokens, &mut 0, &defines, &mut str_maps)?;
            assert_eq!(
                vec![
                    TokenType::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("5".as_bytes()),
                        suffix: None,
                    },
                    TokenType::NEWLINE,
                ],
                tokens[0..2]
                    .iter()
                    .map(|t| t.r#type)
                    .collect::<Vec<TokenType>>(),
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
            let mut str_maps = ByteVecMaps::new();
            let tokens = cpp(
                src.to_vec(),
                "",
                &["./test_c_files"],
                &mut defines,
                &mut str_maps,
            )?;
            assert_eq!(
                vec![
                    TokenType::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("5".as_bytes()),
                        suffix: None,
                    },
                    TokenType::NEWLINE,
                ],
                tokens[0..2]
                    .iter()
                    .map(|t| t.r#type)
                    .collect::<Vec<TokenType>>(),
                "failed 7"
            );
        }
        {
            let src = r##"#define add(a,b) a + b
#if add(4,4) < '8'
4
#elif add(1,4) > '0'
5
#endif
        "##
            .as_bytes();
            let mut defines = HashMap::new();
            let mut str_maps = ByteVecMaps::new();
            let tokens = cpp(
                src.to_vec(),
                "",
                &["./test_c_files"],
                &mut defines,
                &mut str_maps,
            )?;
            assert_eq!(
                vec![
                    TokenType::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("4".as_bytes()),
                        suffix: None,
                    },
                    TokenType::NEWLINE,
                ],
                tokens[0..2]
                    .iter()
                    .map(|t| t.r#type)
                    .collect::<Vec<TokenType>>(),
                "failed 8"
            );
        }
        {
            let src = r##"#if HI && 1
4
#else
5
#endif
"##
            .as_bytes();
            let mut defines = HashMap::new();
            let mut str_maps = ByteVecMaps::new();
            let tokens = cpp(
                src.to_vec(),
                "",
                &["./test_c_files"],
                &mut defines,
                &mut str_maps,
            )?;
            assert_eq!(
                vec![
                    TokenType::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("5".as_bytes()),
                        suffix: None,
                    },
                    TokenType::NEWLINE,
                ],
                tokens[0..2]
                    .iter()
                    .map(|t| t.r#type)
                    .collect::<Vec<TokenType>>(),
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
            let mut str_maps = ByteVecMaps::new();
            let tokens = cpp(
                src.to_vec(),
                "",
                &["./test_c_files"],
                &mut defines,
                &mut str_maps,
            )?;
            assert_eq!(
                vec![
                    TokenType::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("4".as_bytes()),
                        suffix: None,
                    },
                    TokenType::NEWLINE,
                ],
                tokens[0..2]
                    .iter()
                    .map(|t| t.r#type)
                    .collect::<Vec<TokenType>>(),
                "failed 10"
            );
        }
        Ok(())
    }
    #[test]
    fn cpp_test_start_of_rescanning() -> Result<(), String> {
        let src = r##"#define COMMA() ,
#define PP ()
COMMA PP"##;
        let mut defines = HashMap::new();
        let mut str_maps = ByteVecMaps::new();
        let tokens = cpp(
            src.as_bytes().to_vec(),
            "",
            &[""],
            &mut defines,
            &mut str_maps,
        )?;
        assert_eq!(
            vec![
                TokenType::IDENT {
                    str_map_key: str_maps.add_byte_vec("COMMA".as_bytes())
                },
                TokenType::WHITESPACE,
                TokenType::OPEN_PAR,
                TokenType::CLOSE_PAR,
            ],
            tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>(),
        );
        Ok(())
    }
    #[test]
    fn cpp_test_argument_preprocessing_token_replacement_order() -> Result<(), String> {
        let src = r##"#define COMMA() ,
#define PP ()
#define GET_SECOND_(a,b,...)(b)
#define GET_SECOND(a,...)GET_SECOND_(a,__VA_ARGS__,)
GET_SECOND(COMMA PP,T)"##;
        let mut defines = HashMap::new();
        let mut str_maps = ByteVecMaps::new();
        let tokens = cpp(
            src.as_bytes().to_vec(),
            "",
            &[""],
            &mut defines,
            &mut str_maps,
        )?;
        assert_eq!(
            vec![TokenType::OPEN_PAR, TokenType::CLOSE_PAR],
            tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>()
        );
        Ok(())
    }
    #[test]
    fn cpp_test_proper_rescanning() -> Result<(), String> {
        let src = r##"#define f(x) h(x
#define h(x) x()
f(f))"##;
        //f(f))
        //h(f)
        //f()
        let mut defines = HashMap::new();
        let mut str_maps = ByteVecMaps::new();
        let tokens = cpp(
            src.as_bytes().to_vec(),
            "",
            &[""],
            &mut defines,
            &mut str_maps,
        )?;
        assert_eq!(
            vec![
                TokenType::IDENT {
                    str_map_key: str_maps.add_byte_vec("f".as_bytes())
                },
                TokenType::OPEN_PAR,
                TokenType::CLOSE_PAR
            ],
            tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>(),
        );
        Ok(())
    }
    #[test]
    fn cpp_testing_recursive_expansion() -> Result<(), String> {
        let src = r##"#define PP_NAIVE_RECURSIVE(x) ((x), PP_NAIVE_RECURSIVE(x))
PP_NAIVE_RECURSIVE(5) /* ((5), PP_NAIVE_RECURSIVE(5)) */
#define PP_EXPAND(...) __VA_ARGS__
PP_EXPAND(PP_NAIVE_RECURSIVE(5)) /* ((5), PP_NAIVE_RECURSIVE(5)) */"##;
        todo!("need to do this test");
        Ok(())
    }
}
