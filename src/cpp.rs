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
                //expand_macro(
                //    tokens,
                //    &mut expand_macro_index,
                //    defines,
                //    str_maps,
                //    &mut include_tokens,
                //)?;
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

fn parse_identifier_list(
    tokens: &[Token],
    index: &mut usize,
    str_maps: &mut ByteVecMaps,
) -> Result<Vec<usize>, String> {
    let mut identifier_keys = Vec::new();
    loop {
        consume_specifically_spaces(tokens, index);
        if !matches!(
            tokens.get(*index),
            Some(Token {
                r#type: TokenType::ELLIPSIS,
                ..
            })
        ) {
            expected_identifier_and_consume(tokens, str_maps, index)?;
            let Some(Token {
                r#type: TokenType::IDENT { str_map_key },
                location: Some(Location { line, column }),
            }) = tokens.get(*index - 1)
            else {
                unreachable!()
            };
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
        }
        consume_specifically_spaces(tokens, index);
        if matches!(
            tokens.get(*index),
            Some(Token {
                r#type: TokenType::ELLIPSIS,
                ..
            })
        ) {
            identifier_keys.push(str_maps.add_byte_vec(b"__VA_ARGS__"));
            break Ok(identifier_keys);
        }
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
        consume_specifically_spaces(tokens, index);
    }
}

fn apply_HASH_constraint(
    replacement_list: &[Token],
    parameters: &[usize],
    str_maps: &ByteVecMaps,
) -> Result<(), String> {
    for (i, t) in replacement_list.iter().enumerate() {
        if t.r#type == TokenType::HASH {
            let mut j = i;
            consume_specifically_spaces(&replacement_list, &mut j);
            if let Some(Token {
                r#type: TokenType::IDENT { str_map_key },
                location: Some(Location { line, column }),
            }) = replacement_list.get(j)
            {
                if !parameters.contains(str_map_key) {
                    return Err(error("Each '#' pp token in replacement list in fn like macro shall be followed by parameter", *line, *column));
                }
            } else {
                let loc = t.location.expect("Location of token that isn't identifier");
                return Err(error("Each '#' pp token in replacement list in fn like macro shall be followed by parameter", loc.line, loc.column));
            }
        }
    }
    Ok(())
}

fn define_directive(
    tokens: &mut [Token],
    index: &mut usize,
    defines: &mut HashMap<usize, Define>,
    str_maps: &mut ByteVecMaps,
    start_of_directive: usize,
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
        if let Some(Token {
            location: Some(Location { line, column }),
            ..
        }) = tokens.get(*index)
        {
            return Err(error("Expected identifier for macro name", *line, *column));
        }
        return Err("Expected identifier for macro name".to_string());
    };
    *index += 1;
    let position_of_newline = get_newline_location(tokens, index);
    // There shall be white space between the identifier and the replacement list in the definition of an object-like macro.
    if let Some(Token {
        r#type: TokenType::OPEN_PAR,
        ..
    }) = tokens.get(*index)
    {
        *index += 1;
        consume_specifically_spaces(tokens, index);
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
    if let Some(vec) = &def_data.parameters {
        // Each # preprocessing token in the replacement list for a function-like macro
        // shall be followed by a parameter as the next preprocessing token in the
        // replacement list.
        apply_HASH_constraint(&tokens[*index..position_of_newline], vec, str_maps);
    }
    def_data.replacement_list.extend_from_slice(
        &tokens[*index..position_of_newline]
            .iter()
            .map(|t| Token {
                r#type: t.r#type,
                location: None,
            })
            .collect::<Vec<Token>>(),
    );
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
    remove_directive(tokens, start_of_directive, position_of_newline);
    return Ok(());
}
fn error_directive(_tokens: &mut Vec<TokenType>) {
    todo!()
}
fn line_directive(_tokens: &mut Vec<TokenType>, _index: usize, _end: usize) -> Result<(), String> {
    todo!()
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

fn parse_macro_fn_arguments<'a>(
    tokens: &'a [Token],
    idx: &mut usize,
    parameters: &[usize],
    str_maps: &ByteVecMaps,
) -> Result<Vec<&'a [Token]>, String> {
    let start_of_fn_macro = *idx;
    let mut stack = Vec::new();
    let mut args = Vec::new();
    let mut start_of_arg = *idx;
    while *idx < tokens.len() {
        match tokens.get(*idx) {
            Some(Token {
                r#type: TokenType::OPEN_PAR,
                ..
            }) => {
                stack.push(TokenType::OPEN_PAR);
            }
            Some(Token {
                r#type: TokenType::CLOSE_PAR,
                ..
            }) => {
                if let Some(TokenType::OPEN_PAR) = stack.last() {
                    stack.pop();
                }
                if stack.len() == 0 {
                    args.push(&tokens[start_of_arg..*idx]);
                    break;
                }
            }
            Some(Token {
                r#type: TokenType::COMMA,
                ..
            }) => {
                if stack.len() == 0 {
                    args.push(&tokens[start_of_arg..*idx]);
                    *idx += 1;
                    consume_specifically_spaces(tokens, idx);
                    start_of_arg = *idx;
                }
            }
            Some(Token { .. }) => {}
            None => unreachable!(),
        }
        *idx += 1;
    }
    if args.len() < parameters.len() {
        let Token {
            location: Some(Location { line, column }),
            ..
        } = tokens[start_of_fn_macro]
        else {
            unreachable!()
        };
        return Err(error(
            "Too few arguments passed into function-like macro",
            line,
            column,
        ));
    }
    if args.len() > parameters.len()
        && !parameters
            .iter()
            .any(|key| str_maps.key_to_byte_vec[*key] == b"__VA_ARGS__")
    {
        let Token {
            location: Some(Location { line, column }),
            ..
        } = tokens[start_of_fn_macro]
        else {
            unreachable!()
        };
        return Err(error(
            "Too many arguments passed into function-like macro",
            line,
            column,
        ));
    }
    if args.len() > parameters.len() && parameters.len() > 0 {
        let mut va_args = Vec::new();
        while args.len() >= parameters.len() {
            va_args.push(args.remove(parameters.len() - 1));
        }
        args.extend(&va_args);
    }
    Ok(args)
}

fn replace_parameter(
    args: &[&[Token]],
    parameters: &[usize],
    processed_replacement_list: &mut Vec<Token>,
    parameter: Token,
    str_maps: &mut ByteVecMaps,
    defines: &HashMap<usize, Define>,
    macros_being_replaced: &mut Vec<usize>,
) -> Result<(), String> {
    let Token {
        r#type: TokenType::IDENT { str_map_key },
        ..
    } = parameter
    else {
        unreachable!()
    };
    let Some(idx) = parameters.iter().position(|key| *key == str_map_key) else {
        unreachable!()
    };
    let mut arg = args[idx].to_vec();
    let len = arg.len();
    macro_substitutions(
        &mut arg,
        0,
        len,
        str_maps,
        defines,
        &mut Vec::new(),
        macros_being_replaced,
        true,
    )?;
    for t in arg {
        processed_replacement_list.push(t);
    }
    Ok(())
}

fn argument_substitution_non_operands(
    args: &[&[Token]],
    parameters: &[usize],
    replacement_list: &[Token],
    processed_replacement_list: &mut Vec<Token>,
    i: usize,
    str_maps: &mut ByteVecMaps,
    defines: &HashMap<usize, Define>,
    macros_being_replaced: &mut Vec<usize>,
) -> Result<(), String> {
    let prev_token = replacement_list[0..i]
        .iter()
        .rev()
        .find(|t| t.r#type != TokenType::WHITESPACE);
    let next_token = replacement_list[i..]
        .iter()
        .find(|t| t.r#type != TokenType::WHITESPACE);
    if !matches!(
        prev_token,
        Some(Token {
            r#type: TokenType::HASH | TokenType::HASH_HASH,
            ..
        })
    ) && !matches!(
        next_token,
        Some(Token {
            r#type: TokenType::HASH_HASH,
            ..
        })
    ) {
        replace_parameter(
            args,
            parameters,
            processed_replacement_list,
            replacement_list[i],
            str_maps,
            defines,
            macros_being_replaced,
        )?;
    }
    Ok(())
}

fn process_replacement_list(
    args: &[&[Token]],
    replacement_list: &[Token],
    parameters: Option<&[usize]>,
    str_maps: &mut ByteVecMaps,
    defines: &HashMap<usize, Define>,
    macros_being_replaced: &mut Vec<usize>,
) -> Result<Vec<Token>, String> {
    // The reason a separate `processed_replacement_list` is used, is
    // so that `#` and `##` preprocessing tokens do not interfere with the
    // actual `#` and `##` preprocessing tokens in the original replacement list.
    let mut processed_replacement_list = Vec::new();
    let mut i = 0;
    let has_parameters = parameters.is_some();
    let parameters = if let Some(parameters) = parameters {
        parameters
    } else {
        &[]
    };
    while i < replacement_list.len() {
        let t = replacement_list[i];
        match t.r#type {
            TokenType::IDENT { str_map_key } if parameters.contains(&str_map_key) => {
                argument_substitution_non_operands(
                    args,
                    parameters,
                    replacement_list,
                    &mut processed_replacement_list,
                    i,
                    str_maps,
                    defines,
                    macros_being_replaced,
                )?;
                i += 1;
            }
            TokenType::HASH if has_parameters => {
                process_hash_operator(
                    args,
                    replacement_list,
                    &mut processed_replacement_list,
                    parameters,
                    str_maps,
                    &mut i,
                );
            }
            TokenType::HASH_HASH => {
                while let Some(Token {
                    r#type: TokenType::WHITESPACE,
                    ..
                }) = processed_replacement_list.last()
                {
                    processed_replacement_list.pop();
                }
                processed_replacement_list.pop();
                process_hash_hash_operator(
                    args,
                    replacement_list,
                    &mut processed_replacement_list,
                    parameters,
                    str_maps,
                    i,
                )?;
                i += 1;
                consume_specifically_spaces(replacement_list, &mut i);
                i += 1;
            }
            _ => {
                processed_replacement_list.push(t);
                i += 1;
            }
        }
    }
    let len = processed_replacement_list.len();
    macro_substitutions(
        &mut processed_replacement_list,
        0,
        len,
        str_maps,
        defines,
        &mut Vec::new(),
        macros_being_replaced,
        true,
    )?;
    Ok(processed_replacement_list)
}

fn process_hash_operator(
    args: &[&[Token]],
    replacement_list: &[Token],
    processed_replacement_list: &mut Vec<Token>,
    parameters: &[usize],
    str_maps: &mut ByteVecMaps,
    i: &mut usize,
) {
    *i += 1;
    consume_whitespace(replacement_list, i);
    let Token {
        r#type: TokenType::IDENT { str_map_key },
        ..
    } = replacement_list[*i].clone()
    else {
        unreachable!()
    };
    let Some(idx) = parameters.iter().position(|key| *key == str_map_key) else {
        unreachable!()
    };
    let arg = args[idx];
    let mut byte_vec = arg
        .iter()
        .skip_while(|t| t.r#type == TokenType::WHITESPACE)
        .fold(Vec::new(), |mut acc, t| {
            let Some(byte_vec) = t.to_byte_vec(str_maps) else {
                unreachable!()
            };
            acc.extend(&byte_vec);
            acc
        });
    while let Some(b' ') = byte_vec.last() {
        byte_vec.pop();
    }
    let sequence_key = str_maps.add_byte_vec(&byte_vec);
    processed_replacement_list.push(Token {
        r#type: TokenType::StringLiteral {
            str_lit: StringLiteral {
                prefix_key: None,
                sequence_key,
            },
        },
        location: None,
    });
}

fn get_parameter_args<'a, 'b>(
    t: Token,
    parameters: &'b [usize],
    args: &'a [&'a [Token]],
) -> Option<&'a [Token]> {
    if let Token {
        r#type: TokenType::IDENT { str_map_key },
        ..
    } = t
    {
        if let Some(idx) = parameters.iter().position(|key| *key == str_map_key) {
            return Some(args[idx]);
        }
    }
    None
}

fn process_hash_hash_operator(
    args: &[&[Token]],
    replacement_list: &[Token],
    processed_replacement_list: &mut Vec<Token>,
    parameters: &[usize],
    str_maps: &mut ByteVecMaps,
    i: usize,
) -> Result<(), String> {
    let Some(prev_token) = replacement_list[0..i]
        .iter()
        .rev()
        .find(|t| t.r#type != TokenType::WHITESPACE)
    else {
        unreachable!()
    };
    let Some(next_token) = replacement_list[i + 1..]
        .iter()
        .find(|t| t.r#type != TokenType::WHITESPACE)
    else {
        unreachable!()
    };
    let prev_token = if let Some(arg) = get_parameter_args(*prev_token, parameters, args) {
        processed_replacement_list.extend(&arg[0..arg.len() - 1]);
        if arg.len() == 0 {
            Token {
                r#type: TokenType::PLACEMARKER,
                location: None,
            }
        } else {
            *arg.last().expect("Last token in the argument")
        }
    } else {
        *prev_token
    };
    let next_token = if let Some(arg) = get_parameter_args(*next_token, parameters, args) {
        processed_replacement_list.extend(&arg[0..arg.len() - 1]);
        if arg.len() == 0 {
            Token {
                r#type: TokenType::PLACEMARKER,
                location: None,
            }
        } else {
            *arg.last().expect("Last token in the argument")
        }
    } else {
        *next_token
    };
    if prev_token.r#type != TokenType::PLACEMARKER && next_token.r#type != TokenType::PLACEMARKER {
        let Some(mut left_byte_vec) = prev_token.to_byte_vec(str_maps) else {
            unreachable!()
        };
        let Some(right_byte_vec) = next_token.to_byte_vec(str_maps) else {
            unreachable!()
        };
        left_byte_vec.extend(&right_byte_vec);
        let tokens = lexer(&left_byte_vec, true, str_maps)?;
        if tokens.len() > 1 {
            return Err("Not valid preprocessing token".to_string());
        }
        if matches!(tokens[0].r#type, TokenType::IDENT { .. }) {
            processed_replacement_list.push(tokens[0]);
        } else {
            // Doing this so that in the case of `# ## #`, the resulting
            // token isn't seen as a ## operator when rescanning.
            processed_replacement_list.push(prev_token);
            processed_replacement_list.push(next_token);
        }
    } else if prev_token.r#type != next_token.r#type {
        let non_placemarker_token = if prev_token.r#type == TokenType::PLACEMARKER {
            next_token
        } else {
            prev_token
        };
        processed_replacement_list.push(non_placemarker_token);
    } else {
        processed_replacement_list.push(Token {
            r#type: TokenType::PLACEMARKER,
            location: None,
        });
    }
    Ok(())
}

fn macro_substitutions(
    tokens: &mut Vec<Token>,
    start_index: usize,
    end_index: usize,
    str_maps: &mut ByteVecMaps,
    defines: &HashMap<usize, Define>,
    replacements: &mut Vec<Vec<Token>>,
    macros_being_replaced: &mut Vec<usize>,
    rescanning: bool,
) -> Result<(), String> {
    let mut i = start_index;
    loop {
        let break_condition = if rescanning {
            i >= tokens.len()
        } else {
            i >= end_index
        };
        if break_condition {
            break;
        }
        match tokens[i] {
            Token {
                r#type: TokenType::IDENT { str_map_key },
                ..
            } if defines.contains_key(&str_map_key)
                && !macros_being_replaced.contains(&str_map_key) =>
            {
                let start_of_macro = i;
                let Some(define) = defines.get(&str_map_key) else {
                    unreachable!()
                };
                let replacement_list = define.replacement_list.clone();
                if let Some(parameters) = &define.parameters {
                    i += 1;
                    consume_whitespace(tokens, &mut i);
                    if let Some(Token {
                        r#type: TokenType::OPEN_PAR,
                        ..
                    }) = tokens.get(i)
                    {
                        // TODO: make sure each new-line character is considered to be a normal white-space character.
                        i += 1;
                        let args = parse_macro_fn_arguments(tokens, &mut i, parameters, str_maps)?;
                        expected_token(tokens, &mut i, TokenType::CLOSE_PAR, "Expected ')'")?;
                        macros_being_replaced.push(str_map_key);
                        let processed_replacement_list = process_replacement_list(
                            &args,
                            &replacement_list,
                            Some(parameters),
                            str_maps,
                            defines,
                            macros_being_replaced,
                        )?;
                        macros_being_replaced.pop();
                        if rescanning {
                            let length = processed_replacement_list.len();
                            tokens.splice(start_of_macro..i, processed_replacement_list);
                            i = start_of_macro + length;
                        } else {
                            replacements.push(processed_replacement_list);
                            for idx in start_of_macro..i {
                                let location = tokens[idx].location;
                                tokens[idx] = Token {
                                    r#type: TokenType::RCC_INTERNAL_MACRO_REPLACEMENT {
                                        idx_of_macro_replacement: replacements.len() - 1,
                                    },
                                    location,
                                };
                            }
                        }
                    }
                } else {
                    let processed_replacement_list = process_replacement_list(
                        &[],
                        &replacement_list,
                        None,
                        str_maps,
                        defines,
                        macros_being_replaced,
                    )?;
                    if rescanning {
                        let length = processed_replacement_list.len();
                        tokens.splice(i..i + 1, processed_replacement_list);
                        i = start_of_macro + length;
                    } else {
                        replacements.push(processed_replacement_list);
                        let location = tokens[i].location;
                        tokens[i] = Token {
                            r#type: TokenType::RCC_INTERNAL_MACRO_REPLACEMENT {
                                idx_of_macro_replacement: replacements.len() - 1,
                            },
                            location,
                        };
                    }
                }
            }
            _ => {}
        }
        i += 1;
    }
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
                // There's no need to check for an identifier token that matches one of the directive names
                // because of the existence of `non-directives` that are actually directives but have undefined behavior.
                // I handle directives outside of this function
                let newline_comes_after = check_valid_directive(tokens, *index);
                if newline_comes_after {
                    return true;
                }
            }
            Some(Token { .. }) => {
                preceded_only_by_whitespace_or_nothing_or_newline = false;
            }
            None => unreachable!(),
        }
        *index += 1;
    }
    false
}

fn parse_undef_line(
    tokens: &mut [Token],
    index: &mut usize,
    str_maps: &ByteVecMaps,
    defines: &mut HashMap<usize, Define>,
    start_of_directive: usize,
) -> Result<(), String> {
    let newline_location = get_newline_location(tokens, index);
    expected_identifier_and_consume(tokens, str_maps, index)?;
    let Token {
        r#type: TokenType::IDENT { str_map_key },
        ..
    } = tokens[*index - 1]
    else {
        unreachable!()
    };
    defines.remove(&str_map_key);
    remove_directive(tokens, start_of_directive, newline_location);
    Ok(())
}

fn parse_control_line(
    tokens: &mut [Token],
    index: &mut usize,
    curr_path: &str,
    include_paths: &[&str],
    defines: &mut HashMap<usize, Define>,
    str_maps: &mut ByteVecMaps,
    directive: &[u8],
    start_of_directive: usize,
) -> Result<(), String> {
    match directive {
        b"include" => {
            include_directive(tokens, index, curr_path, include_paths, defines, str_maps)?;
        }
        b"define" => {
            define_directive(tokens, index, defines, str_maps, start_of_directive)?;
        }
        b"error" => todo!(),
        b"line" => todo!(),
        b"pragma" => todo!(),
        b"undef" => {
            parse_undef_line(tokens, index, str_maps, defines, start_of_directive)?;
        }
        _ => {
            unreachable!()
        }
    }
    Ok(())
}
fn parse_endif_line(
    tokens: &mut [Token],
    index: &mut usize,
    str_maps: &ByteVecMaps,
    location_of_if_directive: Location,
) -> Result<(), String> {
    let found = look_for_next_preprocessing_directive(tokens, index);
    if found {
        let start_of_directive = *index;
        *index += 1;
        consume_specifically_spaces(tokens, index);
        let Token {
            r#type: TokenType::IDENT { str_map_key },
            ..
        } = tokens[*index]
        else {
            unreachable!()
        };
        if *str_maps.key_to_byte_vec[str_map_key] == *b"endif" {
            *index += 1;
            let newline_location = get_newline_location(tokens, index);
            remove_directive(tokens, start_of_directive, newline_location);
            return Ok(());
        } else {
            *index = start_of_directive;
        }
    }
    let Location { line, column } = location_of_if_directive;
    Err(error(
        "Expected 'endif' for corresponding 'if' directive",
        line,
        column,
    ))
}
fn parse_else_group(
    tokens: &mut Vec<Token>,
    index: &mut usize,
    in_conditional_directive: bool,
    str_maps: &mut ByteVecMaps,
    curr_path: &str,
    include_paths: &[&str],
    defines: &mut HashMap<usize, Define>,
    previous_branch_true: bool,
    replacements: &mut Vec<Vec<Token>>,
) -> Result<(), String> {
    let found = look_for_next_preprocessing_directive(tokens, index);
    if found {
        let start_of_directive = *index;
        *index += 1;
        consume_specifically_spaces(tokens, index);
        if let Some(Token {
            r#type: TokenType::IDENT { str_map_key },
            ..
        }) = tokens.get(*index)
        {
            if str_maps.key_to_byte_vec[*str_map_key] == b"else" {
                *index += 1;
                let newline_location = get_newline_location(tokens, index);
                parse_group(
                    tokens,
                    index,
                    in_conditional_directive,
                    str_maps,
                    curr_path,
                    include_paths,
                    defines,
                    replacements,
                )?;
                remove_directive(tokens, start_of_directive, newline_location);
                if previous_branch_true {
                    remove_group(tokens, newline_location + 1, *index);
                }
            } else {
                *index = start_of_directive;
            }
        }
    }
    Ok(())
}
fn parse_elif_group(
    tokens: &mut Vec<Token>,
    index: &mut usize,
    in_conditional_directive: bool,
    str_maps: &mut ByteVecMaps,
    curr_path: &str,
    include_paths: &[&str],
    defines: &mut HashMap<usize, Define>,
    previous_branch_true: bool,
    start_of_directive: usize,
    replacements: &mut Vec<Vec<Token>>,
) -> Result<bool, String> {
    let newline_location = get_newline_location(tokens, index);
    let res = eval_constant_expression_integer_when_preprocess(
        &mut tokens[*index..newline_location],
        str_maps,
        defines,
    )?;
    let start = newline_location + 1;
    parse_group(
        tokens,
        index,
        in_conditional_directive,
        str_maps,
        curr_path,
        include_paths,
        defines,
        replacements,
    )?;
    remove_directive(tokens, start_of_directive, newline_location);
    if previous_branch_true || res == 0 {
        remove_group(tokens, start, *index);
    }
    Ok(res != 0)
}

fn parse_elif_groups(
    tokens: &mut Vec<Token>,
    index: &mut usize,
    in_conditional_directive: bool,
    str_maps: &mut ByteVecMaps,
    curr_path: &str,
    include_paths: &[&str],
    defines: &mut HashMap<usize, Define>,
    mut previous_branch_true: bool,
    replacements: &mut Vec<Vec<Token>>,
) -> Result<bool, String> {
    loop {
        let found = look_for_next_preprocessing_directive(tokens, index);
        if found {
            let start_of_directive = *index;
            *index += 1;
            consume_specifically_spaces(tokens, index);
            if let Some(Token {
                r#type: TokenType::IDENT { str_map_key },
                ..
            }) = tokens.get(*index)
            {
                let name = &str_maps.key_to_byte_vec[*str_map_key];
                if *name == *b"elif" {
                    *index += 1;
                    consume_specifically_spaces(tokens, index);
                    previous_branch_true = parse_elif_group(
                        tokens,
                        index,
                        in_conditional_directive,
                        str_maps,
                        curr_path,
                        include_paths,
                        defines,
                        previous_branch_true,
                        start_of_directive,
                        replacements,
                    )?;
                    continue;
                }
            }
            // Resetting to the beginning of the directive if it's not `elif`
            *index = start_of_directive;
        }
        break;
    }
    Ok(previous_branch_true)
}

fn remove_directive(tokens: &mut [Token], start: usize, end: usize) {
    tokens[start..end]
        .iter_mut()
        .for_each(|t| t.r#type = TokenType::WHITESPACE);
}

fn remove_group(tokens: &mut [Token], start: usize, end: usize) {
    tokens[start..end].iter_mut().for_each(|t| {
        if t.r#type != TokenType::NEWLINE {
            t.r#type = TokenType::WHITESPACE
        }
    });
}

// My current thoughts on handling conditional inclusion with preprocessing, is that I'll use recursion and recursive descent parsing in order
// to handle nested if sections correctly without having to track depth.
// With if sections handled correctly, I can process if, elif and else correctly as well
fn parse_if_group(
    tokens: &mut Vec<Token>,
    index: &mut usize,
    str_maps: &mut ByteVecMaps,
    if_directive_type: &[u8],
    curr_path: &str,
    include_paths: &[&str],
    defines: &mut HashMap<usize, Define>,
    start_of_directive: usize,
    replacements: &mut Vec<Vec<Token>>,
) -> Result<bool, String> {
    let newline_location = get_newline_location(tokens, index);
    let constant_expr_result = match if_directive_type {
        b"if" => {
            eval_constant_expression_integer_when_preprocess(
                &mut tokens[*index..newline_location],
                str_maps,
                defines,
            )? != 0
        }
        b"ifdef" | b"ifndef" => {
            let non_ident = tokens[*index..newline_location]
                .iter()
                .filter(|t| t.r#type != TokenType::WHITESPACE)
                .find(|t| !matches!(t.r#type, TokenType::IDENT { .. }));
            let ident = tokens[*index..newline_location]
                .iter()
                .filter(|t| t.r#type != TokenType::WHITESPACE)
                .find(|t| matches!(t.r#type, TokenType::IDENT { .. }));
            if non_ident.is_some() || ident.is_none() {
                let Some(Token {
                    location: Some(Location { line, column }),
                    ..
                }) = non_ident
                else {
                    unreachable!()
                };
                return Err(error(
                    "Only identifiers are allowed in 'ifdef'/'ifndef' directives",
                    *line,
                    *column,
                ));
            }
            let Some(Token {
                r#type: TokenType::IDENT { str_map_key },
                ..
            }) = ident
            else {
                unreachable!()
            };
            let contains_key = defines.contains_key(&str_map_key);
            if if_directive_type == b"ifdef" {
                contains_key
            } else {
                !contains_key
            }
        }
        _ => unreachable!(),
    };
    let start_of_group_idx = newline_location + 1;
    parse_group(
        tokens,
        index,
        true,
        str_maps,
        curr_path,
        include_paths,
        defines,
        replacements,
    )?;
    remove_directive(tokens, start_of_directive, newline_location);
    if !constant_expr_result {
        tokens[start_of_group_idx..*index].iter_mut().for_each(|t| {
            if t.r#type != TokenType::NEWLINE {
                t.r#type = TokenType::WHITESPACE
            }
        })
    } else {
    }
    Ok(constant_expr_result)
}

fn parse_if_section(
    tokens: &mut Vec<Token>,
    index: &mut usize,
    in_conditional_directive: bool,
    str_maps: &mut ByteVecMaps,
    if_directive_type: &[u8],
    curr_path: &str,
    include_paths: &[&str],
    defines: &mut HashMap<usize, Define>,
    location: Location,
    start_of_directive: usize,
    replacements: &mut Vec<Vec<Token>>,
) -> Result<(), String> {
    let constant_expr_result = parse_if_group(
        tokens,
        index,
        str_maps,
        if_directive_type,
        curr_path,
        include_paths,
        defines,
        start_of_directive,
        replacements,
    )?;
    let constant_expr_result = parse_elif_groups(
        tokens,
        index,
        in_conditional_directive,
        str_maps,
        curr_path,
        include_paths,
        defines,
        constant_expr_result,
        replacements,
    )?;
    parse_else_group(
        tokens,
        index,
        in_conditional_directive,
        str_maps,
        curr_path,
        include_paths,
        defines,
        constant_expr_result,
        replacements,
    )?;
    parse_endif_line(tokens, index, str_maps, location)?;
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

fn parse_group(
    tokens: &mut Vec<Token>,
    index: &mut usize,
    in_conditional_directive: bool,
    str_maps: &mut ByteVecMaps,
    curr_path: &str,
    include_paths: &[&str],
    defines: &mut HashMap<usize, Define>,
    replacements: &mut Vec<Vec<Token>>,
) -> Result<(), String> {
    // Looping here until I do not find any more preprocessing directives in group
    // and also I need to consume all of the pp tokens in group.
    // Only breaking out of the loop when I encounter something that indicates the
    // end of a group like `else`, `elif`.
    // I think this is only necessary for conditional preprocessing.
    loop {
        let start_of_group = *index;
        let found = look_for_next_preprocessing_directive(tokens, index);
        if found {
            let start_of_directive = *index;
            *index += 1;
            consume_specifically_spaces(tokens, index);
            if let Some(Token {
                r#type: TokenType::NEWLINE,
                ..
            }) = tokens.get(*index)
            {
                let newline_location = get_newline_location(tokens, index);
                remove_directive(tokens, start_of_directive, newline_location);
                return Ok(());
            }
            if !matches!(
                tokens.get(*index),
                Some(Token {
                    r#type: TokenType::IDENT { .. },
                    ..
                })
            ) {
                let newline_location = get_newline_location(tokens, index);
                remove_directive(tokens, start_of_directive, newline_location);
                return Ok(());
            }
            let Some(Token {
                r#type: TokenType::IDENT { str_map_key },
                location: Some(Location { line, column }),
            }) = tokens.get(*index)
            else {
                unreachable!()
            };
            let d = str_maps.key_to_byte_vec[*str_map_key].clone();
            match d.as_slice() {
                b"if" | b"ifdef" | b"ifndef" => {
                    *index += 1;
                    consume_specifically_spaces(tokens, index);
                    parse_if_section(
                        tokens,
                        index,
                        true,
                        str_maps,
                        &str_maps.key_to_byte_vec[*str_map_key].clone(),
                        curr_path,
                        include_paths,
                        defines,
                        Location {
                            line: *line,
                            column: *column,
                        },
                        start_of_directive,
                        replacements,
                    )?;
                }
                b"include" | b"define" | b"error" | b"line" | b"undef" | b"pragma" => {
                    *index += 1;
                    consume_specifically_spaces(tokens, index);
                    parse_control_line(
                        tokens,
                        index,
                        curr_path,
                        include_paths,
                        defines,
                        str_maps,
                        &d,
                        start_of_directive,
                    )?;
                }
                b"else" | b"elif" | b"endif" => {
                    if !in_conditional_directive {
                        return Err(error(
                            &format!(
                                "Unexpected '{}' directive",
                                String::from_utf8(d.to_vec()).unwrap()
                            ),
                            *line,
                            *column,
                        ));
                    }
                    // If there aren't any directives left in group,
                    // then reset the index variable back to the
                    // start ('#' token), so that `else`, `elif`, and `endif` can be caught by
                    // parse_if_section.
                    *index = start_of_directive;
                    break;
                }
                _ => {}
            }
            macro_substitutions(
                tokens,
                start_of_group,
                *index,
                str_maps,
                defines,
                replacements,
                &mut Vec::new(),
                false,
            )?;
        } else {
            macro_substitutions(
                tokens,
                start_of_group,
                *index,
                str_maps,
                defines,
                replacements,
                &mut Vec::new(),
                false,
            )?;
            break;
        }
    }
    Ok(())
}

fn replace_all_replacements(tokens: Vec<Token>, replacements: Vec<Vec<Token>>) -> Vec<Token> {
    let mut new_tokens = Vec::new();
    let mut i = 0;
    while i < tokens.len() {
        let t = tokens[i];
        match t.r#type {
            TokenType::RCC_INTERNAL_MACRO_REPLACEMENT {
                idx_of_macro_replacement,
            } => {
                new_tokens.extend(&replacements[idx_of_macro_replacement]);
                i += 1;
                while i < tokens.len()
                    && match tokens[i].r#type {
                        TokenType::RCC_INTERNAL_MACRO_REPLACEMENT {
                            idx_of_macro_replacement: idx2,
                        } => idx_of_macro_replacement == idx2,
                        _ => false,
                    }
                {
                    i += 1;
                }
            }
            _ => {
                new_tokens.push(t);
            }
        }
        i += 1;
    }
    new_tokens
}

fn preprocessing_directives(
    mut tokens: Vec<Token>,
    curr_path: &str,
    include_paths: &[&str],
    defines: &mut HashMap<usize, Define>,
    str_maps: &mut ByteVecMaps,
) -> Result<Vec<Token>, String> {
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
    let mut replacements = Vec::new();
    while index < tokens.len() {
        parse_group(
            &mut tokens,
            &mut index,
            false,
            str_maps,
            curr_path,
            include_paths,
            defines,
            &mut replacements,
        )?;
    }
    let new_tokens = replace_all_replacements(tokens, replacements);
    Ok(new_tokens)
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
    let lexed_tokens = lexer(&program_str, true, str_maps)?;
    // step 4 in the translation phase
    let preprocessed =
        preprocessing_directives(lexed_tokens, curr_path, include_paths, defines, str_maps)?;
    // concatenating adjacent string literals together
    let tokens = concat_adjacent_strings(preprocessed.as_slice(), str_maps)?;
    // TODO: go through tokens again and make sure location data is correct!
    Ok(tokens)
}

#[cfg(test)]
mod tests {

    use crate::lexer::*;
    use crate::parser::expressions::*;
    use crate::parser::*;
    use std::collections::HashMap;

    use super::{
        cpp, define_directive, parse_group, preprocessing_directives, process_comments, Define,
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
        let tokens = cpp(
            src.to_vec(),
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
        let tokens = cpp(src.to_vec(), "", &[], &mut defines, &mut str_maps)?;
        assert_eq!(
            vec![TokenType::StringLiteral {
                str_lit: StringLiteral {
                    prefix_key: None,
                    sequence_key: str_maps.add_byte_vec("5 5".as_bytes())
                },
            }],
            tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>()
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
        let mut defines = HashMap::new();
        let tokens = cpp(
            src.as_bytes().to_vec(),
            "",
            &[],
            &mut defines,
            &mut str_maps,
        )?;
        assert_eq!(
            vec![TokenType::StringLiteral {
                str_lit: StringLiteral {
                    prefix_key: None,
                    sequence_key: str_maps.add_byte_vec("x ## y".as_bytes())
                },
            }],
            tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>()
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
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::NEWLINE,
                TokenType::StringLiteral {
                    str_lit: StringLiteral {
                        prefix_key: None,
                        sequence_key: str_maps.add_byte_vec(b"hello world")
                    }
                },
                TokenType::NEWLINE
            ],
            tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>()
        );
        Ok(())
    }
    #[test]
    fn test_define_directive() -> Result<(), String> {
        let mut str_maps = ByteVecMaps::new();
        let src = r##"
#define hash_hash # ## #
#define mkstr(a) # a
#define in_between(a) mkstr(a)
#define join(c, d) in_between(c hash_hash d)

join(x, y)
"##;
        let mut defines = HashMap::new();
        let tokens = cpp(
            src.as_bytes().to_vec(),
            ".",
            &[],
            &mut defines,
            &mut str_maps,
        )?;
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
        assert_eq!(
            vec![
                TokenType::NEWLINE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::NEWLINE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::NEWLINE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::NEWLINE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::WHITESPACE,
                TokenType::NEWLINE,
                TokenType::NEWLINE,
                TokenType::StringLiteral {
                    str_lit: StringLiteral {
                        prefix_key: None,
                        sequence_key: 10
                    }
                }
            ],
            tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>()
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
        let tokens = cpp(src.to_vec(), "", &[], &mut defines, &mut str_maps)?;
        assert_eq!(
            vec![TokenType::CONSTANT_DEC_INT {
                value_key: str_maps.add_byte_vec("4".as_bytes()),
                suffix: None,
            }],
            tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>()
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
        let tokens = cpp(src.to_vec(), "", &[], &mut defines, &mut str_maps)?;
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
            tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>()
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
        let tokens = cpp(src.to_vec(), "", &[], &mut defines, &mut str_maps)?;
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
            tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>()
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
        let tokens = cpp(src.to_vec(), "", &[], &mut defines, &mut str_maps)?;
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
            tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>()
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
        cpp(src.to_vec(), "", &[], &mut defines, &mut str_maps)?;
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
            tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>()
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
        cpp(src.to_vec(), "", &[], &mut defines, &mut str_maps)?;
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
            tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>()
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
        let tokens = cpp(src.to_vec(), "", &[], &mut defines, &mut str_maps)?;
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
            tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>()
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
        let tokens = cpp(src.to_vec(), "", &[], &mut defines, &mut str_maps)?;
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
            tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>()
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
        let tokens = cpp(src.to_vec(), "", &[], &mut defines, &mut str_maps)?;
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
            tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>()
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
        let mut defines = HashMap::new();
        let tokens = cpp(src.to_vec(), "", &[], &mut defines, &mut str_maps)?;
        assert_eq!(
            vec![
                TokenType::DIV,
                TokenType::ASTERISK,
                TokenType::ASTERISK,
                TokenType::WHITESPACE,
                TokenType::DIV,
            ],
            tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>()
        );
        Ok(())
    }
    #[test]
    fn eval_expression_test_defined() -> Result<(), String> {
        let src = r##"defined(HI)"##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = ByteVecMaps::new();
        let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer_when_preprocess(
            &mut tokens,
            &mut str_maps,
            &defines,
        )?;
        assert!(res == 0);
        let src = r##"defined HI "##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = ByteVecMaps::new();
        let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression_integer_when_preprocess(
            &mut tokens,
            &mut str_maps,
            &defines,
        )?;
        assert!(res == 0);
        Ok(())
    }
    #[test]
    fn if_directive_test() -> Result<(), String> {
        {
            let src = r##"#if 1
#if 0
#endif
4
#if 1
5
#endif
6
#endif
"##
            .as_bytes();
            let mut defines = HashMap::new();
            let mut str_maps = ByteVecMaps::new();
            let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
            let mut replacements = Vec::new();
            parse_group(
                &mut tokens,
                &mut 0,
                true,
                &mut str_maps,
                ".",
                &[],
                &mut defines,
                &mut replacements,
            )?;

            assert_eq!(
                vec![
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE,
                    TokenType::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("4".as_bytes()),
                        suffix: None
                    },
                    TokenType::NEWLINE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE,
                    TokenType::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("5".as_bytes()),
                        suffix: None
                    },
                    TokenType::NEWLINE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE,
                    TokenType::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("6".as_bytes()),
                        suffix: None
                    },
                    TokenType::NEWLINE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE
                ],
                tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>(),
            );
        }
        {
            let src = r##"#if 1
4
#endif
"##
            .as_bytes();
            let mut defines = HashMap::new();
            let mut str_maps = ByteVecMaps::new();
            let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
            let mut replacements = Vec::new();
            parse_group(
                &mut tokens,
                &mut 0,
                true,
                &mut str_maps,
                ".",
                &[],
                &mut defines,
                &mut replacements,
            )?;

            assert_eq!(
                vec![
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE,
                    TokenType::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("4".as_bytes()),
                        suffix: None
                    },
                    TokenType::NEWLINE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE
                ],
                tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>(),
            );
        }
        {
            let src = r##"#if 0
4
#endif
"##
            .as_bytes();
            let mut defines = HashMap::new();
            let mut str_maps = ByteVecMaps::new();
            let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
            let mut replacements = Vec::new();
            parse_group(
                &mut tokens,
                &mut 0,
                true,
                &mut str_maps,
                ".",
                &[],
                &mut defines,
                &mut replacements,
            )?;
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
            );
        }
        {
            let src = r##"#if 1 + 1 > 0
4
#endif
"##
            .as_bytes();
            let mut defines = HashMap::new();
            let mut str_maps = ByteVecMaps::new();
            let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
            let mut replacements = Vec::new();
            parse_group(
                &mut tokens,
                &mut 0,
                true,
                &mut str_maps,
                ".",
                &[],
                &mut defines,
                &mut replacements,
            )?;

            assert_eq!(
                vec![
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE,
                    TokenType::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("4".as_bytes()),
                        suffix: None
                    },
                    TokenType::NEWLINE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE
                ],
                tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>(),
            );
        }
        {
            let src = r##"#if 1 + 1 > 2
4
#endif
"##
            .as_bytes();
            let mut defines = HashMap::new();
            let mut str_maps = ByteVecMaps::new();
            let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
            let mut replacements = Vec::new();
            parse_group(
                &mut tokens,
                &mut 0,
                true,
                &mut str_maps,
                ".",
                &[],
                &mut defines,
                &mut replacements,
            )?;

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
            );
        }
        {
            let src = r##"#ifndef hi
4
#endif
"##
            .as_bytes();
            let mut defines = HashMap::new();
            let mut str_maps = ByteVecMaps::new();
            let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
            let mut replacements = Vec::new();
            parse_group(
                &mut tokens,
                &mut 0,
                true,
                &mut str_maps,
                ".",
                &[],
                &mut defines,
                &mut replacements,
            )?;

            assert_eq!(
                vec![
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE,
                    TokenType::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("4".as_bytes()),
                        suffix: None
                    },
                    TokenType::NEWLINE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE
                ],
                tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>(),
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
            let mut defines = HashMap::new();
            let mut str_maps = ByteVecMaps::new();
            let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
            let mut replacements = Vec::new();
            parse_group(
                &mut tokens,
                &mut 0,
                true,
                &mut str_maps,
                ".",
                &[],
                &mut defines,
                &mut replacements,
            )?;
            assert_eq!(
                vec![
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE,
                    TokenType::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("5".as_bytes()),
                        suffix: None
                    },
                    TokenType::NEWLINE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE
                ],
                tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>(),
            );
        }
        {
            let src = r##"#if 0
4
#elif 0
5
#elif 1
6
#else
7
#endif
"##
            .as_bytes();
            let mut defines = HashMap::new();
            let mut str_maps = ByteVecMaps::new();
            let mut tokens = lexer(&src.to_vec(), true, &mut str_maps)?;
            let mut replacements = Vec::new();
            parse_group(
                &mut tokens,
                &mut 0,
                true,
                &mut str_maps,
                ".",
                &[],
                &mut defines,
                &mut replacements,
            )?;
            assert_eq!(
                vec![
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE,
                    TokenType::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("6".as_bytes()),
                        suffix: None
                    },
                    TokenType::NEWLINE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE,
                    TokenType::WHITESPACE,
                    TokenType::WHITESPACE,
                    TokenType::NEWLINE
                ],
                tokens.iter().map(|t| t.r#type).collect::<Vec<TokenType>>(),
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
