use std::collections::HashMap;

use crate::lexer;
use crate::parser::expressions;

#[derive(PartialEq, Debug, Clone)]
pub struct Define {
    // Definitely is a better way to see if a macro is a function-like macro or not,
    // but 'parameters' here will be Some(...) if it is a function-like macro and None
    // if it isn't.
    pub parameters: Option<Vec<usize>>,
    pub var_arg: bool,
    pub replacement_list: Vec<lexer::Token>,
}

// Depth is for replacing multiple macros of the same name in the same replacement list
struct Macro {
    macro_key: usize,
    // inclusive start and end
    start: usize,
    end: usize,
    depth: usize,
    arguments: Option<Vec<Vec<lexer::Token>>>,
}

fn concat_adjacent_strings(
    tokens: &[lexer::Token],
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<Vec<lexer::Token>, String> {
    let mut adjacent_strings_concated = Vec::new();
    let mut token_string_concated_index = 0;
    while token_string_concated_index < tokens.len() {
        if let Some(lexer::Token::StringLiteral(first_string_lit)) =
            tokens.get(token_string_concated_index)
        {
            let mut prev_prefix = first_string_lit.prefix_key;
            let mut first_byte_vec =
                str_maps.key_to_byte_vec[first_string_lit.sequence_key].clone();
            let mut adjacent_string_lit_index = token_string_concated_index + 1;
            while matches!(
                tokens.get(adjacent_string_lit_index),
                Some(
                    lexer::Token::WHITESPACE
                        | lexer::Token::NEWLINE
                        | lexer::Token::StringLiteral(_)
                )
            ) && adjacent_string_lit_index < tokens.len()
            {
                if let Some(lexer::Token::StringLiteral(second_string_lit)) =
                    tokens.get(adjacent_string_lit_index)
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
            adjacent_strings_concated.push(lexer::Token::StringLiteral(lexer::StringLiteral {
                prefix_key: prev_prefix,
                sequence_key: str_maps.add_byte_vec(first_byte_vec.as_slice()),
            }));
            // If there is a StringLiteral at adjacent_string_lit_index, then we
            // set token_string_concated_index = adjacent_string_lit_index.
            // else we just increment token_string_concated_index so that it moves on from
            // the current StringLiteral
            if matches!(
                tokens.get(adjacent_string_lit_index),
                Some(lexer::Token::StringLiteral(_))
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
    tokens: &[lexer::Token],
    index: usize,
    curr_path: &str,
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
    let mut include_tokens = Vec::new();
    let mut expand_macro_index = index + 1;
    while expand_macro_index < newline_index {
        if let Some(lexer::Token::IDENT(key)) = tokens.get(expand_macro_index) {
            if defines.contains_key(key) {
                expand_macro_index = expand_macro(
                    tokens,
                    expand_macro_index,
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
    if matches!(tokens.get(include_index), Some(lexer::Token::WHITESPACE)) {
        include_index += 1;
    }
    let mut file_name = None;
    let mut look_at_current_dir = false;
    let mut end_of_file_path_index = 0;
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
            Some(lexer::Token::StringLiteral(lexer::StringLiteral {
                prefix_key: _,
                sequence_key,
            })) => {
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
            .filter(|t| !matches!(t, lexer::Token::WHITESPACE))
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
                    final_tokens.extend_from_slice(&tokens_from_file);
                    return Ok(newline_index + 1);
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
                        final_tokens.extend_from_slice(&tokens_from_file);
                        return Ok(newline_index + 1);
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
    tokens: &[lexer::Token],
    index: usize,
    final_eval_tokens: &mut Vec<lexer::Token>,
    defines: &HashMap<usize, Define>,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<usize, String> {
    let mut defined_index = index + 1;
    if let Some(lexer::Token::WHITESPACE | lexer::Token::PUNCT_OPEN_PAR) = tokens.get(defined_index)
    {
        let start = defined_index;
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
        if let Some(lexer::Token::IDENT(identifier_name_key)) = tokens.get(defined_index) {
            defined_index += 1;
            if defines.contains_key(&identifier_name_key) {
                final_eval_tokens.push(lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec(&[b'1']),
                    suffix: None,
                });
            } else {
                final_eval_tokens.push(lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec(&[b'0']),
                    suffix: None,
                });
            }
            if matches!(tokens.get(start), Some(lexer::Token::PUNCT_OPEN_PAR)) {
                while !matches!(
                    tokens.get(defined_index),
                    Some(lexer::Token::PUNCT_CLOSE_PAR)
                ) && defined_index < tokens.len()
                {
                    defined_index += 1;
                }
                if defined_index == tokens.len() {
                    return Err(format!("Missing closing parenthesis for defined at: TODO!"));
                }
                return Ok(defined_index + 1);
            }
        } else {
            return Err(format!("unexpected token: {:?}", tokens[defined_index]));
        }
    } else {
        return Err(format!("unexpected token: {:?}", tokens[defined_index]));
    }
    Ok(defined_index)
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
            b"if" | b"elif" => {
                let mut eval_vec_index = 0;
                let mut final_eval_tokens = Vec::new();
                while eval_vec_index < eval_vec.len() {
                    if let lexer::Token::IDENT(curr_id_key) = &eval_vec[eval_vec_index] {
                        let curr_id = str_maps.key_to_byte_vec[*curr_id_key].clone();
                        if curr_id != *b"defined" {
                            if !defines.contains_key(curr_id_key) {
                                final_eval_tokens.push(lexer::Token::CONSTANT_DEC_INT {
                                    value_key: str_maps.add_byte_vec(&[b'0']),
                                    suffix: None,
                                });
                                eval_vec_index += 1;
                            } else {
                                eval_vec_index = expand_macro(
                                    &eval_vec,
                                    eval_vec_index,
                                    defines,
                                    str_maps,
                                    &mut final_eval_tokens,
                                )?;
                            }
                        } else {
                            eval_vec_index = parse_defined_in_if_directive(
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
                expressions::eval_constant_expression_integer(eval_vec.as_slice(), str_maps)? != 0
            }
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
                let Some(lexer::Token::IDENT(ident_key)) = eval_vec
                    .iter()
                    .find(|t| matches!(t, lexer::Token::IDENT(_)))
                else {
                    unreachable!()
                };
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
                let Some(lexer::Token::IDENT(ident_key)) = eval_vec
                    .iter()
                    .find(|t| matches!(t, lexer::Token::IDENT(_)))
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
    let Some(lexer::Token::IDENT(identifier_of_macro_key)) = tokens.get(index_of_identifier) else {
        unreachable!()
    };
    //There shall be white space between the identifier and the replacement list in the definition of an object-like macro.
    //-- means that a whitespace character means the start of the replacement list
    let mut define_needle_idx = index_of_identifier + 1;
    if let Some(lexer::Token::PUNCT_OPEN_PAR) = tokens.get(define_needle_idx) {
        let start_open_par_idx = define_needle_idx;
        def_data.parameters = Some(Vec::new());
        let mut fn_like_macro_index = define_needle_idx + 1;
        while matches!(
            tokens.get(fn_like_macro_index),
            Some(lexer::Token::WHITESPACE)
        ) && fn_like_macro_index < end
        {
            define_needle_idx += 1;
        }
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
            tokens.get(fn_like_macro_index),
            Some(lexer::Token::PUNCT_ELLIPSIS)
        ) {
            def_data.var_arg = true;
            fn_like_macro_index += 1;
        }
        while matches!(
            tokens.get(fn_like_macro_index),
            Some(lexer::Token::WHITESPACE)
        ) && fn_like_macro_index < end
        {
            define_needle_idx += 1;
        }
        if !matches!(
            tokens.get(fn_like_macro_index),
            Some(lexer::Token::PUNCT_CLOSE_PAR)
        ) {
            def_data.parameters = None;
            def_data.var_arg = false;
            def_data
                .replacement_list
                .extend_from_slice(&tokens[start_open_par_idx + 1..end]);
        } else {
            def_data
                .replacement_list
                .extend_from_slice(&tokens[fn_like_macro_index + 1..end]);
        }
        defines.insert(*identifier_of_macro_key, def_data);
    } else {
        def_data
            .replacement_list
            .extend_from_slice(&tokens[index_of_identifier + 1..end]);
        defines.insert(*identifier_of_macro_key, def_data);
    }
    if defines.contains_key(&identifier_of_macro_key) {
        let identifier_of_macro = &str_maps.key_to_byte_vec[*identifier_of_macro_key];
        if *identifier_of_macro == *b"defined"
            || *identifier_of_macro == *b"__LINE__"
            || *identifier_of_macro == *b"__FILE__"
            || *identifier_of_macro == *b"__DATE__"
            || *identifier_of_macro == *b"__STDC__"
            || *identifier_of_macro == *b"__STDC_HOSTED__"
            || *identifier_of_macro == *b"__STDC_VERSION__"
            || *identifier_of_macro == *b"__TIME__"
        {
            let Ok(s) = String::from_utf8(identifier_of_macro.to_vec()) else {
                unreachable!()
            };
            return Err(format!("cannot define '{s}' as it is a cpp keyword",));
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
            if let Some(lexer::Token::WHITESPACE) = dd.replacement_list.first() {
                dd.replacement_list.remove(0);
            }
            if let Some(lexer::Token::WHITESPACE) = dd.replacement_list.last() {
                dd.replacement_list.pop();
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
            return Ok(end + 1);
        }
    }
    Err(format!(
        "define directive not properly formed at {}",
        index_of_identifier
    ))
}
fn error_directive(_tokens: &mut Vec<lexer::Token>) {
    todo!()
}
fn line_directive(
    _tokens: &mut Vec<lexer::Token>,
    _index: usize,
    _end: usize,
) -> Result<(), String> {
    todo!()
}
fn undef_directive(
    tokens: &[lexer::Token],
    index: usize,
    defines: &mut HashMap<usize, Define>,
    _str_maps: &mut lexer::ByteVecMaps,
) -> Result<usize, String> {
    let mut index_of_identifier = index + 1;
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
            let mut newline_index = index_of_identifier + 1;
            while !matches!(tokens.get(newline_index), Some(lexer::Token::NEWLINE)) {
                newline_index += 1;
                if matches!(tokens.get(newline_index), None) {
                    return Err(String::from("missing newline for undef directive"));
                }
            }
            return Ok(newline_index + 1);
        }
    }
    Err(format!("undef directive not formed correctly"))
}
fn hash_hash_deletion_and_concat_tokens(replacement_list: &mut Vec<lexer::Token>) {
    let mut hash_hash_process_index = 0;
    while hash_hash_process_index < replacement_list.len() {
        let token = replacement_list[hash_hash_process_index];
        if matches!(token, lexer::Token::PUNCT_HASH_HASH) {
            let mut left_index = hash_hash_process_index - 1;
            // left_index should never be less than zero because in the define_directive
            // function, we check if ## is at the beginning or end and we trim whitespace.
            // Same thing for right_index.
            while matches!(
                replacement_list.get(left_index),
                Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
            ) {
                left_index -= 1;
            }
            let mut right_index = hash_hash_process_index + 1;
            while matches!(
                replacement_list.get(right_index),
                Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
            ) {
                right_index += 1;
            }
            let left_token = replacement_list.get(left_index);
            let right_token = replacement_list.get(right_index);
            //Placemarker preprocessing tokens are handled specially: concatena-
            //tion of two placemarkers results in a single placemarker preprocessing token, and concatenation
            //of a placemarker with a non-placemarker preprocessing token results in the non-placemarker pre-
            //processing token
            match (left_token, right_token) {
                (Some(lexer::Token::PLACEMARKER), Some(lexer::Token::PLACEMARKER)) => {
                    for _ in left_index..=right_index {
                        replacement_list.remove(left_index);
                    }
                    replacement_list.insert(left_index, lexer::Token::PLACEMARKER);
                }
                (Some(_), Some(lexer::Token::PLACEMARKER)) => {
                    for _ in left_index + 1..=right_index {
                        replacement_list.remove(left_index + 1);
                    }
                }
                (Some(lexer::Token::PLACEMARKER), Some(_)) => {
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
    replacement_list: &mut Vec<lexer::Token>,
    str_maps: &mut lexer::ByteVecMaps,
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
            match token {
                lexer::Token::IDENT(id_key) => {
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
                        let argument = arguments[seen_arg_index].clone();
                        let first_condition = token_index > 0
                            && matches!(
                                actual_replacement_list.get(token_index - 1),
                                Some(lexer::Token::PUNCT_HASH)
                            );
                        let second_condition = token_index > 1
                            && matches!(
                                actual_replacement_list.get(token_index - 2),
                                Some(lexer::Token::PUNCT_HASH)
                            )
                            && matches!(
                                actual_replacement_list.get(token_index - 1),
                                Some(lexer::Token::WHITESPACE)
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
                                match t {
                                    lexer::Token::NEWLINE => {
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
                                lexer::Token::StringLiteral(lexer::StringLiteral {
                                    prefix_key: None,
                                    sequence_key: str_maps.add_byte_vec(&sequence),
                                }),
                            );
                        } else {
                            actual_replacement_list.remove(token_index);
                            let mut insert_index = token_index;
                            let count_of_non_whitespace = argument
                                .iter()
                                .filter(|t| {
                                    !matches!(t, lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                                })
                                .count();
                            if count_of_non_whitespace > 0 {
                                for t in argument {
                                    actual_replacement_list.insert(insert_index, t);
                                    insert_index += 1;
                                }
                                token_index = insert_index;
                            } else {
                                // Only add placemarker if parameter is preceded or followed by ##
                                if (token_index > 1
                                    && matches!(
                                        actual_replacement_list.get(token_index - 2),
                                        Some(lexer::Token::PUNCT_HASH_HASH)
                                    )
                                    && matches!(
                                        actual_replacement_list.get(token_index - 1),
                                        Some(lexer::Token::WHITESPACE)
                                    ))
                                    || (token_index > 0
                                        && matches!(
                                            actual_replacement_list.get(token_index - 1),
                                            Some(lexer::Token::PUNCT_HASH_HASH)
                                        ))
                                    || matches!(
                                        actual_replacement_list.get(token_index + 1),
                                        Some(lexer::Token::PUNCT_HASH_HASH)
                                    )
                                    || (matches!(
                                        actual_replacement_list.get(token_index + 1),
                                        Some(lexer::Token::WHITESPACE)
                                    ) && matches!(
                                        actual_replacement_list.get(token_index + 2),
                                        Some(lexer::Token::PUNCT_HASH_HASH)
                                    ))
                                {
                                    actual_replacement_list
                                        .insert(insert_index, lexer::Token::PLACEMARKER);
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
    hash_hash_deletion_and_concat_tokens(&mut actual_replacement_list);
    let mut placemarker_removal_index = 0;
    while placemarker_removal_index < actual_replacement_list.len() {
        if let lexer::Token::PLACEMARKER = actual_replacement_list[placemarker_removal_index] {
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
    let actual_replacement_list = lexer::lexer(byte_vec.as_slice(), true, str_maps)?;
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
        if let Some(lexer::Token::IDENT(key)) = replacement_list.get(moar_macros_index) {
            if defines.contains_key(key) {
                for already_replaced_macros_index in 0..already_replaced_macros.len() {
                    let (macro_key, depth) = already_replaced_macros[already_replaced_macros_index];
                    if *key == macro_key {
                        println!(
                            "skipped: {}",
                            String::from_utf8(str_maps.key_to_byte_vec[*key].clone()).unwrap()
                        );
                        moar_macros_index += 1;
                        continue 'outer;
                    }
                }
                let Some(define_data) = defines.get(key) else {
                    unreachable!()
                };
                if let Some(parameters) = &define_data.parameters {
                    if let Some(mut next_macro) =
                        parse_function_macro(replacement_list, moar_macros_index, defines, *key)
                    {
                        let Some(args) = &mut next_macro.arguments else {
                            unreachable!()
                        };
                        if args.len() < parameters.len() {
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
fn parse_function_macro(
    tokens: &[lexer::Token],
    start_index: usize,
    defines: &HashMap<usize, Define>,
    // have to pass macro_key in because some macro could expand and have a macro name at the end,
    // inside of final_tokens
    macro_key: usize,
) -> Option<Macro> {
    let mut fn_macro_index = start_index;
    if matches!(tokens.get(fn_macro_index), Some(lexer::Token::IDENT(_))) {
        fn_macro_index += 1;
    }
    while matches!(
        tokens.get(fn_macro_index),
        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
    ) {
        fn_macro_index += 1;
    }
    if !matches!(
        tokens.get(fn_macro_index),
        Some(lexer::Token::PUNCT_OPEN_PAR)
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
    let mut parenth_stack = vec![(tokens[fn_macro_index], fn_macro_index)];
    fn_macro_index += 1;
    let mut comma_indices = Vec::<usize>::new();
    while !parenth_stack.is_empty() && tokens.get(fn_macro_index).is_some() {
        match tokens[fn_macro_index] {
            lexer::Token::PUNCT_COMMA => {
                if comma_indices.len() < parameters.len() {
                    comma_indices.push(fn_macro_index);
                }
            }
            lexer::Token::PUNCT_OPEN_PAR => {
                parenth_stack.push((lexer::Token::PUNCT_OPEN_PAR, fn_macro_index));
            }
            lexer::Token::PUNCT_CLOSE_PAR => {
                if let (lexer::Token::PUNCT_OPEN_PAR, par_index) =
                    parenth_stack[parenth_stack.len() - 1]
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
            v.push(tokens[prev_comma_index..comma_idx].to_vec());
            prev_comma_index = comma_idx + 1;
        }
        v.push(tokens[prev_comma_index..close_par_index].to_vec());
        return Some(macro_obj);
    }
    None
}
//Before being substituted, each
//argument’s preprocessing tokens are completely macro replaced as if they formed the rest of the
//preprocessing file; no other preprocessing tokens are available
//-- in isolation basically
fn expand_arguments(
    argument: &mut Vec<lexer::Token>,
    defines: &HashMap<usize, Define>,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(), String> {
    let mut already_replaced_macros = Vec::<(usize, usize)>::new();
    loop {
        let mut moar_macros_index = 0;
        let mut macro_stack = Vec::<Macro>::new();
        'outer: while moar_macros_index < argument.len() {
            if let Some(lexer::Token::IDENT(key)) = argument.get(moar_macros_index) {
                if defines.contains_key(key) {
                    for already_replaced_macros_index in 0..already_replaced_macros.len() {
                        let (macro_key, depth) =
                            already_replaced_macros[already_replaced_macros_index];
                        if *key == macro_key {
                            println!(
                                "skipped: {}",
                                String::from_utf8(str_maps.key_to_byte_vec[*key].clone()).unwrap()
                            );
                            moar_macros_index += 1;
                            continue 'outer;
                        }
                    }
                    let Some(macro_define) = defines.get(&key) else {
                        unreachable!()
                    };
                    if macro_define.parameters.is_some() {
                        let macro_obj =
                            parse_function_macro(argument, moar_macros_index, defines, *key);
                        if let Some(mut m) = macro_obj {
                            let end = m.end + 1;
                            m.depth = 1;
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
                            depth: 1,
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
    }
    Ok(())
}
fn expand_macro(
    tokens: &[lexer::Token],
    index: usize,
    defines: &HashMap<usize, Define>,
    str_maps: &mut lexer::ByteVecMaps,
    final_tokens: &mut Vec<lexer::Token>,
) -> Result<usize, String> {
    let mut accumulated_replacements = Vec::<lexer::Token>::new();
    let mut current_token = tokens[index];
    let mut macro_index = index;
    // vector of (macro_key, depth)
    let mut already_replaced_macros: Vec<(usize, usize)> = Vec::new();
    'recheck: loop {
        let lexer::Token::IDENT(macro_id_key) = current_token else {
            unreachable!("{:?}", current_token)
        };
        let Some(def_data) = defines.get(&macro_id_key) else {
            unreachable!()
        };
        let mut first_macro = if def_data.parameters.is_some() || def_data.var_arg {
            let parsed = parse_function_macro(tokens, macro_index, defines, macro_id_key);
            if let Some(mut m) = parsed {
                let Some(args) = &mut m.arguments else {
                    unreachable!()
                };
                for arg in args {
                    expand_arguments(arg, defines, str_maps)?;
                }
                m
            } else {
                accumulated_replacements.push(current_token);
                break 'recheck;
            }
        } else {
            Macro {
                macro_key: macro_id_key,
                start: macro_index,
                end: macro_index,
                depth: 1,
                arguments: None,
            }
        };
        let macro_end = first_macro.end;
        let mut original_macro = tokens[first_macro.start..first_macro.end + 1].to_vec();
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
        let mut back_idx = original_macro.len() - 1;
        while matches!(
            original_macro.get(back_idx),
            Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
        ) && back_idx > 0
        {
            back_idx -= 1;
        }
        if let Some(lexer::Token::IDENT(key)) = original_macro.get(back_idx) {
            if defines.contains_key(key) {
                for (macro_key, _) in &already_replaced_macros {
                    if *macro_key == *key {
                        accumulated_replacements.extend_from_slice(&original_macro);
                        macro_index = macro_end + 1;
                        break 'recheck;
                    }
                }
                current_token = original_macro[back_idx];
                accumulated_replacements.extend_from_slice(&original_macro[..back_idx]);
                macro_index = macro_end + 1;
                already_replaced_macros.clear();
                already_replaced_macros.push((*key, 1));
                continue 'recheck;
            }
        }
        accumulated_replacements.extend_from_slice(&original_macro);
        macro_index = macro_end;
        break 'recheck;
    }
    final_tokens.extend_from_slice(&accumulated_replacements);
    Ok(macro_index + 1)
}
fn preprocessing_directives(
    tokens: &mut Vec<lexer::Token>,
    curr_path: &str,
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
                                    curr_path,
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
            lexer::Token::IDENT(key) => {
                if defines.contains_key(key) {
                    index = expand_macro(tokens, index, defines, str_maps, &mut final_tokens)?;
                    continue;
                }
            }
            _ => {}
        }
        if index < tokens.len() {
            final_tokens.push(tokens[index]);
        }
        index += 1;
    }
    if index >= tokens.len() {
        *tokens = final_tokens;
        return Ok(());
    }
    Err(String::from("unable to preprocess"))
}
pub fn output_tokens_stdout(tokens: &[lexer::Token], str_maps: &lexer::ByteVecMaps) {
    print!(
        "{}",
        String::from_utf8(
            tokens
                .iter()
                .map(|t| t.to_byte_vec(str_maps).unwrap())
                .fold(Vec::new(), |mut a: Vec<u8>, e| {
                    a.extend_from_slice(&e);
                    a
                })
        )
        .unwrap()
    );
}
// TODO: add flag options so that the user could specify if they wanted to only preprocess
// TODO: implement some kind of warning system
pub fn cpp(
    program_str: Vec<u8>,
    curr_path: &str,
    include_paths: &[&str],
    defines: &mut HashMap<usize, Define>,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<Vec<lexer::Token>, String> {
    // trigraphs (part of step 1 in the translation phase)
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
    let program_str = trigraphs_processed;
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

    use crate::lexer;
    use crate::parser::expressions;
    use std::collections::HashMap;

    use super::{
        comments, cpp, define_directive, expand_macro, if_directive, parse_defined_in_if_directive,
        preprocessing_directives, Define,
    };
    #[test]
    fn comments_removal_outside_quotes() -> Result<(), String> {
        let src = "int main() {\n\"hi\"; // this is me\n}\n";
        let src_bytes = src.as_bytes();
        let removed = comments(src_bytes)?;
        let stringed = String::from_utf8(removed).unwrap();
        assert_eq!(stringed, "int main() {\n\"hi\";  \n}\n");
        Ok(())
    }
    #[test]
    fn comments_removal_inside_single_quotes() -> Result<(), String> {
        let src = "int main() {\n\"hi\"; '// this is me';\n}\n";
        let src_bytes = src.as_bytes();
        let removed = comments(src_bytes)?;
        let stringed = String::from_utf8(removed).unwrap();
        assert_eq!(stringed, "int main() {\n\"hi\"; '// this is me';\n}\n");
        Ok(())
    }
    #[test]
    fn comments_removal_inside_double_quotes() -> Result<(), String> {
        let src = "int main() {\n\"hi\"; \"// this is me\";\n}\n";
        let src_bytes = src.as_bytes();
        let removed = comments(src_bytes)?;
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
        let removed = comments(src_bytes)?;
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
            let mut str_maps = lexer::ByteVecMaps::new();
            let _final_tokens = Vec::<lexer::Token>::new();
            let tokens = cpp(
                src.to_vec(),
                "./test_c_files/hi.h",
                &["./test_c_files"],
                &mut defines,
                &mut str_maps,
            )?;
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
        }
        {
            let src = r##"#define FILE "hi.h"
#include FILE
int main() {
}"##
            .as_bytes();
            let mut defines = HashMap::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let _final_tokens = Vec::<lexer::Token>::new();
            let tokens = cpp(
                src.to_vec(),
                "./test_c_files/hi.h",
                &["./test_c_files"],
                &mut defines,
                &mut str_maps,
            )?;
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
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        preprocessing_directives(
            &mut tokens,
            "./test_c_files/hi2.h",
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
HI(5 5);"##
            .as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        let new_index = define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        expand_macro(
            &mut tokens,
            new_index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![lexer::Token::StringLiteral(lexer::StringLiteral {
                prefix_key: None,
                sequence_key: str_maps.add_byte_vec("5 5".as_bytes())
            }),],
            final_tokens
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
        let mut final_tokens = Vec::new();
        let index = 0;
        let mut defines = HashMap::new();
        let new_index = define_directive(&mut tokens, index, &mut defines, &mut str_maps)?;
        let new_index = define_directive(&mut tokens, new_index, &mut defines, &mut str_maps)?;
        let new_index = define_directive(&mut tokens, new_index, &mut defines, &mut str_maps)?;
        let mut new_index = define_directive(&mut tokens, new_index, &mut defines, &mut str_maps)?;
        while new_index < tokens.len() {
            if let Some(
                [lexer::Token::IDENT(first), lexer::Token::PUNCT_OPEN_PAR, lexer::Token::IDENT(second)],
            ) = tokens.get(new_index..new_index + 3)
            {
                if *first == str_maps.add_byte_vec("join".as_bytes())
                    && *second == str_maps.add_byte_vec("x".as_bytes())
                {
                    break;
                }
            }
            new_index += 1;
        }
        expand_macro(
            &mut tokens,
            new_index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![lexer::Token::StringLiteral(lexer::StringLiteral {
                prefix_key: None,
                sequence_key: str_maps.add_byte_vec("x ## y".as_bytes())
            }),],
            final_tokens
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
        let tokens = cpp(src, "", &["./test_c_files"], &mut defines, &mut str_maps)?;
        assert_eq!(
            vec![
                lexer::Token::StringLiteral(lexer::StringLiteral {
                    prefix_key: None,
                    sequence_key: str_maps.add_byte_vec(" hello world".as_bytes())
                }),
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
            *defines
                .get(&str_maps.add_byte_vec("hash_hash".as_bytes()))
                .unwrap()
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
            *defines
                .get(&str_maps.add_byte_vec("mkstr".as_bytes()))
                .unwrap()
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
        let mut tokens = lexer::lexer(src, true, &mut str_maps)?;
        let new_index = define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        let new_index = define_directive(&mut tokens, new_index, &mut defines, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        expand_macro(
            &mut tokens,
            new_index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![lexer::Token::CONSTANT_DEC_INT {
                value_key: str_maps.add_byte_vec("4".as_bytes()),
                suffix: None
            }],
            final_tokens
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
        let new_index = define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        let new_index = define_directive(&mut tokens, new_index, &mut defines, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        expand_macro(
            &mut tokens,
            new_index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("4".as_bytes()),
                    suffix: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("5".as_bytes()),
                    suffix: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("6".as_bytes()),
                    suffix: None
                }
            ],
            final_tokens
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
        let new_index = define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        expand_macro(
            &mut tokens,
            new_index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
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
            final_tokens
        );
        Ok(())
    }
    #[test]
    fn expand_macro_recursive() -> Result<(), String> {
        let src = r##"#define HEHE(a,b) HEHE(a, b)
HEHE(HEHE(1,2),HEHE(3,4))"##
            .as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let new_index = define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        expand_macro(
            &mut tokens,
            new_index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![
                lexer::Token::IDENT(str_maps.add_byte_vec("HEHE".as_bytes())),
                lexer::Token::PUNCT_OPEN_PAR,
                lexer::Token::IDENT(str_maps.add_byte_vec("HEHE".as_bytes())),
                lexer::Token::PUNCT_OPEN_PAR,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("1".as_bytes()),
                    suffix: None
                },
                lexer::Token::PUNCT_COMMA,
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("2".as_bytes()),
                    suffix: None
                },
                lexer::Token::PUNCT_CLOSE_PAR,
                lexer::Token::PUNCT_COMMA,
                lexer::Token::WHITESPACE,
                lexer::Token::IDENT(str_maps.add_byte_vec("HEHE".as_bytes())),
                lexer::Token::PUNCT_OPEN_PAR,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("3".as_bytes()),
                    suffix: None
                },
                lexer::Token::PUNCT_COMMA,
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("4".as_bytes()),
                    suffix: None
                },
                lexer::Token::PUNCT_CLOSE_PAR,
                lexer::Token::PUNCT_CLOSE_PAR,
            ],
            final_tokens
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
        let new_index = define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        expand_macro(
            &mut tokens,
            new_index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("1".as_bytes()),
                    suffix: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("2".as_bytes()),
                    suffix: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("3".as_bytes()),
                    suffix: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("4".as_bytes()),
                    suffix: None
                },
            ],
            final_tokens
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
        let new_index = define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        let new_index = define_directive(&mut tokens, new_index, &mut defines, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        expand_macro(
            &mut tokens,
            new_index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![
                lexer::Token::PUNCT_COMMA,
                lexer::Token::WHITESPACE,
                lexer::Token::PUNCT_PLUS,
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("4".as_bytes()),
                    suffix: None
                },
            ],
            final_tokens
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
        let new_index = define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        let new_index = define_directive(&mut tokens, new_index, &mut defines, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        expand_macro(
            &mut tokens,
            new_index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("2".as_bytes()),
                    suffix: None
                },
                lexer::Token::PUNCT_MULT,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("9".as_bytes()),
                    suffix: None
                },
                lexer::Token::PUNCT_MULT,
                lexer::Token::IDENT(str_maps.add_byte_vec("g".as_bytes())),
            ],
            final_tokens
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
        let new_index = define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        let new_index = define_directive(&mut tokens, new_index, &mut defines, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        expand_macro(
            &mut tokens,
            new_index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![
                lexer::Token::IDENT(str_maps.add_byte_vec("printf".as_bytes())),
                lexer::Token::PUNCT_OPEN_PAR,
                lexer::Token::StringLiteral(lexer::StringLiteral {
                    prefix_key: None,
                    sequence_key: str_maps.add_byte_vec("FOO".as_bytes())
                }),
                lexer::Token::WHITESPACE,
                lexer::Token::StringLiteral(lexer::StringLiteral {
                    prefix_key: None,
                    sequence_key: str_maps.add_byte_vec(" BAR".as_bytes())
                }),
                lexer::Token::PUNCT_CLOSE_PAR,
            ],
            final_tokens
        );
        Ok(())
    }
    #[test]
    fn __va_args___test() -> Result<(), String> {
        let src = r##"#define CHICKEN(...) __VA_ARGS__
CHICKEN(1 2,3 4)"##
            .as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let new_index = define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        expand_macro(
            &mut tokens,
            new_index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("1".as_bytes()),
                    suffix: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("2".as_bytes()),
                    suffix: None
                },
                lexer::Token::PUNCT_COMMA,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("3".as_bytes()),
                    suffix: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("4".as_bytes()),
                    suffix: None
                },
            ],
            final_tokens
        );
        Ok(())
    }
    #[test]
    fn expand_macro_side_by_side() -> Result<(), String> {
        let src = r##"#define PP(a, b) a ## b
#define PP2(a, b) a/**/b
PP(/,*)PP2(*,/)"##
            .as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let src = comments(src)?;
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let mut defines = HashMap::new();
        let new_index = define_directive(&mut tokens, 0, &mut defines, &mut str_maps)?;
        let new_index = define_directive(&mut tokens, new_index, &mut defines, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        let new_index = expand_macro(
            &tokens,
            new_index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        let _new_index = expand_macro(
            &tokens,
            new_index,
            &defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
        assert_eq!(
            vec![
                lexer::Token::PUNCT_DIV,
                lexer::Token::PUNCT_MULT,
                lexer::Token::PUNCT_MULT,
                lexer::Token::WHITESPACE,
                lexer::Token::PUNCT_DIV
            ],
            final_tokens
        );
        Ok(())
    }
    #[test]
    fn eval_expression_test_defined() -> Result<(), String> {
        let src = r##"defined(HI)"##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut final_tokens = Vec::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        parse_defined_in_if_directive(
            tokens.as_slice(),
            0,
            &mut final_tokens,
            &defines,
            &mut str_maps,
        )?;
        let res = expressions::eval_constant_expression_integer(&final_tokens, &mut str_maps)?;
        assert_eq!(res != 0, false, "failed 1");
        let src = r##"defined HI "##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut final_tokens = Vec::new();
        let tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        parse_defined_in_if_directive(
            tokens.as_slice(),
            0,
            &mut final_tokens,
            &defines,
            &mut str_maps,
        )?;
        let res = expressions::eval_constant_expression_integer(&final_tokens, &mut str_maps)?;
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
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            if_directive(&mut tokens, 0, &defines, &mut str_maps)?;

            assert_eq!(
                vec![
                    lexer::Token::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("4".as_bytes()),
                        suffix: None
                    },
                    lexer::Token::NEWLINE,
                ],
                tokens[0..2].to_vec(),
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
                tokens
                    .iter()
                    .filter(|t| !matches!(t, lexer::Token::WHITESPACE | lexer::Token::NEWLINE))
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
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            if_directive(&mut tokens, 0, &defines, &mut str_maps)?;

            assert_eq!(
                vec![
                    lexer::Token::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("4".as_bytes()),
                        suffix: None
                    },
                    lexer::Token::NEWLINE,
                ],
                tokens[0..2].to_vec(),
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
                tokens
                    .iter()
                    .filter(|t| !matches!(t, lexer::Token::WHITESPACE | lexer::Token::NEWLINE))
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
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            if_directive(&mut tokens, 0, &defines, &mut str_maps)?;

            assert_eq!(
                vec![
                    lexer::Token::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("4".as_bytes()),
                        suffix: None
                    },
                    lexer::Token::NEWLINE,
                ],
                tokens[0..2].to_vec(),
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
                        suffix: None
                    },
                    lexer::Token::NEWLINE,
                ],
                tokens[0..2].to_vec(),
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
            let tokens = cpp(
                src.to_vec(),
                "",
                &["./test_c_files"],
                &mut defines,
                &mut str_maps,
            )?;
            assert_eq!(
                vec![
                    lexer::Token::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("5".as_bytes()),
                        suffix: None
                    },
                    lexer::Token::NEWLINE,
                ],
                tokens[0..2].to_vec(),
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
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = cpp(
                src.to_vec(),
                "",
                &["./test_c_files"],
                &mut defines,
                &mut str_maps,
            )?;
            assert_eq!(
                vec![
                    lexer::Token::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("4".as_bytes()),
                        suffix: None
                    },
                    lexer::Token::NEWLINE,
                ],
                tokens[0..2].to_vec(),
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
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = cpp(
                src.to_vec(),
                "",
                &["./test_c_files"],
                &mut defines,
                &mut str_maps,
            )?;
            assert_eq!(
                vec![
                    lexer::Token::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("5".as_bytes()),
                        suffix: None
                    },
                    lexer::Token::NEWLINE,
                ],
                tokens[0..2].to_vec(),
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
            let tokens = cpp(
                src.to_vec(),
                "",
                &["./test_c_files"],
                &mut defines,
                &mut str_maps,
            )?;
            assert_eq!(
                vec![
                    lexer::Token::CONSTANT_DEC_INT {
                        value_key: str_maps.add_byte_vec("4".as_bytes()),
                        suffix: None
                    },
                    lexer::Token::NEWLINE,
                ],
                tokens[0..2].to_vec(),
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
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = cpp(
            src.as_bytes().to_vec(),
            "",
            &[""],
            &mut defines,
            &mut str_maps,
        )?;
        assert_eq!(
            vec![
                lexer::Token::IDENT(str_maps.add_byte_vec("COMMA".as_bytes())),
                lexer::Token::WHITESPACE,
                lexer::Token::PUNCT_OPEN_PAR,
                lexer::Token::PUNCT_CLOSE_PAR,
            ],
            tokens,
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
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = cpp(
            src.as_bytes().to_vec(),
            "",
            &[""],
            &mut defines,
            &mut str_maps,
        )?;
        assert_eq!(
            vec![
                lexer::Token::PUNCT_OPEN_PAR,
                lexer::Token::IDENT(str_maps.add_byte_vec("T".as_bytes())),
                lexer::Token::PUNCT_CLOSE_PAR,
            ],
            tokens,
        );
        Ok(())
    }
}
