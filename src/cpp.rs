use std::collections::HashMap;
use std::io::Write;

use crate::lexer::{self};
use crate::parser::expressions::{self};

#[derive(PartialEq, Debug, Clone)]
pub struct Define {
    pub parameters: Option<Vec<usize>>,
    pub var_arg: bool,
    pub replacement_list: Vec<lexer::Token>,
}

struct MacroSection {
    macro_key: usize,
    start: usize,
    depth: usize,
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
                // for escaped ' and "
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
            Some(lexer::Token::StringLiteral(lexer::StringLiteral {
                prefix_key: _,
                sequence_key,
            })) => {
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
                    let mut tokens_from_file =
                        cpp(file_contents, include_paths, defines, str_maps)?;
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
                    suffix_key: None,
                });
            } else {
                final_eval_tokens.push(lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec(&[b'0']),
                    suffix_key: None,
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
                                    suffix_key: None,
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
                expressions::eval_constant_expression(eval_vec.as_slice(), str_maps)? != 0
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
fn get_end_of_fn_macro(tokens: &[lexer::Token], index: usize) -> Result<usize, String> {
    let mut starting_index = index + 1;
    while let Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE) = tokens.get(starting_index) {
        starting_index += 1;
    }
    if let Some(lexer::Token::PUNCT_OPEN_PAR) = tokens.get(starting_index) {
        starting_index += 1;
        let mut parenth_bal_counter = 1;
        loop {
            match tokens.get(starting_index) {
                Some(lexer::Token::PUNCT_OPEN_PAR) => {
                    parenth_bal_counter += 1;
                }
                Some(lexer::Token::PUNCT_CLOSE_PAR) => {
                    parenth_bal_counter -= 1;
                }
                Some(_) => {}
                None => break,
            }
            if parenth_bal_counter == 0 {
                starting_index += 1;
                break;
            }
            starting_index += 1;
        }
        if parenth_bal_counter < 0 {
            return Err(format!("Too many closing parentheses"));
        } else if parenth_bal_counter > 0 {
            return Err(format!("Too many opening parentheses"));
        }
    }
    Ok(starting_index)
}
fn hash_hash_deletion_and_concat_tokens(replacement_list: &mut Vec<lexer::Token>) {
    let mut hash_hash_process_index = 0;
    while hash_hash_process_index < replacement_list.len() {
        let token = replacement_list[hash_hash_process_index];
        if let lexer::Token::PUNCT_HASH_HASH = token {
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
            match (left_token, right_token) {
                (Some(lexer::Token::PLACEMARKER), Some(lexer::Token::PLACEMARKER))
                | (Some(_), Some(lexer::Token::PLACEMARKER)) => right_index += 1,
                (Some(lexer::Token::PLACEMARKER), Some(_)) => left_index -= 1,
                _ => {}
            }
            for _ in left_index + 1..right_index {
                replacement_list.remove(left_index + 1);
            }
            hash_hash_process_index = left_index;
            continue;
        }
        hash_hash_process_index += 1;
    }
}
fn combine_tokens_during_macro_replacement(
    replacement_list: &mut Vec<lexer::Token>,
    str_maps: &mut lexer::ByteVecMaps,
) {
    let mut combine_token_index = 0;
    let mut combined_sequence = Vec::new();
    let mut token_type = None;
    while combine_token_index < replacement_list.len() {
        while matches!(
            replacement_list.get(combine_token_index),
            Some(lexer::Token::IDENT(_) | lexer::Token::CONSTANT_DEC_INT { .. })
        ) {
            match replacement_list.get(combine_token_index) {
                Some(lexer::Token::CONSTANT_DEC_INT {
                    value_key,
                    suffix_key,
                }) => {
                    if combined_sequence.is_empty() {
                        token_type = Some(lexer::Token::CONSTANT_DEC_INT {
                            value_key: *value_key,
                            suffix_key: *suffix_key,
                        });
                        replacement_list.remove(combine_token_index);
                        break;
                    }
                    combined_sequence.extend_from_slice(&str_maps.key_to_byte_vec[*value_key]);
                }
                Some(lexer::Token::IDENT(temp_key)) => {
                    combined_sequence.extend_from_slice(&str_maps.key_to_byte_vec[*temp_key]);
                }
                _ => unreachable!(),
            }
            replacement_list.remove(combine_token_index);
        }
        if let Some(t) = token_type {
            replacement_list.insert(combine_token_index, t);
            token_type = None;
        } else if !combined_sequence.is_empty() {
            replacement_list.insert(
                combine_token_index,
                lexer::Token::IDENT(str_maps.add_byte_vec(&combined_sequence)),
            );
            combined_sequence.clear();
        }
        combine_token_index += 1;
    }
}
fn parse_macro_and_replace(
    defines: &HashMap<usize, Define>,
    macro_stack: &mut Vec<MacroSection>,
    replacement_list: &mut Vec<lexer::Token>,
    str_maps: &mut lexer::ByteVecMaps,
    already_replaced_macros: &mut Vec<(usize, usize)>,
) -> Result<(), String> {
    let Some(first_macro_section) = macro_stack.pop() else { unreachable!() };
    let Some(defines_data) = defines.get(&first_macro_section.macro_key) else { unreachable!() };
    let mut actual_replacement_list = defines_data.replacement_list.clone();
    if let Some(parameters) = &defines_data.parameters {
        let curr_token_list = replacement_list.clone();
        // we get the arguments passed in
        let mut seen_args = Vec::new();
        let mut argument_index = {
            let mut first_open_par_finder = first_macro_section.start;
            loop {
                match curr_token_list.get(first_open_par_finder) {
                    Some(lexer::Token::PUNCT_OPEN_PAR) => {
                        break first_open_par_finder + 1;
                    }
                    None => return Ok(()),
                    _ => {}
                }
                first_open_par_finder += 1;
            }
        };
        let mut beginning_curr_arg = argument_index;
        while argument_index < curr_token_list.len() {
            match curr_token_list.get(argument_index) {
                Some(lexer::Token::PUNCT_OPEN_PAR) => {
                    let mut parenth_bal_counter = 0;
                    loop {
                        match curr_token_list.get(argument_index) {
                            Some(lexer::Token::PUNCT_OPEN_PAR) => {
                                parenth_bal_counter += 1;
                            }
                            Some(lexer::Token::PUNCT_CLOSE_PAR) => {
                                parenth_bal_counter -= 1;
                            }
                            Some(_) => {}
                            None => unreachable!(),
                        }
                        if parenth_bal_counter == 0 {
                            break;
                        }
                        argument_index += 1;
                    }
                }
                Some(lexer::Token::PUNCT_COMMA | lexer::Token::PUNCT_CLOSE_PAR) => {
                    if seen_args.len() < parameters.len()
                        || matches!(
                            curr_token_list.get(argument_index),
                            Some(lexer::Token::PUNCT_CLOSE_PAR)
                        )
                    {
                        if let Some(lexer::Token::WHITESPACE) =
                            curr_token_list.get(beginning_curr_arg)
                        {
                            beginning_curr_arg += 1;
                        }
                        seen_args.push(&curr_token_list[beginning_curr_arg..argument_index]);
                        beginning_curr_arg = argument_index + 1;
                    }
                }
                _ => {}
            }
            argument_index += 1;
        }
        if seen_args.len() < parameters.len()
            || (defines_data.var_arg && seen_args.len() < parameters.len() + 1)
        {
            let byte_vec = &str_maps.key_to_byte_vec[first_macro_section.macro_key];
            let macro_str = String::from_utf8_lossy(byte_vec).to_string();
            return Err(format!(
                "Wrong number of arguments given to macro {}, num arguments given: {}, num parameters: {}",
                macro_str, seen_args.len(), parameters.len()
            ));
        }
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
                        let argument = seen_args[seen_arg_index];
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
                                    actual_replacement_list.insert(insert_index, *t);
                                    insert_index += 1;
                                }
                            } else {
                                actual_replacement_list
                                    .insert(insert_index, lexer::Token::PLACEMARKER);
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
    combine_tokens_during_macro_replacement(&mut actual_replacement_list, str_maps);
    let end_of_macro = if defines_data.parameters.is_some() {
        get_end_of_fn_macro(&replacement_list, first_macro_section.start)?
    } else {
        first_macro_section.start + 1
    };
    for _ in first_macro_section.start..end_of_macro {
        replacement_list.remove(first_macro_section.start);
    }
    let mut insert_index = first_macro_section.start;
    for t in &actual_replacement_list {
        replacement_list.insert(insert_index, *t);
        insert_index += 1;
    }
    let mut placemarker_removal_index = 0;
    while placemarker_removal_index < replacement_list.len() {
        if let lexer::Token::PLACEMARKER = replacement_list[placemarker_removal_index] {
            replacement_list.remove(placemarker_removal_index);
            continue;
        }
        placemarker_removal_index += 1;
    }
    let mut moar_macros_index = first_macro_section.start;
    'outer: while moar_macros_index < replacement_list.len() {
        if let Some(lexer::Token::IDENT(key)) = replacement_list.get(moar_macros_index) {
            for already_replaced_macros_index in 0..already_replaced_macros.len() {
                let (macro_key, depth) = already_replaced_macros[already_replaced_macros_index];
                if *key == macro_key && first_macro_section.depth > depth {
                    moar_macros_index += 1;
                    continue 'outer;
                }
            }
            if defines.contains_key(key) {
                let Some(defined_data) = defines.get(key) else { unreachable!() };
                let mut new_end = moar_macros_index + 1;
                if defined_data.parameters.is_some() {
                    new_end = get_end_of_fn_macro(&replacement_list, moar_macros_index)?;
                }
                macro_stack.push(MacroSection {
                    macro_key: *key,
                    start: moar_macros_index,
                    depth: first_macro_section.depth + 1,
                });
                already_replaced_macros.push((*key, first_macro_section.depth + 1));
            }
        }
        moar_macros_index += 1;
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
    let lexer::Token::IDENT(macro_id_key) = tokens[index] else { unreachable!("tried matching on ident but instead got {:?}", tokens[index]) };
    if !defines.contains_key(&macro_id_key) {
        return Ok(index);
    }
    let mut already_replaced_macros: Vec<(usize, usize)> = Vec::new();
    let Some(def_data) = defines.get(&macro_id_key) else { unreachable!() };
    let ending_index = {
        let mut take_ident_and_parenths = if def_data.parameters.is_some() {
            let mut first_open_par = index + 1;
            loop {
                match tokens.get(first_open_par) {
                    Some(lexer::Token::PUNCT_OPEN_PAR) => break first_open_par,
                    Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE) | None => {}
                    Some(lexer::Token::IDENT(_)) => break first_open_par - 1,
                    Some(_) => {
                        return Err(format!(
                            "Unexpected token after fn like macro: {:?}",
                            tokens[first_open_par]
                        ))
                    }
                }
                first_open_par += 1;
            }
        } else {
            index + 1
        };
        loop {
            match tokens.get(take_ident_and_parenths) {
                Some(lexer::Token::PUNCT_OPEN_PAR) => {
                    let mut parenth_balance_counter = 0;
                    loop {
                        match tokens.get(take_ident_and_parenths) {
                            Some(lexer::Token::PUNCT_OPEN_PAR) => parenth_balance_counter += 1,
                            Some(lexer::Token::PUNCT_CLOSE_PAR) => parenth_balance_counter -= 1,
                            _ => {}
                        }
                        if parenth_balance_counter < 0 {
                            return Err(format!("parentheses aren't balanced"));
                        }
                        if parenth_balance_counter == 0 {
                            break;
                        }
                        take_ident_and_parenths += 1;
                    }
                }
                Some(lexer::Token::IDENT(_)) => {}
                _ => break take_ident_and_parenths,
            }
            take_ident_and_parenths += 1;
        }
    };
    let mut original_macro = tokens[index..ending_index].to_vec();
    let mut first_macro_section = MacroSection {
        macro_key: macro_id_key,
        start: 0,
        depth: 1,
    };
    let mut macro_stack: Vec<MacroSection> = vec![first_macro_section];
    // Our idea is that we have a stack of macro sections that describe where the macro is in
    // terms of tokens and each 'macro section' has a replacement list that is the replacement list
    // in the #define macro directive.
    // Algorithm:
    //  loop until we do not add anymore macro sections onto the stack.
    //  while we loop, we go through the initial macro or a macro that is within another
    //  replacement list, if it's a fn-like macro, we parse it to obtain the correct arguments that
    //  are passed into that macro, process the # and ## operators and if there's an argument that
    //  doesn't have # or ## before or after it, then we have to go thru the argument token
    //  sequence and expand any existing macros; where those macros start and their lengths will be
    //  pushed onto the macro stack.
    //
    //  If the macro isn't a fn-like macro, then we just loop thru it and do the same thing (push
    //  any macro sections onto the macro stack)
    //  TODO: I actually do something slightly different to what the spec says due to performance
    //  reasons but the end result is the same so that needs to be documented.
    let mut val = true;
    while !macro_stack.is_empty() {
        parse_macro_and_replace(
            defines,
            &mut macro_stack,
            &mut original_macro,
            str_maps,
            &mut already_replaced_macros,
        )?;
    }
    final_tokens.extend_from_slice(&original_macro);
    Ok(ending_index)
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
                index = expand_macro(tokens, index, defines, str_maps, &mut final_tokens)?;
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
    use crate::parser::expressions::{self};
    use std::collections::HashMap;

    use super::{
        comments, cpp, define_directive, expand_macro, expressions::eval_constant_expression,
        if_directive, include_directive, parse_defined_in_if_directive, preprocessing_directives,
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
}"##
        .as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut final_tokens = Vec::<lexer::Token>::new();
        let mut tokens = cpp(
            src.to_vec(),
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
        let mut index = 0;
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
        let mut tokens = cpp(src, &["./test_c_files"], &mut defines, &mut str_maps)?;
        assert_eq!(
            vec![
                lexer::Token::StringLiteral(lexer::StringLiteral {
                    prefix_key: None,
                    sequence_key: str_maps.add_byte_vec("hello world".as_bytes())
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
                suffix_key: None
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
                    suffix_key: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("5".as_bytes()),
                    suffix_key: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("6".as_bytes()),
                    suffix_key: None
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
                    suffix_key: None
                },
                lexer::Token::PUNCT_COMMA,
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("2".as_bytes()),
                    suffix_key: None
                },
                lexer::Token::PUNCT_CLOSE_PAR,
                lexer::Token::PUNCT_COMMA,
                lexer::Token::WHITESPACE,
                lexer::Token::IDENT(str_maps.add_byte_vec("HEHE".as_bytes())),
                lexer::Token::PUNCT_OPEN_PAR,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("3".as_bytes()),
                    suffix_key: None
                },
                lexer::Token::PUNCT_COMMA,
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("4".as_bytes()),
                    suffix_key: None
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
                    suffix_key: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("2".as_bytes()),
                    suffix_key: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("3".as_bytes()),
                    suffix_key: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("4".as_bytes()),
                    suffix_key: None
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
                    suffix_key: None
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
                    suffix_key: None
                },
                lexer::Token::PUNCT_MULT,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("9".as_bytes()),
                    suffix_key: None
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
                    sequence_key: str_maps.add_byte_vec("BAR".as_bytes())
                }),
                lexer::Token::PUNCT_CLOSE_PAR,
            ],
            final_tokens
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
                    suffix_key: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("2".as_bytes()),
                    suffix_key: None
                },
                lexer::Token::PUNCT_COMMA,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("3".as_bytes()),
                    suffix_key: None
                },
                lexer::Token::WHITESPACE,
                lexer::Token::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("4".as_bytes()),
                    suffix_key: None
                },
            ],
            final_tokens
        );
        Ok(())
    }
    #[test]
    fn expand_macro_not_defined() -> Result<(), String> {
        let src = r##"HI"##.as_bytes();
        let mut defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let mut final_tokens = Vec::new();
        expand_macro(
            &mut tokens,
            0,
            &mut defines,
            &mut str_maps,
            &mut final_tokens,
        )?;
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
    fn eval_expression_test_primary() -> Result<(), String> {
        {
            let src = r##"(1 + 1) * 0"##.as_bytes();
            
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, false, "(1 + 1) * 0");
        }
        {
            let src = r##"1 + (1 * 0)"##.as_bytes();
            
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, true, "1 + (1 * 0)");
        }
        {
            let src = r##"((1 + 1) * 0)"##.as_bytes();
            
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, false, "((1 + 1) * 0)");
        }
        {
            let src = r##"((((1))))"##.as_bytes();
            
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, true, "((((1))))");
        }
        {
            let src = r##"((((1)))))"##.as_bytes();
            
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression(&tokens, &mut str_maps);
            match res {
                Err(_) => {}
                Ok(_) => return Err(String::from("unbalanced parentheses not caught")),
            }
        }
        {
            let src = r##"(((((1))))"##.as_bytes();
            
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression(&tokens, &mut str_maps);
            match res {
                Err(_) => {}
                Ok(_) => return Err(String::from("unbalanced parentheses not caught")),
            }
        }
        {
            let src = r##"0 - (1 + 1)"##.as_bytes();
            
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, true, "0 - (1 + 1)");
        }
        {
            let src = r##"1"##.as_bytes();
            
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, true, "1");
        }
        {
            let src = r##"'1'"##.as_bytes();
            
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, true, "'1'");
        }
        Ok(())
    }
    #[test]
    fn eval_expression_test_unary() -> Result<(), String> {
        let src = r##"!1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false, "!1");
        let src = r##"!0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "!0");
        let src = r##"~0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "~0");
        let src = r##"~~~0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "~~~0");
        let src = r##"~~~~0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false, "~~~~0");
        let src = r##"--------------1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps);
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
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 * !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 / 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 / 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps);
        match res {
            Err(_) => {}
            Ok(_) => return Err("division by zero not caught".to_string()),
        }
        let src = r##"1 + 1 * 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 * 1 + 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_additive() -> Result<(), String> {
        let src = r##"1 + 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 - 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"0 - 1 + 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false, "0 - 1 + 1");
        let src = r##"0 - 1 + !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "0 - 1 + !1");
        {
            let src = r##"'1' - '1'"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, false, "'1' - '1'");
        }
        {
            let src = r##"'2' - '1'"##.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
            let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
            assert_eq!(res != 0, true, "'1' - '1'");
        }
        Ok(())
    }
    #[test]
    fn eval_expression_test_bitshift() -> Result<(), String> {
        let src = r##"1 << 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 >> 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 >> !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        Ok(())
    }
    #[test]
    fn eval_expression_test_relational() -> Result<(), String> {
        let src = r##"1 < 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 < 2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 < !2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 <= 2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"2 <= 2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 > 2"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 > 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 >= 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 >= 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        Ok(())
    }
    #[test]
    fn eval_expression_test_equality() -> Result<(), String> {
        let src = r##"1 == 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 != 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 != !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 != 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        Ok(())
    }
    #[test]
    fn eval_expression_test_bit_and() -> Result<(), String> {
        let src = r##"1 & 0 == 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 & 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 & !0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 & 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 == 0 & 1 == 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_bit_xor() -> Result<(), String> {
        let src = r##"1 ^ 0 == 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"(1 ^ !0) == 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        Ok(())
    }
    #[test]
    fn eval_expression_test_bit_or() -> Result<(), String> {
        let src = r##"1 | 0 == 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"1 | !0 == 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true, "1 | !0 == 0");
        Ok(())
    }
    #[test]
    fn eval_expression_test_logical_and() -> Result<(), String> {
        let src = r##"1 && 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 && 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"1 && !1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_logical_or() -> Result<(), String> {
        let src = r##"1 || 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 || 1"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 || 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_conditional() -> Result<(), String> {
        let src = r##"1 ? 1 : 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"(1 + 1 == 3) ? 1 : 0"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        let src = r##"~0 ? (1 + 1 == 2) : 0 * 4"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 ? 0 : 1 * 4"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, true);
        let src = r##"0 ? 0 : !(1 * 4)"##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps)?;
        assert_eq!(res != 0, false);
        Ok(())
    }
    #[test]
    fn eval_expression_test_defined() -> Result<(), String> {
        let src = r##"defined(HI)"##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut final_tokens = Vec::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        parse_defined_in_if_directive(
            tokens.as_slice(),
            0,
            &mut final_tokens,
            &defines,
            &mut str_maps,
        )?;
        let res = expressions::eval_constant_expression(&final_tokens, &mut str_maps)?;
        assert_eq!(res != 0, false, "failed 1");
        let src = r##"defined HI "##.as_bytes();
        let defines = HashMap::new();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut final_tokens = Vec::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        parse_defined_in_if_directive(
            tokens.as_slice(),
            0,
            &mut final_tokens,
            &defines,
            &mut str_maps,
        )?;
        let res = expressions::eval_constant_expression(&final_tokens, &mut str_maps)?;
        assert_eq!(res != 0, false, "failed 2");
        Ok(())
    }
    #[test]
    fn eval_expression_test_empty() -> Result<(), String> {
        let src = r##""##.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut tokens = lexer::lexer(&src.to_vec(), true, &mut str_maps)?;
        let res = expressions::eval_constant_expression(&tokens, &mut str_maps);
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
                        suffix_key: None
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
                        suffix_key: None
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
                        suffix_key: None
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
                tokens[0..2].to_vec(),
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
            .map(|t| t.to_byte_vec(&str_maps).expect(format!("{:?}", t).as_str()))
            .fold(Vec::new(), |mut a: Vec<u8>, e| {
                a.extend_from_slice(&e);
                a
            });
        println!("{s:?}");
        Ok(())
    }
}
