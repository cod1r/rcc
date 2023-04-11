use std::collections::HashMap;

use crate::lexer;

#[derive(PartialEq, Debug, Clone)]
struct Define {
    identifier: String,
    parameters: Option<Vec<String>>,
    var_arg: bool,
    replacement_list: Vec<lexer::Token>,
}

fn comments(bytes: &[u8]) -> Vec<u8> {
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
                byte_index += 1;
            }
            if byte_index < bytes.len() && bytes[byte_index] == start {
                within_quotes = false;
                comments_removed.push(bytes[byte_index]);
                byte_index += 1;
            }
        } else if byte_index + 1 < bytes.len()
            && bytes[byte_index] == b'/'
            && bytes[byte_index + 1] == b'/'
            && !within_quotes
        {
            comments_removed.push(b' ');
            while byte_index < bytes.len() && bytes[byte_index] != b'\n' {
                byte_index += 1;
            }
        }
        comments_removed.push(bytes[byte_index]);
        byte_index += 1;
    }
    comments_removed
}
fn get_header_name_from_tokens(tokens: &[lexer::Token]) -> Option<String> {
    if let (Some(lexer::Token::PUNCT_LESS_THAN), Some(lexer::Token::PUNCT_GREATER_THAN)) =
        (tokens.first(), tokens.last())
    {
        let mut stringified = tokens[1..tokens.len() - 1].iter().map(|t| match t {
            lexer::Token::IDENT(s) => Some(s.as_str()),
            _ => t.to_string(),
        });
        if stringified.any(|t_opt| t_opt.is_none()) {
            return None;
        }
        return Some(stringified.fold(String::new(), |mut acc, e| {
            acc += e.unwrap();
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
    let index_header_file = index + 2;
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
fn eval_constant_expression(tokens: &[lexer::Token]) -> Result<bool, String> {
    todo!()
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
    end: usize,
    defines: &mut HashMap<String, Define>,
) -> Result<(), String> {
    let mut index_of_identifier = index + 1;
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
                        v.push(arg.to_string());
                    } else {
                        return Err(format!("duplicate argument name found in define directive"));
                    }
                } else {
                    def_data.parameters = Some(vec![arg.to_string()]);
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
        if let Some(dd) = defines.get(&identifier_of_macro) {
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
            let mut length = end - index;
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
    end: usize,
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
    let mut end = index_copy;
    let macro_id = match tokens[index_copy].clone() {
        lexer::Token::IDENT(id) => id,
        _ => return Err(format!("non identifier token given")),
    };
    let mut already_replaced_macro_names: Vec<String> = Vec::new();
    loop {
        if let Some(def_data) = defines.get(&macro_id) {
            if let Some(args) = &def_data.parameters {
                let mut fn_like_macro_index = index_copy + 1;
                if let Some(lexer::Token::PUNCT_OPEN_PAR) = tokens.get(fn_like_macro_index) {
                    fn_like_macro_index += 1;
                    let mut seen_args = Vec::new();
                    let mut beginning_of_argument_index = fn_like_macro_index;
                    while !matches!(
                        tokens.get(fn_like_macro_index),
                        Some(lexer::Token::PUNCT_CLOSE_PAR)
                    ) {
                        match tokens.get(fn_like_macro_index) {
                            Some(lexer::Token::PUNCT_OPEN_PAR) => {
                                let mut parenth_balance_counter = 1;
                                while parenth_balance_counter > 0 {
                                    fn_like_macro_index += 1;
                                    match tokens.get(fn_like_macro_index) {
                                        Some(lexer::Token::PUNCT_OPEN_PAR) => {
                                            parenth_balance_counter += 1
                                        }
                                        Some(lexer::Token::PUNCT_CLOSE_PAR) => {
                                            parenth_balance_counter -= 1
                                        }
                                        _ => {}
                                    }
                                }
                                if matches!(
                                    tokens.get(fn_like_macro_index),
                                    Some(lexer::Token::PUNCT_CLOSE_PAR)
                                ) {
                                    fn_like_macro_index += 1;
                                } else {
                                    return Err(format!(
                                        "no matching parentheses for macro invocation"
                                    ));
                                }
                            }
                            Some(lexer::Token::PUNCT_COMMA) => {
                                if let Some(slice) =
                                    tokens.get(beginning_of_argument_index..fn_like_macro_index)
                                {
                                    seen_args.push(slice.to_vec());
                                }
                                beginning_of_argument_index = fn_like_macro_index + 1;
                            }
                            _ => {}
                        }
                        fn_like_macro_index += 1;
                    }
                    if !matches!(
                        tokens.get(fn_like_macro_index),
                        Some(lexer::Token::PUNCT_CLOSE_PAR)
                    ) {
                        return Err(format!("no closing parenth for fn like macro invoc"));
                    }
                    end = fn_like_macro_index;
                    if seen_args.len() < args.len()
                        || (seen_args.len() != args.len() && !def_data.var_arg)
                    {
                        return Err(format!("wrong number of macro arguments given"));
                    }
                    let mut replacement_list_copy = def_data.replacement_list.clone();
                    let mut replaced_indices = Vec::new();
                    for seen_arg_index in 0..seen_args.len() {
                        let parameter_name = &args[seen_arg_index];
                        for replacement_list_index in 0..replacement_list_copy.len() {
                            match &replacement_list_copy[replacement_list_index] {
                                lexer::Token::IDENT(p_name)
                                    if *p_name == *parameter_name
                                        && !replaced_indices.contains(&replacement_list_index) =>
                                {
                                    if matches!(
                                        replacement_list_copy.get(replacement_list_index - 1),
                                        Some(lexer::Token::PUNCT_HASH)
                                    ) || matches!(
                                        replacement_list_copy.get(
                                            replacement_list_index - 2..replacement_list_index + 1
                                        ),
                                        Some([
                                            lexer::Token::PUNCT_HASH,
                                            lexer::Token::WHITESPACE,
                                            lexer::Token::IDENT(_)
                                        ])
                                    ) {
                                        let mut removal_index = replacement_list_index;
                                        replacement_list_copy.remove(removal_index - 1);
                                        removal_index -= 1;
                                        replacement_list_copy.remove(removal_index);
                                        let mut string_literal_token =
                                            lexer::Token::StringLiteral {
                                                prefix: None,
                                                sequence: String::new(),
                                            };
                                        let argument = &seen_args[seen_arg_index];
                                        let mut argument_begin_index = 0;
                                        let mut argument_end_index = argument.len() - 1;
                                        while matches!(
                                            argument.get(argument_begin_index),
                                            Some(lexer::Token::WHITESPACE)
                                        ) {
                                            argument_begin_index += 1;
                                        }
                                        while argument_end_index > 0
                                            && argument_end_index > argument_begin_index
                                            && matches!(
                                                argument.get(argument_end_index),
                                                Some(lexer::Token::WHITESPACE)
                                            )
                                        {
                                            argument_end_index -= 1;
                                        }
                                        let lexer::Token::StringLiteral { prefix: _, sequence } =
                                        &mut string_literal_token else { panic!("WHAT IN THE FUCK") };
                                        while argument_begin_index <= argument_end_index {
                                            if let Some(stringified_token) =
                                                argument[argument_begin_index].to_string()
                                            {
                                                sequence.push_str(stringified_token);
                                                argument_begin_index += 1;
                                            } else if let lexer::Token::IDENT(mut s) =
                                                argument[argument_begin_index].clone()
                                            {
                                                s.insert_str(0, "\"");
                                                for s_index in 0..s.len() {
                                                    if matches!(
                                                        s.get(s_index..s_index + 1),
                                                        Some("\\") | Some("\"")
                                                    ) {
                                                        s.insert_str(s_index, "\\");
                                                    }
                                                }
                                                s.push_str("\"");
                                                sequence.push_str(&s);
                                                argument_begin_index += 1;
                                            } else {
                                                panic!("tried to stringify token that cannot be stringified");
                                            }
                                        }
                                        replacement_list_copy
                                            .insert(replacement_list_index, string_literal_token);
                                        replaced_indices.push(replacement_list_index);
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
                                                if !matches!(t, lexer::Token::WHITESPACE) {
                                                    replacement_list_copy.insert(
                                                        replacement_list_index_copy,
                                                        t.clone(),
                                                    );
                                                    replacement_list_index_copy += 1;
                                                }
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
                                        replaced_indices.push(replacement_list_index);
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    let mut punct_hash_hash_index = 0;
                    while punct_hash_hash_index < replacement_list_copy.len() {
                        if let Some(lexer::Token::PUNCT_HASH_HASH) =
                            replacement_list_copy.get(punct_hash_hash_index)
                        {
                            let case_for_placemarker = match (
                                replacement_list_copy.get(punct_hash_hash_index - 1),
                                replacement_list_copy.get(punct_hash_hash_index + 1),
                            ) {
                                (
                                    Some(lexer::Token::PLACEMARKER),
                                    Some(lexer::Token::PLACEMARKER),
                                ) => Some(lexer::Token::PLACEMARKER),
                                (s, Some(lexer::Token::PLACEMARKER))
                                | (Some(lexer::Token::PLACEMARKER), s) => s.cloned(),
                                _ => None,
                            };
                            replacement_list_copy.remove(punct_hash_hash_index);
                            if let Some(t) = case_for_placemarker {
                                replacement_list_copy.insert(punct_hash_hash_index, t);
                            }
                            continue;
                        }
                        punct_hash_hash_index += 1;
                    }
                    let mut length = fn_like_macro_index - *index + 1;
                    while length > 0 {
                        tokens.remove(*index);
                        end -= 1;
                        length -= 1;
                    }
                    let mut insert_index = *index;
                    for t in replacement_list_copy {
                        tokens.insert(insert_index, t);
                        end += 1;
                        insert_index += 1;
                    }
                    already_replaced_macro_names.push(macro_id.to_string());
                } else {
                    return Err(format!("no args given for function macro"));
                }
            } else {
                let mut ending_index = *index;
                if let Some(lexer::Token::PUNCT_OPEN_PAR) = tokens.get(*index + 1) {
                    while !matches!(
                        tokens.get(ending_index),
                        Some(lexer::Token::PUNCT_CLOSE_PAR)
                    ) {
                        ending_index += 1;
                    }
                }
                let mut length = ending_index - *index + 1;
                while length > 0 {
                    tokens.remove(*index);
                    end -= 1;
                    length -= 1;
                }
                let mut punct_hash_hash_index = 0;
                let mut replacement_list_copy = def_data.replacement_list.clone();
                while punct_hash_hash_index < replacement_list_copy.len() {
                    if let Some(lexer::Token::PUNCT_HASH_HASH) =
                        replacement_list_copy.get(punct_hash_hash_index)
                    {
                        let case_for_placemarker = match (
                            replacement_list_copy.get(punct_hash_hash_index - 1),
                            replacement_list_copy.get(punct_hash_hash_index + 1),
                        ) {
                            (Some(lexer::Token::PLACEMARKER), Some(lexer::Token::PLACEMARKER)) => {
                                Some(lexer::Token::PLACEMARKER)
                            }
                            (s, Some(lexer::Token::PLACEMARKER))
                            | (Some(lexer::Token::PLACEMARKER), s) => s.cloned(),
                            _ => None,
                        };
                        replacement_list_copy.remove(punct_hash_hash_index);
                        if let Some(t) = case_for_placemarker {
                            replacement_list_copy.insert(punct_hash_hash_index, t);
                        }
                        continue;
                    }
                    punct_hash_hash_index += 1;
                }
                let mut insert_index = *index;
                for t in replacement_list_copy {
                    tokens.insert(insert_index, t);
                    end += 1;
                    insert_index += 1;
                }
                already_replaced_macro_names.push(macro_id.to_string());
            }
        } else {
            *index = end + 1;
            break;
        }
    }
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
                while !matches!(tokens.get(newline), Some(lexer::Token::NEWLINE)) {
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
                                define_directive(tokens, index, newline, &mut defines)?;
                            }
                            "defined" => todo!(),
                            "undef" => {
                                undef_directive(tokens, index, newline, &mut defines)?;
                            }
                            "error" => {}
                            "line" => {}
                            "pragma" => {}
                            _ => {
                                index = newline + 1;
                            }
                        }
                    }
                } else {
                    return Err(format!("no newline at the end of preprocessing directive"));
                }
            }
            lexer::Token::IDENT(_) => {
                expand_macro(tokens, &mut index, &defines)?;
                todo!()
            }
            _ => {
                index += 1;
            }
        }
    }
    if index >= tokens.len() {
        return Ok(());
    }
    Err(String::from("unable to preprocess"))
}
// TODO: add flag options so that the user could specify if they wanted to only preprocess
pub fn cpp(program_str: Vec<u8>) -> Vec<lexer::Token> {
    let backslash_newline_spliced = program_str
        .iter()
        .map(|b| *b as char)
        .collect::<String>()
        .replace("\\\n", "");
    let backslash_newline_spliced = backslash_newline_spliced.as_bytes();
    let comments_removed = comments(backslash_newline_spliced);
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
    fn comments_removal_outside_quotes() {
        let src = "int main() {\n\"hi\"; // this is me\n}\n";
        let src_bytes = src.as_bytes();
        let removed = comments(src_bytes);
        let stringed = String::from_utf8_lossy(&removed);
        assert_eq!(stringed, "int main() {\n\"hi\";  \n}\n");
    }
    #[test]
    fn comments_removal_inside_single_quotes() {
        let src = "int main() {\n\"hi\"; '// this is me';\n}\n";
        let src_bytes = src.as_bytes();
        let removed = comments(src_bytes);
        let stringed = String::from_utf8_lossy(&removed);
        assert_eq!(stringed, "int main() {\n\"hi\"; '// this is me';\n}\n");
    }
    #[test]
    fn comments_removal_inside_double_quotes() {
        let src = "int main() {\n\"hi\"; \"// this is me\";\n}\n";
        let src_bytes = src.as_bytes();
        let removed = comments(src_bytes);
        let stringed = String::from_utf8_lossy(&removed);
        assert_eq!(stringed, "int main() {\n\"hi\"; \"// this is me\";\n}\n");
    }
    #[test]
    fn include_test() -> Result<(), String> {
        let src = r##"#include "hi.h"
        int main() {
        }
        "##;
        let mut tokens = lexer::lexer(src.as_bytes().to_vec(), true)?;
        let mut defines = HashMap::new();
        include_directive(&mut tokens, 0, 18, &["./test_c_files/"], &mut defines)?;
        let assert_tokens = [
            lexer::Token::IDENT(String::from("int")),
            lexer::Token::WHITESPACE,
            lexer::Token::IDENT(String::from("main")),
            lexer::Token::PUNCT_OPEN_PAR,
            lexer::Token::PUNCT_CLOSE_PAR,
            lexer::Token::PUNCT_OPEN_CURLY,
            lexer::Token::NEWLINE,
            lexer::Token::PUNCT_CLOSE_CURLY,
            lexer::Token::NEWLINE,
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
        let assert_tokens = [
            lexer::Token::IDENT(String::from("int")),
            lexer::Token::IDENT(String::from("main")),
            lexer::Token::PUNCT_OPEN_PAR,
            lexer::Token::PUNCT_CLOSE_PAR,
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
        assert_eq!(tokens, assert_tokens);
        Ok(())
    }
    #[test]
    fn preprocess_test_defines() -> Result<(), String> {
        let src = r##"#define hash_hash # ## #
#define mkstr(a) # a
#define in_between(a) mkstr(a)
#define join(c, d) in_between(c hash_hash d)
char p[] = join(x, y); // equivalent to
// char p[] = "x ## y";"##;
        let mut tokens = lexer::lexer(src.as_bytes().to_vec(), true)?;
        preprocessing_directives(&mut tokens, &["./test_c_files"])?;
        let assert_tokens = [
            lexer::Token::IDENT(String::from("int")),
            lexer::Token::IDENT(String::from("main")),
            lexer::Token::PUNCT_OPEN_PAR,
            lexer::Token::PUNCT_CLOSE_PAR,
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
        assert_eq!(tokens, assert_tokens);
        Ok(())
    }
    #[test]
    fn expand_macro_tests() -> Result<(), String> {
        let src = r##"#define hash_hash # ## #
#define mkstr(a) # a
#define in_between(a) mkstr(a)
#define join(c, d) in_between(c hash_hash d)
char p[] = join(x, y);"##;
        let mut tokens = lexer::lexer(src.as_bytes().to_vec(), true)?;
        let mut index = 0;
        let mut defines = HashMap::new();
        defines.insert(
            String::from("hash_hash"),
            Define {
                identifier: String::from("hash_hash"),
                parameters: None,
                var_arg: false,
                replacement_list: vec![
                    lexer::Token::PUNCT_HASH,
                    lexer::Token::WHITESPACE,
                    lexer::Token::PUNCT_HASH_HASH,
                    lexer::Token::WHITESPACE,
                    lexer::Token::PUNCT_HASH,
                ],
            },
        );
        defines.insert(
            String::from("mkstr"),
            Define {
                identifier: String::from("hash_hash"),
                parameters: Some(vec!["a".to_string()]),
                var_arg: false,
                replacement_list: vec![
                    lexer::Token::PUNCT_HASH,
                    lexer::Token::WHITESPACE,
                    lexer::Token::IDENT("a".to_string()),
                ],
            },
        );
        defines.insert(
            String::from("in_between"),
            Define {
                identifier: String::from("in_between"),
                parameters: Some(vec!["a".to_string()]),
                var_arg: false,
                replacement_list: vec![
                    lexer::Token::IDENT("mkstr".to_string()),
                    lexer::Token::PUNCT_OPEN_PAR,
                    lexer::Token::IDENT("a".to_string()),
                    lexer::Token::PUNCT_CLOSE_PAR,
                ],
            },
        );
        defines.insert(
            String::from("join"),
            Define {
                identifier: String::from("join"),
                parameters: Some(vec!["c".to_string(), "d".to_string()]),
                var_arg: false,
                replacement_list: vec![
                    lexer::Token::IDENT("c".to_string()),
                    lexer::Token::WHITESPACE,
                    lexer::Token::IDENT("hash_hash".to_string()),
                    lexer::Token::WHITESPACE,
                    lexer::Token::IDENT("d".to_string()),
                ],
            },
        );
        expand_macro(&mut tokens, &mut index, &defines)?;
        assert!(false);
        Ok(())
    }
    #[test]
    fn test_define_directive() -> Result<(), String> {
        let src = r##"#define hash_hash # ## #"##;
        let src2 = r##"#define mkstr(a) # a"##;
        let src3 = r##"#define in_between(a) mkstr(a)"##;
        let src4 = r##"#define join(c, d) in_between(c hash_hash d)"##;
        let mut tokens = lexer::lexer(src.as_bytes().to_vec(), true)?;
        let mut tokens2 = lexer::lexer(src2.as_bytes().to_vec(), true)?;
        let mut tokens3 = lexer::lexer(src3.as_bytes().to_vec(), true)?;
        let mut tokens4 = lexer::lexer(src4.as_bytes().to_vec(), true)?;
        let mut defines = HashMap::new();
        let tokens_len = tokens.len();
        let tokens2_len = tokens2.len();
        let tokens3_len = tokens3.len();
        let tokens4_len = tokens4.len();
        define_directive(&mut tokens, 0, tokens_len, &mut defines)?;
        define_directive(&mut tokens2, 0, tokens2_len, &mut defines)?;
        define_directive(&mut tokens3, 0, tokens3_len, &mut defines)?;
        define_directive(&mut tokens4, 0, tokens4_len, &mut defines)?;
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
                    lexer::Token::WHITESPACE,
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
                    lexer::Token::WHITESPACE,
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
                    lexer::Token::WHITESPACE,
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
                parameters: Some(vec!["c".to_string(),"d".to_string()]),
                var_arg: false,
                replacement_list: vec![
                    lexer::Token::WHITESPACE,
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
}
