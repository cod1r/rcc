use std::collections::HashMap;

use crate::lexer;

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
    let index_of_identifier = index + 2;
    let mut def_data = Define {
        identifier: String::new(),
        parameters: None,
        var_arg: false,
        replacement_list: Vec::new(),
    };
    if let Some([lexer::Token::IDENT(id), lexer::Token::PUNCT_OPEN_PAR]) =
        tokens.get(index_of_identifier..index_of_identifier + 2)
    {
        def_data.identifier = id.to_string();
        let start = index_of_identifier + 1;
        let mut fn_like_macro_index = start + 1;
        while matches!(
            tokens.get(fn_like_macro_index),
            Some(lexer::Token::IDENT(_)) | Some(lexer::Token::PUNCT_COMMA)
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
            return Ok(());
        } else if matches!(
            tokens.get(fn_like_macro_index..fn_like_macro_index + 2),
            Some([lexer::Token::PUNCT_ELLIPSIS, lexer::Token::PUNCT_CLOSE_PAR])
        ) {
            def_data.var_arg = true;
            def_data
                .replacement_list
                .extend_from_slice(&tokens[fn_like_macro_index + 2..end]);
            defines.insert(def_data.identifier.clone(), def_data);
            return Ok(());
        }
    } else if let Some(lexer::Token::IDENT(id)) = tokens.get(index_of_identifier) {
        def_data
            .replacement_list
            .extend_from_slice(&tokens[index_of_identifier + 1..end]);
        defines.insert(def_data.identifier.clone(), def_data);
        return Ok(());
    }
    Err(String::from("define directive not properly formed"))
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
    let mut index_copy = *index;
    let macro_id = match &tokens[index_copy] {
        lexer::Token::IDENT(id) => id,
        _ => return Err(format!("non identifier token given")),
    };
    if let Some(def_data) = defines.get(macro_id) {
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
                            while !matches!(
                                tokens.get(fn_like_macro_index),
                                Some(lexer::Token::PUNCT_CLOSE_PAR)
                            ) {
                                fn_like_macro_index += 1;
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
                                if slice
                                    .iter()
                                    .filter(|t| !matches!(t, lexer::Token::WHITESPACE))
                                    .count()
                                    > 0
                                {
                                    seen_args.push(slice.to_vec());
                                }
                            }
                            beginning_of_argument_index = fn_like_macro_index + 1;
                        }
                        _ => {}
                    }
                    fn_like_macro_index += 1;
                }
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
                                if replacement_list_index > 0
                                    && matches!(
                                        replacement_list_copy[replacement_list_index - 1],
                                        lexer::Token::PUNCT_HASH
                                    )
                                {
                                    let mut removal_index = replacement_list_index;
                                    replacement_list_copy.remove(removal_index - 1);
                                    removal_index -= 1;
                                    replacement_list_copy.remove(removal_index);
                                    let mut string_literal_token = lexer::Token::StringLiteral {
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
                                        &mut string_literal_token else { todo!() };
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
                                    let mut replacement_list_index_copy = replacement_list_index;
                                    for t in argument {
                                        replacement_list_copy
                                            .insert(replacement_list_index_copy, t.clone());
                                        replacement_list_index_copy += 1;
                                    }
                                    replaced_indices.push(replacement_list_index);
                                }
                            }
                            _ => {}
                        }
                    }
                }
                let mut length = fn_like_macro_index - *index + 1;
                while length > 0 {
                    tokens.remove(*index);
                    length -= 1;
                }
                let mut insert_index = *index;
                for t in replacement_list_copy {
                    tokens.insert(insert_index, t);
                    insert_index += 1;
                }
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
                length -= 1;
            }
            let mut insert_index = *index;
            for t in def_data.replacement_list.clone() {
                tokens.insert(insert_index, t);
                insert_index += 1;
            }
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
                    index += 1;
                    if let Some(lexer::Token::WHITESPACE) = tokens.get(index) {
                        index += 1;
                    }
                    if let Some(lexer::Token::IDENT(s)) = tokens.get(index) {
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

    use super::{comments, include_directive, preprocessing_directives};
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
}
