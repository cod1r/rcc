use std::collections::HashMap;

use crate::lexer;

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
    defines: &HashMap<String, Vec<lexer::Token>>,
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
            if let Some(replacement) = defines.get(identifier) {
                if let Some(new_file_name) = get_header_name_from_tokens(replacement) {
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
    let index_identifier = index + 2;
    let first_newline_index = index_identifier + 1;
    if !matches!(
        tokens.get(index_identifier + 1),
        Some(lexer::Token::NEWLINE)
    ) {
        return Err(format!("unknown token in ifdef directive"));
    }
    let mut index_search = index_identifier + 1;
    let mut if_parts: Vec<(&str, usize)> = Vec::with_capacity(2);
    while index_search < tokens.len() {
        if let [Some(lexer::Token::NEWLINE), Some(lexer::Token::PUNCT_HASH), Some(lexer::Token::IDENT(id))] = [
            tokens.get(index_search),
            tokens.get(index_search + 1),
            tokens.get(index_search + 2),
        ] {
            let index_of_punct_hash = index_search + 1;
            match id.as_str() {
                "elif" => {
                    if_parts.push(("elif", index_of_punct_hash));
                }
                "else" if !matches!(tokens.get(index_search + 3), Some(lexer::Token::NEWLINE)) => {
                    if_parts.push(("else", index_of_punct_hash));
                }
                "endif" if !matches!(tokens.get(index_search + 3), Some(lexer::Token::NEWLINE)) => {
                    if_parts.push(("endif", index_of_punct_hash));
                    break;
                }
                _ => {}
            }
        }
        index_search += 1;
    }
    if let Some(lexer::Token::IDENT(identifier)) = tokens.get(index_identifier) {
        if defines.contains_key(identifier) {
            loop {
                if let Some(lexer::Token::NEWLINE) = tokens.get(first_newline_index) {
                    tokens.remove(first_newline_index);
                    break;
                }
                tokens.remove(first_newline_index);
            }
        } else {
        }
    }
    todo!()
}
fn define_directive(
    tokens: &mut Vec<lexer::Token>,
    index: usize,
    end: usize,
    defines: &mut HashMap<String, Vec<lexer::Token>>,
) -> Result<(), String> {
    let index_of_identifier = index + 2;
    if let Some(lexer::Token::IDENT(identifier_to_be_replaced)) = tokens.get(index_of_identifier) {
        defines.insert(
            identifier_to_be_replaced.to_string(),
            tokens[index + 3..end].to_vec(),
        );
        loop {
            if let lexer::Token::NEWLINE = tokens[index] {
                tokens.remove(index);
                break;
            }
            tokens.remove(index);
        }
        return Ok(());
    }
    Err(String::from("unknown token in define directive"))
}
fn error_directive(tokens: &mut Vec<lexer::Token>) {
    todo!()
}
fn line_directive(
    tokens: &mut Vec<lexer::Token>,
    index: usize,
    end: usize,
) -> Result<(), &'static str> {
    todo!()
}
fn undef_directive(
    tokens: &mut Vec<lexer::Token>,
    index: usize,
    end: usize,
    defines: &mut HashMap<String, Vec<lexer::Token>>,
) -> Result<(), String> {
    let index_of_identifier = index + 2;
    if let Some(lexer::Token::IDENT(identifier_to_be_undef)) = tokens.get(index_of_identifier) {
        defines.remove(identifier_to_be_undef);
    }
    Err(String::from("unknown token in undef directive"))
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
        match &mut tokens[index] {
            lexer::Token::PUNCT_HASH => {
                if index + 1 < tokens.len() {
                    let mut newline = index + 2;
                    while newline < tokens.len()
                        && !matches!(tokens[newline], lexer::Token::NEWLINE)
                    {
                        newline += 1;
                    }
                    if let Some(lexer::Token::NEWLINE) = tokens.get(newline) {
                        if let lexer::Token::IDENT(s) = &tokens[index + 1] {
                            match s.as_str() {
                                "include" => {
                                    include_directive(
                                        tokens,
                                        index,
                                        newline,
                                        include_paths,
                                        &defines,
                                    )?;
                                }
                                "if" => {}
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
                    }
                }
            }
            lexer::Token::IDENT(id) => {
                if let Some(replacement) = defines.get(id) {
                    tokens.remove(index);
                    for t in replacement {
                        tokens.insert(index, t.clone());
                        index += 1;
                    }
                } else {
                    index += 1;
                }
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

    use crate::lexer::{self, lexer};
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
        let mut tokens = lexer(src.as_bytes().to_vec(), true)?;
        let mut defines = HashMap::new();
        include_directive(&mut tokens, 0, 18, &["./test_c_files/"], &mut defines)?;
        let assert_tokens = [
            lexer::Token::IDENT(String::from("int")),
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
        let mut tokens = lexer(src.as_bytes().to_vec(), true)?;
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
