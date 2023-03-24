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
fn include_directive(
    tokens: &mut Vec<lexer::Token>,
    index: usize,
    end: usize,
    include_paths: &[&str],
    defines: &HashMap<String, Vec<lexer::Token>>,
) -> Result<Vec<lexer::Token>, &'static str> {
    todo!(
        "WE HAVE TO KEEP ON INCLUDING FILES AND LEXING THEM UNTIL THERE ARE NO MORE INCLUDE FILES"
    );
    let index_header_file = index + 2;
    let mut file_name = String::new();
    match &tokens[index_header_file] {
        lexer::Token::HeaderName(hn) => {
            file_name = hn.clone();
        }
        lexer::Token::IDENT(identifier) => {}
        _ => return Err("unknown token in include directive"),
    }
    for path in include_paths {
        match std::fs::read_dir(path) {
            Ok(mut ei) => {
                while let Some(entry_res) = ei.next() {
                    match entry_res {
                        Ok(entry) => {
                            let name = entry.file_name().to_string_lossy().to_string();
                            if name == file_name {}
                        }
                        Err(_) => {}
                    }
                }
            }
            Err(_) => {}
        }
    }
    todo!()
}
fn if_directive(tokens: &mut Vec<lexer::Token>) {
    todo!()
}
fn define_directive(
    tokens: &mut Vec<lexer::Token>,
    index: usize,
    end: usize,
    defines: &mut HashMap<String, Vec<lexer::Token>>,
) -> Result<(), &'static str> {
    let index_of_identifier = index + 2;
    if let Some(lexer::Token::IDENT(identifier_to_be_replaced)) = tokens.get(index_of_identifier) {
        defines.insert(
            identifier_to_be_replaced.to_string(),
            tokens[index + 3..end].to_vec(),
        );
        return Ok(());
    }
    Err("unknown token in define directive")
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
) -> Result<(), &'static str> {
    let index_of_identifier = index + 2;
    if let Some(lexer::Token::IDENT(identifier_to_be_undef)) = tokens.get(index_of_identifier) {
        defines.remove(identifier_to_be_undef);
    }
    Err("unknown token in undef directive")
}
fn preprocessing_directives(
    tokens: &mut Vec<lexer::Token>,
    include_paths: &[&str],
) -> Result<Vec<lexer::Token>, &'static str> {
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
        match tokens[index] {
            lexer::Token::PUNCT_HASH => {
                if index + 1 < tokens.len() {
                    let mut newline = index + 2;
                    while newline < tokens.len()
                        && match tokens[newline] {
                            lexer::Token::NEWLINE => false,
                            _ => true,
                        }
                    {
                        newline += 1;
                    }
                    if let Some(lexer::Token::NEWLINE) = tokens.get(newline) {
                        match &tokens[index + 1] {
                            lexer::Token::IDENT(s) => match s.as_str() {
                                "include" => {
                                    *tokens = include_directive(
                                        tokens,
                                        index,
                                        newline,
                                        include_paths,
                                        &defines,
                                    )?;
                                }
                                "if" => {
                                }
                                "ifdef" => {}
                                "ifndef" => {}
                                "define" => {}
                                "undef" => {
                                    undef_directive(tokens, index, newline, &mut defines)?;
                                }
                                "error" => {}
                                "line" => {}
                                "pragma" => {}
                                _ => {}
                            },
                            _ => {}
                        }
                        index = newline + 1;
                    }
                }
            }
            _ => {}
        }
    }
    Err("unable to preprocess")
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

    use super::comments;
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
}
