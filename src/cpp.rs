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
    tokens: &[lexer::Token],
    include_paths: &[&str],
    defines: &HashMap<&str, &str>,
) -> Result<Vec<lexer::Token>, &'static str> {
    // 2 because index 0 and index 1 are the # and "include" tokens
    let mut index: usize = 2;
    let mut file_name;
    match &tokens[index] {
        lexer::Token::PUNCT_LESS_THAN => {}
        lexer::Token::StringLiteral { prefix, sequence } => {
            if prefix.is_some() {
                return Err("unknown token in include directive");
            }
            file_name = sequence.clone();
        }
        lexer::Token::IDENT(identifier) => {}
        _ => return Err("unknown token in include directive"),
    }
    for path in include_paths {
        match std::fs::read_dir(path) {
            Ok(mut e) => {
                while let Some(entry) = e.next() {
                    match entry {
                        Ok(aha) => {}
                        Err(_) => {}
                    }
                }
            }
            Err(_) => {}
        }
    }
    todo!()
}
fn if_directive(tokens: &[lexer::Token]) {}
fn define_directive(tokens: &[lexer::Token]) {}
fn error_directive(tokens: &[lexer::Token]) {}
fn line_directive(tokens: &[lexer::Token]) {}
fn undef_directive(tokens: &[lexer::Token]) {}
fn preprocessing_directives(
    tokens: &[lexer::Token],
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
                    match &tokens[index + 1] {
                        lexer::Token::IDENT(s) => match s.as_str() {
                            "include" => {
                                include_directive(
                                    &tokens[index..newline],
                                    include_paths,
                                    &defines,
                                )?;
                            }
                            "if" => {}
                            "ifdef" => {}
                            "ifndef" => {}
                            "define" => {}
                            "undef" => {}
                            "error" => {}
                            "line" => {}
                            "pragma" => {}
                            _ => {}
                        },
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
    todo!()
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
