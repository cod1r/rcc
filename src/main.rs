mod cpp;
mod lexer;
mod parser;
use std::collections::HashMap;
use std::env;

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
                Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
            ) && adjacent_string_lit_index < tokens.len()
            {
                adjacent_string_lit_index += 1;
            }
            while let Some(lexer::Token::StringLiteral(second_string_lit)) =
                tokens.get(adjacent_string_lit_index)
            {
                match (prev_prefix, second_string_lit.prefix_key) {
                    (Some(prev_key), Some(second_prefix_key)) => {
                        let first_prefix = str_maps.key_to_byte_vec[prev_key].as_slice();
                        let second_prefix = str_maps.key_to_byte_vec[second_prefix_key].as_slice();
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
                adjacent_string_lit_index += 1;
            }
            adjacent_strings_concated.push(lexer::Token::StringLiteral(lexer::StringLiteral {
                prefix_key: prev_prefix,
                sequence_key: str_maps.add_byte_vec(first_byte_vec.as_slice()),
            }));
            token_string_concated_index = adjacent_string_lit_index;
            continue;
        }
        if token_string_concated_index < tokens.len() {
            adjacent_strings_concated.push(tokens[token_string_concated_index]);
        }
        token_string_concated_index += 1;
    }
    Ok(adjacent_strings_concated)
}

// TODO: add flag to enable trigraphs
fn main() -> Result<(), String> {
    let args = env::args();
    let mut files = Vec::new();
    for arg in args {
        if (arg.ends_with(".c") || arg.ends_with(".h")) && arg.len() > 2 && !files.contains(&arg) {
            files.push(arg);
        }
    }
    let mut defines: HashMap<usize, cpp::Define> = HashMap::new();
    let mut str_maps = lexer::ByteVecMaps::new();
    let stdc_version = cpp::Define {
        parameters: None,
        var_arg: false,
        replacement_list: vec![lexer::Token::CONSTANT_DEC_INT {
            value_key: str_maps.add_byte_vec("201710".as_bytes()),
            suffix_key: Some(str_maps.add_byte_vec("L".as_bytes())),
        }],
    };
    defines.insert(
        str_maps.add_byte_vec("__STDC_VERSION__".as_bytes()),
        stdc_version,
    );
    let include_paths = &[
        "./test_c_files",
        "/usr/include",
        "/usr/include/linux",
        "/usr/lib/gcc/x86_64-pc-linux-gnu/13.1.1/include",
    ];
    for file in files {
        let define = cpp::Define {
            parameters: None,
            var_arg: false,
            replacement_list: vec![lexer::Token::StringLiteral(lexer::StringLiteral {
                prefix_key: None,
                sequence_key: str_maps.add_byte_vec(file.as_bytes()),
            })],
        };
        defines.insert(str_maps.add_byte_vec("__FILE__".as_bytes()), define);
        match std::fs::read(file) {
            Ok(contents) => {
                let tokens = cpp::cpp(contents, include_paths, &mut defines, &mut str_maps)?;
                // concatenating adjacent string literals together
                let tokens = concat_adjacent_strings(tokens.as_slice(), &mut str_maps)?;
            }
            Err(_) => println!("error"),
        }
    }
    Ok(())
}
