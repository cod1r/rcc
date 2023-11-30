mod cpp;
mod error;
mod lexer;
mod parser;
mod semantic_analysis;
use std::collections::HashMap;
use std::env;

#[derive(Debug, PartialEq)]
struct ArgumentInfo {
    files: Vec<String>,
    include_paths: Vec<String>,
    library_paths: Vec<String>,
}
fn parse_args(args: Vec<String>) -> Result<ArgumentInfo, String> {
    let mut arg_info = ArgumentInfo {
        files: Vec::new(),
        include_paths: Vec::new(),
        library_paths: Vec::new(),
    };
    enum State {
        File,
        IncludePath,
        LibraryPath,
    }
    let mut flag_state = State::File;
    if args.len() <= 2 && !args.is_empty() {
        if args[0].as_str() == "--help"
            || (args.len() > 1 && args[1].as_str() == "--help")
            || args.len() == 1
        {
            return Err(format!("Usage: rcc <file1> <file2> ... [--include|--library] <include/library path 1> <include/library path 2> ... "));
        }
    }
    for arg in args {
        match &flag_state {
            State::File => match arg.as_str() {
                "--include" => flag_state = State::IncludePath,
                "--library" => flag_state = State::LibraryPath,
                _ => {
                    if arg.ends_with(".c") || arg.ends_with(".h") {
                        if arg.len() == 2 {
                            return Err(format!(
                                "file {} needs to have a name that is longer than two characters",
                                arg
                            ));
                        }
                        if arg_info.files.contains(&arg) {
                            return Err(format!("file {} already passed in", arg));
                        }
                        arg_info.files.push(arg);
                    }
                }
            },
            State::IncludePath => match arg.as_str() {
                "--library" => {
                    flag_state = State::LibraryPath;
                }
                _ => match std::fs::metadata(&arg) {
                    Ok(metadata) => {
                        if !metadata.is_dir() {
                            return Err(format!("{} is not a directory", arg));
                        }
                        arg_info.include_paths.push(arg);
                    }
                    Err(err) => {
                        return Err(err.to_string());
                    }
                },
            },
            State::LibraryPath => match arg.as_str() {
                "--include" => {
                    flag_state = State::IncludePath;
                }
                _ => match std::fs::metadata(&arg) {
                    Ok(metadata) => {
                        if !metadata.is_dir() {
                            return Err(format!("{} is not a directory", arg));
                        }
                        arg_info.library_paths.push(arg);
                    }
                    Err(err) => {
                        return Err(err.to_string());
                    }
                },
            },
        }
    }
    if arg_info.files.is_empty() {
        return Err(format!("no input files passed in"));
    }
    Ok(arg_info)
}

// TODO: add flag to enable trigraphs
fn main() {
    let args = env::args();
    let arg_info = match parse_args(args.collect::<Vec<String>>()) {
        Ok(a) => a,
        Err(err) => {
            eprintln!("{err}");
            return;
        }
    };
    let mut defines: HashMap<usize, cpp::Define> = HashMap::new();
    let mut str_maps = lexer::ByteVecMaps::new();
    let stdc_version = cpp::Define {
        parameters: None,
        var_arg: false,
        replacement_list: vec![lexer::Token::CONSTANT_DEC_INT {
            value_key: str_maps.add_byte_vec("201710".as_bytes()),
            suffix: Some(lexer::Suffix::Integer {
                integer_type: lexer::IntegerSuffix::Long,
                key: str_maps.add_byte_vec("L".as_bytes()),
            }),
        }],
    };
    defines.insert(
        str_maps.add_byte_vec("__STDC_VERSION__".as_bytes()),
        stdc_version,
    );
    let vec_of_str = arg_info
        .include_paths
        .iter()
        .map(|s| s.as_str())
        .collect::<Vec<&str>>();
    for file in &arg_info.files {
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
                let Ok(pathbuf) = std::fs::canonicalize(file) else {
                    unreachable!()
                };
                let path = pathbuf.as_path().to_string_lossy().to_string();
                let tokens = match cpp::cpp(
                    contents,
                    path.as_str(),
                    &vec_of_str,
                    &mut defines,
                    &mut str_maps,
                ) {
                    Ok(tks) => tks,
                    Err(err) => {
                        eprintln!("{err}");
                        return;
                    }
                };
                let mut new_tokens = Vec::new();
                for t in tokens {
                    let Some(byte_vec) = t.to_byte_vec(&str_maps) else {
                        panic!("We should not have a token that isn't to_byte_vec-able")
                    };
                    new_tokens.extend_from_slice(byte_vec.as_slice());
                }
                let tokens = new_tokens;
                let tokens = match lexer::lexer(tokens.as_slice(), false, &mut str_maps) {
                    Ok(tks) => tks,
                    Err(err) => {
                        eprintln!("{err}");
                        return;
                    }
                };
                cpp::output_tokens_stdout(tokens.as_slice(), &str_maps);
                //match parser::parser(&tokens, &mut str_maps) {
                //    Ok(tu) => {
                //    }
                //    Err(err) => {
                //        eprintln!("{err}");
                //        return;
                //    }
                //}
            }
            Err(_) => println!("error"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{parse_args, ArgumentInfo};
    #[test]
    fn parse_arg_test() -> Result<(), String> {
        let vec_args = vec![
            String::from("rcc"),
            String::from("temp.c"),
            String::from("--include"),
            String::from("/usr"),
        ];
        let arg_info = parse_args(vec_args)?;
        assert_eq!(
            arg_info,
            ArgumentInfo {
                files: vec![String::from("temp.c")],
                include_paths: vec![String::from("/usr")],
                library_paths: vec![],
            }
        );
        Ok(())
    }
}
