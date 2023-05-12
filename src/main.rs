mod cpp;
mod lexer;
mod parser;
use std::collections::HashMap;
use std::env;

// TODO: add flag to enable trigraphs
fn main() -> Result<(), String> {
    let args = env::args();
    let mut files = Vec::new();
    for arg in args {
        if arg.ends_with(".c") && arg.len() > 2 && !files.contains(&arg) {
            files.push(arg);
        }
    }
    let mut defines = HashMap::new();
    let stdc_version = cpp::Define {
        identifier: "__STDC_VERSION__".to_string(),
        parameters: None,
        var_arg: false,
        replacement_list: vec![lexer::Token::CONSTANT_DEC_INT {
            value: "201710".to_string(),
            suffix: Some("L".to_string()),
        }],
    };
    defines.insert("__STDC_VERSION__".to_string(), stdc_version);
    let include_paths = &[
        "./test_c_files",
        "/usr/include",
        "/usr/include/linux",
        "/usr/lib/gcc/x86_64-pc-linux-gnu/13.1.1/include",
    ];
    for file in files {
        let define = cpp::Define {
            identifier: "__FILE__".to_string(),
            parameters: None,
            var_arg: false,
            replacement_list: vec![lexer::Token::StringLiteral {
                prefix: None,
                sequence: file.to_string(),
            }],
        };
        defines.insert("__FILE__".to_string(), define);
        match std::fs::read(file) {
            Ok(contents) => {
                let tokens = cpp::cpp(contents, include_paths, &mut defines)?;
                cpp::output_tokens_stdout(&tokens);
            }
            Err(_) => println!("error"),
        }
    }
    Ok(())
}
