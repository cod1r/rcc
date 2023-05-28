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
                cpp::output_tokens_stdout(&tokens, &str_maps);
            }
            Err(_) => println!("error"),
        }
    }
    Ok(())
}
