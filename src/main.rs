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
    let include_paths = &["./test_c_files"];
    for file in files {
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
