mod cpp;
mod lexer;
mod parser;
use std::env;
fn main() {
    let args = env::args();
    let mut files = Vec::new();
    for arg in args {
        if arg.contains(".c") && arg.len() > 2 && !files.contains(&arg) {
            files.push(arg);
        }
    }
}
