mod lexer;
use std::env;
use std::error::Error;
fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if env::args().len() < 2 {
        panic!("number of args low");
    }
    let file_contents = std::fs::read_to_string(args[1].trim())?;
    Ok(())
}
