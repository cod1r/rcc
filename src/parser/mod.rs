use crate::lexer::{self};
pub mod declarations;
pub mod expressions;

pub fn parser(tokens: &[lexer::Token]) -> Result<(), String> {
    let mut parser_index = 0;
    while parser_index < tokens.len() {
        parser_index += 1;
    }
    todo!()
}
