use crate::lexer;
pub mod declarations;
pub mod expressions;

pub struct Flattened {
    pub expressions: Vec<expressions::Expr>,
    pub type_names: Vec<declarations::TypeName>,
}

pub fn parser(tokens: &[lexer::Token]) -> Result<(), String> {
    let mut parser_index = 0;
    while parser_index < tokens.len() {
        parser_index += 1;
    }
    todo!()
}
