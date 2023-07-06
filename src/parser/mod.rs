use crate::lexer;
pub mod declarations;
pub mod expressions;

pub struct Flattened {
    pub expressions: Vec<expressions::Expr>,
    pub type_names: Vec<declarations::TypeName>,
    pub initializers: Vec<declarations::Initializer>,
    pub initializer_lists: Vec<Vec<declarations::InitializerList>>,
    pub designations: Vec<declarations::Designation>,
}

impl Flattened {
    fn new() -> Self {
        Self {
            expressions: Vec::new(),
            type_names: Vec::new(),
            initializers: Vec::new(),
            initializer_lists: Vec::new(),
            designations: Vec::new(),
        }
    }
}

pub fn parser(tokens: &[lexer::Token]) -> Result<(), String> {
    let mut parser_index = 0;
    while parser_index < tokens.len() {
        parser_index += 1;
    }
    todo!()
}
