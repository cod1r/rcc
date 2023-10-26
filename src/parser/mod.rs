use crate::lexer;
pub mod declarations;
pub mod expressions;
pub mod statements;

type ParserTypeIndex = usize;

pub struct Flattened {
    pub expressions: Vec<expressions::Expr>,
    pub type_names: Vec<declarations::TypeName>,
    pub initializers: Vec<declarations::Initializer>,
    pub initializer_lists: Vec<Vec<declarations::InitializerList>>,
    pub designations: Vec<declarations::Designation>,
    pub abstract_declarators: Vec<declarations::AbstractDeclarator>,
    pub statements: Vec<statements::Statement>,
    pub argument_expr_list_list: Vec<Vec<expressions::Expr>>,
}

impl Flattened {
    fn new() -> Self {
        Self {
            expressions: Vec::new(),
            type_names: Vec::new(),
            initializers: Vec::new(),
            initializer_lists: Vec::new(),
            designations: Vec::new(),
            abstract_declarators: Vec::new(),
            statements: Vec::new(),
            argument_expr_list_list: Vec::new(),
        }
    }
}

pub enum ParserTypes {}

pub fn parser(
    tokens: &[lexer::Token],
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<ParserTypes, String> {
    // TODO: we need to finish parsing statements or syntax that encapsulates a lot of things
    let mut flattened = Flattened::new();
    let mut parser_index = 0;
    while parser_index < tokens.len() {}
    todo!()
}
