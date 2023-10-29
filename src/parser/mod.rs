use crate::lexer;
pub mod declarations;
pub mod expressions;
pub mod statements;

type ParserTypeIndex = usize;

// Some structures don't need to be in here
// because those types don't get cloned often
pub struct Flattened {
    pub expressions: Vec<expressions::Expr>,
    pub type_names: Vec<declarations::TypeName>,
    pub initializers: Vec<declarations::Initializer>,
    pub initializer_lists: Vec<Vec<declarations::InitializerList>>,
    pub designations: Vec<declarations::Designation>,
    pub abstract_declarators: Vec<declarations::AbstractDeclarator>,
    pub statements: Vec<statements::Statement>,
    pub label_statements: Vec<statements::Label>,
    pub compound_statements: Vec<statements::Compound>,
    pub iteration_statements: Vec<statements::Iteration>,
    pub selection_statements: Vec<statements::Selection>,
    pub jump_statements: Vec<statements::Jump>,
    pub declarations: Vec<declarations::Declaration>,
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
            declarations: Vec::new(),
            argument_expr_list_list: Vec::new(),
            label_statements: Vec::new(),
            compound_statements: Vec::new(),
            iteration_statements: Vec::new(),
            selection_statements: Vec::new(),
            jump_statements: Vec::new(),
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
