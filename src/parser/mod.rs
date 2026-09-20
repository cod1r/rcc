pub mod abstract_syntax_tree;
pub mod declarations;
pub mod expressions;
pub mod external_definitions;
pub mod statements;
use crate::lexer::*;
type ParserTypeIndex = usize;

pub fn consume_whitespace(tokens: &[Token], index: &mut usize) {
    while matches!(
        tokens.get(*index),
        Some(Token {
            r#type: TokenType::WHITESPACE | TokenType::NEWLINE,
            ..
        })
    ) {
        *index += 1;
    }
}

pub fn expected_token(
    tokens: &[Token],
    str_maps: &mut ByteVecMaps,
    idx: &mut usize,
    token: TokenType,
) -> Result<(), String> {
    let dummy_token = Token {
        r#type: token,
        location: None,
    };
    let msg = format!(
        "Expected '{}'",
        match String::from_utf8(dummy_token.to_byte_vec(str_maps).unwrap()) {
            Ok(s) => s,
            Err(_) => {
                return Err("Could not convert token to string".to_string());
            }
        }
    );
    match tokens.get(*idx) {
        Some(t) if t.r#type != token => {
            let Token {
                location: Some(Location { column, line }),
                ..
            } = t
            else {
                unreachable!()
            };
            return Err(error(&msg, *line, *column));
        }
        None => return Err(msg),
        _ => {}
    };
    *idx += 1;
    Ok(())
}

pub fn consume_token(tokens: &[Token], index: &mut usize, token: TokenType) -> Result<(), String> {
    if matches!(tokens.get(*index), Some(Token { r#type: token, .. })) {
        *index += 1;
    }
}

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
    tokens: &[Token],
    str_maps: &mut ByteVecMaps,
) -> Result<external_definitions::TranslationUnit, String> {
    // TODO: we need to finish parsing statements or syntax that encapsulates a lot of things
    let mut flattened = Flattened::new();
    let mut index = 0;
    let translation_units = external_definitions::parse_translation_units(
        tokens,
        &mut index,
        &mut flattened,
        str_maps,
    )?;
    Ok(translation_units)
}
