pub mod abstract_syntax_tree;
pub mod declarations;
pub mod expressions;
pub mod external_definitions;
pub mod statements;
use crate::error::*;
use crate::lexer::*;
use crate::parser::declarations::*;
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

pub fn consume_specifically_spaces(tokens: &[Token], index: &mut usize) {
    while matches!(
        tokens.get(*index),
        Some(Token {
            r#type: TokenType::WHITESPACE,
            ..
        })
    ) {
        *index += 1;
    }
}

pub fn expected_token(
    tokens: &[Token],
    idx: &mut usize,
    token: TokenType,
    msg: &'static str,
) -> Result<(), String> {
    match tokens.get(*idx) {
        Some(t) if t.r#type != token => {
            let Token {
                location: Some(Location { column, line }),
                ..
            } = t
            else {
                unreachable!()
            };
            return Err(error(msg, *line, *column));
        }
        None => return Err(msg.to_string()),
        _ => {}
    };
    *idx += 1;
    Ok(())
}

pub fn consume_token(tokens: &[Token], index: &mut usize, token: TokenType) -> Result<(), String> {
    if matches!(tokens.get(*index), Some(Token { r#type: token, .. })) {
        *index += 1;
    }
    Ok(())
}

pub fn expected_identifier(
    tokens: &[Token],
    str_maps: &mut ByteVecMaps,
    idx: &mut usize,
) -> Result<(), String> {
    match tokens.get(*idx) {
        Some(t) if !matches!(t.r#type, TokenType::IDENT { .. }) => {
            let Token {
                location: Some(Location { column, line }),
                ..
            } = t
            else {
                unreachable!()
            };
            let msg = format!("Expected an identifier",);
            return Err(error(&msg, *line, *column));
        }
        None => {
            let msg = format!("Expected an identifier",);
            return Err(msg);
        }
        _ => {}
    };
    *idx += 1;
    Ok(())
}

// Some structures don't need to be in here
// because those types don't get cloned often
pub struct Flattened {
    pub expressions: Vec<expressions::Expr>,
    pub type_names: Vec<declarations::TypeName>,
    pub initializers: Vec<declarations::Initializer>,
    pub initializer_lists: Vec<InitializerList>,
    pub designations: Vec<declarations::Designation>,
    pub abstract_declarators: Vec<declarations::AbstractDeclarator>,
    pub statements: Vec<statements::Statement>,
    pub label_statements: Vec<statements::Label>,
    pub compound_statements: Vec<statements::Compound>,
    pub iteration_statements: Vec<statements::Iteration>,
    pub selection_statements: Vec<statements::Selection>,
    pub jump_statements: Vec<statements::Jump>,
    pub declarations: Vec<declarations::Declaration>,
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
