use crate::{lexer, parser};
pub fn print_abstract_syntax_tree(
    translation_units: parser::external_definitions::TranslationUnit,
    flattened: &parser::Flattened,
    str_maps: &lexer::ByteVecMaps,
) {
    match translation_units.first() {
        _ => {}
    }
    todo!()
}

fn print_tokens(tokens: lexer::Token, indent: usize) {}

fn print_declarations(declaration: parser::declarations::Declaration, indent: usize) {}

fn print_statement(statement: parser::statements::Statement, indent: usize) {}

fn print() {
}
