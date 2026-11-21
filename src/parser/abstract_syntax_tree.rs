use crate::{lexer, parser};
pub fn print_abstract_syntax_tree(
    translation_units: parser::external_definitions::TranslationUnit,
    flattened: &parser::Flattened,
    str_maps: &lexer::ByteVecMaps,
) {
    use parser::external_definitions::ExternalDeclaration;
    println!("Translation Unit:");
    for external_dec in translation_units {
        println!("External Declaration:");
    }
}

fn print_tokens(tokens: lexer::Token, indent: usize) {}

fn print_declarations(declaration: parser::declarations::Declaration, indent: usize) {}

fn print_statement(statement: parser::statements::Statement, indent: usize) {}
