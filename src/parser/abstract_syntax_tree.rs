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
        match external_dec {
            ExternalDeclaration::FunctionDef {
                declaration_specifier,
                declarator,
                declaration_list,
                compound_statement,
            } => {}
            ExternalDeclaration::Declaration(declaration) => {}
        }
    }
}
fn print_declaration_specifiers(
    declaration_specifier: &parser::declarations::DeclarationSpecifier,
) {
    println!("Storage Class Specifiers:");
    for scs in &declaration_specifier.storage_class_specifiers {
        println!("{}", scs);
    }
    println!("Type Specifiers:");
    for ts in &declaration_specifier.type_specifiers {
        println!("{}", ts);
    }
    println!("Type Qualifiers:");
    for tq in &declaration_specifier.type_qualifiers {
        println!("{}", tq);
    }
    println!("Function specifiers:");
    for fs in &declaration_specifier.function_specifiers {
        println!("{fs}");
    }
}

fn print_tokens(tokens: lexer::Token, indent: usize) {}

fn print_declarations(declaration: parser::declarations::Declaration, indent: usize) {}

fn print_statement(statement: parser::statements::Statement, indent: usize) {}
