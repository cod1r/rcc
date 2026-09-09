use crate::lexer;
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::parser;
use crate::parser::consume_whitespace;
use crate::parser::declarations::parse_declaration_specifiers;
use crate::parser::declarations::parse_declarations;
use crate::parser::declarations::parse_declarator;
use crate::parser::statements::expected_token;
use crate::parser::statements::parse_compound_statement;
pub type TranslationUnit = Vec<ExternalDeclaration>;
pub fn parse_translation_units(
    tokens: &[lexer::Token],
    index: &mut usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<TranslationUnit, String> {
    let mut translation_units = Vec::new();
    while *index < tokens.len() {
        let external_declaration = parse_external_declarations(tokens, index, flattened, str_maps)?;
        translation_units.push(external_declaration);
        consume_whitespace(tokens, index);
    }
    Ok(translation_units)
}
pub enum ExternalDeclaration {
    FunctionDef {
        declaration_specifier: parser::declarations::DeclarationSpecifier,
        declarator: parser::declarations::Declarator,
        declaration_list: Option<Vec<parser::declarations::Declaration>>,
        compound_statement: parser::statements::Compound,
    },
    Declaration(parser::declarations::Declaration),
}
pub fn parse_external_declarations(
    tokens: &[lexer::Token],
    index: &mut usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<ExternalDeclaration, String> {
    let declaration_specifier = parse_declaration_specifiers(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    let declarator = parse_declarator(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    let Some(t) = tokens.get(*index) else {
        return Err("Unexpected end of tokens".to_string());
    };
    if parser::declarations::is_declaration_token(*t)
        || matches!(
            *t,
            Token {
                r#type: TokenType::PUNCT_OPEN_CURLY,
                ..
            }
        )
    {
        let mut declaration_list = Vec::new();
        while !matches!(
            tokens.get(*index),
            Some(Token {
                r#type: TokenType::PUNCT_OPEN_CURLY,
                ..
            }) | None
        ) {
            let declaration = parse_declarations(tokens, index, flattened, str_maps)?;
            declaration_list.push(declaration);
            consume_whitespace(tokens, index);
        }
        expected_token(tokens, str_maps, index, TokenType::PUNCT_OPEN_CURLY)?;
        let compound = parse_compound_statement(tokens, index, flattened, str_maps)?;
        expected_token(tokens, str_maps, index, TokenType::PUNCT_CLOSE_CURLY)?;
        Ok(ExternalDeclaration::FunctionDef {
            declaration_specifier,
            declarator,
            declaration_list: if !declaration_list.is_empty() {
                Some(declaration_list)
            } else {
                None
            },
            compound_statement: compound,
        })
    } else {
        let declaration = parse_declarations(tokens, index, flattened, str_maps)?;
        Ok(ExternalDeclaration::Declaration(declaration))
    }
}

#[cfg(test)]
mod tests {
    use super::{parse_external_declarations, ExternalDeclaration};
    use crate::{lexer, parser};
    #[test]
    fn parse_external_declarations_test() -> Result<(), String> {
        {
            let src = r#"void f(int hi, int hi2) {
                int deez = 4;
            }"#;
            let mut flattened = parser::Flattened::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(src.as_bytes(), false, &mut str_maps)?;
            let (external_declaration, _) =
                parse_external_declarations(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(
                external_declaration,
                ExternalDeclaration::FunctionDef { .. }
            ));
            let ExternalDeclaration::FunctionDef {
                declaration_specifier,
                declarator,
                declaration_list,
                compound_statement,
            } = external_declaration
            else {
                unreachable!()
            };
            assert!(matches!(
                declaration_specifier.type_specifiers.get(0),
                Some(parser::declarations::TypeSpecifier::Void)
            ));
            assert!(declarator.direct_declarator.is_some());
            let Some(direct_declarator) = declarator.direct_declarator else {
                unreachable!()
            };
            assert!(direct_declarator.parameter_type_list.is_some());
            let Some(ptl) = direct_declarator.parameter_type_list else {
                unreachable!()
            };
            assert!(ptl.parameter_declarations.len() == 2);
            assert!(matches!(
                ptl.parameter_declarations.get(0),
                Some(parser::declarations::ParameterDeclaration::WithDeclarator { .. })
            ));
            assert!(matches!(
                ptl.parameter_declarations.get(1),
                Some(parser::declarations::ParameterDeclaration::WithDeclarator { .. })
            ));
            assert!(declaration_list.is_none());
        }
        {
            let src = r#"void f(int hi, int hi2);"#;
            let mut flattened = parser::Flattened::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(src.as_bytes(), false, &mut str_maps)?;
            let (external_declaration, _) =
                parse_external_declarations(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(
                external_declaration,
                ExternalDeclaration::Declaration(_)
            ));
        }
        Ok(())
    }
}
