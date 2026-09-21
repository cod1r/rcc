use crate::lexer::*;
use crate::parser::declarations::*;
use crate::parser::statements::*;
use crate::parser::*;
pub type TranslationUnit = Vec<ExternalDeclaration>;
pub fn parse_translation_units(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
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
        declaration_specifier: DeclarationSpecifier,
        declarator: Declarator,
        declaration_list: Option<Vec<Declaration>>,
        compound_statement: Compound,
    },
    Declaration(Declaration),
}
pub fn parse_external_declarations(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<ExternalDeclaration, String> {
    let declaration_specifier = parse_declaration_specifiers(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    let declarator = parse_declarator(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    let Some(t) = tokens.get(*index) else {
        return Err("Unexpected end of tokens".to_string());
    };
    if is_declaration_token(*t)
        || matches!(
            *t,
            Token {
                r#type: TokenType::OPEN_CURLY,
                ..
            }
        )
    {
        let mut declaration_list = Vec::new();
        while !matches!(
            tokens.get(*index),
            Some(Token {
                r#type: TokenType::OPEN_CURLY,
                ..
            }) | None
        ) {
            let declaration = parse_declarations(tokens, index, flattened, str_maps)?;
            declaration_list.push(declaration);
            consume_whitespace(tokens, index);
        }
        expected_token(tokens, index, TokenType::OPEN_CURLY, "Expected '{'")?;
        let compound = parse_compound_statement(tokens, index, flattened, str_maps)?;
        expected_token(tokens, index, TokenType::CLOSE_CURLY, "Expected '}'")?;
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
            let external_declaration =
                parse_external_declarations(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
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
            let external_declaration =
                parse_external_declarations(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(
                external_declaration,
                ExternalDeclaration::Declaration(_)
            ));
        }
        Ok(())
    }
}
