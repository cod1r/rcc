use crate::lexer;
use crate::parser;
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
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(ExternalDeclaration, usize), String> {
    let mut external_definition_idx = start_index;
    let (declaration_specifier, new_index) = parser::declarations::parse_declaration_specifiers(
        tokens,
        external_definition_idx,
        flattened,
        str_maps,
    )?;
    external_definition_idx = new_index;
    while matches!(
        tokens.get(external_definition_idx),
        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
    ) {
        external_definition_idx += 1;
    }
    let (new_index, declarator) = parser::declarations::parse_declarator(
        tokens,
        external_definition_idx,
        flattened,
        str_maps,
    )?;
    external_definition_idx = new_index;
    while matches!(
        tokens.get(external_definition_idx),
        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
    ) {
        external_definition_idx += 1;
    }
    let Some(t) = tokens.get(external_definition_idx) else {
        return Err("Unexpected end of tokens".to_string());
    };
    if parser::declarations::is_declaration_token(*t)
        || matches!(*t, lexer::Token::PUNCT_OPEN_CURLY)
    {
        let mut declaration_list = Vec::new();
        while !matches!(
            tokens.get(external_definition_idx),
            Some(lexer::Token::PUNCT_OPEN_CURLY) | None
        ) {
            let (declaration, new_index) = parser::declarations::parse_declarations(
                tokens,
                external_definition_idx,
                flattened,
                str_maps,
            )?;
            declaration_list.push(declaration);
            external_definition_idx = new_index;
            while matches!(
                tokens.get(external_definition_idx),
                Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
            ) {
                external_definition_idx += 1;
            }
        }
        if matches!(
            tokens.get(external_definition_idx),
            Some(lexer::Token::PUNCT_OPEN_CURLY)
        ) {
            let (compound, new_index) = parser::statements::parse_compound_statement(
                tokens,
                external_definition_idx,
                flattened,
                str_maps,
            )?;
            external_definition_idx = new_index;
            Ok((
                ExternalDeclaration::FunctionDef {
                    declaration_specifier,
                    declarator,
                    declaration_list: if !declaration_list.is_empty() {
                        Some(declaration_list)
                    } else {
                        None
                    },
                    compound_statement: compound,
                },
                external_definition_idx,
            ))
        } else {
            return Err("Expected {".to_string());
        }
    } else {
        let (declaration, new_index) =
            parser::declarations::parse_declarations(tokens, start_index, flattened, str_maps)?;
        external_definition_idx = new_index;
        Ok((
            ExternalDeclaration::Declaration(declaration),
            external_definition_idx,
        ))
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
        Ok(())
    }
}
