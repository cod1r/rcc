use crate::error::*;
use crate::lexer::*;
use crate::parser::consume_whitespace;
use crate::parser::declarations::*;
use crate::parser::expressions::*;
use crate::parser::*;

pub type StatementIndex = usize;
pub type LabelIndex = usize;
pub type CompoundIndex = usize;
pub type SelectionIndex = usize;
pub type IterationIndex = usize;
pub type JumpIndex = usize;
#[derive(Copy, Clone)]
pub enum Label {
    Identifier {
        identifier: usize,
        statement: StatementIndex,
    },
    Case {
        const_expr: expressions::ExpressionIndex,
        statement: StatementIndex,
    },
    Default(StatementIndex),
}
#[derive(Copy, Clone)]
pub enum BlockItem {
    Declaration(declarations::DeclarationIndex),
    Statement(StatementIndex),
}
#[derive(Clone)]
pub struct Compound {
    block_item_list: Vec<BlockItem>,
}
#[derive(Copy, Clone)]
pub struct Expression(Option<expressions::ExpressionIndex>);
#[derive(Copy, Clone)]
pub enum Selection {
    If {
        expression_index: expressions::ExpressionIndex,
        statement_index: StatementIndex,
    },
    IfElse {
        expression_index: expressions::ExpressionIndex,
        if_statement_index: StatementIndex,
        else_statement_index: StatementIndex,
    },
    Switch {
        expression_index: expressions::ExpressionIndex,
        statement_index: StatementIndex,
    },
}
#[derive(Copy, Clone)]
pub enum Iteration {
    While {
        expression_index: expressions::ExpressionIndex,
        statement_index: StatementIndex,
    },
    DoWhile {
        statement_index: StatementIndex,
        while_expression: expressions::ExpressionIndex,
    },
    ForThreeExpr {
        first_expr_index: Option<expressions::ExpressionIndex>,
        second_expr_index: Option<expressions::ExpressionIndex>,
        third_expr_index: Option<expressions::ExpressionIndex>,
        statement_index: StatementIndex,
    },
    ForDeclaration {
        declaration_index: declarations::DeclarationIndex,
        expression1: Option<expressions::ExpressionIndex>,
        expression2: Option<expressions::ExpressionIndex>,
        statement_index: StatementIndex,
    },
}
#[derive(Copy, Clone)]
pub enum Jump {
    Goto(usize),
    Continue,
    Break,
    Return(Option<expressions::ExpressionIndex>),
}
#[derive(Copy, Clone)]
pub enum Statement {
    Label(LabelIndex),
    Compound(CompoundIndex),
    Selection(SelectionIndex),
    Iteration(IterationIndex),
    Jump(JumpIndex),
}
pub fn is_statement_token(t: TokenType) -> bool {
    match t {
        TokenType::IDENT { .. } => true,
        TokenType::CASE => true,
        TokenType::DEFAULT => true,
        TokenType::OPEN_CURLY => true,
        TokenType::IF => true,
        TokenType::SWITCH => true,
        TokenType::WHILE => true,
        TokenType::DO => true,
        TokenType::FOR => true,
        TokenType::GOTO => true,
        TokenType::CONTINUE => true,
        TokenType::BREAK => true,
        TokenType::RETURN => true,
        _ => false,
    }
}
pub fn parse_statement(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Statement, String> {
    match tokens.get(*index) {
        Some(Token {
            r#type: TokenType::IDENT { .. } | TokenType::CASE | TokenType::DEFAULT,
            ..
        }) => {
            let labeled = parse_labeled_statement(tokens, index, flattened, str_maps)?;
            flattened.label_statements.push(labeled);
            Ok(Statement::Label(flattened.label_statements.len() - 1))
        }
        Some(Token {
            r#type: TokenType::OPEN_CURLY,
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            let compound = parse_compound_statement(tokens, index, flattened, str_maps)?;
            flattened.compound_statements.push(compound);
            Ok(Statement::Compound(flattened.compound_statements.len() - 1))
        }
        Some(Token {
            r#type: TokenType::IF | TokenType::SWITCH,
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            let selection = parse_selection_statement(tokens, index, flattened, str_maps)?;
            flattened.selection_statements.push(selection);
            Ok(Statement::Selection(
                flattened.selection_statements.len() - 1,
            ))
        }
        Some(Token {
            r#type: TokenType::WHILE | TokenType::DO { .. } | TokenType::FOR { .. },
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            let iteration = parse_iteration_statement(tokens, index, flattened, str_maps)?;
            flattened.iteration_statements.push(iteration);
            Ok(Statement::Iteration(
                flattened.iteration_statements.len() - 1,
            ))
        }
        Some(Token {
            r#type: TokenType::GOTO | TokenType::CONTINUE | TokenType::BREAK | TokenType::RETURN,
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            let jump = parse_jump_statement(tokens, index, flattened, str_maps)?;
            flattened.jump_statements.push(jump);
            Ok(Statement::Jump(flattened.jump_statements.len() - 1))
        }
        None => unreachable!(),
        _ => todo!("parse expression-statement: {:?}", tokens[*index]),
    }
}

pub fn parse_labeled_statement(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Label, String> {
    match tokens[*index].r#type {
        TokenType::IDENT { str_map_key, .. } => {
            *index += 1;
            consume_whitespace(tokens, index);
            expected_token(tokens, index, TokenType::COLON, "Expected ':'")?;
            let statement = parse_statement(tokens, index, flattened, str_maps)?;
            flattened.statements.push(statement);
            Ok(Label::Identifier {
                identifier: str_map_key,
                statement: flattened.statements.len() - 1,
            })
        }
        TokenType::CASE => {
            consume_whitespace(tokens, index);
            expected_token(tokens, index, TokenType::COLON, "Expected ':'")?;
            // TODO: this is a constant expression so I might need to eval it
            // to make sure the constant expression restraints are applied
            let expression = parse_expressions(tokens, index, flattened, str_maps)?;
            consume_whitespace(tokens, index);
            let statement = parse_statement(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(expression);
            flattened.statements.push(statement);
            Ok(Label::Case {
                const_expr: flattened.expressions.len() - 1,
                statement: flattened.statements.len() - 1,
            })
        }
        TokenType::DEFAULT => {
            consume_whitespace(tokens, index);
            expected_token(tokens, index, TokenType::COLON, "Expected ':'")?;
            let statement = parse_statement(tokens, index, flattened, str_maps)?;
            flattened.statements.push(statement);
            Ok(Label::Default(flattened.statements.len() - 1))
        }
        _ => unreachable!(),
    }
}

pub fn parse_compound_statement(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Compound, String> {
    let mut compound = Compound {
        block_item_list: Vec::new(),
    };
    while *index < tokens.len() {
        consume_whitespace(tokens, index);
        if let Some(token) = tokens.get(*index) {
            if is_statement_token(token.r#type) {
                let statement = parse_statement(&tokens, index, flattened, str_maps)?;
                flattened.statements.push(statement);
                compound
                    .block_item_list
                    .push(BlockItem::Statement(flattened.statements.len() - 1));
            } else if declarations::is_declaration_token(*token) {
                let declaration = parse_declarations(&tokens, index, flattened, str_maps)?;
                flattened.declarations.push(declaration);
                compound
                    .block_item_list
                    .push(BlockItem::Declaration(flattened.declarations.len() - 1));
            } else if matches!(token.r#type, TokenType::CLOSE_CURLY) {
                break;
            } else {
                unreachable!("What the fuck bro: {:?}", token);
            }
        }
        consume_whitespace(tokens, index);
    }
    expected_token(tokens, index, TokenType::CLOSE_CURLY, "Expected '}'")?;
    Ok(compound)
}

pub fn parse_selection_statement(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Selection, String> {
    match tokens.get(*index) {
        Some(Token {
            r#type: TokenType::IF,
            ..
        }) => {
            *index += 1;
            expected_token(tokens, index, TokenType::OPEN_PAR, "Expected '('")?;
            let expression = parse_expressions(tokens, index, flattened, str_maps)?;
            expected_token(tokens, index, TokenType::CLOSE_PAR, "Expected ')'")?;
            flattened.expressions.push(expression);
            consume_whitespace(tokens, index);
            let stmt = parse_statement(tokens, index, flattened, str_maps)?;
            flattened.statements.push(stmt);
            let if_statement_index = flattened.statements.len() - 1;
            consume_whitespace(tokens, index);
            if matches!(
                tokens.get(*index),
                Some(Token {
                    r#type: TokenType::ELSE,
                    ..
                })
            ) {
                *index += 1;
                let stmt2 = parse_statement(tokens, index, flattened, str_maps)?;
                flattened.statements.push(stmt2);
                let else_statement_index = flattened.statements.len() - 1;
                Ok(Selection::IfElse {
                    expression_index: flattened.expressions.len() - 1,
                    if_statement_index,
                    else_statement_index,
                })
            } else {
                Ok(Selection::If {
                    expression_index: flattened.expressions.len() - 1,
                    statement_index: if_statement_index,
                })
            }
        }
        Some(Token {
            r#type: TokenType::SWITCH,
            ..
        }) => {
            expected_token(tokens, index, TokenType::OPEN_PAR, "Expected '('")?;
            let expression = parse_expressions(&tokens, index, flattened, str_maps)?;
            expected_token(tokens, index, TokenType::CLOSE_PAR, "Expected ')'")?;
            flattened.expressions.push(expression);
            consume_whitespace(tokens, index);
            let stmt = parse_statement(tokens, index, flattened, str_maps)?;
            flattened.statements.push(stmt);
            Ok(Selection::Switch {
                expression_index: flattened.expressions.len() - 1,
                statement_index: flattened.statements.len() - 1,
            })
        }
        _ => {
            unreachable!();
        }
    }
}

pub fn parse_iteration_statement(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Iteration, String> {
    match tokens.get(*index) {
        Some(Token {
            r#type: TokenType::WHILE,
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            expected_token(tokens, index, TokenType::OPEN_PAR, "Expected '('")?;
            let expr = parse_expressions(tokens, index, flattened, str_maps)?;
            expected_token(tokens, index, TokenType::CLOSE_PAR, "Expected ')'")?;
            let expression = parse_expressions(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(expression);
            consume_whitespace(tokens, index);
            let stmt = parse_statement(tokens, index, flattened, str_maps)?;
            flattened.statements.push(stmt);
            Ok(Iteration::While {
                expression_index: flattened.expressions.len() - 1,
                statement_index: flattened.statements.len() - 1,
            })
        }
        Some(Token {
            r#type: TokenType::DO,
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            let stmt = parse_statement(tokens, index, flattened, str_maps)?;
            flattened.statements.push(stmt);
            consume_whitespace(tokens, index);
            expected_token(tokens, index, TokenType::WHILE, "Expected 'while'");
            consume_whitespace(tokens, index);
            expected_token(tokens, index, TokenType::OPEN_PAR, "Expected '('");
            let expression = parse_expressions(&tokens, index, flattened, str_maps)?;
            flattened.expressions.push(expression);
            consume_whitespace(tokens, index);
            expected_token(tokens, index, TokenType::SEMI_COLON, "Expected ';'")?;
            Ok(Iteration::DoWhile {
                while_expression: flattened.expressions.len() - 1,
                statement_index: flattened.statements.len() - 1,
            })
        }
        Some(Token {
            r#type: TokenType::FOR,
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            expected_token(tokens, index, TokenType::OPEN_PAR, "Expected '('")?;
            let Some(t) = tokens.get(*index) else {
                return Err("Unexpected end of tokens".to_string());
            };
            if is_declaration_token(*t) {
                let declaration = parse_declarations(&tokens, index, flattened, str_maps)?;
                flattened.declarations.push(declaration);
                let mut ifd = Iteration::ForDeclaration {
                    declaration_index: flattened.declarations.len() - 1,
                    expression1: None,
                    expression2: None,
                    statement_index: 0,
                };
                if !matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: TokenType::SEMI_COLON,
                        ..
                    })
                ) {
                    let expression = parse_expressions(&tokens, index, flattened, str_maps)?;
                    flattened.expressions.push(expression);
                    let Iteration::ForDeclaration { expression1, .. } = &mut ifd else {
                        unreachable!()
                    };
                    *expression1 = Some(flattened.expressions.len() - 1);
                }
                let Iteration::ForDeclaration {
                    expression2,
                    statement_index,
                    ..
                } = &mut ifd
                else {
                    unreachable!()
                };
                if *index - 1 - (*index + 1) > 0 {
                    let expression = parse_expressions(&tokens, index, flattened, str_maps)?;
                    flattened.expressions.push(expression);
                    *expression2 = Some(flattened.expressions.len() - 1);
                }
                consume_whitespace(tokens, index);
                let stmt = parse_statement(tokens, index, flattened, str_maps)?;
                flattened.statements.push(stmt);
                *statement_index = flattened.statements.len() - 1;
                Ok(ifd)
            } else {
                let mut fe = Iteration::ForThreeExpr {
                    first_expr_index: None,
                    second_expr_index: None,
                    third_expr_index: None,
                    statement_index: 0,
                };
                if !matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: TokenType::SEMI_COLON,
                        ..
                    })
                ) {
                    let expression = parse_expressions(tokens, index, flattened, str_maps)?;
                    expected_token(tokens, index, TokenType::SEMI_COLON, "Expected ';'")?;
                    flattened.expressions.push(expression);
                    let Iteration::ForThreeExpr {
                        first_expr_index, ..
                    } = &mut fe
                    else {
                        unreachable!()
                    };
                    *first_expr_index = Some(flattened.expressions.len() - 1);
                }
                expected_token(tokens, index, TokenType::SEMI_COLON, "Expected ';'")?;
                if !matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: TokenType::SEMI_COLON,
                        ..
                    })
                ) {
                    expected_token(tokens, index, TokenType::SEMI_COLON, "Expected ';'")?;
                    let expression = parse_expressions(tokens, index, flattened, str_maps)?;
                    flattened.expressions.push(expression);
                    let Iteration::ForThreeExpr {
                        second_expr_index, ..
                    } = &mut fe
                    else {
                        unreachable!()
                    };
                    *second_expr_index = Some(flattened.expressions.len() - 1);
                }
                expected_token(tokens, index, TokenType::SEMI_COLON, "Expected ';'")?;
                if !matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: TokenType::CLOSE_PAR,
                        ..
                    })
                ) {
                    let expression = parse_expressions(&tokens, index, flattened, str_maps)?;
                    flattened.expressions.push(expression);
                    let Iteration::ForThreeExpr {
                        third_expr_index, ..
                    } = &mut fe
                    else {
                        unreachable!()
                    };
                    *third_expr_index = Some(flattened.expressions.len() - 1);
                    expected_token(tokens, index, TokenType::CLOSE_PAR, "Expected ')'")?;
                }
                let Iteration::ForThreeExpr {
                    statement_index, ..
                } = &mut fe
                else {
                    unreachable!()
                };
                consume_whitespace(tokens, index);
                let stmt = parse_statement(tokens, index, flattened, str_maps)?;
                flattened.statements.push(stmt);
                *statement_index = flattened.statements.len() - 1;
                Ok(fe)
            }
        }
        _ => unreachable!(),
    }
}

pub fn parse_jump_statement(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Jump, String> {
    match tokens.get(*index) {
        Some(Token {
            r#type: TokenType::GOTO,
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            expected_identifier(tokens, str_maps, index)?;
            let Some(Token {
                r#type: TokenType::IDENT { str_map_key, .. },
                ..
            }) = tokens.get(*index - 1)
            else {
                unreachable!()
            };
            consume_whitespace(tokens, index);
            expected_token(tokens, index, TokenType::SEMI_COLON, "Expected ';'")?;
            Ok(Jump::Goto(*str_map_key))
        }
        Some(Token {
            r#type: TokenType::CONTINUE,
            ..
        }) => {
            consume_whitespace(tokens, index);
            expected_token(tokens, index, TokenType::SEMI_COLON, "Expected ';'")?;
            Ok(Jump::Continue)
        }
        Some(Token {
            r#type: TokenType::BREAK,
            ..
        }) => {
            consume_whitespace(tokens, index);
            expected_token(tokens, index, TokenType::SEMI_COLON, "Expected ';'")?;
            Ok(Jump::Break)
        }
        Some(Token {
            r#type: TokenType::RETURN,
            ..
        }) => {
            consume_whitespace(tokens, index);
            let mut r = Jump::Return(None);
            if !matches!(
                tokens.get(*index),
                Some(Token {
                    r#type: TokenType::SEMI_COLON,
                    ..
                })
            ) {
                let expr = parse_expressions(tokens, index, flattened, str_maps)?;
                expected_token(tokens, index, TokenType::SEMI_COLON, "Expected ';'")?;
                flattened.expressions.push(expr);
                r = Jump::Return(Some(flattened.expressions.len() - 1));
            }
            Ok(r)
        }
        _ => {
            unreachable!();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        parse_compound_statement, parse_iteration_statement, parse_jump_statement,
        parse_labeled_statement, parse_selection_statement, parse_statement, BlockItem, Compound,
        Iteration, Jump, Label, Selection, Statement,
    };
    use crate::lexer::*;
    use crate::parser::*;
    #[test]
    fn parse_compound_statement_test() -> Result<(), String> {
        {
            let src = r#"{ int hi = 5; }"#;
            let mut flattened = Flattened::new();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(src.as_bytes(), false, &mut str_maps)?;
            let stmt = parse_compound_statement(&tokens, &mut 1, &mut flattened, &mut str_maps)?;
            let Compound { block_item_list } = stmt;
            assert!(matches!(
                block_item_list.get(0),
                Some(BlockItem::Declaration(_))
            ));
        }
        Ok(())
    }
    #[test]
    fn parse_labeled_statement_test() -> Result<(), String> {
        {
            let src = r#"case 1 + 1 : { int hi = 5; }"#;
            let mut flattened = Flattened::new();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(src.as_bytes(), false, &mut str_maps)?;
            let label = parse_labeled_statement(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(label, Label::Case { .. }));
            let Label::Case {
                const_expr,
                statement,
            } = label
            else {
                unreachable!()
            };
            assert!(flattened.expressions.len() > const_expr);
            assert!(flattened.statements.len() > statement);
            assert!(matches!(
                flattened.statements[statement],
                Statement::Compound(_)
            ));
            let Statement::Compound(key) = flattened.statements[statement] else {
                unreachable!()
            };
            let Compound { block_item_list } = &flattened.compound_statements[key];
            assert!(matches!(
                block_item_list.get(0),
                Some(BlockItem::Declaration(_))
            ));
        }
        Ok(())
    }
    #[test]
    fn parse_selection_statement_test() -> Result<(), String> {
        {
            let src = r#"if (1 + 1) { int hi = 5; }"#;
            let mut flattened = Flattened::new();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(src.as_bytes(), false, &mut str_maps)?;
            let selection =
                parse_selection_statement(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(selection, Selection::If { .. }));
            let Selection::If {
                expression_index,
                statement_index,
            } = selection
            else {
                unreachable!()
            };
            assert!(flattened.expressions.len() > expression_index);
            assert!(flattened.statements.len() > statement_index);
            assert!(matches!(
                flattened.statements[statement_index],
                Statement::Compound(_)
            ));
            let Statement::Compound(key) = flattened.statements[statement_index] else {
                unreachable!()
            };
            let Compound { block_item_list } = &flattened.compound_statements[key];
            assert!(matches!(
                block_item_list.get(0),
                Some(BlockItem::Declaration(_))
            ));
        }
        {
            let src = r#"switch (1) {
                case 1 + 1: {
                    int hi = 5;
                }
            }"#;
            let mut flattened = Flattened::new();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(src.as_bytes(), false, &mut str_maps)?;
            let selection =
                parse_selection_statement(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(selection, Selection::Switch { .. }));
            let Selection::Switch {
                expression_index,
                statement_index,
            } = selection
            else {
                unreachable!()
            };
            assert!(flattened.expressions.len() > expression_index);
            assert!(flattened.statements.len() > statement_index);
            assert!(matches!(
                flattened.statements[statement_index],
                Statement::Compound(_)
            ));
            let Statement::Compound(key) = flattened.statements[statement_index] else {
                unreachable!()
            };
            let Compound { block_item_list } = &flattened.compound_statements[key];
            assert!(matches!(
                block_item_list.get(0),
                Some(BlockItem::Statement(_))
            ));
        }
        Ok(())
    }
    #[test]
    fn parse_iteration_statement_test() -> Result<(), String> {
        {
            let src = r#"while (1) {
                int hi = 5;
            }"#;
            let mut flattened = Flattened::new();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(src.as_bytes(), false, &mut str_maps)?;
            let iteration =
                parse_iteration_statement(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(iteration, Iteration::While { .. }));
        }
        {
            let src = r#"do {
                int hi = 5;
            } while (1);"#;
            let mut flattened = Flattened::new();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(src.as_bytes(), false, &mut str_maps)?;
            let iteration =
                parse_iteration_statement(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(iteration, Iteration::DoWhile { .. }));
        }
        {
            let src = r#"do while(1) {} while (0);"#;
            let mut flattened = Flattened::new();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(src.as_bytes(), false, &mut str_maps)?;
            let iteration =
                parse_iteration_statement(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(iteration, Iteration::DoWhile { .. }));
        }
        {
            let src = r#"for (1;1;1) {
                int hi = 5;
            }"#;
            let mut flattened = Flattened::new();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(src.as_bytes(), false, &mut str_maps)?;
            let iteration =
                parse_iteration_statement(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(iteration, Iteration::ForThreeExpr { .. }));
        }
        Ok(())
    }
    #[test]
    fn parse_jump_statement_test() -> Result<(), String> {
        {
            let src = r#"goto chicken;"#;
            let mut flattened = Flattened::new();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(src.as_bytes(), false, &mut str_maps)?;
            let jump = parse_jump_statement(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(jump, Jump::Goto(_)));
        }
        {
            let src = r#"continue;"#;
            let mut flattened = Flattened::new();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(src.as_bytes(), false, &mut str_maps)?;
            let jump = parse_jump_statement(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(jump, Jump::Continue));
        }
        {
            let src = r#"break;"#;
            let mut flattened = Flattened::new();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(src.as_bytes(), false, &mut str_maps)?;
            let jump = parse_jump_statement(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(jump, Jump::Break));
        }
        {
            let src = r#"return 1;"#;
            let mut flattened = Flattened::new();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(src.as_bytes(), false, &mut str_maps)?;
            let jump = parse_jump_statement(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(jump, Jump::Return(_)));
        }
        Ok(())
    }
}
