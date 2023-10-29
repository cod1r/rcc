use crate::error;
use crate::lexer;
use crate::parser;

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
        const_expr: parser::expressions::ExpressionIndex,
        statement: StatementIndex,
    },
    Default(StatementIndex),
}
#[derive(Copy, Clone)]
pub enum BlockItem {
    Declaration(parser::declarations::DeclarationIndex),
    Statement(StatementIndex),
}
#[derive(Clone)]
pub struct Compound {
    block_item_list: Vec<BlockItem>,
}
#[derive(Copy, Clone)]
pub struct Expression(Option<parser::expressions::ExpressionIndex>);
#[derive(Copy, Clone)]
pub enum Selection {
    If {
        expression_index: parser::expressions::ExpressionIndex,
        statement_index: StatementIndex,
    },
    IfElse {
        expression_index: parser::expressions::ExpressionIndex,
        if_statement_index: StatementIndex,
        else_statement_index: StatementIndex,
    },
    Switch {
        expression_index: parser::expressions::ExpressionIndex,
        statement_index: StatementIndex,
    },
}
#[derive(Copy, Clone)]
pub enum Iteration {
    While {
        expression_index: parser::expressions::ExpressionIndex,
        statement_index: StatementIndex,
    },
    DoWhile {
        statement_index: StatementIndex,
        while_expression: parser::expressions::ExpressionIndex,
    },
    ForThreeExpr {
        first_expr_index: Option<parser::expressions::ExpressionIndex>,
        second_expr_index: Option<parser::expressions::ExpressionIndex>,
        third_expr_index: Option<parser::expressions::ExpressionIndex>,
    },
    ForDeclaration {
        declaration_index: parser::declarations::DeclarationIndex,
        expression1: Option<parser::expressions::ExpressionIndex>,
        expression2: Option<parser::expressions::ExpressionIndex>,
    },
}
#[derive(Copy, Clone)]
pub enum Jump {
    Goto(usize),
    Continue,
    Break,
    Return(Option<parser::expressions::ExpressionIndex>),
}
#[derive(Copy, Clone)]
pub enum Statement {
    Label(LabelIndex),
    Compound(CompoundIndex),
    Selection(SelectionIndex),
    Iteration(IterationIndex),
    Jump(JumpIndex),
}
pub fn is_statement_token(t: lexer::Token) -> bool {
    match t {
        lexer::Token::IDENT(_) => true,
        lexer::Token::KEYWORD_CASE => true,
        lexer::Token::KEYWORD_DEFAULT => true,
        lexer::Token::PUNCT_OPEN_CURLY => true,
        lexer::Token::KEYWORD_IF => true,
        lexer::Token::KEYWORD_SWITCH => true,
        lexer::Token::KEYWORD_WHILE => true,
        lexer::Token::KEYWORD_DO => true,
        lexer::Token::KEYWORD_FOR => true,
        lexer::Token::KEYWORD_GOTO => true,
        lexer::Token::KEYWORD_CONTINUE => true,
        lexer::Token::KEYWORD_BREAK => true,
        lexer::Token::KEYWORD_RETURN => true,
        _ => false,
    }
}
pub fn parse_statement(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(Statement, usize), String> {
    let mut idx = start_index;
    match tokens.get(idx) {
        Some(
            lexer::Token::IDENT(_) | lexer::Token::KEYWORD_CASE | lexer::Token::KEYWORD_DEFAULT,
        ) => {
            let (labeled, new_index) = parse_labeled_statement(tokens, idx, flattened, str_maps)?;
            flattened.label_statements.push(labeled);
            Ok((
                Statement::Label(flattened.label_statements.len() - 1),
                new_index,
            ))
        }
        Some(lexer::Token::PUNCT_OPEN_CURLY) => {
            let (compound, new_index) = parse_compound_statement(tokens, idx, flattened, str_maps)?;
            flattened.compound_statements.push(compound);
            Ok((
                Statement::Compound(flattened.compound_statements.len() - 1),
                new_index,
            ))
        }
        Some(lexer::Token::KEYWORD_IF | lexer::Token::KEYWORD_SWITCH) => {
            let (selection, new_index) =
                parse_selection_statement(tokens, idx, flattened, str_maps)?;
            flattened.selection_statements.push(selection);
            Ok((
                Statement::Selection(flattened.selection_statements.len() - 1),
                new_index,
            ))
        }
        Some(
            lexer::Token::KEYWORD_WHILE | lexer::Token::KEYWORD_DO | lexer::Token::KEYWORD_FOR,
        ) => {
            let (iteration, new_index) =
                parse_iteration_statement(tokens, idx, flattened, str_maps)?;
            flattened.iteration_statements.push(iteration);
            Ok((
                Statement::Iteration(flattened.iteration_statements.len() - 1),
                new_index,
            ))
        }
        Some(
            lexer::Token::KEYWORD_GOTO
            | lexer::Token::KEYWORD_CONTINUE
            | lexer::Token::KEYWORD_BREAK
            | lexer::Token::KEYWORD_RETURN,
        ) => {
            let (jump, new_index) = parse_jump_statement(tokens, idx, flattened, str_maps)?;
            flattened.jump_statements.push(jump);
            Ok((
                Statement::Jump(flattened.jump_statements.len() - 1),
                new_index,
            ))
        }
        _ => todo!("parse expression-statement"),
    }
}
pub fn parse_labeled_statement(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(Label, usize), String> {
    let mut label_idx = start_index;
    match tokens[label_idx] {
        lexer::Token::IDENT(key) => {
            label_idx += 1;
            while matches!(
                tokens.get(label_idx),
                Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
            ) && label_idx < tokens.len()
            {
                label_idx += 1;
            }
            if !matches!(tokens.get(label_idx), Some(lexer::Token::PUNCT_COLON)) {
                let Some(bv) = lexer::Token::PUNCT_COLON.to_byte_vec(str_maps) else {
                    unreachable!()
                };
                let Ok(s) = String::from_utf8(bv) else {
                    unreachable!()
                };
                return Err(error::RccErrorInfo::new(
                    error::RccError::ExpectedToken(s),
                    label_idx..label_idx + 1,
                    tokens,
                    str_maps,
                )
                .to_string());
            }
            let (statement, new_index) =
                parse_statement(tokens, label_idx + 1, flattened, str_maps)?;
            flattened.statements.push(statement);
            Ok((
                Label::Identifier {
                    identifier: key,
                    statement: flattened.statements.len() - 1,
                },
                new_index,
            ))
        }
        lexer::Token::KEYWORD_CASE => {
            loop {
                label_idx += 1;
                if matches!(
                    tokens.get(label_idx),
                    Some(lexer::Token::PUNCT_COLON) | None
                ) && label_idx < tokens.len()
                {
                    break;
                }
            }
            if !matches!(tokens.get(label_idx), Some(lexer::Token::PUNCT_COLON)) {
                let Some(bv) = lexer::Token::PUNCT_COLON.to_byte_vec(str_maps) else {
                    unreachable!()
                };
                let Ok(s) = String::from_utf8(bv) else {
                    unreachable!()
                };
                return Err(error::RccErrorInfo::new(
                    error::RccError::ExpectedToken(s),
                    label_idx..label_idx + 1,
                    tokens,
                    str_maps,
                )
                .to_string());
            }
            let (_, expression) = parser::expressions::parse_expressions(
                &tokens[start_index + 1..label_idx],
                0,
                flattened,
                str_maps,
            )?;
            loop {
                label_idx += 1;
                if !matches!(
                    tokens.get(label_idx),
                    Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                ) && label_idx < tokens.len()
                {
                    break;
                }
            }
            let (statement, new_index) = parse_statement(tokens, label_idx, flattened, str_maps)?;
            flattened.expressions.push(expression);
            flattened.statements.push(statement);
            Ok((
                Label::Case {
                    const_expr: flattened.expressions.len() - 1,
                    statement: flattened.statements.len() - 1,
                },
                new_index,
            ))
        }
        lexer::Token::KEYWORD_DEFAULT => {
            label_idx += 1;
            while matches!(
                tokens.get(label_idx),
                Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
            ) && label_idx < tokens.len()
            {
                label_idx += 1;
            }
            if !matches!(tokens.get(label_idx), Some(lexer::Token::PUNCT_COLON)) {
                let Some(bv) = lexer::Token::PUNCT_COLON.to_byte_vec(str_maps) else {
                    unreachable!()
                };
                let Ok(s) = String::from_utf8(bv) else {
                    unreachable!()
                };
                return Err(error::RccErrorInfo::new(
                    error::RccError::ExpectedToken(s),
                    label_idx..label_idx + 1,
                    tokens,
                    str_maps,
                )
                .to_string());
            }
            let (statement, new_index) =
                parse_statement(tokens, label_idx + 1, flattened, str_maps)?;
            flattened.statements.push(statement);
            Ok((Label::Default(flattened.statements.len() - 1), new_index))
        }
        _ => unreachable!(),
    }
}
pub fn parse_compound_statement(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(Compound, usize), String> {
    let mut idx = start_index;
    if !matches!(
        tokens.get(start_index),
        Some(lexer::Token::PUNCT_OPEN_CURLY)
    ) {
        return Err(error::RccErrorInfo::new(
            error::RccError::Custom("Expected {".to_string()),
            start_index..start_index + 1,
            tokens,
            str_maps,
        )
        .to_string());
    }
    loop {
        idx += 1; // first is guaranteed to be open curly
        if !matches!(
            tokens.get(idx),
            Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE),
        ) {
            break;
        }
    }
    let mut curly_balancer = 1;
    let mut curly_balance_idx = idx;
    while curly_balancer > 0 {
        match tokens.get(curly_balance_idx) {
            Some(lexer::Token::PUNCT_OPEN_CURLY) => curly_balancer += 1,
            Some(lexer::Token::PUNCT_CLOSE_CURLY) => curly_balancer -= 1,
            None => {
                return Err(error::RccErrorInfo::new(
                    error::RccError::Custom("Unexpected end of tokens".to_string()),
                    idx..curly_balance_idx + 1,
                    tokens,
                    str_maps,
                )
                .to_string())
            }
            _ => {}
        }
        curly_balance_idx += 1;
    }
    if !matches!(
        tokens.get(curly_balance_idx - 1),
        Some(lexer::Token::PUNCT_CLOSE_CURLY)
    ) {
        return Err(error::RccErrorInfo::new(
            error::RccError::ExpectedToken("{".to_string()),
            idx..curly_balance_idx + 1,
            tokens,
            str_maps,
        )
        .to_string());
    }
    let mut compound = Compound {
        block_item_list: Vec::new(),
    };
    let mut compound_idx = idx;
    let end_compound = curly_balance_idx - 1;
    while compound_idx < end_compound {
        if let Some(t) = tokens.get(compound_idx) {
            if is_statement_token(*t) {
                let (statement, new_index_offset) =
                    parse_statement(&tokens[idx..end_compound], 0, flattened, str_maps)?;
                flattened.statements.push(statement);
                compound
                    .block_item_list
                    .push(BlockItem::Statement(flattened.statements.len() - 1));
                compound_idx += new_index_offset;
            } else if parser::declarations::is_declaration_token(*t) {
                let (declaration, new_index_offset) = parser::declarations::parse_declarations(
                    &tokens[idx..end_compound],
                    0,
                    flattened,
                    str_maps,
                )?;
                flattened.declarations.push(declaration);
                compound
                    .block_item_list
                    .push(BlockItem::Declaration(flattened.declarations.len() - 1));
                compound_idx += new_index_offset;
            } else {
                unreachable!("What the fuck bro: {:?}", t);
            }
        }
        while matches!(
            tokens.get(compound_idx),
            Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
        ) {
            compound_idx += 1;
        }
    }
    Ok((compound, curly_balance_idx))
}
pub fn parse_selection_statement(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(Selection, usize), String> {
    todo!()
}
pub fn parse_iteration_statement(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(Iteration, usize), String> {
    todo!()
}
pub fn parse_jump_statement(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(Jump, usize), String> {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::{
        parse_compound_statement, parse_labeled_statement, parse_statement, BlockItem, Compound,
        Label, Statement,
    };
    use crate::{lexer, parser};
    #[test]
    fn parse_compound_statement_test() -> Result<(), String> {
        {
            let src = r#"{ int hi = 5; }"#;
            let mut flattened = parser::Flattened::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(src.as_bytes(), false, &mut str_maps)?;
            let (stmt, _) = parse_compound_statement(&tokens, 0, &mut flattened, &mut str_maps)?;
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
            let mut flattened = parser::Flattened::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(src.as_bytes(), false, &mut str_maps)?;
            let (label, _) = parse_labeled_statement(&tokens, 0, &mut flattened, &mut str_maps)?;
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
}
