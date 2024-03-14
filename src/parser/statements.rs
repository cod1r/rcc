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
        statement_index: StatementIndex,
    },
    ForDeclaration {
        declaration_index: parser::declarations::DeclarationIndex,
        expression1: Option<parser::expressions::ExpressionIndex>,
        expression2: Option<parser::expressions::ExpressionIndex>,
        statement_index: StatementIndex,
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
        lexer::Token::IDENT { .. } => true,
        lexer::Token::KEYWORD_CASE { .. } => true,
        lexer::Token::KEYWORD_DEFAULT { .. } => true,
        lexer::Token::PUNCT_OPEN_CURLY { .. } => true,
        lexer::Token::KEYWORD_IF { .. } => true,
        lexer::Token::KEYWORD_SWITCH { .. } => true,
        lexer::Token::KEYWORD_WHILE { .. } => true,
        lexer::Token::KEYWORD_DO { .. } => true,
        lexer::Token::KEYWORD_FOR { .. } => true,
        lexer::Token::KEYWORD_GOTO { .. } => true,
        lexer::Token::KEYWORD_CONTINUE { .. } => true,
        lexer::Token::KEYWORD_BREAK { .. } => true,
        lexer::Token::KEYWORD_RETURN { .. } => true,
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
            lexer::Token::IDENT { .. }
            | lexer::Token::KEYWORD_CASE { .. }
            | lexer::Token::KEYWORD_DEFAULT { .. },
        ) => {
            let (labeled, new_index) = parse_labeled_statement(tokens, idx, flattened, str_maps)?;
            flattened.label_statements.push(labeled);
            Ok((
                Statement::Label(flattened.label_statements.len() - 1),
                new_index,
            ))
        }
        Some(lexer::Token::PUNCT_OPEN_CURLY { .. }) => {
            let (compound, new_index) = parse_compound_statement(tokens, idx, flattened, str_maps)?;
            flattened.compound_statements.push(compound);
            Ok((
                Statement::Compound(flattened.compound_statements.len() - 1),
                new_index,
            ))
        }
        Some(lexer::Token::KEYWORD_IF { .. } | lexer::Token::KEYWORD_SWITCH { .. }) => {
            let (selection, new_index) =
                parse_selection_statement(tokens, idx, flattened, str_maps)?;
            flattened.selection_statements.push(selection);
            Ok((
                Statement::Selection(flattened.selection_statements.len() - 1),
                new_index,
            ))
        }
        Some(
            lexer::Token::KEYWORD_WHILE { .. }
            | lexer::Token::KEYWORD_DO { .. }
            | lexer::Token::KEYWORD_FOR { .. },
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
            lexer::Token::KEYWORD_GOTO { .. }
            | lexer::Token::KEYWORD_CONTINUE { .. }
            | lexer::Token::KEYWORD_BREAK { .. }
            | lexer::Token::KEYWORD_RETURN { .. },
        ) => {
            let (jump, new_index) = parse_jump_statement(tokens, idx, flattened, str_maps)?;
            flattened.jump_statements.push(jump);
            Ok((
                Statement::Jump(flattened.jump_statements.len() - 1),
                new_index,
            ))
        }
        None => unreachable!(),
        _ => todo!("parse expression-statement: {:?}", tokens[idx]),
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
        lexer::Token::IDENT { str_map_key, .. } => {
            label_idx += 1;
            while matches!(
                tokens.get(label_idx),
                Some(lexer::Token::WHITESPACE { .. } | lexer::Token::NEWLINE { .. })
            ) && label_idx < tokens.len()
            {
                label_idx += 1;
            }
            if !matches!(
                tokens.get(label_idx),
                Some(lexer::Token::PUNCT_COLON { .. })
            ) {
                todo!("ERROR HERE")
            }
            let (statement, new_index) =
                parse_statement(tokens, label_idx + 1, flattened, str_maps)?;
            flattened.statements.push(statement);
            Ok((
                Label::Identifier {
                    identifier: str_map_key,
                    statement: flattened.statements.len() - 1,
                },
                new_index,
            ))
        }
        lexer::Token::KEYWORD_CASE { .. } => {
            loop {
                label_idx += 1;
                if matches!(
                    tokens.get(label_idx),
                    Some(lexer::Token::PUNCT_COLON { .. }) | None
                ) && label_idx < tokens.len()
                {
                    break;
                }
            }
            if !matches!(
                tokens.get(label_idx),
                Some(lexer::Token::PUNCT_COLON { .. })
            ) {
                todo!("ERROR HERE")
            }
            // TODO: this is a constant expression so I might need to eval it
            // to make sure the constant expression restraints are applied
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
                    Some(lexer::Token::WHITESPACE { .. } | lexer::Token::NEWLINE { .. })
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
        lexer::Token::KEYWORD_DEFAULT { .. } => {
            label_idx += 1;
            while matches!(
                tokens.get(label_idx),
                Some(lexer::Token::WHITESPACE { .. } | lexer::Token::NEWLINE { .. })
            ) && label_idx < tokens.len()
            {
                label_idx += 1;
            }
            if !matches!(
                tokens.get(label_idx),
                Some(lexer::Token::PUNCT_COLON { .. })
            ) {
                todo!("ERROR HERE")
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
        Some(lexer::Token::PUNCT_OPEN_CURLY { .. })
    ) {
        todo!("ERROR HERE")
    }
    loop {
        idx += 1; // first is guaranteed to be open curly
        if !matches!(
            tokens.get(idx),
            Some(lexer::Token::WHITESPACE { .. } | lexer::Token::NEWLINE { .. }),
        ) {
            break;
        }
    }
    let mut curly_balancer = 1;
    let mut curly_balance_idx = idx;
    while curly_balancer > 0 {
        match tokens.get(curly_balance_idx) {
            Some(lexer::Token::PUNCT_OPEN_CURLY { .. }) => curly_balancer += 1,
            Some(lexer::Token::PUNCT_CLOSE_CURLY { .. }) => curly_balancer -= 1,
            None => {
                todo!("ERROR HERE")
            }
            _ => {}
        }
        curly_balance_idx += 1;
    }
    if !matches!(
        tokens.get(curly_balance_idx - 1),
        Some(lexer::Token::PUNCT_CLOSE_CURLY { .. })
    ) {
        todo!("ERROR HERE")
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
            Some(lexer::Token::WHITESPACE { .. } | lexer::Token::NEWLINE { .. })
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
    let mut selection_idx = start_index;
    match tokens.get(selection_idx) {
        Some(lexer::Token::KEYWORD_IF { .. }) => {
            loop {
                selection_idx += 1;
                if matches!(
                    tokens.get(selection_idx),
                    Some(lexer::Token::PUNCT_OPEN_PAR { .. }) | None
                ) {
                    break;
                }
            }
            if !matches!(
                tokens.get(selection_idx),
                Some(lexer::Token::PUNCT_OPEN_PAR { .. })
            ) {
                return Err("EXPECTED (".to_string());
            }
            selection_idx += 1;
            let start = selection_idx;
            let mut parenth_bal = 1;
            while parenth_bal > 0 {
                match tokens.get(selection_idx) {
                    Some(lexer::Token::PUNCT_OPEN_PAR { .. }) => {
                        parenth_bal += 1;
                    }
                    Some(lexer::Token::PUNCT_CLOSE_PAR { .. }) => {
                        parenth_bal -= 1;
                    }
                    Some(_) => {}
                    None => {
                        return Err("UNBALANCED".to_string());
                    }
                }
                selection_idx += 1;
            }
            if !matches!(
                tokens.get(selection_idx - 1),
                Some(lexer::Token::PUNCT_CLOSE_PAR { .. })
            ) {
                return Err("UNBALANCED".to_string());
            }
            let (_, expression) = parser::expressions::parse_expressions(
                &tokens[start..selection_idx - 1],
                0,
                flattened,
                str_maps,
            )?;
            flattened.expressions.push(expression);
            while matches!(
                tokens.get(selection_idx),
                Some(lexer::Token::WHITESPACE { .. } | lexer::Token::NEWLINE { .. })
            ) {
                selection_idx += 1;
            }
            let (stmt, new_index) = parse_statement(tokens, selection_idx, flattened, str_maps)?;
            flattened.statements.push(stmt);
            let if_statement_index = flattened.statements.len() - 1;
            selection_idx = new_index;
            while matches!(
                tokens.get(selection_idx),
                Some(lexer::Token::WHITESPACE { .. } | lexer::Token::NEWLINE { .. })
            ) {
                selection_idx += 1;
            }
            if matches!(
                tokens.get(selection_idx),
                Some(lexer::Token::KEYWORD_ELSE { .. })
            ) {
                selection_idx += 1;
                let (stmt2, new_index) =
                    parse_statement(tokens, selection_idx, flattened, str_maps)?;
                flattened.statements.push(stmt2);
                let else_statement_index = flattened.statements.len() - 1;
                selection_idx = new_index;
                Ok((
                    Selection::IfElse {
                        expression_index: flattened.expressions.len() - 1,
                        if_statement_index,
                        else_statement_index,
                    },
                    selection_idx,
                ))
            } else {
                Ok((
                    Selection::If {
                        expression_index: flattened.expressions.len() - 1,
                        statement_index: if_statement_index,
                    },
                    selection_idx,
                ))
            }
        }
        Some(lexer::Token::KEYWORD_SWITCH { .. }) => {
            loop {
                selection_idx += 1;
                if matches!(
                    tokens.get(selection_idx),
                    Some(lexer::Token::PUNCT_OPEN_PAR { .. }) | None
                ) {
                    break;
                }
            }
            if !matches!(
                tokens.get(selection_idx),
                Some(lexer::Token::PUNCT_OPEN_PAR { .. })
            ) {
                return Err("EXPECTED (".to_string());
            }
            selection_idx += 1;
            let start = selection_idx;
            let mut parenth_bal = 1;
            while parenth_bal > 0 {
                match tokens.get(selection_idx) {
                    Some(lexer::Token::PUNCT_OPEN_PAR { .. }) => {
                        parenth_bal += 1;
                    }
                    Some(lexer::Token::PUNCT_CLOSE_PAR { .. }) => {
                        parenth_bal -= 1;
                    }
                    Some(_) => {}
                    None => {
                        return Err("UNBALANCED".to_string());
                    }
                }
                selection_idx += 1;
            }
            if !matches!(
                tokens.get(selection_idx - 1),
                Some(lexer::Token::PUNCT_CLOSE_PAR { .. })
            ) {
                return Err("UNBALANCED".to_string());
            }
            let (_, expression) = parser::expressions::parse_expressions(
                &tokens[start..selection_idx - 1],
                0,
                flattened,
                str_maps,
            )?;
            flattened.expressions.push(expression);
            while matches!(
                tokens.get(selection_idx),
                Some(lexer::Token::WHITESPACE { .. } | lexer::Token::NEWLINE { .. })
            ) {
                selection_idx += 1;
            }
            let (stmt, new_index) = parse_statement(tokens, selection_idx, flattened, str_maps)?;
            flattened.statements.push(stmt);
            selection_idx = new_index;
            Ok((
                Selection::Switch {
                    expression_index: flattened.expressions.len() - 1,
                    statement_index: flattened.statements.len() - 1,
                },
                selection_idx,
            ))
        }
        _ => {
            return Err("EXPECTED IF OR SWITCH".to_string());
        }
    }
}
pub fn parse_iteration_statement(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(Iteration, usize), String> {
    let mut iteration_idx = start_index;
    match tokens.get(iteration_idx) {
        Some(lexer::Token::KEYWORD_WHILE { .. }) => {
            loop {
                iteration_idx += 1;
                if matches!(
                    tokens.get(iteration_idx),
                    Some(lexer::Token::PUNCT_OPEN_PAR { .. }) | None
                ) {
                    break;
                }
            }
            if !matches!(
                tokens.get(iteration_idx),
                Some(lexer::Token::PUNCT_OPEN_PAR { .. })
            ) {
                return Err("EXPECTED (".to_string());
            }
            iteration_idx += 1;
            let start = iteration_idx;
            let mut parenth_bal = 1;
            while parenth_bal > 0 {
                match tokens.get(iteration_idx) {
                    Some(lexer::Token::PUNCT_OPEN_PAR { .. }) => {
                        parenth_bal += 1;
                    }
                    Some(lexer::Token::PUNCT_CLOSE_PAR { .. }) => {
                        parenth_bal -= 1;
                    }
                    Some(_) => {}
                    None => {
                        return Err("UNBALANCED".to_string());
                    }
                }
                iteration_idx += 1;
            }
            if !matches!(
                tokens.get(iteration_idx - 1),
                Some(lexer::Token::PUNCT_CLOSE_PAR { .. })
            ) {
                return Err("UNBALANCED".to_string());
            }
            let (_, expression) = parser::expressions::parse_expressions(
                &tokens[start..iteration_idx - 1],
                0,
                flattened,
                str_maps,
            )?;
            flattened.expressions.push(expression);
            while matches!(
                tokens.get(iteration_idx),
                Some(lexer::Token::WHITESPACE { .. } | lexer::Token::NEWLINE { .. })
            ) {
                iteration_idx += 1;
            }
            let (stmt, new_index) = parse_statement(tokens, iteration_idx, flattened, str_maps)?;
            flattened.statements.push(stmt);
            iteration_idx = new_index;
            Ok((
                Iteration::While {
                    expression_index: flattened.expressions.len() - 1,
                    statement_index: flattened.statements.len() - 1,
                },
                iteration_idx,
            ))
        }
        Some(lexer::Token::KEYWORD_DO { .. }) => {
            iteration_idx += 1;
            while matches!(
                tokens.get(iteration_idx),
                Some(lexer::Token::WHITESPACE { .. } | lexer::Token::NEWLINE { .. })
            ) {
                iteration_idx += 1;
            }
            let (stmt, new_index) = parse_statement(tokens, iteration_idx, flattened, str_maps)?;
            flattened.statements.push(stmt);
            iteration_idx = new_index;
            while matches!(
                tokens.get(iteration_idx),
                Some(lexer::Token::WHITESPACE { .. } | lexer::Token::NEWLINE { .. })
            ) {
                iteration_idx += 1;
            }
            if !matches!(
                tokens.get(iteration_idx),
                Some(lexer::Token::KEYWORD_WHILE { .. })
            ) {
                return Err("EXPECTED WHILE".to_string());
            }
            loop {
                iteration_idx += 1;
                if matches!(
                    tokens.get(iteration_idx),
                    Some(lexer::Token::PUNCT_OPEN_PAR { .. }) | None
                ) {
                    break;
                }
            }
            if !matches!(
                tokens.get(iteration_idx),
                Some(lexer::Token::PUNCT_OPEN_PAR { .. })
            ) {
                return Err("EXPECTED (".to_string());
            }
            iteration_idx += 1;
            let start = iteration_idx;
            let mut parenth_bal = 1;
            while parenth_bal > 0 {
                match tokens.get(iteration_idx) {
                    Some(lexer::Token::PUNCT_OPEN_PAR { .. }) => {
                        parenth_bal += 1;
                    }
                    Some(lexer::Token::PUNCT_CLOSE_PAR { .. }) => {
                        parenth_bal -= 1;
                    }
                    Some(_) => {}
                    None => {
                        return Err("UNBALANCED".to_string());
                    }
                }
                iteration_idx += 1;
            }
            if !matches!(
                tokens.get(iteration_idx - 1),
                Some(lexer::Token::PUNCT_CLOSE_PAR { .. })
            ) {
                return Err("UNBALANCED".to_string());
            }
            let (_, expression) = parser::expressions::parse_expressions(
                &tokens[start..iteration_idx - 1],
                0,
                flattened,
                str_maps,
            )?;
            flattened.expressions.push(expression);
            while matches!(
                tokens.get(iteration_idx),
                Some(lexer::Token::WHITESPACE { .. } | lexer::Token::NEWLINE { .. })
            ) {
                iteration_idx += 1;
            }
            if !matches!(
                tokens.get(iteration_idx),
                Some(lexer::Token::PUNCT_SEMI_COLON { .. })
            ) {
                return Err("EXPECTED ;".to_string());
            }
            iteration_idx += 1;
            Ok((
                Iteration::DoWhile {
                    while_expression: flattened.expressions.len() - 1,
                    statement_index: flattened.statements.len() - 1,
                },
                iteration_idx,
            ))
        }
        Some(lexer::Token::KEYWORD_FOR { .. }) => {
            loop {
                iteration_idx += 1;
                if matches!(
                    tokens.get(iteration_idx),
                    Some(lexer::Token::PUNCT_OPEN_PAR { .. }) | None
                ) {
                    break;
                }
            }
            if !matches!(
                tokens.get(iteration_idx),
                Some(lexer::Token::PUNCT_OPEN_PAR { .. })
            ) {
                return Err("EXPECTED (".to_string());
            }
            iteration_idx += 1;
            let start = iteration_idx;
            let mut until_first_semi_colon = start + 1;
            let mut parenth_bal = 1;
            while parenth_bal > 0 {
                match tokens.get(iteration_idx) {
                    Some(lexer::Token::PUNCT_OPEN_PAR { .. }) => {
                        parenth_bal += 1;
                    }
                    Some(lexer::Token::PUNCT_CLOSE_PAR { .. }) => {
                        parenth_bal -= 1;
                    }
                    Some(_) => {}
                    None => {
                        return Err("UNBALANCED".to_string());
                    }
                }
                iteration_idx += 1;
            }
            if !matches!(
                tokens.get(iteration_idx - 1),
                Some(lexer::Token::PUNCT_CLOSE_PAR { .. })
            ) {
                return Err("UNBALANCED".to_string());
            }
            while matches!(
                tokens.get(until_first_semi_colon),
                Some(lexer::Token::WHITESPACE { .. } | lexer::Token::NEWLINE { .. })
            ) {
                until_first_semi_colon += 1;
            }
            let Some(t) = tokens.get(until_first_semi_colon) else {
                return Err("end no good".to_string());
            };
            if parser::declarations::is_declaration_token(*t) {
                let (declaration, new_index) = parser::declarations::parse_declarations(
                    &tokens[start..until_first_semi_colon],
                    0,
                    flattened,
                    str_maps,
                )?;
                until_first_semi_colon = new_index;
                flattened.declarations.push(declaration);
                let mut ifd = Iteration::ForDeclaration {
                    declaration_index: flattened.declarations.len() - 1,
                    expression1: None,
                    expression2: None,
                    statement_index: 0,
                };
                while matches!(
                    tokens.get(until_first_semi_colon),
                    Some(lexer::Token::WHITESPACE { .. } | lexer::Token::NEWLINE { .. })
                ) {
                    until_first_semi_colon += 1;
                }
                if !matches!(
                    tokens.get(until_first_semi_colon),
                    Some(lexer::Token::PUNCT_SEMI_COLON { .. })
                ) {
                    let start = until_first_semi_colon;
                    while !matches!(
                        tokens.get(until_first_semi_colon),
                        Some(lexer::Token::PUNCT_SEMI_COLON { .. }) | None
                    ) {
                        until_first_semi_colon += 1;
                    }
                    if !matches!(
                        tokens.get(until_first_semi_colon),
                        Some(lexer::Token::PUNCT_SEMI_COLON { .. })
                    ) {
                        return Err("missing ;".to_string());
                    }
                    let (_, expression) = parser::expressions::parse_expressions(
                        &tokens[start..until_first_semi_colon],
                        0,
                        flattened,
                        str_maps,
                    )?;
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
                if iteration_idx - 1 - (until_first_semi_colon + 1) > 0 {
                    let (_, expression) = parser::expressions::parse_expressions(
                        &tokens[until_first_semi_colon + 1..iteration_idx - 1],
                        0,
                        flattened,
                        str_maps,
                    )?;
                    flattened.expressions.push(expression);
                    *expression2 = Some(flattened.expressions.len() - 1);
                }
                while matches!(
                    tokens.get(iteration_idx),
                    Some(lexer::Token::WHITESPACE { .. } | lexer::Token::NEWLINE { .. })
                ) {
                    iteration_idx += 1;
                }
                let (stmt, new_index) =
                    parse_statement(tokens, iteration_idx, flattened, str_maps)?;
                iteration_idx = new_index;
                flattened.statements.push(stmt);
                *statement_index = flattened.statements.len() - 1;
                Ok((ifd, iteration_idx))
            } else {
                let mut fe = Iteration::ForThreeExpr {
                    first_expr_index: None,
                    second_expr_index: None,
                    third_expr_index: None,
                    statement_index: 0,
                };
                if !matches!(
                    tokens.get(until_first_semi_colon),
                    Some(lexer::Token::PUNCT_SEMI_COLON { .. })
                ) {
                    let start = until_first_semi_colon;
                    while !matches!(
                        tokens.get(until_first_semi_colon),
                        Some(lexer::Token::PUNCT_SEMI_COLON { .. }) | None
                    ) {
                        until_first_semi_colon += 1;
                    }
                    if !matches!(
                        tokens.get(until_first_semi_colon),
                        Some(lexer::Token::PUNCT_SEMI_COLON { .. })
                    ) {
                        return Err("missing ;".to_string());
                    }
                    let (_, expression) = parser::expressions::parse_expressions(
                        &tokens[start..until_first_semi_colon],
                        0,
                        flattened,
                        str_maps,
                    )?;
                    flattened.expressions.push(expression);
                    let Iteration::ForThreeExpr {
                        first_expr_index, ..
                    } = &mut fe
                    else {
                        unreachable!()
                    };
                    *first_expr_index = Some(flattened.expressions.len() - 1);
                }
                if !matches!(
                    tokens.get(until_first_semi_colon),
                    Some(lexer::Token::PUNCT_SEMI_COLON { .. })
                ) {
                    return Err("missing ;".to_string());
                }
                until_first_semi_colon += 1;
                if !matches!(
                    tokens.get(until_first_semi_colon),
                    Some(lexer::Token::PUNCT_SEMI_COLON { .. })
                ) {
                    let start = until_first_semi_colon;
                    while !matches!(
                        tokens.get(until_first_semi_colon),
                        Some(lexer::Token::PUNCT_SEMI_COLON { .. }) | None
                    ) {
                        until_first_semi_colon += 1;
                    }
                    if !matches!(
                        tokens.get(until_first_semi_colon),
                        Some(lexer::Token::PUNCT_SEMI_COLON { .. })
                    ) {
                        return Err("missing ;".to_string());
                    }
                    let (_, expression) = parser::expressions::parse_expressions(
                        &tokens[start..until_first_semi_colon],
                        0,
                        flattened,
                        str_maps,
                    )?;
                    flattened.expressions.push(expression);
                    let Iteration::ForThreeExpr {
                        second_expr_index, ..
                    } = &mut fe
                    else {
                        unreachable!()
                    };
                    *second_expr_index = Some(flattened.expressions.len() - 1);
                }
                if !matches!(
                    tokens.get(until_first_semi_colon),
                    Some(lexer::Token::PUNCT_SEMI_COLON { .. })
                ) {
                    return Err("missing ;".to_string());
                }
                until_first_semi_colon += 1;
                let mut until_close_par = until_first_semi_colon;
                if !matches!(
                    tokens.get(until_close_par),
                    Some(lexer::Token::PUNCT_CLOSE_PAR { .. })
                ) {
                    let start = until_close_par;
                    while !matches!(
                        tokens.get(until_close_par),
                        Some(lexer::Token::PUNCT_CLOSE_PAR { .. }) | None
                    ) {
                        until_close_par += 1;
                    }
                    if !matches!(
                        tokens.get(until_close_par),
                        Some(lexer::Token::PUNCT_CLOSE_PAR { .. })
                    ) {
                        return Err("missing ;".to_string());
                    }
                    let (_, expression) = parser::expressions::parse_expressions(
                        &tokens[start..until_close_par],
                        0,
                        flattened,
                        str_maps,
                    )?;
                    flattened.expressions.push(expression);
                    let Iteration::ForThreeExpr {
                        third_expr_index, ..
                    } = &mut fe
                    else {
                        unreachable!()
                    };
                    *third_expr_index = Some(flattened.expressions.len() - 1);
                }
                let Iteration::ForThreeExpr {
                    statement_index, ..
                } = &mut fe
                else {
                    unreachable!()
                };
                while matches!(
                    tokens.get(iteration_idx),
                    Some(lexer::Token::WHITESPACE { .. } | lexer::Token::NEWLINE { .. })
                ) {
                    iteration_idx += 1;
                }
                let (stmt, new_index) =
                    parse_statement(tokens, iteration_idx, flattened, str_maps)?;
                iteration_idx = new_index;
                flattened.statements.push(stmt);
                *statement_index = flattened.statements.len() - 1;
                Ok((fe, iteration_idx))
            }
        }
        _ => unreachable!(),
    }
}
pub fn parse_jump_statement(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(Jump, usize), String> {
    let mut jump_idx = start_index;
    match tokens.get(jump_idx) {
        Some(lexer::Token::KEYWORD_GOTO { .. }) => {
            loop {
                jump_idx += 1;
                if matches!(
                    tokens.get(jump_idx),
                    Some(lexer::Token::IDENT { .. }) | None
                ) {
                    break;
                }
            }
            if !matches!(tokens.get(jump_idx), Some(lexer::Token::IDENT { .. })) {
                return Err("EXPECTED IDENTIFIER".to_string());
            }
            let Some(lexer::Token::IDENT { str_map_key, .. }) = tokens.get(jump_idx) else {
                unreachable!()
            };
            jump_idx += 1;
            while matches!(
                tokens.get(jump_idx),
                Some(lexer::Token::WHITESPACE { .. } | lexer::Token::NEWLINE { .. })
            ) {
                jump_idx += 1;
            }
            if !matches!(
                tokens.get(jump_idx),
                Some(lexer::Token::PUNCT_SEMI_COLON { .. })
            ) {
                return Err("EXPECTED ;".to_string());
            }
            jump_idx += 1;
            Ok((Jump::Goto(*str_map_key), jump_idx))
        }
        Some(lexer::Token::KEYWORD_CONTINUE { .. }) => {
            loop {
                jump_idx += 1;
                if matches!(
                    tokens.get(jump_idx),
                    Some(lexer::Token::PUNCT_SEMI_COLON { .. }) | None
                ) {
                    break;
                }
            }
            if !matches!(
                tokens.get(jump_idx),
                Some(lexer::Token::PUNCT_SEMI_COLON { .. })
            ) {
                return Err("EXPECTED ;".to_string());
            }
            jump_idx += 1;
            Ok((Jump::Continue, jump_idx))
        }
        Some(lexer::Token::KEYWORD_BREAK { .. }) => {
            loop {
                jump_idx += 1;
                if matches!(
                    tokens.get(jump_idx),
                    Some(lexer::Token::PUNCT_SEMI_COLON { .. }) | None
                ) {
                    break;
                }
            }
            if !matches!(
                tokens.get(jump_idx),
                Some(lexer::Token::PUNCT_SEMI_COLON { .. })
            ) {
                return Err("EXPECTED ;".to_string());
            }
            jump_idx += 1;
            Ok((Jump::Break, jump_idx))
        }
        Some(lexer::Token::KEYWORD_RETURN { .. }) => {
            loop {
                jump_idx += 1;
                if !matches!(
                    tokens.get(jump_idx),
                    Some(lexer::Token::WHITESPACE { .. } | lexer::Token::NEWLINE { .. })
                ) && matches!(tokens.get(jump_idx), Some(_) | None)
                {
                    break;
                }
            }
            let mut r = Jump::Return(None);
            if !matches!(
                tokens.get(jump_idx),
                Some(lexer::Token::PUNCT_SEMI_COLON { .. })
            ) {
                let start = jump_idx;
                while !matches!(
                    tokens.get(jump_idx),
                    Some(lexer::Token::PUNCT_SEMI_COLON { .. }) | None,
                ) {
                    jump_idx += 1;
                }
                if !matches!(
                    tokens.get(jump_idx),
                    Some(lexer::Token::PUNCT_SEMI_COLON { .. })
                ) {
                    return Err("EXPECTED ;".to_string());
                }
                let (_, expr) = parser::expressions::parse_expressions(
                    &tokens[start..jump_idx],
                    0,
                    flattened,
                    str_maps,
                )?;
                flattened.expressions.push(expr);
                r = Jump::Return(Some(flattened.expressions.len() - 1));
            }
            jump_idx += 1;
            Ok((r, jump_idx))
        }
        _ => {
            return Err("EXPECTED GOTO, CONTINUE, BREAK, RETURN".to_string());
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
    #[test]
    fn parse_selection_statement_test() -> Result<(), String> {
        {
            let src = r#"if (1 + 1) { int hi = 5; }"#;
            let mut flattened = parser::Flattened::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(src.as_bytes(), false, &mut str_maps)?;
            let (selection, _) =
                parse_selection_statement(&tokens, 0, &mut flattened, &mut str_maps)?;
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
            let mut flattened = parser::Flattened::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(src.as_bytes(), false, &mut str_maps)?;
            let (selection, _) =
                parse_selection_statement(&tokens, 0, &mut flattened, &mut str_maps)?;
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
            let mut flattened = parser::Flattened::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(src.as_bytes(), false, &mut str_maps)?;
            let (iteration, _) =
                parse_iteration_statement(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(iteration, Iteration::While { .. }));
        }
        {
            let src = r#"do {
                int hi = 5;
            } while (1);"#;
            let mut flattened = parser::Flattened::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(src.as_bytes(), false, &mut str_maps)?;
            let (iteration, _) =
                parse_iteration_statement(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(iteration, Iteration::DoWhile { .. }));
        }
        {
            let src = r#"do while(1) {} while (0);"#;
            let mut flattened = parser::Flattened::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(src.as_bytes(), false, &mut str_maps)?;
            let (iteration, _) =
                parse_iteration_statement(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(iteration, Iteration::DoWhile { .. }));
        }
        {
            let src = r#"for (1;1;1) {
                int hi = 5;
            }"#;
            let mut flattened = parser::Flattened::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(src.as_bytes(), false, &mut str_maps)?;
            let (iteration, _) =
                parse_iteration_statement(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(iteration, Iteration::ForThreeExpr { .. }));
        }
        Ok(())
    }
    #[test]
    fn parse_jump_statement_test() -> Result<(), String> {
        {
            let src = r#"goto chicken;"#;
            let mut flattened = parser::Flattened::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(src.as_bytes(), false, &mut str_maps)?;
            let (jump, _) = parse_jump_statement(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(jump, Jump::Goto(_)));
        }
        {
            let src = r#"continue;"#;
            let mut flattened = parser::Flattened::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(src.as_bytes(), false, &mut str_maps)?;
            let (jump, _) = parse_jump_statement(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(jump, Jump::Continue));
        }
        {
            let src = r#"break;"#;
            let mut flattened = parser::Flattened::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(src.as_bytes(), false, &mut str_maps)?;
            let (jump, _) = parse_jump_statement(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(jump, Jump::Break));
        }
        {
            let src = r#"return 1;"#;
            let mut flattened = parser::Flattened::new();
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(src.as_bytes(), false, &mut str_maps)?;
            let (jump, _) = parse_jump_statement(&tokens, 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(jump, Jump::Return(_)));
        }
        Ok(())
    }
}
