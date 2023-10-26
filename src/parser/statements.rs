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
fn is_statement_token(t: lexer::Token) -> bool {
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
    while idx < tokens.len() {
        match tokens.get(idx) {
            Some(
                lexer::Token::IDENT(_) | lexer::Token::KEYWORD_CASE | lexer::Token::KEYWORD_DEFAULT,
            ) => {
                let (labeled, new_index) =
                    parse_labeled_statement(tokens, idx, flattened, str_maps)?;
            }
            Some(lexer::Token::PUNCT_OPEN_CURLY) => {
                let (compound, new_index) =
                    parse_compound_statement(tokens, idx, flattened, str_maps)?;
            }
            Some(lexer::Token::KEYWORD_IF | lexer::Token::KEYWORD_SWITCH) => {
                let (selection, new_index) =
                    parse_selection_statement(tokens, idx, flattened, str_maps)?;
            }
            Some(
                lexer::Token::KEYWORD_WHILE | lexer::Token::KEYWORD_DO | lexer::Token::KEYWORD_FOR,
            ) => {
                let (iteration, new_index) =
                    parse_iteration_statement(tokens, idx, flattened, str_maps)?;
            }
            Some(
                lexer::Token::KEYWORD_GOTO
                | lexer::Token::KEYWORD_CONTINUE
                | lexer::Token::KEYWORD_BREAK
                | lexer::Token::KEYWORD_RETURN,
            ) => {
                let (jump, new_index) = parse_jump_statement(tokens, idx, flattened, str_maps)?;
            }
            _ => todo!("parse expression-statement"),
        }
    }
    todo!()
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
            let (_, expression) = parser::expressions::parse_expressions(
                &tokens[start_index + 1..label_idx],
                0,
                flattened,
                str_maps,
            )?;
            let (statement, new_index) =
                parse_statement(tokens, label_idx + 1, flattened, str_maps)?;
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
    idx += 1; // first is guaranteed to be open curly
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
