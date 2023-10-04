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
    Identifier(StatementIndex),
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
pub fn parse_labeled_statement(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(), String> {
    todo!()
}
pub fn parse_compound_statement(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(), String> {
    todo!()
}
pub fn parse_selection_statement(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(), String> {
    todo!()
}
pub fn parse_iteration_statement(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(), String> {
    todo!()
}
pub fn parse_jump_statement(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(), String> {
    todo!()
}
