use crate::lexer;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::Range;
#[derive(Debug, PartialEq)]
pub enum RccError {
    UnknownToken,
    ExpectedToken(String),
    UnexpectedToken(String),
    Custom(String),
}
impl Display for RccError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            RccError::UnknownToken => f.write_str("UnknownToken"),
            RccError::ExpectedToken(t) => f.write_fmt(format_args!("ExpectedToken: {}", t)),
            RccError::UnexpectedToken(t) => f.write_fmt(format_args!("UnexpectedToken: {}", t)),
            RccError::Custom(s) => f.write_str(s),
        }
    }
}
#[derive(Debug, PartialEq)]
pub struct RccErrorInfo {
    error: RccError,
    lines: Range<usize>,
    columns: Vec<(usize, Range<usize>)>,
}
impl RccErrorInfo {
    pub fn new(
        error: RccError,
        index_range: Range<usize>,
        tokens: &[lexer::Token],
        str_maps: &lexer::ByteVecMaps,
    ) -> Self {
        let mut stringified = String::new();
        // TODO: probably make another abstraction called `Source` which is
        // some struct that contains what line number etc.
        if index_range.start >= tokens.len() {
            panic!("{:?}", error);
        }
        let newlines_before = tokens[..index_range.start]
            .iter()
            .filter(|t| matches!(t, lexer::Token::NEWLINE))
            .count();
        let initial_offset_for_start_of_column_offset = {
            let mut idx = index_range.start;
            while idx > 0 && !matches!(tokens[idx], lexer::Token::NEWLINE) {
                idx -= 1;
            }
            idx
        };
        for t in &tokens[index_range.clone()] {
            if let Some(byte_vec) = t.to_byte_vec(&str_maps) {
                stringified += String::from_utf8(byte_vec).unwrap().as_str();
            }
        }
        let mut newlines_encountered = newlines_before;
        let mut lines = 0..0;
        let mut line_lengths = Vec::new();
        let mut character_count = if matches!(
            tokens.get(initial_offset_for_start_of_column_offset),
            Some(lexer::Token::NEWLINE)
        ) {
            initial_offset_for_start_of_column_offset
        } else {
            0
        };
        for c in stringified.chars() {
            match c {
                '\n' => {
                    line_lengths.push((newlines_encountered, character_count));
                    character_count = 0;
                    newlines_encountered += 1;
                }
                _ => {
                    character_count += 1;
                }
            }
        }
        line_lengths.push((newlines_encountered, character_count));
        lines.start = line_lengths[0].0;
        lines.end = line_lengths[line_lengths.len() - 1].0 + 1;
        let mut columns = Vec::new();
        for (line, length) in line_lengths.iter() {
            columns.push((*line, 0..*length));
        }
        Self {
            error,
            lines,
            columns,
        }
    }
}
impl Display for RccErrorInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut mappings = String::new();
        for (line, columns) in &self.columns {
            mappings += &format!(
                "Line: {} Columns: {} .. {}\n",
                line, columns.start, columns.end
            );
        }
        f.write_fmt(format_args!("{}\n{}", self.error, mappings))
    }
}
impl Error for RccErrorInfo {}

#[cfg(test)]
mod tests {
    use super::{RccError, RccErrorInfo};
    use crate::lexer;
    #[test]
    fn rcc_error_info_test() -> Result<(), String> {
        let src = r#"UNIX2003
BRUH"#;
        let mut str_maps = lexer::ByteVecMaps::new();
        let tokens = lexer::lexer(src.as_bytes(), true, &mut str_maps)?;
        let rcc_error_info = RccErrorInfo::new(RccError::UnknownToken, 0..3, &tokens, &str_maps);
        assert_eq!(
            rcc_error_info,
            RccErrorInfo {
                error: RccError::UnknownToken,
                lines: 0..2,
                columns: vec![(0, 0..8), (1, 0..4)]
            }
        );
        Ok(())
    }
}
