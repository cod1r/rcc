use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::Range;
#[derive(Debug)]
pub enum RccError {
    UnknownToken,
}
#[derive(Debug)]
pub struct RccErrorInfo<T> {
    error: RccError,
    lines: Range<T>,
    columns: Vec<(usize, Range<T>)>,
}
impl<T> Display for RccErrorInfo<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self.error {
            RccError::UnknownToken => f.write_fmt(format_args!("{}", self)),
        }
    }
}
impl<T: std::fmt::Debug> Error for RccErrorInfo<T> {}
