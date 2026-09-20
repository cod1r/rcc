pub fn error<'a>(msg: &'a str, line: usize, column: usize) -> String {
    return format!("{} at line: {}, column: {}", msg, line, column);
}
