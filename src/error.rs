pub fn error_msg(msg: &'static str, line: usize, column: usize) -> String {
    return format!("{} at line: {}, column: {}", msg, line, column);
}
