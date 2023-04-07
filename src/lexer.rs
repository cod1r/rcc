#[allow(non_camel_case_types)]
#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    IDENT(String),
    PLACEMARKER,
    WHITESPACE,
    NEWLINE,
    PREDEF_IDENT___FUNC__,
    PUNCT_OPEN_SQR,
    PUNCT_CLOSE_SQR,
    PUNCT_OPEN_PAR,
    PUNCT_CLOSE_PAR,
    PUNCT_OPEN_CURLY,
    PUNCT_CLOSE_CURLY,
    PUNCT_DOT,
    PUNCT_ARROW,
    PUNCT_INCREMENT,
    PUNCT_DECREMENT,
    PUNCT_AND_BIT,
    PUNCT_MULT,
    PUNCT_PLUS,
    PUNCT_MINUS,
    PUNCT_TILDE,
    PUNCT_NOT_BOOL,
    PUNCT_DIV,
    PUNCT_MODULO,
    PUNCT_BITSHFT_LEFT,
    PUNCT_BITSHFT_RIGHT,
    PUNCT_LESS_THAN,
    PUNCT_GREATER_THAN,
    PUNCT_LESS_THAN_EQ,
    PUNCT_GREATER_THAN_EQ,
    PUNCT_EQ_BOOL,
    PUNCT_NOT_EQ_BOOL,
    PUNCT_XOR_BIT,
    PUNCT_OR_BIT,
    PUNCT_AND_BOOL,
    PUNCT_OR_BOOL,
    PUNCT_QUESTION_MARK,
    PUNCT_COLON,
    PUNCT_SEMI_COLON,
    PUNCT_ELLIPSIS,
    PUNCT_ASSIGNMENT,
    PUNCT_MULT_ASSIGN,
    PUNCT_DIV_ASSIGN,
    PUNCT_MODULO_ASSIGN,
    PUNCT_ADD_ASSIGN,
    PUNCT_SUB_ASSIGN,
    PUNCT_L_SHIFT_BIT_ASSIGN,
    PUNCT_R_SHIFT_BIT_ASSIGN,
    PUNCT_AND_BIT_ASSIGN,
    PUNCT_XOR_BIT_ASSIGN,
    PUNCT_OR_BIT_ASSIGN,
    PUNCT_COMMA,
    PUNCT_HASH,
    PUNCT_HASH_HASH,
    PUNCT_DIGRAPH_OPEN_SQR,
    PUNCT_DIGRAPH_CLOSE_SQR,
    PUNCT_DIGRAPH_OPEN_CURLY,
    PUNCT_DIGRAPH_CLOSE_CURLY,
    PUNCT_DIGRAPH_HASH,
    PUNCT_DIGRAPH_HASH_HASH,
    KEYWORD_AUTO,
    KEYWORD_BREAK,
    KEYWORD_CASE,
    KEYWORD_CHAR,
    KEYWORD_CONST,
    KEYWORD_CONTINUE,
    KEYWORD_DEFAULT,
    KEYWORD_DO,
    KEYWORD_DOUBLE,
    KEYWORD_ELSE,
    KEYWORD_ENUM,
    KEYWORD_EXTERN,
    KEYWORD_FLOAT,
    KEYWORD_FOR,
    KEYWORD_GOTO,
    KEYWORD_IF,
    KEYWORD_INLINE,
    KEYWORD_INT,
    KEYWORD_LONG,
    KEYWORD_REGISTER,
    KEYWORD_RESTRICT,
    KEYWORD_RETURN,
    KEYWORD_SHORT,
    KEYWORD_SIGNED,
    KEYWORD_SIZEOF,
    KEYWORD_STATIC,
    KEYWORD_STRUCT,
    KEYWORD_SWITCH,
    KEYWORD_TYPEDEF,
    KEYWORD_UNION,
    KEYWORD_UNSIGNED,
    KEYWORD_VOID,
    KEYWORD_VOLATILE,
    KEYWORD_WHILE,
    KEYWORD__ALIGNAS,
    KEYWORD__ALIGNOF,
    KEYWORD__ATOMIC,
    KEYWORD__BOOL,
    KEYWORD__COMPLEX,
    KEYWORD__GENERIC,
    KEYWORD__IMAGINARY,
    KEYWORD__NORETURN,
    KEYWORD__STATIC_ASSERT,
    KEYWORD__THREAD_LOCAL,
    TYPE,
    CONSTANT_OCTAL_INT {
        value: String,
        suffix: Option<String>,
    },
    CONSTANT_HEXA_INT {
        value: String,
        suffix: Option<String>,
    },
    CONSTANT_DEC_INT {
        value: String,
        suffix: Option<String>,
    },
    CONSTANT_DEC_FLOAT {
        value: String,
        exp_part: Option<String>,
        suffix: Option<String>,
    },
    CONSTANT_HEXA_FLOAT {
        value: String,
        binary_exp_part: String,
        suffix: Option<String>,
    },
    StringLiteral {
        prefix: Option<String>,
        sequence: String,
    },
    CONSTANT_CHAR(String),
}
impl Token {
    pub fn to_string(&self) -> Option<&str> {
        match self {
            Token::WHITESPACE => Some(" "),
            Token::NEWLINE => Some("\n"),
            Token::PUNCT_OPEN_SQR => Some("["),
            Token::PUNCT_CLOSE_SQR => Some("]"),
            Token::PUNCT_OPEN_PAR => Some("("),
            Token::PUNCT_CLOSE_PAR => Some(")"),
            Token::PUNCT_OPEN_CURLY => Some("{"),
            Token::PUNCT_CLOSE_CURLY => Some("}"),
            Token::PUNCT_DOT => Some("."),
            Token::PUNCT_ARROW => Some("=>"),
            Token::PUNCT_INCREMENT => Some("++"),
            Token::PUNCT_DECREMENT => Some("--"),
            Token::PUNCT_AND_BIT => Some("&"),
            Token::PUNCT_MULT => Some("*"),
            Token::PUNCT_PLUS => Some("+"),
            Token::PUNCT_MINUS => Some("-"),
            Token::PUNCT_TILDE => Some("~"),
            Token::PUNCT_NOT_BOOL => Some("!"),
            Token::PUNCT_DIV => Some("/"),
            Token::PUNCT_MODULO => Some("%"),
            Token::PUNCT_BITSHFT_LEFT => Some("<<"),
            Token::PUNCT_BITSHFT_RIGHT => Some(">>"),
            Token::PUNCT_LESS_THAN => Some("<"),
            Token::PUNCT_GREATER_THAN => Some(">"),
            Token::PUNCT_LESS_THAN_EQ => Some("<="),
            Token::PUNCT_GREATER_THAN_EQ => Some(">="),
            Token::PUNCT_EQ_BOOL => Some("=="),
            Token::PUNCT_NOT_EQ_BOOL => Some("!="),
            Token::PUNCT_XOR_BIT => Some("^"),
            Token::PUNCT_OR_BIT => Some("|"),
            Token::PUNCT_AND_BOOL => Some("&&"),
            Token::PUNCT_OR_BOOL => Some("||"),
            Token::PUNCT_QUESTION_MARK => Some("?"),
            Token::PUNCT_COLON => Some(":"),
            Token::PUNCT_SEMI_COLON => Some(";"),
            Token::PUNCT_ELLIPSIS => Some("..."),
            Token::PUNCT_ASSIGNMENT => Some("="),
            Token::PUNCT_MULT_ASSIGN => Some("*="),
            Token::PUNCT_DIV_ASSIGN => Some("/="),
            Token::PUNCT_MODULO_ASSIGN => Some("%="),
            Token::PUNCT_ADD_ASSIGN => Some("+="),
            Token::PUNCT_SUB_ASSIGN => Some("-="),
            Token::PUNCT_L_SHIFT_BIT_ASSIGN => Some("<<="),
            Token::PUNCT_R_SHIFT_BIT_ASSIGN => Some(">>="),
            Token::PUNCT_AND_BIT_ASSIGN => Some("&="),
            Token::PUNCT_XOR_BIT_ASSIGN => Some("^="),
            Token::PUNCT_OR_BIT_ASSIGN => Some("|="),
            Token::PUNCT_COMMA => Some(","),
            Token::PUNCT_HASH => Some("#"),
            Token::PUNCT_HASH_HASH => Some("##"),
            Token::PUNCT_DIGRAPH_OPEN_SQR => Some("<:"),
            Token::PUNCT_DIGRAPH_CLOSE_SQR => Some(":>"),
            Token::PUNCT_DIGRAPH_OPEN_CURLY => Some("<%"),
            Token::PUNCT_DIGRAPH_CLOSE_CURLY => Some("%>"),
            Token::PUNCT_DIGRAPH_HASH => Some("%:"),
            Token::PUNCT_DIGRAPH_HASH_HASH => Some("%:%:"),
            _ => None,
        }
    }
}
fn match_string_literal(program_str_bytes: &[u8], index: &mut usize) -> Option<Token> {
    let mut byte_index = *index;
    let mut token = Token::StringLiteral {
        prefix: None,
        sequence: String::new(),
    };
    if byte_index < program_str_bytes.len()
        && [b'u', b'U', b'L'].contains(&program_str_bytes[byte_index])
    {
        if byte_index + 1 < program_str_bytes.len() && program_str_bytes[byte_index + 1] == b'8' {
            if let Token::StringLiteral {
                prefix,
                sequence: _,
            } = &mut token
            {
                *prefix = Some(
                    String::from_utf8_lossy(&program_str_bytes[byte_index..byte_index + 2])
                        .to_string(),
                );
            }
            byte_index += 2;
        } else {
            if let Token::StringLiteral {
                prefix,
                sequence: _,
            } = &mut token
            {
                *prefix = Some(
                    String::from_utf8_lossy(&program_str_bytes[byte_index..byte_index + 1])
                        .to_string(),
                );
            }
            byte_index += 1;
        }
    }
    if byte_index < program_str_bytes.len() && program_str_bytes[byte_index] == b'\"' {
        byte_index += 1;
        let start_of_seq = byte_index;
        let symbols = [
            b'!', b'\'', b'#', b'%', b'&', b'(', b')', b'*', b'+', b',', b'-', b'.', b'/', b':',
            b';', b'<', b'=', b'>', b'?', b'[', b'\\', b']', b'^', b'_', b'{', b'|', b'}', b'~',
            b' ', b'\t', 11, 12,
        ];
        while byte_index < program_str_bytes.len()
            && (program_str_bytes[byte_index].is_ascii_alphanumeric()
                || symbols.contains(&program_str_bytes[byte_index]))
        {
            byte_index += 1;
        }
        if byte_index < program_str_bytes.len() && program_str_bytes[byte_index] == b'"' {
            if let Token::StringLiteral {
                prefix: _,
                sequence,
            } = &mut token
            {
                *sequence = String::from_utf8_lossy(&program_str_bytes[start_of_seq..byte_index])
                    .to_string();
            }
            byte_index += 1;
            *index = byte_index;
            return Some(token);
        }
    }
    None
}
fn match_integer_constant(program_str_bytes: &[u8], index: &mut usize) -> Option<Token> {
    let mut byte_index = *index;
    match program_str_bytes[byte_index] {
        b'0' if byte_index + 1 < program_str_bytes.len()
            && (program_str_bytes[byte_index + 1].is_ascii_digit()
                || program_str_bytes[byte_index + 1].to_ascii_lowercase() == b'x') =>
        {
            let mut is_hexa = false;
            if byte_index + 1 < program_str_bytes.len()
                && program_str_bytes[byte_index + 1].to_ascii_lowercase() == b'x'
            {
                byte_index += 2;
                is_hexa = true;
            }
            while byte_index < program_str_bytes.len()
                && program_str_bytes[byte_index].is_ascii_digit()
            {
                byte_index += 1;
            }
            let start_suffex = byte_index;
            while byte_index < program_str_bytes.len()
                && program_str_bytes[byte_index].is_ascii_alphabetic()
            {
                byte_index += 1;
            }
            match program_str_bytes[start_suffex..byte_index] {
                [b'U', b'L']
                | [b'U', b'l']
                | [b'u', b'L']
                | [b'u', b'l']
                | [b'L', b'u']
                | [b'L', b'U']
                | [b'l', b'U']
                | [b'l', b'u']
                | [b'l', b'l', b'u']
                | [b'l', b'l', b'U']
                | [b'L', b'L', b'u']
                | [b'L', b'L', b'U']
                | [b'u', b'l', b'l']
                | [b'u', b'L', b'L']
                | [b'U', b'l', b'l']
                | [b'U', b'L', b'L']
                | [] => {
                    let suffix = if byte_index - start_suffex > 0 {
                        Some(
                            String::from_utf8_lossy(&program_str_bytes[start_suffex..byte_index])
                                .to_string(),
                        )
                    } else {
                        None
                    };
                    if is_hexa {
                        let token = Some(Token::CONSTANT_HEXA_INT {
                            value: String::from_utf8_lossy(
                                &program_str_bytes[*index..start_suffex],
                            )
                            .to_string(),
                            suffix,
                        });
                        *index = byte_index;
                        token
                    } else {
                        let token = Some(Token::CONSTANT_OCTAL_INT {
                            value: String::from_utf8_lossy(
                                &program_str_bytes[*index..start_suffex],
                            )
                            .to_string(),
                            suffix,
                        });
                        *index = byte_index;
                        token
                    }
                }
                _ => None,
            }
        }
        _ => {
            if program_str_bytes[byte_index].is_ascii_digit() {
                while byte_index < program_str_bytes.len()
                    && program_str_bytes[byte_index].is_ascii_digit()
                {
                    byte_index += 1;
                }
                let start_suffex = byte_index;
                while byte_index < program_str_bytes.len()
                    && program_str_bytes[byte_index].is_ascii_alphabetic()
                {
                    byte_index += 1;
                }
                match program_str_bytes[start_suffex..byte_index] {
                    [b'U', b'L']
                    | [b'U', b'l']
                    | [b'u', b'L']
                    | [b'u', b'l']
                    | [b'L', b'u']
                    | [b'L', b'U']
                    | [b'l', b'U']
                    | [b'l', b'u']
                    | [b'l', b'l', b'u']
                    | [b'l', b'l', b'U']
                    | [b'L', b'L', b'u']
                    | [b'L', b'L', b'U']
                    | [b'u', b'l', b'l']
                    | [b'u', b'L', b'L']
                    | [b'U', b'l', b'l']
                    | [b'U', b'L', b'L']
                    | [] => {
                        let suffix = if byte_index - start_suffex > 0 {
                            Some(
                                String::from_utf8_lossy(
                                    &program_str_bytes[start_suffex..byte_index],
                                )
                                .to_string(),
                            )
                        } else {
                            None
                        };
                        let token = Some(Token::CONSTANT_DEC_INT {
                            value: String::from_utf8_lossy(
                                &program_str_bytes[*index..start_suffex],
                            )
                            .to_string(),
                            suffix,
                        });
                        *index = byte_index;
                        return token;
                    }
                    _ => {
                        return None;
                    }
                }
            }
            None
        }
    }
}
fn match_floating_constant(program_str_bytes: &[u8], index: &mut usize) -> Option<Token> {
    let mut byte_index = *index;
    let is_hexa = if byte_index + 1 < program_str_bytes.len()
        && (program_str_bytes[byte_index + 1] == b'x' || program_str_bytes[byte_index + 1] == b'X')
    {
        byte_index += 2;
        true
    } else {
        false
    };
    let start = byte_index;
    while byte_index < program_str_bytes.len() && program_str_bytes[byte_index].is_ascii_digit() {
        byte_index += 1;
    }
    let length_first_digit_sequence = byte_index - start;
    let end_of_first_digit_sequence = byte_index;
    let has_dot = byte_index < program_str_bytes.len() && program_str_bytes[byte_index] == b'.';
    let mut length_of_second_digit_sequence = 0;
    if has_dot {
        byte_index += 1;
        while byte_index < program_str_bytes.len() && program_str_bytes[byte_index].is_ascii_digit()
        {
            byte_index += 1;
        }
        length_of_second_digit_sequence = byte_index - end_of_first_digit_sequence - 1;
    }
    let end_of_second_digit_sequence = byte_index;
    let has_exponential = byte_index < program_str_bytes.len()
        && ((!is_hexa
            && (program_str_bytes[byte_index] == b'e' || program_str_bytes[byte_index] == b'E'))
            || (is_hexa
                && (program_str_bytes[byte_index] == b'p'
                    || program_str_bytes[byte_index] == b'P')));
    let mut length_of_exp_digit_sequence = 0;
    if has_exponential {
        byte_index += 1;
        if byte_index < program_str_bytes.len()
            && (program_str_bytes[byte_index] == b'-' || program_str_bytes[byte_index] == b'+')
        {
            byte_index += 1;
        }
        while byte_index < program_str_bytes.len() && program_str_bytes[byte_index].is_ascii_digit()
        {
            byte_index += 1;
        }
        length_of_exp_digit_sequence = byte_index - end_of_second_digit_sequence - 1;
    }
    let end_of_exp_digit_sequence = byte_index;
    if byte_index < program_str_bytes.len()
        && (program_str_bytes[byte_index] == b'f'
            || program_str_bytes[byte_index] == b'F'
            || program_str_bytes[byte_index] == b'L'
            || program_str_bytes[byte_index] == b'l')
    {
        byte_index += 1;
    }
    let length_of_suffix = byte_index - end_of_exp_digit_sequence;
    if (has_dot
        && !has_exponential
        && (length_first_digit_sequence > 0 || length_of_second_digit_sequence > 0))
        || (has_exponential
            && !has_dot
            && length_of_exp_digit_sequence > 0
            && length_first_digit_sequence > 0)
        || (has_dot
            && has_exponential
            && (length_first_digit_sequence > 0 || length_of_second_digit_sequence > 0)
            && length_of_exp_digit_sequence > 0)
    {
        let suffix = if length_of_suffix > 0 {
            Some(
                String::from_utf8_lossy(&program_str_bytes[end_of_exp_digit_sequence..byte_index])
                    .to_string(),
            )
        } else {
            None
        };
        if is_hexa {
            let token = Some(Token::CONSTANT_HEXA_FLOAT {
                value: String::from_utf8_lossy(
                    &program_str_bytes[*index..end_of_second_digit_sequence],
                )
                .to_string(),
                binary_exp_part: String::from_utf8_lossy(
                    &program_str_bytes[end_of_second_digit_sequence..end_of_exp_digit_sequence],
                )
                .to_string(),
                suffix,
            });
            *index = byte_index;
            return token;
        } else {
            let token = Some(Token::CONSTANT_DEC_FLOAT {
                value: String::from_utf8_lossy(
                    &program_str_bytes[*index..end_of_second_digit_sequence],
                )
                .to_string(),
                exp_part: if has_exponential {
                    Some(
                        String::from_utf8_lossy(
                            &program_str_bytes
                                [end_of_second_digit_sequence..end_of_exp_digit_sequence],
                        )
                        .to_string(),
                    )
                } else {
                    None
                },
                suffix,
            });
            *index = byte_index;
            return token;
        }
    }
    None
}
fn match_enumeration_constant(program_str_bytes: &[u8], index: &mut usize) -> Option<Token> {
    None
}
fn match_character_constant(program_str_bytes: &[u8], index: &mut usize) -> Option<Token> {
    let mut byte_index = *index;
    if program_str_bytes[byte_index] == b'L'
        || program_str_bytes[byte_index] == b'u'
        || program_str_bytes[byte_index] == b'U'
        || program_str_bytes[byte_index] == b'\''
    {
        byte_index += 1;
        if byte_index < program_str_bytes.len()
            && program_str_bytes[byte_index] == b'\''
            && program_str_bytes[byte_index - 1] != b'\''
        {
            byte_index += 1;
        }
        let symbols = [
            b'!', b'"', b'#', b'%', b'&', b'(', b')', b'*', b'+', b',', b'-', b'.', b'/', b':',
            b';', b'<', b'=', b'>', b'?', b'[', b'\\', b']', b'^', b'_', b'{', b'|', b'}', b'~',
            b' ', b'\t', 11, 12,
        ];
        while byte_index < program_str_bytes.len()
            && (program_str_bytes[byte_index].is_ascii_alphanumeric()
                || symbols.contains(&program_str_bytes[byte_index]))
        {
            if program_str_bytes[byte_index] == b'\\' {
                // allowed escape characters
                // else return None
                if byte_index + 1 < program_str_bytes.len()
                    && (([
                        b'\'', b'\"', b'?', b'\\', b'a', b'b', b'f', b'n', b'r', b't', b'v',
                    ]
                    .contains(&program_str_bytes[byte_index + 1])
                        || (b'0'..=b'7').contains(&program_str_bytes[byte_index + 1]))
                        || (byte_index + 2 < program_str_bytes.len()
                            && program_str_bytes[byte_index + 1] == b'x'
                            && program_str_bytes[byte_index + 2].is_ascii_hexdigit()))
                {
                    byte_index += 2;
                    continue;
                } else {
                    return None;
                }
            }
            byte_index += 1;
        }
        if byte_index < program_str_bytes.len() && program_str_bytes[byte_index] == b'\'' {
            byte_index += 1;
            let token = Some(Token::CONSTANT_CHAR(
                String::from_utf8_lossy(&program_str_bytes[*index..byte_index]).to_string(),
            ));
            *index = byte_index;
            return token;
        }
    }
    None
}
fn match_punctuator(program_str_bytes: &[u8], index: &mut usize) -> Option<Token> {
    let byte_index = *index;
    if byte_index < program_str_bytes.len() {
        match program_str_bytes[byte_index] {
            b'[' => {
                *index += 1;
                return Some(Token::PUNCT_OPEN_SQR);
            }
            b']' => {
                *index += 1;
                return Some(Token::PUNCT_CLOSE_SQR);
            }
            b'(' => {
                *index += 1;
                return Some(Token::PUNCT_OPEN_PAR);
            }
            b')' => {
                *index += 1;
                return Some(Token::PUNCT_CLOSE_PAR);
            }
            b'{' => {
                *index += 1;
                return Some(Token::PUNCT_OPEN_CURLY);
            }
            b'}' => {
                *index += 1;
                return Some(Token::PUNCT_CLOSE_CURLY);
            }
            b'.' => {
                if byte_index + 2 < program_str_bytes.len() {
                    if let (b'.', b'.', b'.') = (
                        program_str_bytes[byte_index],
                        program_str_bytes[byte_index + 1],
                        program_str_bytes[byte_index + 2],
                    ) {
                        *index += 3;
                        return Some(Token::PUNCT_ELLIPSIS);
                    }
                }
                *index += 1;
                return Some(Token::PUNCT_DOT);
            }
            b'-' => {
                if byte_index + 1 < program_str_bytes.len() {
                    match program_str_bytes[byte_index + 1] {
                        b'-' => {
                            *index += 2;
                            return Some(Token::PUNCT_DECREMENT);
                        }
                        b'=' => {
                            *index += 2;
                            return Some(Token::PUNCT_SUB_ASSIGN);
                        }
                        b'>' => {
                            *index += 2;
                            return Some(Token::PUNCT_ARROW);
                        }
                        _ => {}
                    }
                }
                *index += 1;
                return Some(Token::PUNCT_MINUS);
            }
            b'+' => {
                if byte_index + 1 < program_str_bytes.len() {
                    match program_str_bytes[byte_index + 1] {
                        b'+' => {
                            *index += 2;
                            return Some(Token::PUNCT_INCREMENT);
                        }
                        b'=' => {
                            *index += 2;
                            return Some(Token::PUNCT_ADD_ASSIGN);
                        }
                        _ => {}
                    }
                }
                *index += 1;
                return Some(Token::PUNCT_PLUS);
            }
            b'&' => {
                if byte_index + 1 < program_str_bytes.len() {
                    match program_str_bytes[byte_index + 1] {
                        b'&' => {
                            *index += 2;
                            return Some(Token::PUNCT_AND_BOOL);
                        }
                        b'=' => {
                            *index += 2;
                            return Some(Token::PUNCT_AND_BIT_ASSIGN);
                        }
                        _ => {}
                    }
                }
                *index += 1;
                return Some(Token::PUNCT_AND_BIT);
            }
            b'*' => {
                if byte_index + 1 < program_str_bytes.len()
                    && program_str_bytes[byte_index + 1] == b'='
                {
                    *index += 2;
                    return Some(Token::PUNCT_MULT_ASSIGN);
                }
                *index += 1;
                return Some(Token::PUNCT_MULT);
            }
            b'~' => {
                *index += 1;
                return Some(Token::PUNCT_TILDE);
            }
            b'!' => {
                if byte_index + 1 < program_str_bytes.len()
                    && program_str_bytes[byte_index + 1] == b'='
                {
                    *index += 2;
                    return Some(Token::PUNCT_NOT_EQ_BOOL);
                }
                *index += 1;
                return Some(Token::PUNCT_NOT_BOOL);
            }
            b'/' => {
                if byte_index + 1 < program_str_bytes.len()
                    && program_str_bytes[byte_index + 1] == b'='
                {
                    *index += 2;
                    return Some(Token::PUNCT_DIV_ASSIGN);
                }
                *index += 1;
                return Some(Token::PUNCT_DIV);
            }
            b'%' => {
                if byte_index + 1 < program_str_bytes.len() {
                    match program_str_bytes[byte_index + 1] {
                        b'=' => {
                            *index += 2;
                            return Some(Token::PUNCT_MODULO_ASSIGN);
                        }
                        b'>' => {
                            *index += 2;
                            return Some(Token::PUNCT_DIGRAPH_CLOSE_CURLY);
                        }
                        b':' => {
                            if byte_index + 3 < program_str_bytes.len() {
                                if let (b'%', b':', b'%', b':') = (
                                    program_str_bytes[byte_index],
                                    program_str_bytes[byte_index + 1],
                                    program_str_bytes[byte_index + 2],
                                    program_str_bytes[byte_index + 3],
                                ) {
                                    *index += 4;
                                    return Some(Token::PUNCT_DIGRAPH_HASH_HASH);
                                }
                            }
                            *index += 2;
                            return Some(Token::PUNCT_DIGRAPH_HASH);
                        }
                        _ => {}
                    }
                }
                *index += 1;
                return Some(Token::PUNCT_MODULO);
            }
            b'<' => {
                if byte_index + 1 < program_str_bytes.len() {
                    match program_str_bytes[byte_index + 1] {
                        b'<' => {
                            if byte_index + 2 < program_str_bytes.len()
                                && program_str_bytes[byte_index + 2] == b'='
                            {
                                *index += 3;
                                return Some(Token::PUNCT_L_SHIFT_BIT_ASSIGN);
                            }
                            *index += 2;
                            return Some(Token::PUNCT_BITSHFT_LEFT);
                        }
                        b'=' => {
                            *index += 2;
                            return Some(Token::PUNCT_LESS_THAN_EQ);
                        }
                        b':' => {
                            *index += 2;
                            return Some(Token::PUNCT_DIGRAPH_OPEN_SQR);
                        }
                        b'%' => {
                            *index += 2;
                            return Some(Token::PUNCT_DIGRAPH_OPEN_CURLY);
                        }
                        _ => {}
                    }
                }
                *index += 1;
                return Some(Token::PUNCT_LESS_THAN);
            }
            b'>' => {
                if byte_index + 1 < program_str_bytes.len() {
                    match program_str_bytes[byte_index + 1] {
                        b'>' => {
                            if byte_index + 2 < program_str_bytes.len()
                                && program_str_bytes[byte_index + 2] == b'='
                            {
                                *index += 3;
                                return Some(Token::PUNCT_R_SHIFT_BIT_ASSIGN);
                            }
                            *index += 2;
                            return Some(Token::PUNCT_BITSHFT_RIGHT);
                        }
                        b'=' => {
                            *index += 2;
                            return Some(Token::PUNCT_GREATER_THAN_EQ);
                        }
                        _ => {}
                    }
                }
                *index += 1;
                return Some(Token::PUNCT_GREATER_THAN);
            }
            b'=' => {
                if byte_index + 1 < program_str_bytes.len()
                    && program_str_bytes[byte_index + 1] == b'='
                {
                    *index += 2;
                    return Some(Token::PUNCT_EQ_BOOL);
                }
                *index += 1;
                return Some(Token::PUNCT_ASSIGNMENT);
            }
            b'^' => {
                if byte_index + 1 < program_str_bytes.len()
                    && program_str_bytes[byte_index + 1] == b'='
                {
                    *index += 2;
                    return Some(Token::PUNCT_XOR_BIT_ASSIGN);
                }
                *index += 1;
                return Some(Token::PUNCT_XOR_BIT);
            }
            b'|' => {
                if byte_index + 1 < program_str_bytes.len() {
                    match program_str_bytes[byte_index + 1] {
                        b'=' => {
                            *index += 2;
                            return Some(Token::PUNCT_OR_BIT_ASSIGN);
                        }
                        b'|' => {
                            *index += 2;
                            return Some(Token::PUNCT_OR_BOOL);
                        }
                        _ => {}
                    }
                }
                *index += 1;
                return Some(Token::PUNCT_OR_BIT);
            }
            b'?' => {
                *index += 1;
                return Some(Token::PUNCT_QUESTION_MARK);
            }
            b':' => {
                if byte_index + 1 < program_str_bytes.len()
                    && program_str_bytes[byte_index + 1] == b'>'
                {
                    *index += 2;
                    return Some(Token::PUNCT_DIGRAPH_CLOSE_SQR);
                }
                *index += 1;
                return Some(Token::PUNCT_COLON);
            }
            b';' => {
                *index += 1;
                return Some(Token::PUNCT_SEMI_COLON);
            }
            b',' => {
                *index += 1;
                return Some(Token::PUNCT_COMMA);
            }
            b'#' => {
                if byte_index + 1 < program_str_bytes.len()
                    && program_str_bytes[byte_index + 1] == b'#'
                {
                    *index += 2;
                    return Some(Token::PUNCT_HASH_HASH);
                }
                *index += 1;
                return Some(Token::PUNCT_HASH);
            }
            _ => {}
        }
    }
    None
}
fn match_identifier(program_str_bytes: &[u8], index: &mut usize) -> Option<Token> {
    let mut byte_index = *index;
    while byte_index < program_str_bytes.len()
        && (program_str_bytes[byte_index].is_ascii_alphanumeric()
            || program_str_bytes[byte_index] == b'_')
    {
        byte_index += 1;
    }
    let bytes = &program_str_bytes[*index..byte_index];
    let utf8_lossy = String::from_utf8_lossy(bytes).to_string();
    // TODO: we need to check for universal character names
    if !bytes.is_empty() && !bytes[0].is_ascii_digit() && utf8_lossy != "__func__" {
        *index = byte_index;
        return Some(Token::IDENT(utf8_lossy));
    } else if utf8_lossy == "__func__" {
        *index = byte_index;
        return Some(Token::PREDEF_IDENT___FUNC__);
    }
    None
}
fn match_keyword(program_str_bytes: &[u8], index: &mut usize) -> Option<Token> {
    let mut byte_index = *index;
    while byte_index < program_str_bytes.len()
        && (program_str_bytes[byte_index].is_ascii_alphanumeric()
            || program_str_bytes[byte_index] == b'_')
    {
        byte_index += 1;
    }
    let bytes = &program_str_bytes[*index..byte_index];
    const KEYWORD_AUTO: &str = "auto";
    const KEYWORD_BREAK: &str = "break";
    const KEYWORD_CASE: &str = "case";
    const KEYWORD_CHAR: &str = "char";
    const KEYWORD_CONST: &str = "const";
    const KEYWORD_CONTINUE: &str = "continue";
    const KEYWORD_DEFAULT: &str = "default";
    const KEYWORD_DO: &str = "do";
    const KEYWORD_DOUBLE: &str = "double";
    const KEYWORD_ELSE: &str = "else";
    const KEYWORD_ENUM: &str = "enum";
    const KEYWORD_EXTERN: &str = "extern";
    const KEYWORD_FLOAT: &str = "float";
    const KEYWORD_FOR: &str = "for";
    const KEYWORD_GOTO: &str = "goto";
    const KEYWORD_IF: &str = "if";
    const KEYWORD_INLINE: &str = "inline";
    const KEYWORD_INT: &str = "int";
    const KEYWORD_LONG: &str = "long";
    const KEYWORD_REGISTER: &str = "register";
    const KEYWORD_RESTRICT: &str = "restrict";
    const KEYWORD_RETURN: &str = "return";
    const KEYWORD_SHORT: &str = "short";
    const KEYWORD_SIGNED: &str = "signed";
    const KEYWORD_SIZEOF: &str = "sizeof";
    const KEYWORD_STATIC: &str = "static";
    const KEYWORD_STRUCT: &str = "struct";
    const KEYWORD_SWITCH: &str = "switch";
    const KEYWORD_TYPEDEF: &str = "typedef";
    const KEYWORD_UNION: &str = "union";
    const KEYWORD_UNSIGNED: &str = "unsigned";
    const KEYWORD_VOID: &str = "void";
    const KEYWORD_VOLATILE: &str = "volatile";
    const KEYWORD_WHILE: &str = "while";
    const KEYWORD__ALIGNAS: &str = "_Alignas";
    const KEYWORD__ALIGNOF: &str = "_Alignof";
    const KEYWORD__ATOMIC: &str = "_Atomic";
    const KEYWORD__BOOL: &str = "_Bool";
    const KEYWORD__COMPLEX: &str = "_Complex";
    const KEYWORD__GENERIC: &str = "_Generic";
    const KEYWORD__IMAGINARY: &str = "_Imaginary";
    const KEYWORD__NORETURN: &str = "_Noreturn";
    const KEYWORD__STATIC_ASSERT: &str = "_Static_assert";
    const KEYWORD__THREAD_LOCAL: &str = "_Thread_local";
    let keyword = match String::from_utf8_lossy(bytes).to_string().as_str() {
        KEYWORD_AUTO => Some(Token::KEYWORD_AUTO),
        KEYWORD_BREAK => Some(Token::KEYWORD_BREAK),
        KEYWORD_CASE => Some(Token::KEYWORD_CASE),
        KEYWORD_CHAR => Some(Token::KEYWORD_CHAR),
        KEYWORD_CONST => Some(Token::KEYWORD_CONST),
        KEYWORD_CONTINUE => Some(Token::KEYWORD_CONTINUE),
        KEYWORD_DEFAULT => Some(Token::KEYWORD_DEFAULT),
        KEYWORD_DO => Some(Token::KEYWORD_DO),
        KEYWORD_DOUBLE => Some(Token::KEYWORD_DOUBLE),
        KEYWORD_ELSE => Some(Token::KEYWORD_ELSE),
        KEYWORD_ENUM => Some(Token::KEYWORD_ENUM),
        KEYWORD_EXTERN => Some(Token::KEYWORD_EXTERN),
        KEYWORD_FLOAT => Some(Token::KEYWORD_FLOAT),
        KEYWORD_FOR => Some(Token::KEYWORD_FOR),
        KEYWORD_GOTO => Some(Token::KEYWORD_GOTO),
        KEYWORD_IF => Some(Token::KEYWORD_IF),
        KEYWORD_INLINE => Some(Token::KEYWORD_INLINE),
        KEYWORD_INT => Some(Token::KEYWORD_INT),
        KEYWORD_LONG => Some(Token::KEYWORD_LONG),
        KEYWORD_REGISTER => Some(Token::KEYWORD_REGISTER),
        KEYWORD_RESTRICT => Some(Token::KEYWORD_RESTRICT),
        KEYWORD_RETURN => Some(Token::KEYWORD_RETURN),
        KEYWORD_SHORT => Some(Token::KEYWORD_SHORT),
        KEYWORD_SIGNED => Some(Token::KEYWORD_SIGNED),
        KEYWORD_SIZEOF => Some(Token::KEYWORD_SIZEOF),
        KEYWORD_STATIC => Some(Token::KEYWORD_STATIC),
        KEYWORD_STRUCT => Some(Token::KEYWORD_STRUCT),
        KEYWORD_SWITCH => Some(Token::KEYWORD_SWITCH),
        KEYWORD_TYPEDEF => Some(Token::KEYWORD_TYPEDEF),
        KEYWORD_UNION => Some(Token::KEYWORD_UNION),
        KEYWORD_UNSIGNED => Some(Token::KEYWORD_UNSIGNED),
        KEYWORD_VOID => Some(Token::KEYWORD_VOID),
        KEYWORD_VOLATILE => Some(Token::KEYWORD_VOLATILE),
        KEYWORD_WHILE => Some(Token::KEYWORD_WHILE),
        KEYWORD__ALIGNAS => Some(Token::KEYWORD__ALIGNAS),
        KEYWORD__ALIGNOF => Some(Token::KEYWORD__ALIGNOF),
        KEYWORD__ATOMIC => Some(Token::KEYWORD__ATOMIC),
        KEYWORD__BOOL => Some(Token::KEYWORD__BOOL),
        KEYWORD__COMPLEX => Some(Token::KEYWORD__COMPLEX),
        KEYWORD__GENERIC => Some(Token::KEYWORD__GENERIC),
        KEYWORD__IMAGINARY => Some(Token::KEYWORD__IMAGINARY),
        KEYWORD__NORETURN => Some(Token::KEYWORD__NORETURN),
        KEYWORD__STATIC_ASSERT => Some(Token::KEYWORD__STATIC_ASSERT),
        KEYWORD__THREAD_LOCAL => Some(Token::KEYWORD__THREAD_LOCAL),
        _ => None,
    };
    if keyword.is_some() {
        *index = byte_index;
    }
    keyword
}
fn chain_lex(program_str_bytes: &[u8], index: &mut usize, is_pp: bool) -> Option<Token> {
    let punctuator = match_punctuator(program_str_bytes, index);
    if punctuator.is_some() {
        return punctuator;
    }
    if !is_pp {
        let keyword = match_keyword(program_str_bytes, index);
        if keyword.is_some() {
            return keyword;
        }
    }
    let identifier = match_identifier(program_str_bytes, index);
    if identifier.is_some() {
        return identifier;
    }
    let string_lit = match_string_literal(program_str_bytes, index);
    if string_lit.is_some() {
        return string_lit;
    }
    let integer_const = match_integer_constant(program_str_bytes, index);
    if integer_const.is_some() {
        return integer_const;
    }
    let float_const = match_floating_constant(program_str_bytes, index);
    if float_const.is_some() {
        return float_const;
    }
    None
}
pub fn lexer(program_str_bytes: Vec<u8>, is_pp: bool) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut index: usize = 0;
    while index < program_str_bytes.len() {
        if program_str_bytes[index] == b'\n' {
            tokens.push(Token::NEWLINE);
            index += 1;
        } else if !program_str_bytes[index].is_ascii_whitespace() {
            let token = chain_lex(&program_str_bytes, &mut index, is_pp);
            if let Some(t) = token {
                tokens.push(t);
            } else {
                return Err(format!(
                    "unhandled token: \"{}\" at index: {}",
                    program_str_bytes[index] as char, index
                ));
            }
        } else {
            while matches!(program_str_bytes.get(index), Some(b' ')) {
                index += 1;
            }
            tokens.push(Token::WHITESPACE);
        }
    }
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::{
        lexer, match_character_constant, match_floating_constant, match_string_literal, Token,
    };

    #[test]
    fn test_match_float_constant_valid_hexadecimal_second_digit_sequence() {
        let s = "0x.0p0";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let float_token = match_floating_constant(s_bytes, &mut index);
        match &float_token {
            Some(super::Token::CONSTANT_HEXA_FLOAT {
                value,
                binary_exp_part,
                suffix: _,
            }) => {
                assert_eq!(*value, "0x.0");
                assert_eq!(*binary_exp_part, "p0");
            }
            _ => panic!(),
        }
        assert!(float_token.is_some());
    }
    #[test]
    fn test_match_float_constant_valid_hexadecimal_first_digit_sequence() {
        let s = "0x0.p0";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let float_token = match_floating_constant(s_bytes, &mut index);
        match &float_token {
            Some(super::Token::CONSTANT_HEXA_FLOAT {
                value,
                binary_exp_part,
                suffix: _,
            }) => {
                assert_eq!(*value, "0x0.");
                assert_eq!(*binary_exp_part, "p0");
            }
            _ => panic!(),
        }
        assert!(float_token.is_some());
    }
    #[test]
    fn test_match_float_constant_none_invalid_fractional_constant() {
        let s = "0x.p0";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let float_token = match_floating_constant(s_bytes, &mut index);
        match &float_token {
            None => {}
            _ => panic!(),
        }
        assert!(float_token.is_none());
    }
    #[test]
    fn test_match_float_constant_none_invalid_exponent() {
        let s = "0x0e";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let float_token = match_floating_constant(s_bytes, &mut index);
        match &float_token {
            None => {}
            _ => panic!(),
        }
        assert!(float_token.is_none());
    }
    #[test]
    fn test_match_float_constant_none() {
        let s = "";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let float_token = match_floating_constant(s_bytes, &mut index);
        match &float_token {
            None => {}
            _ => panic!(),
        }
        assert!(float_token.is_none());
    }
    #[test]
    fn test_match_float_constant_none_invalid_decimal() {
        let s = "01";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let float_token = match_floating_constant(s_bytes, &mut index);
        match &float_token {
            None => {}
            _ => panic!(),
        }
        assert!(float_token.is_none());
    }
    #[test]
    fn test_match_float_constant_none_invalid_hexadecimal() {
        let s = "0x1";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let float_token = match_floating_constant(s_bytes, &mut index);
        match &float_token {
            None => {}
            _ => panic!(),
        }
        assert!(float_token.is_none());
    }
    #[test]
    fn test_match_float_constant_none_invalid_hexa_exponent() {
        let s = "0x1e0";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let float_token = match_floating_constant(s_bytes, &mut index);
        match &float_token {
            None => {}
            _ => panic!(),
        }
        assert!(float_token.is_none());
    }
    #[test]
    fn test_match_float_constant_valid_hexadecimal() {
        let s = "0x1p0";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let float_token = match_floating_constant(s_bytes, &mut index);
        match &float_token {
            Some(super::Token::CONSTANT_HEXA_FLOAT {
                value,
                binary_exp_part,
                suffix: _,
            }) => {
                assert_eq!(value, "0x1");
                assert_eq!(binary_exp_part, "p0");
            }
            _ => panic!(),
        }
        assert!(float_token.is_some());
    }
    #[test]
    fn test_match_float_constant_valid_decimal() {
        let s = "001223e0";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let float_token = match_floating_constant(s_bytes, &mut index);
        match &float_token {
            Some(super::Token::CONSTANT_DEC_FLOAT {
                value,
                exp_part,
                suffix: _,
            }) => {
                assert_eq!(value, "001223");
                assert_eq!(exp_part.clone().unwrap(), "e0");
            }
            _ => panic!(),
        }
        assert!(float_token.is_some());
    }
    #[test]
    fn test_match_float_constant_valid_decimal_suffix() {
        let s = "001223e0L";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let float_token = match_floating_constant(s_bytes, &mut index);
        match &float_token {
            Some(super::Token::CONSTANT_DEC_FLOAT {
                value,
                exp_part,
                suffix,
            }) => {
                assert_eq!(value, "001223");
                assert_eq!(exp_part.clone().unwrap(), "e0");
                assert_eq!(suffix.clone().unwrap(), "L");
            }
            _ => panic!(),
        }
        assert!(float_token.is_some());
    }
    #[test]
    fn test_match_float_constant_valid_hexadecimal_suffix() {
        let s = "0x01223p0L";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let float_token = match_floating_constant(s_bytes, &mut index);
        match &float_token {
            Some(super::Token::CONSTANT_HEXA_FLOAT {
                value,
                binary_exp_part,
                suffix,
            }) => {
                assert_eq!(value, "0x01223");
                assert_eq!(binary_exp_part, "p0");
                assert_eq!(suffix.clone().unwrap(), "L");
            }
            _ => panic!(),
        }
        assert!(float_token.is_some());
    }
    #[test]
    fn test_match_float_constant_valid_hexadecimal_suffix_sign() {
        let s = "0x01223p+0L";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let float_token = match_floating_constant(s_bytes, &mut index);
        match &float_token {
            Some(super::Token::CONSTANT_HEXA_FLOAT {
                value,
                binary_exp_part,
                suffix,
            }) => {
                assert_eq!(value, "0x01223");
                assert_eq!(binary_exp_part, "p+0");
                assert_eq!(suffix.clone().unwrap(), "L");
            }
            _ => panic!(),
        }
        assert!(float_token.is_some());
    }
    #[test]
    fn test_match_character_constant() {
        let s = "U'hi'";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let char_token = match_character_constant(s_bytes, &mut index);
        match &char_token {
            Some(super::Token::CONSTANT_CHAR(s)) => {
                assert_eq!(s, "U'hi'");
            }
            _ => panic!(),
        }
        assert!(char_token.is_some());
    }
    #[test]
    fn test_match_character_constant_no_prefix() {
        let s = "'hi'";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let char_token = match_character_constant(s_bytes, &mut index);
        match &char_token {
            Some(super::Token::CONSTANT_CHAR(s)) => {
                assert_eq!(s, "'hi'");
            }
            _ => panic!(),
        }
        assert!(char_token.is_some());
    }
    #[test]
    fn test_match_character_constant_wchar_t() {
        let s = "L'hi'";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let char_token = match_character_constant(s_bytes, &mut index);
        match &char_token {
            Some(super::Token::CONSTANT_CHAR(s)) => {
                assert_eq!(s, "L'hi'");
            }
            _ => panic!(),
        }
        assert!(char_token.is_some());
    }
    #[test]
    fn test_match_character_constant_char16_t() {
        let s = "u'hi'";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let char_token = match_character_constant(s_bytes, &mut index);
        match &char_token {
            Some(super::Token::CONSTANT_CHAR(s)) => {
                assert_eq!(s, "u'hi'");
            }
            _ => panic!(),
        }
        assert!(char_token.is_some());
    }
    #[test]
    fn test_match_character_constant_char32_t() {
        let s = "U'hi'";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let char_token = match_character_constant(s_bytes, &mut index);
        match &char_token {
            Some(super::Token::CONSTANT_CHAR(s)) => {
                assert_eq!(s, "U'hi'");
            }
            _ => panic!(),
        }
        assert!(char_token.is_some());
    }
    #[test]
    fn test_match_string_literal() {
        let s = "U\"hi\"";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let string_literal = match_string_literal(s_bytes, &mut index);
        match &string_literal {
            Some(super::Token::StringLiteral { prefix, sequence }) => {
                assert_eq!(prefix.clone().unwrap(), "U");
                assert_eq!(sequence.clone(), "hi");
            }
            _ => panic!(),
        }
        assert!(string_literal.is_some());
    }
    #[test]
    fn test_match_string_literal_no_prefix() {
        let s = "\"hi\"";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let string_literal = match_string_literal(s_bytes, &mut index);
        match &string_literal {
            Some(super::Token::StringLiteral { prefix, sequence }) => {
                assert!(prefix.is_none());
                assert_eq!(sequence.clone(), "hi");
            }
            _ => panic!(),
        }
        assert!(string_literal.is_some());
    }
    #[test]
    fn test_match_string_literal_no_prefix_universal_char_name() {
        let s = "\"\\U0001F600\"";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let string_literal = match_string_literal(s_bytes, &mut index);
        match &string_literal {
            Some(super::Token::StringLiteral { prefix, sequence }) => {
                assert!(prefix.is_none());
                assert_eq!(sequence.clone(), "\\U0001F600");
            }
            _ => panic!(),
        }
        assert!(string_literal.is_some());
    }
    #[test]
    fn test_lexer() -> Result<(), String> {
        let s = "int main() {\nint hi = 4;\nreturn 0;\n}";
        let s_bytes = s.as_bytes();
        let tokens = lexer(s_bytes.to_vec(), false)?;
        let tokens_assert = vec![
            Token::KEYWORD_INT,
            Token::WHITESPACE,
            Token::IDENT("main".to_string()),
            Token::PUNCT_OPEN_PAR,
            Token::PUNCT_CLOSE_PAR,
            Token::WHITESPACE,
            Token::PUNCT_OPEN_CURLY,
            Token::NEWLINE,
            Token::KEYWORD_INT,
            Token::WHITESPACE,
            Token::IDENT("hi".to_string()),
            Token::WHITESPACE,
            Token::PUNCT_ASSIGNMENT,
            Token::WHITESPACE,
            Token::CONSTANT_DEC_INT {
                value: "4".to_string(),
                suffix: None,
            },
            Token::PUNCT_SEMI_COLON,
            Token::NEWLINE,
            Token::KEYWORD_RETURN,
            Token::WHITESPACE,
            Token::CONSTANT_DEC_INT {
                value: "0".to_string(),
                suffix: None,
            },
            Token::PUNCT_SEMI_COLON,
            Token::NEWLINE,
            Token::PUNCT_CLOSE_CURLY,
        ];
        assert_eq!(tokens, tokens_assert);
        Ok(())
    }
    #[test]
    fn test_lexer_directives() -> Result<(), String> {
        let s = "#include <stdio.h>\nint main() {}";
        let s_bytes = s.as_bytes();
        let tokens = lexer(s_bytes.to_vec(), true)?;
        let tokens_assert = vec![
            Token::PUNCT_HASH,
            Token::IDENT("include".to_string()),
            Token::WHITESPACE,
            Token::PUNCT_LESS_THAN,
            Token::IDENT("stdio".to_string()),
            Token::PUNCT_DOT,
            Token::IDENT("h".to_string()),
            Token::PUNCT_GREATER_THAN,
            Token::NEWLINE,
            Token::IDENT("int".to_string()),
            Token::WHITESPACE,
            Token::IDENT("main".to_string()),
            Token::PUNCT_OPEN_PAR,
            Token::PUNCT_CLOSE_PAR,
            Token::WHITESPACE,
            Token::PUNCT_OPEN_CURLY,
            Token::PUNCT_CLOSE_CURLY,
        ];
        assert_eq!(tokens, tokens_assert);
        Ok(())
    }
    #[test]
    fn test_lexer_if_directives() -> Result<(), String> {
        let s = "#if 1 + 1\n#define CHICKEN 5\n#endif\n";
        let s_bytes = s.as_bytes();
        let tokens = lexer(s_bytes.to_vec(), true)?;
        let tokens_assert = vec![
            Token::PUNCT_HASH,
            Token::IDENT("if".to_string()),
            Token::WHITESPACE,
            Token::CONSTANT_DEC_INT {
                value: "1".to_string(),
                suffix: None,
            },
            Token::WHITESPACE,
            Token::PUNCT_PLUS,
            Token::WHITESPACE,
            Token::CONSTANT_DEC_INT {
                value: "1".to_string(),
                suffix: None,
            },
            Token::NEWLINE,
            Token::PUNCT_HASH,
            Token::IDENT("define".to_string()),
            Token::WHITESPACE,
            Token::IDENT("CHICKEN".to_string()),
            Token::WHITESPACE,
            Token::CONSTANT_DEC_INT {
                value: "5".to_string(),
                suffix: None,
            },
            Token::NEWLINE,
            Token::PUNCT_HASH,
            Token::IDENT("endif".to_string()),
            Token::NEWLINE,
        ];
        assert_eq!(tokens, tokens_assert);
        Ok(())
    }
}
