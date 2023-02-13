#[allow(non_camel_case_types)]
enum Token {
    IDENT(String),
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
    CONSTANT_ENUM(String),
    CONSTANT_CHAR(String),
}
fn match_constant_literal(program_str_bytes: &[u8], index: &mut usize) -> Option<Token> {
    let mut byte_index = *index;
    match program_str_bytes[byte_index] {
        b'0' => {
            // could be octal or hexadecimal
            let mut is_hexa = false;
            if byte_index + 1 < program_str_bytes.len()
                && (program_str_bytes[byte_index + 1] == b'x'
                    || program_str_bytes[byte_index + 1] == b'X')
            {
                byte_index += 2;
                is_hexa = true;
            }
            while byte_index < program_str_bytes.len()
                && program_str_bytes[byte_index].is_ascii_digit()
            {
                byte_index += 1;
            }
            let has_dot_exponent =
                [b'.', b'e', b'E', b'p', b'P'].contains(&program_str_bytes[byte_index]);
            let mut token = match (is_hexa, has_dot_exponent) {
                (true, true) => Token::CONSTANT_HEXA_FLOAT {
                    value: program_str_bytes[*index..byte_index]
                        .iter()
                        .fold(String::new(), |acc, e| acc + &e.to_string()),
                    binary_exp_part: String::new(),
                    suffix: None,
                },
                (true, false) => Token::CONSTANT_HEXA_INT {
                    value: program_str_bytes[*index..byte_index]
                        .iter()
                        .fold(String::new(), |acc, e| acc + &e.to_string()),
                    suffix: None,
                },
                (false, true) => Token::CONSTANT_DEC_FLOAT {
                    value: program_str_bytes[*index..byte_index]
                        .iter()
                        .fold(String::new(), |acc, e| acc + &e.to_string()),
                    exp_part: None,
                    suffix: None,
                },
                (false, false) => Token::CONSTANT_OCTAL_INT {
                    value: program_str_bytes[*index..byte_index]
                        .iter()
                        .fold(String::new(), |acc, e| acc + &e.to_string()),
                    suffix: None,
                },
            };
            // TODO: handle the rest of the floating constant
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
                | [b'U', b'L', b'L'] => {}
                _ => {
                    return None;
                }
            }
        }
        b'u' | b'U' | b'L' | b'"' => {}
        _ => {
            if program_str_bytes[byte_index].is_ascii_digit() {
                while byte_index < program_str_bytes.len()
                    && program_str_bytes[byte_index].is_ascii_digit()
                {
                    byte_index += 1;
                }
            }
        }
    }
    None
}
fn match_punctuator(program_str_bytes: &[u8], index: &mut usize) -> Option<Token> {
    let byte_index = *index;
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
            if byte_index + 1 < program_str_bytes.len() && program_str_bytes[byte_index + 1] == b'='
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
            if byte_index + 1 < program_str_bytes.len() && program_str_bytes[byte_index + 1] == b'='
            {
                *index += 2;
                return Some(Token::PUNCT_NOT_EQ_BOOL);
            }
            *index += 1;
            return Some(Token::PUNCT_NOT_BOOL);
        }
        b'/' => {
            if byte_index + 1 < program_str_bytes.len() && program_str_bytes[byte_index + 1] == b'='
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
            if byte_index + 1 < program_str_bytes.len() && program_str_bytes[byte_index + 1] == b'='
            {
                *index += 2;
                return Some(Token::PUNCT_EQ_BOOL);
            }
            *index += 1;
            return Some(Token::PUNCT_ASSIGNMENT);
        }
        b'^' => {
            if byte_index + 1 < program_str_bytes.len() && program_str_bytes[byte_index + 1] == b'='
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
            if byte_index + 1 < program_str_bytes.len() && program_str_bytes[byte_index + 1] == b'>'
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
            if byte_index + 1 < program_str_bytes.len() && program_str_bytes[byte_index + 1] == b'#'
            {
                *index += 2;
                return Some(Token::PUNCT_HASH_HASH);
            }
            *index += 1;
            return Some(Token::PUNCT_HASH);
        }
        _ => {}
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
    // TODO: we need to check for universal character names
    if bytes.len() > 0
        && !bytes[0].is_ascii_digit()
        && bytes
            .iter()
            .map(|b| b.to_string())
            .fold(String::new(), |acc, e| acc + &e)
            != "__func__"
    {
        *index = byte_index;
        return Some(Token::IDENT(
            bytes
                .iter()
                .map(|b| b.to_string())
                .fold(String::new(), |acc, e| acc + &e),
        ));
    } else if bytes
        .iter()
        .map(|b| b.to_string())
        .fold(String::new(), |acc, e| acc + &e)
        == "__func__"
    {
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
    let keyword = match &bytes
        .iter()
        .map(|b| b.to_string())
        .fold(String::new(), |acc, e| acc + &e)[..]
    {
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
fn lexer(program_str: String) -> Vec<Token> {
    let tokens = Vec::new();
    let program_str_bytes = program_str.as_bytes();
    let mut index: usize = 0;
    while index < program_str_bytes.len() {
        if !program_str_bytes[index].is_ascii_whitespace() {
            let _matched_keyword = match_keyword(program_str_bytes, &mut index);
        }
    }
    tokens
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_lexer() {
        let _program_str: &'static str = "int main() {\n\tint four = 4;\n\treturn 0;\n}";
    }
}
