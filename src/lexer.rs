use crate::error;
use std::collections::HashMap;

// TODO: it'd be faster to just push slices into the vec and keep the original bytes vector
// from reading the file alive for the rest of the program.
// This should only be used for identifiers but as of right now, it's being used for both string
// literals and identifiers.
pub struct ByteVecMaps {
    pub key_to_byte_vec: Vec<Vec<u8>>,
    pub byte_vec_to_key: HashMap<Vec<u8>, usize>,
}

impl ByteVecMaps {
    pub fn new() -> ByteVecMaps {
        ByteVecMaps {
            key_to_byte_vec: Vec::new(),
            byte_vec_to_key: HashMap::new(),
        }
    }
    pub fn add_byte_vec(&mut self, bytes_vec: &[u8]) -> usize {
        if !self.byte_vec_to_key.contains_key(bytes_vec) {
            let key = self.key_to_byte_vec.len();
            self.key_to_byte_vec.push(bytes_vec.to_vec());
            self.byte_vec_to_key.insert(bytes_vec.to_vec(), key);
            key
        } else {
            let Some(key) = self.byte_vec_to_key.get(bytes_vec) else {
                unreachable!()
            };
            *key
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct ConstantChar {
    pub prefix: Option<u8>,
    pub sequence_key: usize,
}

impl ConstantChar {
    fn new(prefix: Option<u8>, sequence_key: usize) -> ConstantChar {
        ConstantChar {
            prefix,
            sequence_key,
        }
    }
    pub fn parse_to_value(&self, str_maps: &ByteVecMaps) -> Result<u32, String> {
        let mut byte_vec = Vec::new();
        let mut byte_index = 0;
        while byte_index < str_maps.key_to_byte_vec[self.sequence_key].len() {
            if str_maps.key_to_byte_vec[self.sequence_key][byte_index] == b'\\' {
                if byte_index + 1 < str_maps.key_to_byte_vec[self.sequence_key].len() {
                    let b = str_maps.key_to_byte_vec[self.sequence_key][byte_index + 1];
                    match b {
                        b'a' | b'b' | b'f' | b'n' | b'r' | b't' | b'v' | b'\'' | b'"' | b'?'
                        | b'\\' => {
                            let escaped_byte = match b {
                                b'a' => 7,  // bell ascii
                                b'b' => 8,  // backspace ascii
                                b'f' => 12, // formfeed ascii
                                b'n' => b'\n',
                                b'r' => b'\r',
                                b't' => b'\t',
                                b'v' => 11, // vertical tab ascii
                                b'\'' => b'\'',
                                b'"' => b'"',
                                b'?' => b'?',
                                b'\\' => b'\\',
                                _ => unreachable!(),
                            };
                            byte_vec.push(escaped_byte);
                            byte_index += 2;
                            continue;
                        }
                        b'0' | b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' => {
                            let mut octal_seq_index = byte_index + 1;
                            while octal_seq_index < byte_index + 4
                                && octal_seq_index
                                    < str_maps.key_to_byte_vec[self.sequence_key].len()
                                && (b'0'..=b'7').contains(
                                    &str_maps.key_to_byte_vec[self.sequence_key][octal_seq_index],
                                )
                            {
                                octal_seq_index += 1;
                            }
                            let octal_seq_vec = str_maps.key_to_byte_vec[self.sequence_key]
                                [byte_index + 1..octal_seq_index]
                                .to_vec();
                            let Ok(octal_str) = String::from_utf8(octal_seq_vec) else {
                                unreachable!()
                            };
                            match u8::from_str_radix(octal_str.as_str(), 8) {
                                Ok(v) => {
                                    byte_vec.push(v);
                                    byte_index = octal_seq_index;
                                    continue;
                                }
                                Err(parse_int_error) => match *parse_int_error.kind() {
                                    std::num::IntErrorKind::Empty => {
                                        return Err(format!(
                                            "Attempted to parse an empty string into a octal digit"
                                        ));
                                    }
                                    std::num::IntErrorKind::InvalidDigit => {
                                        return Err(format!("Attempted to parse an invalid string into an octal digit"));
                                    }
                                    std::num::IntErrorKind::PosOverflow => {
                                        return Err(format!("Attempted to parse a string that is too positive to fit into an octal digit"));
                                    }
                                    std::num::IntErrorKind::NegOverflow => {
                                        return Err(format!("Attempted to parse a string that is too negative to fit into an octal digit"));
                                    }
                                    std::num::IntErrorKind::Zero => {
                                        return Err(format!(
                                            "Value parsed was zero but should not be"
                                        ));
                                    }
                                    _ => unreachable!("Probably new parse int error enum variant"),
                                },
                            }
                        }
                        // TODO: there is implemenation-defined behavior here
                        // TODO: we need to go back and document all of the implementation defined
                        // behaviors sooner or later.
                        b'x' => {
                            let mut hex_seq_index = byte_index + 2;
                            while hex_seq_index < str_maps.key_to_byte_vec[self.sequence_key].len()
                                && str_maps.key_to_byte_vec[self.sequence_key][hex_seq_index]
                                    .is_ascii_hexdigit()
                            {
                                hex_seq_index += 1;
                            }
                            let mut start_of_seq = byte_index + 2;
                            if (hex_seq_index - start_of_seq) & 1 != 0 {
                                let b = str_maps.key_to_byte_vec[self.sequence_key][start_of_seq];
                                let hex_val_byte = if b.is_ascii_digit() {
                                    b - b'0'
                                } else {
                                    b.to_ascii_lowercase() - b'a' + 10
                                };
                                byte_vec.push(hex_val_byte);
                                start_of_seq += 1;
                            }
                            while start_of_seq < hex_seq_index {
                                let hex_seq_vec = str_maps.key_to_byte_vec[self.sequence_key]
                                    [start_of_seq..start_of_seq + 2]
                                    .to_vec();
                                let Ok(hex_str) = String::from_utf8(hex_seq_vec) else {
                                    unreachable!()
                                };
                                match u8::from_str_radix(hex_str.as_str(), 16) {
                                    Ok(v) => {
                                        byte_vec.push(v);
                                    }
                                    Err(parse_int_error) => match *parse_int_error.kind() {
                                        std::num::IntErrorKind::Empty => {
                                            return Err(format!("Attempted to parse an empty string into a hex digit"));
                                        }
                                        std::num::IntErrorKind::InvalidDigit => {
                                            return Err(format!("Attempted to parse an invalid string into an hex digit"));
                                        }
                                        std::num::IntErrorKind::PosOverflow => {
                                            return Err(format!("Attempted to parse a string that is too positive to fit into an hex digit"));
                                        }
                                        std::num::IntErrorKind::NegOverflow => {
                                            return Err(format!("Attempted to parse a string that is too negative to fit into an hex digit"));
                                        }
                                        std::num::IntErrorKind::Zero => {
                                            return Err(format!(
                                                "Value parsed was zero but should not be"
                                            ));
                                        }
                                        _ => unreachable!(
                                            "Probably new parse int error enum variant"
                                        ),
                                    },
                                }
                                start_of_seq += 2;
                            }
                            byte_index = hex_seq_index;
                            continue;
                        }
                        _ => {
                            return Err(format!("unknown escape character \\{}", b as char));
                        }
                    }
                } else {
                    return Err(format!("character constant cannot have \\ in sequence"));
                }
            } else {
                byte_vec.push(str_maps.key_to_byte_vec[self.sequence_key][byte_index]);
            }
            byte_index += 1;
        }
        if byte_vec.len() > 4 {
            return Err(format!("character constant has too many bytes"));
        }
        let mut val: u32 = 0;
        for index in (0..byte_vec.len()).rev() {
            val |= (byte_vec[index] << ((byte_vec.len() - 1 - index) * 8)) as u32;
        }
        Ok(val)
    }
}

// TODO: create another data structure for storing string literal
// It isn't a good idea to use str_maps to store identifiers and string_literals
#[derive(Copy, Clone, PartialEq, Debug)]
pub struct StringLiteral {
    pub prefix_key: Option<usize>,
    pub sequence_key: usize,
}

impl StringLiteral {
    fn new(prefix_key: Option<usize>, sequence_key: usize) -> StringLiteral {
        StringLiteral {
            prefix_key,
            sequence_key,
        }
    }
    pub fn parse_to_byte_vec(&self, str_maps: &ByteVecMaps) -> Result<Vec<u8>, String> {
        let mut byte_vec = Vec::new();
        let mut byte_index = 0;
        while byte_index < str_maps.key_to_byte_vec[self.sequence_key].len() {
            if str_maps.key_to_byte_vec[self.sequence_key][byte_index] == b'\\' {
                if byte_index + 1 < str_maps.key_to_byte_vec[self.sequence_key].len() {
                    let b = str_maps.key_to_byte_vec[self.sequence_key][byte_index + 1];
                    match b {
                        b'a' | b'b' | b'f' | b'n' | b'r' | b't' | b'v' | b'\'' | b'"' | b'?'
                        | b'\\' => {
                            let escaped_byte = match b {
                                b'a' => 7,  // bell ascii
                                b'b' => 8,  // backspace ascii
                                b'f' => 12, // formfeed ascii
                                b'n' => b'\n',
                                b'r' => b'\r',
                                b't' => b'\t',
                                b'v' => 11, // vertical tab ascii
                                b'\'' => b'\'',
                                b'"' => b'"',
                                b'?' => b'?',
                                b'\\' => b'\\',
                                _ => unreachable!(),
                            };
                            byte_vec.push(escaped_byte);
                            byte_index += 2;
                            continue;
                        }
                        b'0' | b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' => {
                            let mut octal_seq_index = byte_index + 1;
                            while octal_seq_index < byte_index + 4
                                && octal_seq_index
                                    < str_maps.key_to_byte_vec[self.sequence_key].len()
                                && (b'0'..=b'7').contains(
                                    &str_maps.key_to_byte_vec[self.sequence_key][octal_seq_index],
                                )
                            {
                                octal_seq_index += 1;
                            }
                            let octal_seq_vec = str_maps.key_to_byte_vec[self.sequence_key]
                                [byte_index + 1..octal_seq_index]
                                .to_vec();
                            let Ok(octal_str) = String::from_utf8(octal_seq_vec) else {
                                unreachable!()
                            };
                            match u8::from_str_radix(octal_str.as_str(), 8) {
                                Ok(v) => {
                                    byte_vec.push(v);
                                    byte_index = octal_seq_index;
                                    continue;
                                }
                                Err(parse_int_error) => match *parse_int_error.kind() {
                                    std::num::IntErrorKind::Empty => {
                                        return Err(format!(
                                            "Attempted to parse an empty string into a octal digit"
                                        ));
                                    }
                                    std::num::IntErrorKind::InvalidDigit => {
                                        return Err(format!("Attempted to parse an invalid string into an octal digit"));
                                    }
                                    std::num::IntErrorKind::PosOverflow => {
                                        return Err(format!("Attempted to parse a string that is too positive to fit into an octal digit"));
                                    }
                                    std::num::IntErrorKind::NegOverflow => {
                                        return Err(format!("Attempted to parse a string that is too negative to fit into an octal digit"));
                                    }
                                    std::num::IntErrorKind::Zero => {
                                        return Err(format!(
                                            "Value parsed was zero but should not be"
                                        ));
                                    }
                                    _ => unreachable!("Probably new parse int error enum variant"),
                                },
                            }
                        }
                        // TODO: there is implemenation-defined behavior here
                        // TODO: we need to go back and document all of the implementation defined
                        // behaviors sooner or later.
                        b'x' => {
                            let mut hex_seq_index = byte_index + 2;
                            while hex_seq_index < str_maps.key_to_byte_vec[self.sequence_key].len()
                                && str_maps.key_to_byte_vec[self.sequence_key][hex_seq_index]
                                    .is_ascii_hexdigit()
                            {
                                hex_seq_index += 1;
                            }
                            let mut start_of_seq = byte_index + 2;
                            if (hex_seq_index - start_of_seq) & 1 != 0 {
                                let b = str_maps.key_to_byte_vec[self.sequence_key][start_of_seq];
                                let hex_val_byte = if b.is_ascii_digit() {
                                    b - b'0'
                                } else {
                                    b.to_ascii_lowercase() - b'a' + 10
                                };
                                byte_vec.push(hex_val_byte);
                                start_of_seq += 1;
                            }
                            while start_of_seq < hex_seq_index {
                                let hex_seq_vec = str_maps.key_to_byte_vec[self.sequence_key]
                                    [start_of_seq..start_of_seq + 2]
                                    .to_vec();
                                let Ok(hex_str) = String::from_utf8(hex_seq_vec) else {
                                    unreachable!()
                                };
                                match u8::from_str_radix(hex_str.as_str(), 16) {
                                    Ok(v) => {
                                        byte_vec.push(v);
                                    }
                                    Err(parse_int_error) => match *parse_int_error.kind() {
                                        std::num::IntErrorKind::Empty => {
                                            return Err(format!("Attempted to parse an empty string into a hex digit"));
                                        }
                                        std::num::IntErrorKind::InvalidDigit => {
                                            return Err(format!("Attempted to parse an invalid string into an hex digit"));
                                        }
                                        std::num::IntErrorKind::PosOverflow => {
                                            return Err(format!("Attempted to parse a string that is too positive to fit into an hex digit"));
                                        }
                                        std::num::IntErrorKind::NegOverflow => {
                                            return Err(format!("Attempted to parse a string that is too negative to fit into an hex digit"));
                                        }
                                        std::num::IntErrorKind::Zero => {
                                            return Err(format!(
                                                "Value parsed was zero but should not be"
                                            ));
                                        }
                                        _ => unreachable!(
                                            "Probably new parse int error enum variant"
                                        ),
                                    },
                                }
                                start_of_seq += 2;
                            }
                            byte_index = hex_seq_index;
                            continue;
                        }
                        _ => {
                            return Err(format!("unknown escape character \\{}", b as char));
                        }
                    }
                } else {
                    return Err(format!("string literal cannot have \\ in sequence"));
                }
            } else {
                byte_vec.push(str_maps.key_to_byte_vec[self.sequence_key][byte_index]);
            }
            byte_index += 1;
        }
        byte_vec.push(0);
        Ok(byte_vec)
    }
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum IntegerSuffix {
    Unsigned,
    Long,
    LongLong,
    UnsignedLong,
    UnsignedLongLong,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum FloatSuffix {
    Float,
    LongDouble,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Suffix {
    Float {
        float_type: FloatSuffix,
        key: usize,
    },
    Integer {
        integer_type: IntegerSuffix,
        key: usize,
    },
}

#[allow(non_camel_case_types)]
#[derive(PartialEq, Debug, Copy, Clone)]
pub enum TokenType {
    IDENT {
        str_map_key: usize,
    },
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
    PUNCT_BITSHIFT_LEFT,
    PUNCT_BITSHIFT_RIGHT,
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
    CONSTANT_ENUM {
        value_key: usize,
    },
    CONSTANT_OCTAL_INT {
        value_key: usize,
        suffix: Option<Suffix>,
    },
    CONSTANT_HEXA_INT {
        value_key: usize,
        suffix: Option<Suffix>,
    },
    CONSTANT_DEC_INT {
        value_key: usize,
        suffix: Option<Suffix>,
    },
    CONSTANT_DEC_FLOAT {
        value_key: usize,
        exp_part_key: Option<usize>,
        suffix: Option<Suffix>,
    },
    CONSTANT_HEXA_FLOAT {
        value_key: usize,
        binary_exp_part_key: usize,
        suffix: Option<Suffix>,
    },
    StringLiteral {
        str_lit: StringLiteral,
    },
    CONSTANT_CHAR {
        const_char: ConstantChar,
    },
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Location {
    pub column: usize,
    pub line: usize,
}

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Token {
    pub r#type: TokenType,
    pub location: Option<Location>,
}

impl Token {
    pub fn to_byte_vec(&self, str_maps: &ByteVecMaps) -> Option<Vec<u8>> {
        match self.r#type {
            TokenType::CONSTANT_HEXA_FLOAT {
                value_key,
                binary_exp_part_key,
                suffix,
            } => {
                if let Some(suff_key) = suffix {
                    let Suffix::Float { float_type, key } = suff_key else {
                        unreachable!()
                    };
                    let mut vec = str_maps.key_to_byte_vec[value_key].to_vec();
                    vec.extend_from_slice(&str_maps.key_to_byte_vec[binary_exp_part_key]);
                    vec.extend_from_slice(&str_maps.key_to_byte_vec[key]);
                    Some(vec)
                } else {
                    let mut vec = str_maps.key_to_byte_vec[value_key].to_vec();
                    vec.extend_from_slice(&str_maps.key_to_byte_vec[binary_exp_part_key]);
                    Some(vec)
                }
            }
            TokenType::CONSTANT_HEXA_INT { value_key, suffix } => {
                if let Some(suff_key) = suffix {
                    let Suffix::Integer { integer_type, key } = suff_key else {
                        unreachable!()
                    };
                    let mut vec = str_maps.key_to_byte_vec[value_key].to_vec();
                    vec.extend_from_slice(&str_maps.key_to_byte_vec[key]);
                    Some(vec)
                } else {
                    Some(str_maps.key_to_byte_vec[value_key].to_vec())
                }
            }
            TokenType::CONSTANT_DEC_FLOAT {
                value_key,
                exp_part_key,
                suffix,
            } => match (exp_part_key, suffix) {
                (Some(ep_key), Some(suff_key)) => {
                    let Suffix::Float { float_type, key } = suff_key else {
                        unreachable!()
                    };
                    let mut vec = str_maps.key_to_byte_vec[value_key].to_vec();
                    vec.extend_from_slice(&str_maps.key_to_byte_vec[ep_key]);
                    vec.extend_from_slice(&str_maps.key_to_byte_vec[key]);
                    Some(vec)
                }
                (_, Some(suff_key)) => {
                    let Suffix::Float { float_type, key } = suff_key else {
                        unreachable!()
                    };
                    let mut vec = str_maps.key_to_byte_vec[value_key].to_vec();
                    vec.extend_from_slice(&str_maps.key_to_byte_vec[key]);
                    Some(vec)
                }
                _ => Some(str_maps.key_to_byte_vec[value_key].to_vec()),
            },
            TokenType::CONSTANT_CHAR {
                const_char:
                    ConstantChar {
                        prefix,
                        sequence_key,
                    },
            } => {
                if let Some(prefix_byte) = prefix {
                    let mut vec = vec![prefix_byte];
                    vec.push(b'\'');
                    vec.extend_from_slice(&str_maps.key_to_byte_vec[sequence_key]);
                    vec.push(b'\'');
                    Some(vec)
                } else {
                    let mut vec = vec![];
                    vec.push(b'\'');
                    vec.extend_from_slice(&str_maps.key_to_byte_vec[sequence_key]);
                    vec.push(b'\'');
                    Some(vec)
                }
            }
            TokenType::StringLiteral {
                str_lit:
                    StringLiteral {
                        prefix_key,
                        sequence_key,
                    },
            } => {
                if let Some(pre_key) = prefix_key {
                    let mut vec = str_maps.key_to_byte_vec[pre_key].to_vec();
                    vec.push(b'\"');
                    vec.extend_from_slice(&str_maps.key_to_byte_vec[sequence_key]);
                    vec.push(b'\"');
                    Some(vec)
                } else {
                    let mut vec = vec![b'\"'];
                    vec.extend_from_slice(&str_maps.key_to_byte_vec[sequence_key]);
                    vec.push(b'\"');
                    Some(vec)
                }
            }
            TokenType::CONSTANT_DEC_INT { value_key, suffix } => {
                if let Some(suff_key) = suffix {
                    let Suffix::Integer { integer_type, key } = suff_key else {
                        unreachable!()
                    };
                    let mut vec = str_maps.key_to_byte_vec[value_key].to_vec();
                    vec.extend_from_slice(&str_maps.key_to_byte_vec[key]);
                    Some(vec)
                } else {
                    Some(str_maps.key_to_byte_vec[value_key].to_vec())
                }
            }
            TokenType::IDENT { str_map_key, .. } => {
                Some(str_maps.key_to_byte_vec[str_map_key].to_vec())
            }
            TokenType::WHITESPACE => Some(" ".as_bytes().to_vec()),
            TokenType::NEWLINE => Some("\n".as_bytes().to_vec()),
            TokenType::PUNCT_OPEN_SQR => Some("[".as_bytes().to_vec()),
            TokenType::PUNCT_CLOSE_SQR => Some("]".as_bytes().to_vec()),
            TokenType::PUNCT_OPEN_PAR => Some("(".as_bytes().to_vec()),
            TokenType::PUNCT_CLOSE_PAR => Some(")".as_bytes().to_vec()),
            TokenType::PUNCT_OPEN_CURLY => Some("{".as_bytes().to_vec()),
            TokenType::PUNCT_CLOSE_CURLY => Some("}".as_bytes().to_vec()),
            TokenType::PUNCT_DOT => Some(".".as_bytes().to_vec()),
            TokenType::PUNCT_ARROW => Some("{..}=>".as_bytes().to_vec()),
            TokenType::PUNCT_INCREMENT => Some("++".as_bytes().to_vec()),
            TokenType::PUNCT_DECREMENT => Some("--".as_bytes().to_vec()),
            TokenType::PUNCT_AND_BIT => Some("&".as_bytes().to_vec()),
            TokenType::PUNCT_MULT => Some("*".as_bytes().to_vec()),
            TokenType::PUNCT_PLUS => Some("+".as_bytes().to_vec()),
            TokenType::PUNCT_MINUS => Some("-".as_bytes().to_vec()),
            TokenType::PUNCT_TILDE => Some("~".as_bytes().to_vec()),
            TokenType::PUNCT_NOT_BOOL => Some("!".as_bytes().to_vec()),
            TokenType::PUNCT_DIV => Some("/".as_bytes().to_vec()),
            TokenType::PUNCT_MODULO => Some("%".as_bytes().to_vec()),
            TokenType::PUNCT_BITSHIFT_LEFT => Some("<<".as_bytes().to_vec()),
            TokenType::PUNCT_BITSHIFT_RIGHT => Some(">>".as_bytes().to_vec()),
            TokenType::PUNCT_LESS_THAN => Some("<".as_bytes().to_vec()),
            TokenType::PUNCT_GREATER_THAN => Some(">".as_bytes().to_vec()),
            TokenType::PUNCT_LESS_THAN_EQ => Some("<=".as_bytes().to_vec()),
            TokenType::PUNCT_GREATER_THAN_EQ => Some(">=".as_bytes().to_vec()),
            TokenType::PUNCT_EQ_BOOL => Some("==".as_bytes().to_vec()),
            TokenType::PUNCT_NOT_EQ_BOOL => Some("!=".as_bytes().to_vec()),
            TokenType::PUNCT_XOR_BIT => Some("^".as_bytes().to_vec()),
            TokenType::PUNCT_OR_BIT => Some("|".as_bytes().to_vec()),
            TokenType::PUNCT_AND_BOOL => Some("&&".as_bytes().to_vec()),
            TokenType::PUNCT_OR_BOOL => Some("||".as_bytes().to_vec()),
            TokenType::PUNCT_QUESTION_MARK => Some("?".as_bytes().to_vec()),
            TokenType::PUNCT_COLON => Some(":".as_bytes().to_vec()),
            TokenType::PUNCT_SEMI_COLON => Some(";".as_bytes().to_vec()),
            TokenType::PUNCT_ELLIPSIS => Some("...".as_bytes().to_vec()),
            TokenType::PUNCT_ASSIGNMENT => Some("=".as_bytes().to_vec()),
            TokenType::PUNCT_MULT_ASSIGN => Some("*=".as_bytes().to_vec()),
            TokenType::PUNCT_DIV_ASSIGN => Some("/=".as_bytes().to_vec()),
            TokenType::PUNCT_MODULO_ASSIGN => Some("%=".as_bytes().to_vec()),
            TokenType::PUNCT_ADD_ASSIGN => Some("+=".as_bytes().to_vec()),
            TokenType::PUNCT_SUB_ASSIGN => Some("-=".as_bytes().to_vec()),
            TokenType::PUNCT_L_SHIFT_BIT_ASSIGN => Some("<<=".as_bytes().to_vec()),
            TokenType::PUNCT_R_SHIFT_BIT_ASSIGN => Some(">>=".as_bytes().to_vec()),
            TokenType::PUNCT_AND_BIT_ASSIGN => Some("&=".as_bytes().to_vec()),
            TokenType::PUNCT_XOR_BIT_ASSIGN => Some("^=".as_bytes().to_vec()),
            TokenType::PUNCT_OR_BIT_ASSIGN => Some("|=".as_bytes().to_vec()),
            TokenType::PUNCT_COMMA => Some(",".as_bytes().to_vec()),
            TokenType::PUNCT_HASH => Some("#".as_bytes().to_vec()),
            TokenType::PUNCT_HASH_HASH => Some("##".as_bytes().to_vec()),
            TokenType::PUNCT_DIGRAPH_OPEN_SQR => Some("<:".as_bytes().to_vec()),
            TokenType::PUNCT_DIGRAPH_CLOSE_SQR => Some(":>".as_bytes().to_vec()),
            TokenType::PUNCT_DIGRAPH_OPEN_CURLY => Some("<%".as_bytes().to_vec()),
            TokenType::PUNCT_DIGRAPH_CLOSE_CURLY => Some("%>".as_bytes().to_vec()),
            TokenType::PUNCT_DIGRAPH_HASH => Some("%:".as_bytes().to_vec()),
            TokenType::PUNCT_DIGRAPH_HASH_HASH => Some("%:%:".as_bytes().to_vec()),
            TokenType::KEYWORD_AUTO => Some("auto".as_bytes().to_vec()),
            TokenType::KEYWORD_BREAK => Some("break".as_bytes().to_vec()),
            TokenType::KEYWORD_CASE => Some("case".as_bytes().to_vec()),
            TokenType::KEYWORD_CHAR => Some("char".as_bytes().to_vec()),
            TokenType::KEYWORD_CONST => Some("const".as_bytes().to_vec()),
            TokenType::KEYWORD_CONTINUE => Some("continue".as_bytes().to_vec()),
            TokenType::KEYWORD_DEFAULT => Some("default".as_bytes().to_vec()),
            TokenType::KEYWORD_DO => Some("do".as_bytes().to_vec()),
            TokenType::KEYWORD_DOUBLE => Some("double".as_bytes().to_vec()),
            TokenType::KEYWORD_ELSE => Some("else".as_bytes().to_vec()),
            TokenType::KEYWORD_ENUM => Some("enum".as_bytes().to_vec()),
            TokenType::KEYWORD_EXTERN => Some("extern".as_bytes().to_vec()),
            TokenType::KEYWORD_FLOAT => Some("float".as_bytes().to_vec()),
            TokenType::KEYWORD_FOR => Some("for".as_bytes().to_vec()),
            TokenType::KEYWORD_GOTO => Some("goto".as_bytes().to_vec()),
            TokenType::KEYWORD_IF => Some("if".as_bytes().to_vec()),
            TokenType::KEYWORD_INLINE => Some("inline".as_bytes().to_vec()),
            TokenType::KEYWORD_INT => Some("int".as_bytes().to_vec()),
            TokenType::KEYWORD_LONG => Some("long".as_bytes().to_vec()),
            TokenType::KEYWORD_REGISTER => Some("register".as_bytes().to_vec()),
            TokenType::KEYWORD_RESTRICT => Some("restrict".as_bytes().to_vec()),
            TokenType::KEYWORD_RETURN => Some("return".as_bytes().to_vec()),
            TokenType::KEYWORD_SHORT => Some("short".as_bytes().to_vec()),
            TokenType::KEYWORD_SIGNED => Some("signed".as_bytes().to_vec()),
            TokenType::KEYWORD_SIZEOF => Some("sizeof".as_bytes().to_vec()),
            TokenType::KEYWORD_STATIC => Some("static".as_bytes().to_vec()),
            TokenType::KEYWORD_STRUCT => Some("struct".as_bytes().to_vec()),
            TokenType::KEYWORD_SWITCH => Some("switch".as_bytes().to_vec()),
            TokenType::KEYWORD_TYPEDEF => Some("typedef".as_bytes().to_vec()),
            TokenType::KEYWORD_UNION => Some("union".as_bytes().to_vec()),
            TokenType::KEYWORD_UNSIGNED => Some("unsigned".as_bytes().to_vec()),
            TokenType::KEYWORD_VOID => Some("void".as_bytes().to_vec()),
            TokenType::KEYWORD_VOLATILE => Some("volatile".as_bytes().to_vec()),
            TokenType::KEYWORD_WHILE => Some("while".as_bytes().to_vec()),
            TokenType::KEYWORD__ALIGNAS => Some("_Alignas".as_bytes().to_vec()),
            TokenType::KEYWORD__ALIGNOF => Some("_Alignof".as_bytes().to_vec()),
            TokenType::KEYWORD__ATOMIC => Some("_Atomic".as_bytes().to_vec()),
            TokenType::KEYWORD__BOOL => Some("_Bool".as_bytes().to_vec()),
            TokenType::KEYWORD__COMPLEX => Some("_Complex".as_bytes().to_vec()),
            TokenType::KEYWORD__GENERIC => Some("_Generic".as_bytes().to_vec()),
            TokenType::KEYWORD__IMAGINARY => Some("_Imaginary".as_bytes().to_vec()),
            TokenType::KEYWORD__NORETURN => Some("_Noreturn".as_bytes().to_vec()),
            TokenType::KEYWORD__STATIC_ASSERT => Some("_Static_assert".as_bytes().to_vec()),
            TokenType::KEYWORD__THREAD_LOCAL => Some("_Thread_local".as_bytes().to_vec()),
            _ => None,
        }
    }
}
fn match_universal_character_name<'a>(
    program_str_bytes: &'a [u8],
    index: usize,
) -> Result<Option<&'a [u8]>, String> {
    if program_str_bytes[index] == b'\\' {
        match program_str_bytes[index + 1] {
            b'U' => {
                let mut eight_digit_index = index + 2;
                while eight_digit_index < index + 10
                    && eight_digit_index < program_str_bytes.len()
                    && program_str_bytes[eight_digit_index].is_ascii_hexdigit()
                    && program_str_bytes.len() - 1 - (index + 1) >= 8
                {
                    eight_digit_index += 1;
                }
                let slice = &program_str_bytes[index..eight_digit_index];
                if slice.len() == 2 {
                    return Err(format!("empty universal character name"));
                }
                let Ok(hex_str) = String::from_utf8(slice[2..].to_vec()) else {
                    unreachable!()
                };
                let Ok(v) = u32::from_str_radix(hex_str.as_str(), 16) else {
                    unreachable!()
                };
                //A universal character name shall not specify a character whose short identifier is less than 00A0
                //other than 0024 ($), 0040 (@), or 0060 (‘), nor one in the range D800 through DFFF inclusive
                if (v != b'$'.into() && v != b'@'.into() && v != b'`'.into() && v < 160)
                    || (v >= 55296 && v <= 57343)
                {
                    return Err(format!("A universal character name shall not specify a character whose short identifier is less than 00A0
other than 0024 ($), 0040 (@), or 0060 (‘), nor one in the range D800 through DFFF inclusive"));
                }
                return Ok(Some(slice));
            }
            b'u' => {
                let mut four_digit_index = index + 2;
                while four_digit_index < index + 6
                    && four_digit_index < program_str_bytes.len()
                    && program_str_bytes[four_digit_index].is_ascii_hexdigit()
                    && program_str_bytes.len() - 1 - (index + 1) >= 4
                {
                    four_digit_index += 1;
                }
                let slice = &program_str_bytes[index..four_digit_index];
                if slice.len() == 2 {
                    return Err(format!("empty universal character name"));
                }
                let Ok(hex_str) = String::from_utf8(slice[2..].to_vec()) else {
                    unreachable!()
                };
                let Ok(v) = u32::from_str_radix(hex_str.as_str(), 16) else {
                    unreachable!()
                };
                //A universal character name shall not specify a character whose short identifier is less than 00A0
                //other than 0024 ($), 0040 (@), or 0060 (‘), nor one in the range D800 through DFFF inclusive
                if (v != b'$'.into() && v != b'@'.into() && v != b'`'.into() && v < 160)
                    || (v >= 55296 && v <= 57343)
                {
                    return Err(format!("A universal character name shall not specify a character whose short identifier is less than 00A0
other than 0024 ($), 0040 (@), or 0060 (‘), nor one in the range D800 through DFFF inclusive"));
                }
                return Ok(Some(slice));
            }
            _ => {}
        }
    }
    Ok(None)
}
fn match_string_literal(
    program_str_bytes: &[u8],
    index: &mut usize,
    str_maps: &mut ByteVecMaps,
) -> Result<Option<TokenType>, String> {
    let mut byte_index = *index;
    let mut token = TokenType::StringLiteral {
        str_lit: StringLiteral {
            prefix_key: None,
            sequence_key: 0,
        },
    };
    let TokenType::StringLiteral {
        str_lit: StringLiteral {
            prefix_key,
            sequence_key,
        },
        ..
    } = &mut token
    else {
        unreachable!()
    };
    if byte_index < program_str_bytes.len()
        && [b'u', b'U', b'L'].contains(&program_str_bytes[byte_index])
    {
        if byte_index + 1 < program_str_bytes.len()
            && matches!(program_str_bytes[byte_index..byte_index + 2], [b'u', b'8'])
        {
            *prefix_key =
                Some(str_maps.add_byte_vec(&program_str_bytes[byte_index..byte_index + 2]));
            byte_index += 2;
        } else {
            *prefix_key =
                Some(str_maps.add_byte_vec(&program_str_bytes[byte_index..byte_index + 1]));
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
            if program_str_bytes[byte_index] == b'\\' {
                // allowed escape characters
                // else return None
                if byte_index + 1 < program_str_bytes.len() {
                    if ([
                        b'\'', b'\"', b'?', b'\\', b'a', b'b', b'f', b'n', b'r', b't', b'v',
                    ]
                    .contains(&program_str_bytes[byte_index + 1])
                        || (b'0'..=b'7').contains(&program_str_bytes[byte_index + 1]))
                        || (byte_index + 2 < program_str_bytes.len()
                            && program_str_bytes[byte_index + 1] == b'x'
                            && program_str_bytes[byte_index + 2].is_ascii_hexdigit())
                    {
                        byte_index += 2;
                        continue;
                    } else if program_str_bytes[byte_index + 1] == b'u'
                        || program_str_bytes[byte_index + 1] == b'U'
                    {
                        if let Some(ucn) =
                            match_universal_character_name(program_str_bytes, byte_index)?
                        {
                            byte_index += ucn.len();
                            continue;
                        }
                    }
                } else {
                    return Ok(None);
                }
            }
            byte_index += 1;
        }
        if byte_index < program_str_bytes.len() && program_str_bytes[byte_index] == b'"' {
            *sequence_key = str_maps.add_byte_vec(&program_str_bytes[start_of_seq..byte_index]);
            byte_index += 1;
            *index = byte_index;
            return Ok(Some(token));
        }
    }
    Ok(None)
}
fn match_integer_constant(
    program_str_bytes: &[u8],
    index: &mut usize,
    str_maps: &mut ByteVecMaps,
) -> Option<TokenType> {
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
                && (program_str_bytes[byte_index].is_ascii_digit()
                    || if is_hexa {
                        program_str_bytes[byte_index].is_ascii_hexdigit()
                    } else {
                        false
                    })
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
                [b'u']
                | [b'U']
                | [b'l']
                | [b'L']
                | [b'l', b'l']
                | [b'L', b'L']
                | [b'U', b'L']
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
                    let suffix_key = if byte_index - start_suffex > 0 {
                        Some(str_maps.add_byte_vec(&program_str_bytes[start_suffex..byte_index]))
                    } else {
                        None
                    };
                    if is_hexa {
                        let token = Some(TokenType::CONSTANT_HEXA_INT {
                            value_key: {
                                str_maps.add_byte_vec(&program_str_bytes[*index..start_suffex])
                            },
                            suffix: match suffix_key {
                                Some(key) => Some(Suffix::Integer {
                                    integer_type: match str_maps.key_to_byte_vec[key].as_slice() {
                                        [b'u'] | [b'U'] => IntegerSuffix::Unsigned,
                                        [b'l'] | [b'L'] => IntegerSuffix::Long,
                                        [b'l', b'l'] | [b'L', b'L'] => IntegerSuffix::LongLong,
                                        [b'U', b'L']
                                        | [b'U', b'l']
                                        | [b'u', b'L']
                                        | [b'u', b'l']
                                        | [b'L', b'u']
                                        | [b'L', b'U']
                                        | [b'l', b'U']
                                        | [b'l', b'u'] => IntegerSuffix::UnsignedLong,
                                        [b'l', b'l', b'u']
                                        | [b'l', b'l', b'U']
                                        | [b'L', b'L', b'u']
                                        | [b'L', b'L', b'U']
                                        | [b'u', b'l', b'l']
                                        | [b'u', b'L', b'L']
                                        | [b'U', b'l', b'l']
                                        | [b'U', b'L', b'L'] => IntegerSuffix::UnsignedLongLong,
                                        _ => unreachable!(),
                                    },
                                    key,
                                }),
                                None => None,
                            },
                        });
                        *index = byte_index;
                        token
                    } else {
                        let token = Some(TokenType::CONSTANT_OCTAL_INT {
                            value_key: {
                                str_maps.add_byte_vec(&program_str_bytes[*index..start_suffex])
                            },
                            suffix: match suffix_key {
                                Some(key) => Some(Suffix::Integer {
                                    integer_type: match str_maps.key_to_byte_vec[key].as_slice() {
                                        [b'u'] | [b'U'] => IntegerSuffix::Unsigned,
                                        [b'l'] | [b'L'] => IntegerSuffix::Long,
                                        [b'l', b'l'] | [b'L', b'L'] => IntegerSuffix::LongLong,
                                        [b'U', b'L']
                                        | [b'U', b'l']
                                        | [b'u', b'L']
                                        | [b'u', b'l']
                                        | [b'L', b'u']
                                        | [b'L', b'U']
                                        | [b'l', b'U']
                                        | [b'l', b'u'] => IntegerSuffix::UnsignedLong,
                                        [b'l', b'l', b'u']
                                        | [b'l', b'l', b'U']
                                        | [b'L', b'L', b'u']
                                        | [b'L', b'L', b'U']
                                        | [b'u', b'l', b'l']
                                        | [b'u', b'L', b'L']
                                        | [b'U', b'l', b'l']
                                        | [b'U', b'L', b'L'] => IntegerSuffix::UnsignedLongLong,
                                        _ => unreachable!(),
                                    },
                                    key,
                                }),
                                None => None,
                            },
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
                    [b'u']
                    | [b'U']
                    | [b'l']
                    | [b'L']
                    | [b'l', b'l']
                    | [b'L', b'L']
                    | [b'U', b'L']
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
                        let suffix_key = if byte_index - start_suffex > 0 {
                            Some(
                                str_maps.add_byte_vec(&program_str_bytes[start_suffex..byte_index]),
                            )
                        } else {
                            None
                        };
                        let token = Some(TokenType::CONSTANT_DEC_INT {
                            value_key: {
                                str_maps.add_byte_vec(&program_str_bytes[*index..start_suffex])
                            },
                            suffix: match suffix_key {
                                Some(key) => Some(Suffix::Integer {
                                    integer_type: match str_maps.key_to_byte_vec[key].as_slice() {
                                        [b'u'] | [b'U'] => IntegerSuffix::Unsigned,
                                        [b'l'] | [b'L'] => IntegerSuffix::Long,
                                        [b'l', b'l'] | [b'L', b'L'] => IntegerSuffix::LongLong,
                                        [b'U', b'L']
                                        | [b'U', b'l']
                                        | [b'u', b'L']
                                        | [b'u', b'l']
                                        | [b'L', b'u']
                                        | [b'L', b'U']
                                        | [b'l', b'U']
                                        | [b'l', b'u'] => IntegerSuffix::UnsignedLong,
                                        [b'l', b'l', b'u']
                                        | [b'l', b'l', b'U']
                                        | [b'L', b'L', b'u']
                                        | [b'L', b'L', b'U']
                                        | [b'u', b'l', b'l']
                                        | [b'u', b'L', b'L']
                                        | [b'U', b'l', b'l']
                                        | [b'U', b'L', b'L'] => IntegerSuffix::UnsignedLongLong,
                                        _ => unreachable!(),
                                    },
                                    key,
                                }),
                                None => None,
                            },
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
fn match_floating_constant(
    program_str_bytes: &[u8],
    index: &mut usize,
    str_maps: &mut ByteVecMaps,
) -> Option<TokenType> {
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
        let suffix_key = if length_of_suffix > 0 {
            Some(str_maps.add_byte_vec(&program_str_bytes[end_of_exp_digit_sequence..byte_index]))
        } else {
            None
        };
        if is_hexa {
            let token = Some(TokenType::CONSTANT_HEXA_FLOAT {
                value_key: {
                    str_maps.add_byte_vec(&program_str_bytes[*index..end_of_second_digit_sequence])
                },
                binary_exp_part_key: {
                    str_maps.add_byte_vec(
                        &program_str_bytes[end_of_second_digit_sequence..end_of_exp_digit_sequence],
                    )
                },
                suffix: match suffix_key {
                    Some(key) => match str_maps.key_to_byte_vec[key].as_slice() {
                        [b'f' | b'F'] => Some(Suffix::Float {
                            float_type: FloatSuffix::Float,
                            key,
                        }),
                        [b'l' | b'L'] => Some(Suffix::Float {
                            float_type: FloatSuffix::LongDouble,
                            key,
                        }),
                        _ => unreachable!(),
                    },
                    None => None,
                },
            });
            *index = byte_index;
            return token;
        } else {
            let token = Some(TokenType::CONSTANT_DEC_FLOAT {
                value_key: {
                    str_maps.add_byte_vec(&program_str_bytes[*index..end_of_second_digit_sequence])
                },
                exp_part_key: if has_exponential {
                    Some(str_maps.add_byte_vec(
                        &program_str_bytes[end_of_second_digit_sequence..end_of_exp_digit_sequence],
                    ))
                } else {
                    None
                },
                suffix: match suffix_key {
                    Some(key) => match str_maps.key_to_byte_vec[key].as_slice() {
                        [b'f' | b'F'] => Some(Suffix::Float {
                            float_type: FloatSuffix::Float,
                            key,
                        }),
                        [b'l' | b'L'] => Some(Suffix::Float {
                            float_type: FloatSuffix::LongDouble,
                            key,
                        }),
                        _ => unreachable!(),
                    },
                    None => None,
                },
            });
            *index = byte_index;
            return token;
        }
    }
    None
}
fn match_enumeration_constant(_program_str_bytes: &[u8], _index: &mut usize) -> Option<Token> {
    todo!()
}
fn match_character_constant(
    program_str_bytes: &[u8],
    index: &mut usize,
    str_maps: &mut ByteVecMaps,
) -> Result<Option<TokenType>, String> {
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
                if byte_index + 1 < program_str_bytes.len() {
                    if ([
                        b'\'', b'\"', b'?', b'\\', b'a', b'b', b'f', b'n', b'r', b't', b'v',
                    ]
                    .contains(&program_str_bytes[byte_index + 1])
                        || (b'0'..=b'7').contains(&program_str_bytes[byte_index + 1]))
                        || (byte_index + 2 < program_str_bytes.len()
                            && program_str_bytes[byte_index + 1] == b'x'
                            && program_str_bytes[byte_index + 2].is_ascii_hexdigit())
                    {
                        byte_index += 2;
                        continue;
                    } else if program_str_bytes[byte_index + 1] == b'u'
                        || program_str_bytes[byte_index + 1] == b'U'
                    {
                        if let Some(ucn) =
                            match_universal_character_name(program_str_bytes, byte_index)?
                        {
                            byte_index += ucn.len();
                            continue;
                        }
                    }
                } else {
                    return Ok(None);
                }
            }
            byte_index += 1;
        }
        if byte_index < program_str_bytes.len() && program_str_bytes[byte_index] == b'\'' {
            byte_index += 1;
            let mut start_of_sequence = *index;
            let token = Some(TokenType::CONSTANT_CHAR {
                const_char: ConstantChar {
                    prefix: if program_str_bytes[*index] == b'\'' {
                        None
                    } else {
                        start_of_sequence += 1;
                        Some(program_str_bytes[*index])
                    },
                    sequence_key: str_maps
                        .add_byte_vec(&program_str_bytes[start_of_sequence + 1..byte_index - 1]),
                },
            });
            *index = byte_index;
            return Ok(token);
        }
    }
    Ok(None)
}
fn match_punctuator(program_str_bytes: &[u8], index: &mut usize) -> Option<TokenType> {
    let byte_index = *index;
    if byte_index < program_str_bytes.len() {
        match program_str_bytes[byte_index] {
            b'[' => {
                *index += 1;
                return Some(TokenType::PUNCT_OPEN_SQR);
            }
            b']' => {
                *index += 1;
                return Some(TokenType::PUNCT_CLOSE_SQR);
            }
            b'(' => {
                *index += 1;
                return Some(TokenType::PUNCT_OPEN_PAR);
            }
            b')' => {
                *index += 1;
                return Some(TokenType::PUNCT_CLOSE_PAR);
            }
            b'{' => {
                *index += 1;
                return Some(TokenType::PUNCT_OPEN_CURLY);
            }
            b'}' => {
                *index += 1;
                return Some(TokenType::PUNCT_CLOSE_CURLY);
            }
            b'.' => {
                if byte_index + 2 < program_str_bytes.len() {
                    if let (b'.', b'.', b'.') = (
                        program_str_bytes[byte_index],
                        program_str_bytes[byte_index + 1],
                        program_str_bytes[byte_index + 2],
                    ) {
                        *index += 3;
                        return Some(TokenType::PUNCT_ELLIPSIS);
                    }
                }
                *index += 1;
                return Some(TokenType::PUNCT_DOT);
            }
            b'-' => {
                if byte_index + 1 < program_str_bytes.len() {
                    match program_str_bytes[byte_index + 1] {
                        b'-' => {
                            *index += 2;
                            return Some(TokenType::PUNCT_DECREMENT);
                        }
                        b'=' => {
                            *index += 2;
                            return Some(TokenType::PUNCT_SUB_ASSIGN);
                        }
                        b'>' => {
                            *index += 2;
                            return Some(TokenType::PUNCT_ARROW);
                        }
                        _ => {}
                    }
                }
                *index += 1;
                return Some(TokenType::PUNCT_MINUS);
            }
            b'+' => {
                if byte_index + 1 < program_str_bytes.len() {
                    match program_str_bytes[byte_index + 1] {
                        b'+' => {
                            *index += 2;
                            return Some(TokenType::PUNCT_INCREMENT);
                        }
                        b'=' => {
                            *index += 2;
                            return Some(TokenType::PUNCT_ADD_ASSIGN);
                        }
                        _ => {}
                    }
                }
                *index += 1;
                return Some(TokenType::PUNCT_PLUS);
            }
            b'&' => {
                if byte_index + 1 < program_str_bytes.len() {
                    match program_str_bytes[byte_index + 1] {
                        b'&' => {
                            *index += 2;
                            return Some(TokenType::PUNCT_AND_BOOL);
                        }
                        b'=' => {
                            *index += 2;
                            return Some(TokenType::PUNCT_AND_BIT_ASSIGN);
                        }
                        _ => {}
                    }
                }
                *index += 1;
                return Some(TokenType::PUNCT_AND_BIT);
            }
            b'*' => {
                if byte_index + 1 < program_str_bytes.len()
                    && program_str_bytes[byte_index + 1] == b'='
                {
                    *index += 2;
                    return Some(TokenType::PUNCT_MULT_ASSIGN);
                }
                *index += 1;
                return Some(TokenType::PUNCT_MULT);
            }
            b'~' => {
                *index += 1;
                return Some(TokenType::PUNCT_TILDE);
            }
            b'!' => {
                if byte_index + 1 < program_str_bytes.len()
                    && program_str_bytes[byte_index + 1] == b'='
                {
                    *index += 2;
                    return Some(TokenType::PUNCT_NOT_EQ_BOOL);
                }
                *index += 1;
                return Some(TokenType::PUNCT_NOT_BOOL);
            }
            b'/' => {
                if byte_index + 1 < program_str_bytes.len()
                    && program_str_bytes[byte_index + 1] == b'='
                {
                    *index += 2;
                    return Some(TokenType::PUNCT_DIV_ASSIGN);
                }
                *index += 1;
                return Some(TokenType::PUNCT_DIV);
            }
            b'%' => {
                if byte_index + 1 < program_str_bytes.len() {
                    match program_str_bytes[byte_index + 1] {
                        b'=' => {
                            *index += 2;
                            return Some(TokenType::PUNCT_MODULO_ASSIGN);
                        }
                        b'>' => {
                            *index += 2;
                            return Some(TokenType::PUNCT_DIGRAPH_CLOSE_CURLY);
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
                                    return Some(TokenType::PUNCT_DIGRAPH_HASH_HASH);
                                }
                            }
                            *index += 2;
                            return Some(TokenType::PUNCT_DIGRAPH_HASH);
                        }
                        _ => {}
                    }
                }
                *index += 1;
                return Some(TokenType::PUNCT_MODULO);
            }
            b'<' => {
                if byte_index + 1 < program_str_bytes.len() {
                    match program_str_bytes[byte_index + 1] {
                        b'<' => {
                            if byte_index + 2 < program_str_bytes.len()
                                && program_str_bytes[byte_index + 2] == b'='
                            {
                                *index += 3;
                                return Some(TokenType::PUNCT_L_SHIFT_BIT_ASSIGN);
                            }
                            *index += 2;
                            return Some(TokenType::PUNCT_BITSHIFT_LEFT);
                        }
                        b'=' => {
                            *index += 2;
                            return Some(TokenType::PUNCT_LESS_THAN_EQ);
                        }
                        b':' => {
                            *index += 2;
                            return Some(TokenType::PUNCT_DIGRAPH_OPEN_SQR);
                        }
                        b'%' => {
                            *index += 2;
                            return Some(TokenType::PUNCT_DIGRAPH_OPEN_CURLY);
                        }
                        _ => {}
                    }
                }
                *index += 1;
                return Some(TokenType::PUNCT_LESS_THAN);
            }
            b'>' => {
                if byte_index + 1 < program_str_bytes.len() {
                    match program_str_bytes[byte_index + 1] {
                        b'>' => {
                            if byte_index + 2 < program_str_bytes.len()
                                && program_str_bytes[byte_index + 2] == b'='
                            {
                                *index += 3;
                                return Some(TokenType::PUNCT_R_SHIFT_BIT_ASSIGN);
                            }
                            *index += 2;
                            return Some(TokenType::PUNCT_BITSHIFT_RIGHT);
                        }
                        b'=' => {
                            *index += 2;
                            return Some(TokenType::PUNCT_GREATER_THAN_EQ);
                        }
                        _ => {}
                    }
                }
                *index += 1;
                return Some(TokenType::PUNCT_GREATER_THAN);
            }
            b'=' => {
                if byte_index + 1 < program_str_bytes.len()
                    && program_str_bytes[byte_index + 1] == b'='
                {
                    *index += 2;
                    return Some(TokenType::PUNCT_EQ_BOOL);
                }
                *index += 1;
                return Some(TokenType::PUNCT_ASSIGNMENT);
            }
            b'^' => {
                if byte_index + 1 < program_str_bytes.len()
                    && program_str_bytes[byte_index + 1] == b'='
                {
                    *index += 2;
                    return Some(TokenType::PUNCT_XOR_BIT_ASSIGN);
                }
                *index += 1;
                return Some(TokenType::PUNCT_XOR_BIT);
            }
            b'|' => {
                if byte_index + 1 < program_str_bytes.len() {
                    match program_str_bytes[byte_index + 1] {
                        b'=' => {
                            *index += 2;
                            return Some(TokenType::PUNCT_OR_BIT_ASSIGN);
                        }
                        b'|' => {
                            *index += 2;
                            return Some(TokenType::PUNCT_OR_BOOL);
                        }
                        _ => {}
                    }
                }
                *index += 1;
                return Some(TokenType::PUNCT_OR_BIT);
            }
            b'?' => {
                *index += 1;
                return Some(TokenType::PUNCT_QUESTION_MARK);
            }
            b':' => {
                if byte_index + 1 < program_str_bytes.len()
                    && program_str_bytes[byte_index + 1] == b'>'
                {
                    *index += 2;
                    return Some(TokenType::PUNCT_DIGRAPH_CLOSE_SQR);
                }
                *index += 1;
                return Some(TokenType::PUNCT_COLON);
            }
            b';' => {
                *index += 1;
                return Some(TokenType::PUNCT_SEMI_COLON);
            }
            b',' => {
                *index += 1;
                return Some(TokenType::PUNCT_COMMA);
            }
            b'#' => {
                if byte_index + 1 < program_str_bytes.len()
                    && program_str_bytes[byte_index + 1] == b'#'
                {
                    *index += 2;
                    return Some(TokenType::PUNCT_HASH_HASH);
                }
                *index += 1;
                return Some(TokenType::PUNCT_HASH);
            }
            _ => {}
        }
    }
    None
}

fn match_identifier(
    program_str_bytes: &[u8],
    index: &mut usize,
    str_maps: &mut ByteVecMaps,
) -> Result<Option<TokenType>, String> {
    let mut byte_index = *index;
    while byte_index < program_str_bytes.len() {
        if matches!(
            program_str_bytes.get(byte_index..byte_index + 2),
            Some([b'\\', b'U' | b'u'])
        ) {
            if let Some(ucn) = match_universal_character_name(program_str_bytes, byte_index)? {
                byte_index += ucn.len();
                continue;
            }
        } else if program_str_bytes[byte_index].is_ascii_alphanumeric()
            || program_str_bytes[byte_index] == b'_'
        {
            byte_index += 1;
        } else {
            break;
        }
    }
    let bytes = &program_str_bytes[*index..byte_index];
    if !bytes.is_empty() && !bytes[0].is_ascii_digit() && *bytes != *"__func__".as_bytes() {
        *index = byte_index;
        return Ok(Some(TokenType::IDENT {
            str_map_key: str_maps.add_byte_vec(bytes),
        }));
    } else if *bytes == *"__func__".as_bytes() {
        *index = byte_index;
        return Ok(Some(TokenType::PREDEF_IDENT___FUNC__));
    }
    Ok(None)
}
fn match_keyword(program_str_bytes: &[u8], index: &mut usize) -> Option<TokenType> {
    let mut byte_index = *index;
    while byte_index < program_str_bytes.len()
        && (program_str_bytes[byte_index].is_ascii_alphanumeric()
            || program_str_bytes[byte_index] == b'_')
    {
        byte_index += 1;
    }
    let bytes = &program_str_bytes[*index..byte_index];
    const KEYWORD_AUTO: &[u8] = "auto".as_bytes();
    const KEYWORD_BREAK: &[u8] = "break".as_bytes();
    const KEYWORD_CASE: &[u8] = "case".as_bytes();
    const KEYWORD_CHAR: &[u8] = "char".as_bytes();
    const KEYWORD_CONST: &[u8] = "const".as_bytes();
    const KEYWORD_CONTINUE: &[u8] = "continue".as_bytes();
    const KEYWORD_DEFAULT: &[u8] = "default".as_bytes();
    const KEYWORD_DO: &[u8] = "do".as_bytes();
    const KEYWORD_DOUBLE: &[u8] = "double".as_bytes();
    const KEYWORD_ELSE: &[u8] = "else".as_bytes();
    const KEYWORD_ENUM: &[u8] = "enum".as_bytes();
    const KEYWORD_EXTERN: &[u8] = "extern".as_bytes();
    const KEYWORD_FLOAT: &[u8] = "float".as_bytes();
    const KEYWORD_FOR: &[u8] = "for".as_bytes();
    const KEYWORD_GOTO: &[u8] = "goto".as_bytes();
    const KEYWORD_IF: &[u8] = "if".as_bytes();
    const KEYWORD_INLINE: &[u8] = "inline".as_bytes();
    const KEYWORD_INT: &[u8] = "int".as_bytes();
    const KEYWORD_LONG: &[u8] = "long".as_bytes();
    const KEYWORD_REGISTER: &[u8] = "register".as_bytes();
    const KEYWORD_RESTRICT: &[u8] = "restrict".as_bytes();
    const KEYWORD_RETURN: &[u8] = "return".as_bytes();
    const KEYWORD_SHORT: &[u8] = "short".as_bytes();
    const KEYWORD_SIGNED: &[u8] = "signed".as_bytes();
    const KEYWORD_SIZEOF: &[u8] = "sizeof".as_bytes();
    const KEYWORD_STATIC: &[u8] = "static".as_bytes();
    const KEYWORD_STRUCT: &[u8] = "struct".as_bytes();
    const KEYWORD_SWITCH: &[u8] = "switch".as_bytes();
    const KEYWORD_TYPEDEF: &[u8] = "typedef".as_bytes();
    const KEYWORD_UNION: &[u8] = "union".as_bytes();
    const KEYWORD_UNSIGNED: &[u8] = "unsigned".as_bytes();
    const KEYWORD_VOID: &[u8] = "void".as_bytes();
    const KEYWORD_VOLATILE: &[u8] = "volatile".as_bytes();
    const KEYWORD_WHILE: &[u8] = "while".as_bytes();
    const KEYWORD__ALIGNAS: &[u8] = "_Alignas".as_bytes();
    const KEYWORD__ALIGNOF: &[u8] = "_Alignof".as_bytes();
    const KEYWORD__ATOMIC: &[u8] = "_Atomic".as_bytes();
    const KEYWORD__BOOL: &[u8] = "_Bool".as_bytes();
    const KEYWORD__COMPLEX: &[u8] = "_Complex".as_bytes();
    const KEYWORD__GENERIC: &[u8] = "_Generic".as_bytes();
    const KEYWORD__IMAGINARY: &[u8] = "_Imaginary".as_bytes();
    const KEYWORD__NORETURN: &[u8] = "_Noreturn".as_bytes();
    const KEYWORD__STATIC_ASSERT: &[u8] = "_Static_assert".as_bytes();
    const KEYWORD__THREAD_LOCAL: &[u8] = "_Thread_local".as_bytes();
    let keyword = match bytes {
        KEYWORD_AUTO => Some(TokenType::KEYWORD_AUTO),
        KEYWORD_BREAK => Some(TokenType::KEYWORD_BREAK),
        KEYWORD_CASE => Some(TokenType::KEYWORD_CASE),
        KEYWORD_CHAR => Some(TokenType::KEYWORD_CHAR),
        KEYWORD_CONST => Some(TokenType::KEYWORD_CONST),
        KEYWORD_CONTINUE => Some(TokenType::KEYWORD_CONTINUE),
        KEYWORD_DEFAULT => Some(TokenType::KEYWORD_DEFAULT),
        KEYWORD_DO => Some(TokenType::KEYWORD_DO),
        KEYWORD_DOUBLE => Some(TokenType::KEYWORD_DOUBLE),
        KEYWORD_ELSE => Some(TokenType::KEYWORD_ELSE),
        KEYWORD_ENUM => Some(TokenType::KEYWORD_ENUM),
        KEYWORD_EXTERN => Some(TokenType::KEYWORD_EXTERN),
        KEYWORD_FLOAT => Some(TokenType::KEYWORD_FLOAT),
        KEYWORD_FOR => Some(TokenType::KEYWORD_FOR),
        KEYWORD_GOTO => Some(TokenType::KEYWORD_GOTO),
        KEYWORD_IF => Some(TokenType::KEYWORD_IF),
        KEYWORD_INLINE => Some(TokenType::KEYWORD_INLINE),
        KEYWORD_INT => Some(TokenType::KEYWORD_INT),
        KEYWORD_LONG => Some(TokenType::KEYWORD_LONG),
        KEYWORD_REGISTER => Some(TokenType::KEYWORD_REGISTER),
        KEYWORD_RESTRICT => Some(TokenType::KEYWORD_RESTRICT),
        KEYWORD_RETURN => Some(TokenType::KEYWORD_RETURN),
        KEYWORD_SHORT => Some(TokenType::KEYWORD_SHORT),
        KEYWORD_SIGNED => Some(TokenType::KEYWORD_SIGNED),
        KEYWORD_SIZEOF => Some(TokenType::KEYWORD_SIZEOF),
        KEYWORD_STATIC => Some(TokenType::KEYWORD_STATIC),
        KEYWORD_STRUCT => Some(TokenType::KEYWORD_STRUCT),
        KEYWORD_SWITCH => Some(TokenType::KEYWORD_SWITCH),
        KEYWORD_TYPEDEF => Some(TokenType::KEYWORD_TYPEDEF),
        KEYWORD_UNION => Some(TokenType::KEYWORD_UNION),
        KEYWORD_UNSIGNED => Some(TokenType::KEYWORD_UNSIGNED),
        KEYWORD_VOID => Some(TokenType::KEYWORD_VOID),
        KEYWORD_VOLATILE => Some(TokenType::KEYWORD_VOLATILE),
        KEYWORD_WHILE => Some(TokenType::KEYWORD_WHILE),
        KEYWORD__ALIGNAS => Some(TokenType::KEYWORD__ALIGNAS),
        KEYWORD__ALIGNOF => Some(TokenType::KEYWORD__ALIGNOF),
        KEYWORD__ATOMIC => Some(TokenType::KEYWORD__ATOMIC),
        KEYWORD__BOOL => Some(TokenType::KEYWORD__BOOL),
        KEYWORD__COMPLEX => Some(TokenType::KEYWORD__COMPLEX),
        KEYWORD__GENERIC => Some(TokenType::KEYWORD__GENERIC),
        KEYWORD__IMAGINARY => Some(TokenType::KEYWORD__IMAGINARY),
        KEYWORD__NORETURN => Some(TokenType::KEYWORD__NORETURN),
        KEYWORD__STATIC_ASSERT => Some(TokenType::KEYWORD__STATIC_ASSERT),
        KEYWORD__THREAD_LOCAL => Some(TokenType::KEYWORD__THREAD_LOCAL),
        _ => None,
    };
    if keyword.is_some() {
        *index = byte_index;
    }
    keyword
}
fn chain_lex(
    program_str_bytes: &[u8],
    index: &mut usize,
    is_pp: bool,
    str_maps: &mut ByteVecMaps,
) -> Result<Option<TokenType>, String> {
    let punctuator = match_punctuator(program_str_bytes, index);
    if punctuator.is_some() {
        return Ok(punctuator);
    }
    if !is_pp {
        let keyword = match_keyword(program_str_bytes, index);
        if keyword.is_some() {
            return Ok(keyword);
        }
    }
    let char_const = match_character_constant(program_str_bytes, index, str_maps)?;
    if char_const.is_some() {
        return Ok(char_const);
    }
    let string_lit = match_string_literal(program_str_bytes, index, str_maps)?;
    if string_lit.is_some() {
        return Ok(string_lit);
    }
    let identifier = match_identifier(program_str_bytes, index, str_maps)?;
    if identifier.is_some() {
        return Ok(identifier);
    }
    let float_const = match_floating_constant(program_str_bytes, index, str_maps);
    if float_const.is_some() {
        return Ok(float_const);
    }
    let integer_const = match_integer_constant(program_str_bytes, index, str_maps);
    if integer_const.is_some() {
        return Ok(integer_const);
    }
    Ok(None)
}

pub fn unknown_token(c: char, line: usize) -> String {
    return format!("Unknown token {} at line: {}", c, line);
}

pub fn lexer(
    program_str_bytes: &[u8],
    // when we preprocess, keywords are just identifiers
    is_pp: bool,
    str_maps: &mut ByteVecMaps,
) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut column: usize = 0;
    let mut index: usize = 0;
    let mut line: usize = 0;
    while index < program_str_bytes.len() {
        if program_str_bytes[index] == b'\n' {
            tokens.push(Token {
                r#type: TokenType::NEWLINE,
                location: Some(Location { column, line }),
            });
            index += 1;
            line += 1;
            column = 0;
        } else if !program_str_bytes[index].is_ascii_whitespace() {
            let old_index = index;
            let token_type = chain_lex(&program_str_bytes, &mut index, is_pp, str_maps);
            if let Ok(Some(token_type)) = token_type {
                tokens.push(Token {
                    r#type: token_type,
                    location: Some(Location { column, line }),
                });
                column += index - old_index;
            } else {
                return Err(unknown_token(char::from(program_str_bytes[index]), line));
            }
        } else {
            column += 1;
            while matches!(program_str_bytes.get(index), Some(b' ' | b'\t')) {
                index += 1;
            }
            tokens.push(Token {
                r#type: TokenType::WHITESPACE,
                location: Some(Location { column, line }),
            });
        }
    }
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::{
        lexer, match_character_constant, match_floating_constant, match_identifier,
        match_string_literal, ByteVecMaps, ConstantChar, StringLiteral, Token,
    };
    use crate::lexer;

    #[test]
    fn chain_lex_test_constant_float_variable() -> Result<(), String> {
        let s = r#"const float f = 0.4"#.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let tokens = lexer(s, false, &mut str_maps)?;
        assert_eq!(
            vec![
                lexer::TokenType::KEYWORD_CONST { pos_in_src: 0 },
                lexer::TokenType::WHITESPACE { pos_in_src: 6 },
                lexer::TokenType::KEYWORD_FLOAT { pos_in_src: 6 },
                lexer::TokenType::WHITESPACE { pos_in_src: 12 },
                lexer::TokenType::IDENT {
                    str_map_key: 0,
                    pos_in_src: 13
                },
                lexer::TokenType::WHITESPACE { pos_in_src: 14 },
                lexer::TokenType::PUNCT_ASSIGNMENT { pos_in_src: 15 },
                lexer::TokenType::WHITESPACE { pos_in_src: 16 },
                lexer::TokenType::CONSTANT_DEC_FLOAT {
                    value_key: 1,
                    exp_part_key: None,
                    suffix: None,
                    pos_in_src: 19
                }
            ],
            tokens
        );
        Ok(())
    }

    #[test]
    fn chain_lex_test_universal_char_name_identifiers() -> Result<(), String> {
        let s = r#"int \UAAAA_URMOM = 4"#.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let tokens = lexer(s, false, &mut str_maps)?;
        assert_eq!(
            vec![
                TokenType::KEYWORD_INT { pos_in_src: 0 },
                TokenType::WHITESPACE { pos_in_src: 1 },
                TokenType::IDENT {
                    str_map_key: str_maps.add_byte_vec("\\UAAAA_URMOM".as_bytes()),
                    pos_in_src: 2
                },
                TokenType::WHITESPACE { pos_in_src: 3 },
                TokenType::PUNCT_ASSIGNMENT { pos_in_src: 4 },
                TokenType::WHITESPACE { pos_in_src: 5 },
                TokenType::CONSTANT_DEC_INT {
                    suffix: None,
                    value_key: str_maps.add_byte_vec("4".as_bytes()),
                    pos_in_src: 6
                }
            ],
            tokens
        );
        Ok(())
    }
    #[test]
    fn chain_lex_test() -> Result<(), String> {
        let s = r#"u8"hi""#.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let tokens = lexer(s, false, &mut str_maps)?;
        assert_eq!(
            vec![TokenType::StringLiteral {
                str_lit: StringLiteral {
                    prefix_key: Some(str_maps.add_byte_vec("u8".as_bytes())),
                    sequence_key: str_maps.add_byte_vec("hi".as_bytes()),
                },
                pos_in_src: 0
            }],
            tokens
        );
        Ok(())
    }

    #[test]
    fn chain_lex_test_character_string_lit() -> Result<(), String> {
        let s = r#"u'hehe';u8"hi""#.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let tokens = lexer(s, false, &mut str_maps)?;
        assert_eq!(
            vec![
                TokenType::CONSTANT_CHAR {
                    const_char: ConstantChar {
                        prefix: Some(b'u'),
                        sequence_key: str_maps.add_byte_vec("hehe".as_bytes())
                    },
                    pos_in_src: 0
                },
                TokenType::PUNCT_SEMI_COLON { pos_in_src: 1 },
                TokenType::StringLiteral {
                    str_lit: StringLiteral {
                        prefix_key: Some(str_maps.add_byte_vec("u8".as_bytes())),
                        sequence_key: str_maps.add_byte_vec("hi".as_bytes()),
                    },
                    pos_in_src: 2
                }
            ],
            tokens
        );
        Ok(())
    }
    #[test]
    fn test_match_identifier_universal_character_name_within() -> Result<(), String> {
        let src = r#"foo\u1234bar"#.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let token = match_identifier(src, &mut 0, &mut str_maps)?;
        assert_eq!(
            token,
            Some(lexer::TokenType::IDENT {
                str_map_key: str_maps.add_byte_vec("foo\\u1234bar".as_bytes()),
                pos_in_src: 0
            })
        );
        Ok(())
    }
    #[test]
    fn test_match_float_constant_valid_hexadecimal_second_digit_sequence() {
        let s = "0x.0p0";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let mut str_maps = ByteVecMaps::new();
        let float_token = match_floating_constant(s_bytes, &mut index, &mut str_maps);
        match &float_token {
            Some(super::TokenType::CONSTANT_HEXA_FLOAT {
                value_key,
                binary_exp_part_key,
                suffix: _,
                pos_in_src: _,
            }) => {
                let value = str_maps.key_to_byte_vec.get(*value_key).unwrap();
                let binary_exp_part = str_maps.key_to_byte_vec.get(*binary_exp_part_key).unwrap();
                assert_eq!(value, b"0x.0");
                assert_eq!(binary_exp_part, b"p0");
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
        let mut str_maps = ByteVecMaps::new();
        let float_token = match_floating_constant(s_bytes, &mut index, &mut str_maps);
        match &float_token {
            Some(super::TokenType::CONSTANT_HEXA_FLOAT {
                value_key,
                binary_exp_part_key,
                suffix: _,
                pos_in_src: _,
            }) => {
                let value = str_maps.key_to_byte_vec.get(*value_key).unwrap();
                let binary_exp_part = str_maps.key_to_byte_vec.get(*binary_exp_part_key).unwrap();
                assert_eq!(value, b"0x0.");
                assert_eq!(binary_exp_part, b"p0");
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
        let mut str_maps = ByteVecMaps::new();
        let float_token = match_floating_constant(s_bytes, &mut index, &mut str_maps);
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
        let mut str_maps = ByteVecMaps::new();
        let float_token = match_floating_constant(s_bytes, &mut index, &mut str_maps);
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
        let mut str_maps = ByteVecMaps::new();
        let float_token = match_floating_constant(s_bytes, &mut index, &mut str_maps);
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
        let mut str_maps = ByteVecMaps::new();
        let float_token = match_floating_constant(s_bytes, &mut index, &mut str_maps);
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
        let mut str_maps = ByteVecMaps::new();
        let float_token = match_floating_constant(s_bytes, &mut index, &mut str_maps);
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
        let mut str_maps = ByteVecMaps::new();
        let float_token = match_floating_constant(s_bytes, &mut index, &mut str_maps);
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
        let mut str_maps = ByteVecMaps::new();
        let float_token = match_floating_constant(s_bytes, &mut index, &mut str_maps);
        match &float_token {
            Some(super::TokenType::CONSTANT_HEXA_FLOAT {
                value_key,
                binary_exp_part_key,
                suffix: _,
                pos_in_src: _,
            }) => {
                let value = str_maps.key_to_byte_vec.get(*value_key).unwrap();
                let binary_exp_part = str_maps.key_to_byte_vec.get(*binary_exp_part_key).unwrap();
                assert_eq!(value, b"0x1");
                assert_eq!(binary_exp_part, b"p0");
            }
            _ => panic!(),
        }
        assert!(float_token.is_some());
    }
    #[test]
    fn test_match_float_constant_valid_decimal() {
        let s = "001223e0";
        let s_bytes = s.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let float_token = match_floating_constant(s_bytes, &mut 0, &mut str_maps);
        match &float_token {
            Some(super::TokenType::CONSTANT_DEC_FLOAT {
                value_key,
                exp_part_key,
                suffix: _,
                pos_in_src: _,
            }) => {
                let value = str_maps.key_to_byte_vec.get(*value_key).unwrap();
                let exp_part = str_maps.key_to_byte_vec.get(exp_part_key.unwrap()).unwrap();
                assert_eq!(value, b"001223");
                assert_eq!(exp_part, b"e0");
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
        let mut str_maps = ByteVecMaps::new();
        let float_token = match_floating_constant(s_bytes, &mut index, &mut str_maps);
        match &float_token {
            Some(super::TokenType::CONSTANT_DEC_FLOAT {
                value_key,
                exp_part_key,
                suffix,
                pos_in_src: _,
            }) => {
                let Some(lexer::Suffix::Float { float_type, key }) = suffix else {
                    unreachable!()
                };
                let value = str_maps.key_to_byte_vec.get(*value_key).unwrap();
                let exp_part = str_maps.key_to_byte_vec.get(exp_part_key.unwrap()).unwrap();
                let suffix = str_maps.key_to_byte_vec.get(*key).unwrap();
                assert_eq!(value, b"001223");
                assert_eq!(exp_part, b"e0");
                assert_eq!(suffix, b"L");
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
        let mut str_maps = ByteVecMaps::new();
        let float_token = match_floating_constant(s_bytes, &mut index, &mut str_maps);
        match &float_token {
            Some(super::TokenType::CONSTANT_HEXA_FLOAT {
                value_key,
                binary_exp_part_key,
                suffix,
                pos_in_src: _,
            }) => {
                let Some(lexer::Suffix::Float { float_type, key }) = suffix else {
                    unreachable!()
                };
                let value = str_maps.key_to_byte_vec.get(*value_key).unwrap();
                let binary_exp_part = str_maps.key_to_byte_vec.get(*binary_exp_part_key).unwrap();
                let suffix = str_maps.key_to_byte_vec.get(*key).unwrap();
                assert_eq!(value, b"0x01223");
                assert_eq!(binary_exp_part, b"p0");
                assert_eq!(suffix, b"L");
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
        let mut str_maps = ByteVecMaps::new();
        let float_token = match_floating_constant(s_bytes, &mut index, &mut str_maps);
        match &float_token {
            Some(super::TokenType::CONSTANT_HEXA_FLOAT {
                value_key,
                binary_exp_part_key,
                suffix,
                pos_in_src: _,
            }) => {
                let Some(lexer::Suffix::Float { float_type, key }) = suffix else {
                    unreachable!()
                };
                let value = str_maps.key_to_byte_vec.get(*value_key).unwrap();
                let binary_exp_part = str_maps.key_to_byte_vec.get(*binary_exp_part_key).unwrap();
                let suffix = str_maps.key_to_byte_vec.get(*key).unwrap();
                assert_eq!(value, b"0x01223");
                assert_eq!(binary_exp_part, b"p+0");
                assert_eq!(suffix, b"L");
            }
            _ => panic!(),
        }
        assert!(float_token.is_some());
    }
    #[test]
    fn test_match_character_constant_no_prefix() -> Result<(), String> {
        let s = "'hi'";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let mut str_maps = ByteVecMaps::new();
        let Some(char_token) = match_character_constant(s_bytes, &mut index, &mut str_maps)? else {
            panic!("Didn't get Some(char token)")
        };
        match &char_token {
            super::TokenType::CONSTANT_CHAR {
                const_char:
                    ConstantChar {
                        prefix: _,
                        sequence_key,
                    },
                pos_in_src: _,
            } => {
                assert_eq!(str_maps.key_to_byte_vec[*sequence_key], b"hi");
            }
            _ => panic!(),
        }
        Ok(())
    }
    #[test]
    fn test_match_character_constant_wchar_t() -> Result<(), String> {
        let s = "L'hi'";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let mut str_maps = ByteVecMaps::new();
        let Some(char_token) = match_character_constant(s_bytes, &mut index, &mut str_maps)? else {
            panic!("Didn't get Some(char token)")
        };
        match &char_token {
            super::TokenType::CONSTANT_CHAR {
                const_char:
                    ConstantChar {
                        prefix,
                        sequence_key,
                    },
                pos_in_src: _,
            } => {
                assert_eq!(*prefix, Some(b'L'));
                assert_eq!(str_maps.key_to_byte_vec[*sequence_key], b"hi");
            }
            _ => panic!(),
        }
        Ok(())
    }
    #[test]
    fn test_match_character_constant_char16_t() -> Result<(), String> {
        let s = "u'hi'";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let mut str_maps = ByteVecMaps::new();
        let Some(char_token) = match_character_constant(s_bytes, &mut index, &mut str_maps)? else {
            panic!("Didn't get Some(char token)")
        };
        match &char_token {
            super::TokenType::CONSTANT_CHAR {
                const_char:
                    ConstantChar {
                        prefix,
                        sequence_key,
                    },
                pos_in_src: _,
            } => {
                assert_eq!(*prefix, Some(b'u'));
                assert_eq!(str_maps.key_to_byte_vec[*sequence_key], b"hi");
            }
            _ => panic!(),
        }
        Ok(())
    }
    #[test]
    fn test_match_character_constant_char32_t() -> Result<(), String> {
        let s = "U'hi'";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let mut str_maps = ByteVecMaps::new();
        let Some(char_token) = match_character_constant(s_bytes, &mut index, &mut str_maps)? else {
            panic!("Didn't get Some(char token)")
        };
        match &char_token {
            super::TokenType::CONSTANT_CHAR {
                const_char:
                    ConstantChar {
                        prefix,
                        sequence_key,
                    },
                pos_in_src: _,
            } => {
                assert_eq!(*prefix, Some(b'U'));
                assert_eq!(str_maps.key_to_byte_vec[*sequence_key], b"hi");
            }
            _ => panic!(),
        }
        Ok(())
    }
    #[test]
    fn test_match_string_literal() -> Result<(), String> {
        let s = "U\"hi\"";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let mut str_maps = ByteVecMaps::new();
        let Some(string_literal) = match_string_literal(s_bytes, &mut index, &mut str_maps)? else {
            panic!("Didn't get Some(string literal)")
        };
        match &string_literal {
            super::TokenType::StringLiteral {
                str_lit:
                    StringLiteral {
                        prefix_key,
                        sequence_key,
                    },
                pos_in_src: 0,
            } => {
                assert_eq!(
                    str_maps.key_to_byte_vec.get(prefix_key.unwrap()).unwrap(),
                    b"U"
                );
                assert_eq!(str_maps.key_to_byte_vec.get(*sequence_key).unwrap(), b"hi");
            }
            _ => panic!(),
        }
        Ok(())
    }
    #[test]
    fn test_match_string_literal_no_prefix() -> Result<(), String> {
        let s = "\"hi\"";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let mut str_maps = ByteVecMaps::new();
        let Some(string_literal) = match_string_literal(s_bytes, &mut index, &mut str_maps)? else {
            panic!("Didn't get Some(string literal)")
        };
        match &string_literal {
            super::TokenType::StringLiteral {
                str_lit:
                    StringLiteral {
                        prefix_key,
                        sequence_key,
                    },
                pos_in_src: 0,
            } => {
                assert!(prefix_key.is_none());
                assert_eq!(str_maps.key_to_byte_vec[*sequence_key], b"hi");
            }
            _ => panic!(),
        }
        Ok(())
    }
    #[test]
    fn test_match_string_literal_no_prefix_universal_char_name() -> Result<(), String> {
        let s = "\"\\U0001F600\"";
        let s_bytes = s.as_bytes();
        let mut index = 0;
        let mut str_maps = ByteVecMaps::new();
        let Some(string_literal) = match_string_literal(s_bytes, &mut index, &mut str_maps)? else {
            panic!("Didn't get Some(string literal)")
        };
        match &string_literal {
            super::TokenType::StringLiteral {
                str_lit:
                    StringLiteral {
                        prefix_key,
                        sequence_key,
                    },
                pos_in_src: 0,
            } => {
                assert!(prefix_key.is_none());
                assert_eq!(
                    str_maps.key_to_byte_vec.get(*sequence_key).unwrap(),
                    b"\\U0001F600"
                );
            }
            _ => panic!(),
        }
        Ok(())
    }
    #[test]
    fn test_lexer() -> Result<(), String> {
        let s = "int main() {\nint hi = 4;\nreturn 0;\n}";
        let s_bytes = s.as_bytes();
        let mut str_maps = ByteVecMaps::new();

        let tokens = lexer(&s_bytes.to_vec(), false, &mut str_maps)?;
        let tokens_assert = vec![
            TokenType::KEYWORD_INT { pos_in_src: 0 },
            TokenType::WHITESPACE { pos_in_src: 1 },
            TokenType::IDENT {
                str_map_key: str_maps.add_byte_vec("main".as_bytes()),
                pos_in_src: 2,
            },
            TokenType::PUNCT_OPEN_PAR { pos_in_src: 3 },
            TokenType::PUNCT_CLOSE_PAR { pos_in_src: 4 },
            TokenType::WHITESPACE { pos_in_src: 6 },
            TokenType::PUNCT_OPEN_CURLY { pos_in_src: 7 },
            TokenType::NEWLINE { pos_in_src: 8 },
            TokenType::KEYWORD_INT { pos_in_src: 10 },
            TokenType::WHITESPACE { pos_in_src: 11 },
            TokenType::IDENT {
                str_map_key: str_maps.add_byte_vec("hi".as_bytes()),
                pos_in_src: 12,
            },
            TokenType::WHITESPACE { pos_in_src: 13 },
            TokenType::PUNCT_ASSIGNMENT { pos_in_src: 14 },
            TokenType::WHITESPACE { pos_in_src: 15 },
            TokenType::CONSTANT_DEC_INT {
                value_key: str_maps.add_byte_vec("4".as_bytes()),
                suffix: None,
                pos_in_src: 16,
            },
            TokenType::PUNCT_SEMI_COLON { pos_in_src: 17 },
            TokenType::NEWLINE { pos_in_src: 18 },
            TokenType::KEYWORD_RETURN { pos_in_src: 19 },
            TokenType::WHITESPACE { pos_in_src: 20 },
            TokenType::CONSTANT_DEC_INT {
                value_key: str_maps.add_byte_vec("0".as_bytes()),
                suffix: None,
                pos_in_src: 21,
            },
            TokenType::PUNCT_SEMI_COLON { pos_in_src: 22 },
            TokenType::NEWLINE { pos_in_src: 23 },
            TokenType::PUNCT_CLOSE_CURLY { pos_in_src: 24 },
        ];
        assert_eq!(tokens, tokens_assert);
        Ok(())
    }
    #[test]
    fn test_lexer_directives() -> Result<(), String> {
        let s = "#include <stdio.h>\nint main() {}";
        let s_bytes = s.as_bytes();
        let mut str_maps = ByteVecMaps::new();

        str_maps.add_byte_vec("include".as_bytes());

        str_maps.add_byte_vec("stdio".as_bytes());

        str_maps.add_byte_vec("h".as_bytes());

        str_maps.add_byte_vec("int".as_bytes());

        str_maps.add_byte_vec("main".as_bytes());
        let tokens = lexer(&s_bytes.to_vec(), true, &mut str_maps)?;
        let tokens_assert = vec![
            TokenType::PUNCT_HASH { pos_in_src: 0 },
            TokenType::IDENT {
                str_map_key: str_maps.add_byte_vec("include".as_bytes()),
                pos_in_src: 1,
            },
            TokenType::WHITESPACE { pos_in_src: 2 },
            TokenType::PUNCT_LESS_THAN { pos_in_src: 3 },
            TokenType::IDENT {
                str_map_key: str_maps.add_byte_vec("stdio".as_bytes()),
                pos_in_src: 4,
            },
            TokenType::PUNCT_DOT { pos_in_src: 5 },
            TokenType::IDENT {
                str_map_key: str_maps.add_byte_vec("h".as_bytes()),
                pos_in_src: 6,
            },
            TokenType::PUNCT_GREATER_THAN { pos_in_src: 7 },
            TokenType::NEWLINE { pos_in_src: 8 },
            TokenType::IDENT {
                pos_in_src: 9,
                str_map_key: str_maps.add_byte_vec("int".as_bytes()),
            },
            TokenType::WHITESPACE { pos_in_src: 10 },
            TokenType::IDENT {
                pos_in_src: 11,
                str_map_key: str_maps.add_byte_vec("main".as_bytes()),
            },
            TokenType::PUNCT_OPEN_PAR { pos_in_src: 12 },
            TokenType::PUNCT_CLOSE_PAR { pos_in_src: 13 },
            TokenType::WHITESPACE { pos_in_src: 14 },
            TokenType::PUNCT_OPEN_CURLY { pos_in_src: 15 },
            TokenType::PUNCT_CLOSE_CURLY { pos_in_src: 16 },
        ];
        assert_eq!(tokens, tokens_assert);
        Ok(())
    }
    #[test]
    fn test_lexer_if_directives() -> Result<(), String> {
        let s = "#if 1 + 1\n#define CHICKEN 5\n#endif\n";
        let s_bytes = s.as_bytes();
        let mut str_maps: ByteVecMaps = ByteVecMaps::new();

        str_maps.add_byte_vec("endif".as_bytes());
        let tokens = lexer(&s_bytes.to_vec(), true, &mut str_maps)?;
        let tokens_assert = vec![
            TokenType::PUNCT_HASH { pos_in_src: 0 },
            TokenType::IDENT {
                pos_in_src: 1,
                str_map_key: str_maps.add_byte_vec("if".as_bytes()),
            },
            TokenType::WHITESPACE { pos_in_src: 2 },
            TokenType::CONSTANT_DEC_INT {
                value_key: str_maps.add_byte_vec("1".as_bytes()),
                suffix: None,
                pos_in_src: 3,
            },
            TokenType::WHITESPACE { pos_in_src: 4 },
            TokenType::PUNCT_PLUS { pos_in_src: 5 },
            TokenType::WHITESPACE { pos_in_src: 6 },
            TokenType::CONSTANT_DEC_INT {
                value_key: str_maps.add_byte_vec("1".as_bytes()),
                suffix: None,
                pos_in_src: 7,
            },
            TokenType::NEWLINE { pos_in_src: 8 },
            TokenType::PUNCT_HASH { pos_in_src: 9 },
            TokenType::IDENT {
                pos_in_src: 10,
                str_map_key: str_maps.add_byte_vec("define".as_bytes()),
            },
            TokenType::WHITESPACE { pos_in_src: 11 },
            TokenType::IDENT {
                pos_in_src: 12,
                str_map_key: str_maps.add_byte_vec("CHICKEN".as_bytes()),
            },
            TokenType::WHITESPACE { pos_in_src: 13 },
            TokenType::CONSTANT_DEC_INT {
                value_key: str_maps.add_byte_vec("5".as_bytes()),
                suffix: None,
                pos_in_src: 14,
            },
            TokenType::NEWLINE { pos_in_src: 15 },
            TokenType::PUNCT_HASH { pos_in_src: 16 },
            TokenType::IDENT {
                pos_in_src: 17,
                str_map_key: str_maps.add_byte_vec("endif".as_bytes()),
            },
            TokenType::NEWLINE { pos_in_src: 18 },
        ];
        assert_eq!(tokens, tokens_assert);
        Ok(())
    }
}
