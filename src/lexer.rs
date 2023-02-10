#[allow(non_camel_case_types)]
enum Token {
    IDENT(&'static str),
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
    PUNCT_ASTERISK,
    PUNCT_PLUS,
    PUNCT_MINUS,
    PUNCT_TILDE,
    PUNCT_EXCLAMATION,
    PUNCT_FORWARD_SLASH,
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
    CONSTANT_INT(&'static str),
    CONSTANT_FLOAT(&'static str),
    CONSTANT_ENUM(&'static str),
    CONSTANT_CHAT(&'static str),
}
fn match_constant() {}
fn match_punctuator(token: &'static str) -> Option<Token> {
    if token.len() <= 3 {
        let _bytes = token.as_bytes();
    }
    None
}
fn match_identifier(token: &'static str) -> Option<Token> {
    let bytes = token.as_bytes();
    // TODO: we need to check for universal character names
    if !bytes[0].is_ascii_digit()
        && bytes
            .iter()
            .find(|b| !b.is_ascii() && **b != b'_')
            .is_none()
        && token != "__func__"
    {
        return Some(Token::IDENT(token.clone()));
    } else if token == "__func__" {
        return Some(Token::PREDEF_IDENT___FUNC__);
    }
    None
}
fn match_keyword(token: &str) -> Option<Token> {
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
    match token {
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
    }
}
fn lexer(program_str: String) -> Vec<Token> {
    let tokens = Vec::new();
    let mut curr_token = String::new();
    for character in program_str.chars() {
        if character != ' ' && character == '\n' && character != '\t' {
            curr_token.push(character);
        } else {
            let _matched_keyword = match_keyword(&curr_token);
        }
    }
    tokens
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_lexer() {
        let program_str: &'static str = "int main() {\n\tint four = 4;\n\treturn 0;\n}";
    }
}
