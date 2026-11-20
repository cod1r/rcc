use crate::error::*;
use crate::lexer;
use crate::lexer::*;
use crate::parser::expressions::*;
use crate::parser::statements::*;
use crate::parser::*;
use std::fmt::Display;

pub type TypeNameIndex = usize;
pub type DeclarationIndex = usize;
#[derive(Copy, Clone)]
pub enum StorageClassSpecifier {
    TypeDef = 1,
    Extern = 2,
    Static = 4,
    ThreadLocal = 8,
    Auto = 16,
    Register = 32,
}
impl Display for StorageClassSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_str(match self {
            StorageClassSpecifier::TypeDef => "TypeDef",
            StorageClassSpecifier::Extern => "Extern",
            StorageClassSpecifier::Static => "Static",
            StorageClassSpecifier::ThreadLocal => "ThreadLocal",
            StorageClassSpecifier::Auto => "Auto",
            StorageClassSpecifier::Register => "Register",
        })
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct ParameterDeclaration {
    pub declaration_specifiers: DeclarationSpecifiers,
    pub declarator: Declarator,
}
#[derive(Clone, Debug, PartialEq)]
pub struct ParameterTypeList {
    pub parameter_list: ParameterList,
    pub ellipsis: bool,
}
#[derive(Clone, Debug, PartialEq)]
pub struct ParameterList {
    pub parameter_declarations: Vec<ParameterDeclaration>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DeclaratorType {
    Abstract,
    Direct { identifier: Option<usize> },
}

#[derive(Clone, Debug, PartialEq)]
pub enum DeclaratorForm {
    Array {
        type_qualifier_flags: u8,
        is_static: bool,
        variable_length: bool,
        assign_expr: Option<ExpressionIndex>,
    },
    Function {
        parameter_type_list: Option<ParameterTypeList>,
        identifier_list: Option<Vec<usize>>,
    },
}

// This Declarator type is used for BOTH
// AbstractDeclarator and Declarator
#[derive(Clone, Debug, PartialEq)]
pub struct Declarator {
    pub r#type: DeclaratorType,
    pub declarator: Option<Box<Declarator>>,
    pub pointer: Vec<Pointer>,
    pub declarator_form: Option<DeclaratorForm>,
}
#[derive(Clone, Debug, PartialEq)]
pub struct TypeName {
    pub specifier_qualifier_list: SpecifierQualifierList,
    pub abstract_declarator: Option<Declarator>,
}
impl TypeName {
    fn new() -> Self {
        Self {
            specifier_qualifier_list: SpecifierQualifierList::new(),
            abstract_declarator: None,
        }
    }
}
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum StructOrUnion {
    Struct,
    Union,
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Pointer {
    pub type_qualifier_flags: u8,
}
#[derive(Clone, Debug, PartialEq)]
pub enum StructDeclarator {
    Declarator(Declarator),
    WithConstExpr {
        declarator: Declarator,
        const_expr: ExpressionIndex,
    },
}
#[derive(Clone, Debug, PartialEq)]
pub struct SpecifierQualifierList {
    pub type_qualifier_flags: u8,
    pub type_specifiers: Vec<TypeSpecifier>,
    pub alignment_specifier: Vec<AlignmentSpecifier>,
}
impl SpecifierQualifierList {
    fn new() -> Self {
        Self {
            type_qualifier_flags: 0,
            type_specifiers: Vec::new(),
            alignment_specifier: Vec::new(),
        }
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct StructDeclaration {
    pub specifier_qualifier_list: SpecifierQualifierList,
    pub struct_declarator_list: Vec<StructDeclarator>,
}
#[derive(Clone, Debug, PartialEq)]
pub struct StructUnionSpecifier {
    pub struct_or_union: StructOrUnion,
    pub identifier: Option<usize>,
    pub struct_declaration_list: Vec<StructDeclaration>,
}
#[derive(Debug, PartialEq, Clone)]
pub enum Enumerator {
    Enum(usize),
    EnumWithConstantExpr(usize, Expr),
}
#[derive(Clone, Debug, PartialEq)]
pub struct EnumSpecifier {
    identifier: Option<usize>,
    enumerator_list: Vec<Enumerator>,
}
#[derive(Clone, Debug, PartialEq)]
pub enum TypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    _Bool,
    _Complex,
    _Atomic(TypeName),
    StructUnion(StructUnionSpecifier),
    Enum(EnumSpecifier),
    IdentTypeDef(usize),
}
impl Display for TypeSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_str(match self {
            TypeSpecifier::Void => "Void",
            TypeSpecifier::Char => "Char",
            TypeSpecifier::Short => "Short",
            TypeSpecifier::Int => "Int",
            TypeSpecifier::Long => "Long",
            TypeSpecifier::Float => "Float",
            TypeSpecifier::Double => "Double",
            TypeSpecifier::Signed => "Signed",
            TypeSpecifier::Unsigned => "Unsigned",
            TypeSpecifier::_Bool => "_Bool",
            TypeSpecifier::_Complex => "_Complex",
            TypeSpecifier::_Atomic(typename) => "_Atomic",
            TypeSpecifier::StructUnion(sus) => "StructUnion",
            TypeSpecifier::Enum(es) => "Enum",
            TypeSpecifier::IdentTypeDef(_) => "IdentTypeDef",
        })
    }
}
#[derive(Copy, Clone)]
pub enum TypeQualifier {
    Const = 1,
    Restrict = 2,
    Volatile = 4,
    _Atomic = 8,
}
impl Display for TypeQualifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_str(match self {
            TypeQualifier::Const => "Const",
            TypeQualifier::Restrict => "Restrict",
            TypeQualifier::Volatile => "Volatile",
            TypeQualifier::_Atomic => "_Atomic",
        })
    }
}
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum FunctionSpecifier {
    Inline,
    _Noreturn,
}
impl Display for FunctionSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_str(match self {
            FunctionSpecifier::Inline => "Inline",
            FunctionSpecifier::_Noreturn => "_Noreturn",
        })
    }
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AlignmentSpecifier {
    _Alignas(TypeNameIndex),
    _AlignasConstExpr(ExpressionIndex),
}
#[derive(Clone, Debug, PartialEq)]
pub struct DeclarationSpecifiers {
    pub storage_class_specifiers_flags: u8,
    pub type_specifiers: Vec<TypeSpecifier>,
    pub type_qualifier_flags: u8,
    pub function_specifier: Option<FunctionSpecifier>,
    pub alignment_specifiers: Vec<AlignmentSpecifier>,
}
impl DeclarationSpecifiers {
    fn new() -> Self {
        Self {
            storage_class_specifiers_flags: 0,
            type_specifiers: Vec::new(),
            type_qualifier_flags: 0,
            function_specifier: None,
            alignment_specifiers: Vec::new(),
        }
    }
}
#[derive(Debug, PartialEq, Clone)]
pub enum Designator {
    WithConstantExpr(ExpressionIndex),
    WithIdentifier(usize),
}
#[derive(Debug, PartialEq, Clone)]
pub struct Designation {
    designator_list: Vec<Designator>,
}
pub type InitializerIndex = usize;
pub type DesignationIndex = usize;
pub type InitializerListIndex = usize;
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct DesignationInitializer {
    pub designation: Option<DesignationIndex>,
    pub initializer: InitializerIndex,
}
#[derive(Debug, PartialEq, Clone)]
pub struct InitializerList {
    designation_initializers: Vec<DesignationInitializer>,
}
#[derive(Copy, Clone, PartialEq)]
pub enum Initializer {
    AssignmentExpression(ExpressionIndex),
    InitializerList(InitializerListIndex),
}
#[derive(Clone)]
pub struct DeclaratorWithInitializer {
    declarator: Declarator,
    initializer: Initializer,
}
#[derive(Clone)]
pub enum InitDeclarator {
    Declarator(Declarator),
    DeclaratorWithInitializer(DeclaratorWithInitializer),
}
#[derive(Clone)]
pub struct Declaration {
    pub declaration_specifiers: DeclarationSpecifiers,
    pub init_declarator_list: Vec<InitDeclarator>,
}
impl Declaration {
    fn new() -> Declaration {
        Declaration {
            declaration_specifiers: DeclarationSpecifiers::new(),
            init_declarator_list: Vec::new(),
        }
    }
}
pub fn is_declaration_token(t: Token) -> bool {
    match t {
        Token {
            r#type:
                TokenType::TYPEDEF
                | TokenType::EXTERN
                | TokenType::STATIC
                | TokenType::_THREAD_LOCAL
                | TokenType::AUTO
                | TokenType::REGISTER
                | TokenType::VOID
                | TokenType::CHAR
                | TokenType::SHORT
                | TokenType::INT
                | TokenType::LONG
                | TokenType::FLOAT
                | TokenType::DOUBLE
                | TokenType::SIGNED
                | TokenType::UNSIGNED
                | TokenType::_BOOL
                | TokenType::_COMPLEX
                | TokenType::STRUCT
                | TokenType::UNION
                | TokenType::ENUM
                | TokenType::CONST
                | TokenType::RESTRICT
                | TokenType::VOLATILE
                | TokenType::_ATOMIC
                | TokenType::INLINE
                | TokenType::_NORETURN
                | TokenType::_ALIGNAS,
            ..
        } => true,
        _ => false,
    }
}

fn parse_init_declarator(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<InitDeclarator, String> {
    let declarator = parse_declarator(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    if matches!(
        tokens.get(*index),
        Some(Token {
            r#type: TokenType::ASSIGNMENT,
            ..
        })
    ) {
        *index += 1;
        consume_whitespace(tokens, index);
        let initializer = parse_initializer(tokens, index, flattened, str_maps)?;
        return Ok(InitDeclarator::DeclaratorWithInitializer(
            DeclaratorWithInitializer {
                declarator,
                initializer,
            },
        ));
    }
    return Ok(InitDeclarator::Declarator(declarator));
}

pub fn parse_init_declarator_list(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Vec<InitDeclarator>, String> {
    let mut init_declarators = Vec::new();
    loop {
        match tokens.get(*index) {
            Some(Token { r#type, .. })
                if *r#type != TokenType::COMMA && *r#type != TokenType::SEMI_COLON =>
            {
                let init_declarator = parse_init_declarator(tokens, index, flattened, str_maps)?;
                init_declarators.push(init_declarator);
            }
            Some(Token {
                r#type: TokenType::COMMA,
                ..
            }) => {
                continue;
            }
            Some(Token {
                r#type: TokenType::SEMI_COLON,
                ..
            })
            | None => {}
            Some(Token { location, .. }) => {
                let location = location.unwrap();
                return Err(error("Unexpected token", location.line, location.column));
            }
        }
        break;
    }
    Ok(init_declarators)
}

pub fn parse_declarations(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Declaration, String> {
    let mut declaration = Declaration::new();
    let declaration_specifiers = parse_declaration_specifiers(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    declaration.declaration_specifiers = declaration_specifiers;
    declaration.init_declarator_list =
        parse_init_declarator_list(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    expected_token(tokens, index, TokenType::SEMI_COLON, "Expected ';'")?;
    Ok(declaration)
}

fn parse_designator(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Designator, String> {
    match tokens.get(*index) {
        Some(Token {
            r#type: TokenType::OPEN_SQR,
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            let expr = parse_expressions(&tokens, index, flattened, str_maps)?;
            consume_whitespace(tokens, index);
            expected_token(tokens, index, TokenType::CLOSE_SQR, "Expected ']'")?;
            flattened.expressions.push(expr);
            return Ok(Designator::WithConstantExpr(
                flattened.expressions.len() - 1,
            ));
        }
        Some(Token {
            r#type: TokenType::DOT,
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            expected_identifier_and_consume(tokens, str_maps, index)?;
            let Some(Token {
                r#type: TokenType::IDENT { str_map_key, .. },
                ..
            }) = tokens.get(*index - 1)
            else {
                unreachable!()
            };
            return Ok(Designator::WithIdentifier(*str_map_key));
        }
        Some(t @ Token { location, .. }) => {
            let location = location.unwrap();
            return Err(error(
                format!(
                    "Unexpected {}",
                    str::from_utf8(&t.to_byte_vec(str_maps).unwrap()).unwrap()
                )
                .as_str(),
                location.line,
                location.column,
            ));
        }
        None => {}
    }
    return Err("Expected . or [".to_string());
}

fn parse_designator_list(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Vec<Designator>, String> {
    let mut designators = Vec::new();
    loop {
        designators.push(parse_designator(tokens, index, flattened, str_maps)?);
        consume_whitespace(tokens, index);
        match tokens.get(*index) {
            Some(Token {
                r#type: TokenType::ASSIGNMENT,
                ..
            }) => {}
            Some(Token {
                r#type: TokenType::OPEN_SQR | TokenType::DOT,
                ..
            }) => {
                continue;
            }
            Some(t @ Token { location, .. }) => {
                let location = location.unwrap();
                return Err(error(
                    format!(
                        "Unexpected {}",
                        str::from_utf8(&t.to_byte_vec(str_maps).unwrap()).unwrap()
                    )
                    .as_str(),
                    location.line,
                    location.column,
                ));
            }
            None => {
                return Err("Unexpected end of tokens".to_string());
            }
        }
        break;
    }
    Ok(designators)
}

fn parse_designation(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Designation, String> {
    let designation = Designation {
        designator_list: parse_designator_list(tokens, index, flattened, str_maps)?,
    };
    consume_whitespace(tokens, index);
    expected_token(tokens, index, TokenType::ASSIGNMENT, "Expected '='")?;
    Ok(designation)
}

// one thing i don't like about this flattened structure keeping all of the data is that, i dont know if a certain structures are already pushed to flattened or not.
// basically i have to look at the implementation to know...
pub fn parse_initializer_list(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<InitializerList, String> {
    consume_whitespace(tokens, index);
    let mut designation_initializers = Vec::new();
    loop {
        let mut designation = None;
        if matches!(
            tokens.get(*index),
            Some(Token {
                r#type: TokenType::OPEN_SQR | TokenType::DOT,
                ..
            })
        ) {
            let designation_parsed = parse_designation(tokens, index, flattened, str_maps)?;
            flattened.designations.push(designation_parsed);
            designation = Some(flattened.designations.len() - 1);
        }
        let init = parse_initializer(&tokens, index, flattened, str_maps)?;
        flattened.initializers.push(init);
        let designation_initializer = DesignationInitializer {
            designation,
            initializer: flattened.initializers.len() - 1,
        };
        designation_initializers.push(designation_initializer);
        consume_whitespace(tokens, index);
        match tokens.get(*index) {
            Some(Token {
                r#type: TokenType::COMMA,
                ..
            }) => {
                *index += 1;
                consume_whitespace(tokens, index);
                if matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: TokenType::CLOSE_CURLY,
                        ..
                    })
                ) {
                    break;
                }
                continue;
            }
            Some(Token {
                r#type: TokenType::CLOSE_CURLY,
                ..
            }) => {
                break;
            }
            Some(t @ Token { location, .. }) => {
                let location = location.unwrap();
                return Err(error(
                    format!(
                        "Unexpected {}",
                        str::from_utf8(&t.to_byte_vec(str_maps).unwrap()).unwrap()
                    )
                    .as_str(),
                    location.line,
                    location.column,
                ));
            }
            None => {
                return Err("Unexpected end of tokens".to_string());
            }
        }
    }
    consume_token(tokens, index, TokenType::COMMA);
    consume_whitespace(tokens, index);
    expected_token(tokens, index, TokenType::CLOSE_CURLY, "Expected '}'")?;
    Ok(InitializerList {
        designation_initializers,
    })
}

pub fn parse_initializer(
    tokens: &[lexer::Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<Initializer, String> {
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(Token {
            r#type: TokenType::OPEN_CURLY,
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            let il = parse_initializer_list(&tokens, index, flattened, str_maps)?;
            flattened.initializer_lists.push(il);
            Ok(Initializer::InitializerList(
                flattened.initializer_lists.len() - 1,
            ))
        }
        _ => {
            let expr = parse_assignment_expression(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(expr);
            Ok(Initializer::AssignmentExpression(
                flattened.expressions.len() - 1,
            ))
        }
    }
}

fn parse_pointer(tokens: &[lexer::Token], index: &mut usize) -> Vec<Pointer> {
    let mut pointer_stack = Vec::new();
    while matches!(
        tokens.get(*index),
        Some(Token {
            r#type: TokenType::ASTERISK,
            ..
        })
    ) {
        *index += 1;
        let mut pointer = Pointer {
            type_qualifier_flags: 0,
        };
        pointer.type_qualifier_flags = parse_type_qualifiers(tokens, index);
        pointer_stack.push(pointer);
        consume_whitespace(tokens, index);
    }
    pointer_stack
}

pub fn parse_identifier_list(
    tokens: &[Token],
    index: &mut usize,
    str_maps: &mut ByteVecMaps,
) -> Result<Vec<usize>, String> {
    let mut identifier_keys = Vec::new();
    loop {
        consume_specifically_spaces(tokens, index);
        if let Some(Token {
            r#type: TokenType::IDENT { str_map_key },
            location: Some(Location { line, column }),
        }) = tokens.get(*index)
        {
            *index += 1;
            let arg = &str_maps.key_to_byte_vec[*str_map_key];
            if identifier_keys.contains(str_map_key) {
                // A parameter identifier in a function-like macro shall be uniquely declared within its scope.
                return Err(error(
                    &format!(
                        "duplicate parameter name: '{}'",
                        String::from_utf8(str_maps.key_to_byte_vec[*str_map_key].clone()).unwrap()
                    ),
                    *line,
                    *column,
                ));
            }
            if *arg == *b"__VA_ARGS__" {
                return Err(error(
                    "__VA_ARGS__ cannot be used as a parameter name",
                    *line,
                    *column,
                ));
            }
            identifier_keys.push(*str_map_key);
        }
        if matches!(
            tokens.get(*index),
            Some(Token {
                r#type: TokenType::COMMA,
                ..
            })
        ) {
            *index += 1;
            continue;
        }
        consume_specifically_spaces(tokens, index);
        if matches!(
            tokens.get(*index),
            Some(Token {
                r#type: TokenType::ELLIPSIS,
                ..
            })
        ) {
            *index += 1;
            identifier_keys.push(str_maps.add_byte_vec(b"__VA_ARGS__"));
            break Ok(identifier_keys);
        }
        if matches!(
            tokens.get(*index),
            Some(Token {
                r#type: TokenType::CLOSE_PAR,
                ..
            })
        ) {
            break Ok(identifier_keys);
        }
    }
}

pub fn parse_declarator(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<Declarator, String> {
    let mut declarator = Declarator {
        r#type: DeclaratorType::Abstract,
        declarator: None,
        pointer: Vec::new(),
        declarator_form: None,
    };
    declarator.pointer = parse_pointer(tokens, index);
    match tokens.get(*index) {
        Some(Token {
            r#type: TokenType::IDENT { str_map_key, .. },
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            declarator.r#type = DeclaratorType::Direct {
                identifier: Some(*str_map_key),
            };
        }
        Some(Token {
            r#type: TokenType::OPEN_PAR,
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            let inner_declarator = parse_declarator(&tokens, index, flattened, str_maps)?;
            if matches!(inner_declarator.r#type, DeclaratorType::Direct { .. }) {
                declarator.r#type = DeclaratorType::Direct { identifier: None };
            }
            expected_token(tokens, index, TokenType::CLOSE_PAR, "Expected ')'")?;
            declarator.declarator = Some(Box::new(inner_declarator));
        }
        Some(Token { location, .. }) => {
            let location = location.unwrap();
            return Err(error(
                "Expected identifier or open parentheses",
                location.line,
                location.column,
            ));
        }
        None => return Err("Expected identifier or open parentheses".to_string()),
    }
    match tokens.get(*index) {
        Some(Token {
            r#type: TokenType::OPEN_SQR,
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            let mut is_static = false;
            let mut type_qualifier_flags = 0;
            let mut variable_length = false;
            let mut assign_expr = None;
            match tokens.get(*index) {
                Some(Token {
                    r#type: TokenType::STATIC,
                    ..
                }) => {
                    *index += 1;
                    consume_whitespace(tokens, index);
                    is_static = true;
                    type_qualifier_flags = parse_type_qualifiers(tokens, index);
                    consume_whitespace(tokens, index);
                    let expr = parse_expressions(&tokens, index, flattened, str_maps)?;
                    flattened.expressions.push(expr);
                    assign_expr = Some(flattened.expressions.len() - 1);
                    expected_token(tokens, index, TokenType::CLOSE_SQR, "Expected ']'")?;
                }
                Some(Token {
                    r#type: TokenType::CLOSE_SQR,
                    ..
                }) => {
                    *index += 1;
                }
                _ => {
                    type_qualifier_flags = parse_type_qualifiers(tokens, index);
                    consume_whitespace(tokens, index);
                    if let Some(Token {
                        r#type: TokenType::STATIC,
                        location,
                    }) = tokens.get(*index)
                    {
                        let location = location.unwrap();
                        *index += 1;
                        if type_qualifier_flags == 0 {
                            return Err(error(
                                "Expected type qualifiers",
                                location.line,
                                location.column,
                            ));
                        }
                        is_static = true;
                    }
                    if let Some(Token {
                        r#type: TokenType::ASTERISK,
                        ..
                    }) = tokens.get(*index)
                    {
                        *index += 1;
                        consume_whitespace(tokens, index);
                        variable_length = true;
                        expected_token(tokens, index, TokenType::CLOSE_SQR, "Expected ']'")?;
                    }
                    if !matches!(
                        tokens.get(*index),
                        Some(Token {
                            r#type: TokenType::CLOSE_SQR,
                            ..
                        })
                    ) {
                        let expr = parse_expressions(&tokens, index, flattened, str_maps)?;
                        flattened.expressions.push(expr);
                        assign_expr = Some(flattened.expressions.len() - 1);
                    }
                    expected_token(tokens, index, TokenType::CLOSE_SQR, "Expected ']'")?;
                }
            }
            declarator.declarator_form = Some(DeclaratorForm::Array {
                type_qualifier_flags,
                is_static,
                variable_length,
                assign_expr,
            });
        }
        Some(Token {
            r#type: TokenType::OPEN_PAR,
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            if is_declaration_token(tokens[*index]) {
                let ptl = parse_parameter_type_list(&tokens, index, flattened, str_maps)?;
                expected_token(tokens, index, TokenType::CLOSE_PAR, "Expected ')'")?;
                declarator.declarator_form = Some(DeclaratorForm::Function {
                    parameter_type_list: Some(ptl),
                    identifier_list: None,
                });
            } else {
                let ident_list = parse_identifier_list(tokens, index, str_maps)?;
                expected_token(tokens, index, TokenType::CLOSE_PAR, "Expected ')'")?;
                declarator.declarator_form = Some(DeclaratorForm::Function {
                    parameter_type_list: None,
                    identifier_list: if !ident_list.is_empty() {
                        Some(ident_list)
                    } else {
                        None
                    },
                });
            }
        }
        _ => {}
    }
    Ok(declarator)
}

fn parse_struct_declarator(
    tokens: &[lexer::Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<StructDeclarator, String> {
    let declarator = parse_declarator(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    if matches!(
        tokens.get(*index),
        Some(Token {
            r#type: TokenType::COLON,
            ..
        })
    ) {
        *index += 1;
        let const_expr = parse_expressions(&tokens, index, flattened, str_maps)?;
        flattened.expressions.push(const_expr);
        Ok(StructDeclarator::WithConstExpr {
            declarator,
            const_expr: flattened.expressions.len() - 1,
        })
    } else {
        Ok(StructDeclarator::Declarator(declarator))
    }
}

fn parse_struct_declaration(
    tokens: &[lexer::Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<StructDeclaration, String> {
    let mut struct_declaration = StructDeclaration {
        specifier_qualifier_list: SpecifierQualifierList::new(),
        struct_declarator_list: Vec::new(),
    };
    consume_whitespace(tokens, index);
    let specifier_qualifier_list = parse_specifiers_qualifiers(tokens, index, flattened, str_maps)?;
    struct_declaration.specifier_qualifier_list = specifier_qualifier_list;
    loop {
        let struct_declarator = parse_struct_declarator(&tokens, index, flattened, str_maps)?;
        struct_declaration
            .struct_declarator_list
            .push(struct_declarator);
        if matches!(
            tokens.get(*index),
            Some(Token {
                r#type: TokenType::SEMI_COLON,
                ..
            })
        ) {
            *index += 1;
            return Ok(struct_declaration);
        }
        expected_token(tokens, index, TokenType::COMMA, "Expected ','")?;
    }
    expected_token(tokens, index, TokenType::SEMI_COLON, "Expected ';'")?;
}

fn parse_struct_union_specifier(
    tokens: &[lexer::Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<StructUnionSpecifier, String> {
    let mut struct_union_specifier = StructUnionSpecifier {
        struct_or_union: match tokens.get(*index) {
            Some(Token {
                r#type: TokenType::STRUCT,
                ..
            }) => StructOrUnion::Struct,
            Some(Token {
                r#type: TokenType::UNION,
                ..
            }) => StructOrUnion::Union,
            _ => unreachable!(),
        },
        identifier: None,
        struct_declaration_list: Vec::new(),
    };
    consume_whitespace(tokens, index);
    if let Some(Token {
        r#type: TokenType::IDENT { str_map_key, .. },
        ..
    }) = tokens.get(*index)
    {
        struct_union_specifier.identifier = Some(*str_map_key);
        *index += 1;
    }
    consume_whitespace(tokens, index);
    if matches!(tokens.get(*index), None) {
        return Ok(struct_union_specifier);
    }
    expected_token(tokens, index, TokenType::OPEN_CURLY, "Expected '{'")?;
    while *index < tokens.len() {
        let struct_declaration = parse_struct_declaration(tokens, index, flattened, str_maps)?;
        struct_union_specifier
            .struct_declaration_list
            .push(struct_declaration);
        consume_whitespace(tokens, index);
        if matches!(
            tokens.get(*index),
            Some(Token {
                r#type: TokenType::CLOSE_CURLY,
                ..
            })
        ) {
            *index += 1;
            return Ok(struct_union_specifier);
        }
    }
    let Token {
        location: Some(Location { line, .. }),
        ..
    } = tokens.last().unwrap()
    else {
        unreachable!()
    };
    return Err(error(
        "Unexpected end of struct-or-union specific",
        *line,
        0,
    ));
}

fn parse_enumerator_specifier(
    tokens: &[lexer::Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<EnumSpecifier, String> {
    let mut enum_specifier = EnumSpecifier {
        identifier: None,
        enumerator_list: Vec::new(),
    };
    expected_token(tokens, index, TokenType::ENUM, "Expected 'enum'")?;
    *index += 1;
    consume_whitespace(tokens, index);
    if let Some(Token {
        r#type: TokenType::IDENT { str_map_key, .. },
        ..
    }) = tokens.get(*index)
    {
        enum_specifier.identifier = Some(*str_map_key);
        *index += 1;
    }
    consume_whitespace(tokens, index);
    expected_token(tokens, index, TokenType::OPEN_CURLY, "Expected '{'")?;
    loop {
        match tokens[*index] {
            Token {
                r#type: TokenType::WHITESPACE | TokenType::NEWLINE,
                ..
            } => {
                consume_whitespace(tokens, index);
                continue;
            }
            Token {
                r#type: TokenType::IDENT { str_map_key, .. },
                ..
            } => {
                *index += 1;
                consume_whitespace(tokens, index);
                if matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: TokenType::ASSIGNMENT,
                        ..
                    })
                ) {
                    *index += 1;
                    consume_whitespace(tokens, index);
                    let expr = parse_conditional_expression(tokens, index, flattened, str_maps)?;
                    enum_specifier
                        .enumerator_list
                        .push(Enumerator::EnumWithConstantExpr(str_map_key, expr));
                } else {
                    enum_specifier
                        .enumerator_list
                        .push(Enumerator::Enum(str_map_key));
                }
                consume_whitespace(tokens, index);
                match tokens.get(*index) {
                    Some(Token {
                        r#type: TokenType::CLOSE_CURLY,
                        ..
                    }) => {
                        *index += 1;
                        break;
                    }
                    Some(Token {
                        r#type: TokenType::COMMA,
                        ..
                    }) => {
                        *index += 1;
                        continue;
                    }
                    Some(Token { location, .. }) => {
                        let location = location.unwrap();
                        return Err(error("Unexpected token", location.line, location.column));
                    }
                    None => {
                        return Err("Unexpected end of tokens".to_string());
                    }
                }
            }
            Token {
                location: Some(location),
                ..
            } => return Err(error("Unexpected token", location.line, location.column)),
            _ => unreachable!(),
        }
    }
    Ok(enum_specifier)
}

pub fn is_type_qualifier_token(tokens: &[Token], index: &mut usize) -> bool {
    match tokens.get(*index) {
        Some(
            t @ Token {
                r#type:
                    TokenType::CONST | TokenType::RESTRICT | TokenType::VOLATILE | TokenType::_ATOMIC,
                ..
            },
        ) => {
            if t.r#type == TokenType::_ATOMIC {
                let mut index_copy = *index + 1;
                consume_whitespace(tokens, &mut index_copy);
                if let Some(Token {
                    r#type: OPEN_PAR, ..
                }) = tokens.get(index_copy)
                {
                    return false;
                }
            }
            return true;
        }
        _ => return false,
    }
}

pub fn is_alignment_specifier_token(t: Token) -> bool {
    match t.r#type {
        TokenType::_ALIGNAS => true,
        _ => false,
    }
}

pub fn parse_alignment_specifier(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<AlignmentSpecifier, String> {
    expected_token(tokens, index, TokenType::OPEN_PAR, "Expected '('")?;
    consume_whitespace(tokens, index);
    match tokens.get(*index) {
        Some(t) => {
            if is_type_specifier_token(*t)
                || is_type_qualifier_token(tokens, index)
                || is_alignment_specifier_token(*t)
            {
                let type_name = parse_type_names(tokens, index, flattened, str_maps)?;
                flattened.type_names.push(type_name);
                expected_token(tokens, index, TokenType::CLOSE_PAR, "Expected ')'")?;
                Ok(AlignmentSpecifier::_Alignas(flattened.type_names.len() - 1))
            } else {
                let expr = parse_expressions(tokens, index, flattened, str_maps)?;
                flattened.expressions.push(expr);
                expected_token(tokens, index, TokenType::CLOSE_PAR, "Expected ')'")?;
                Ok(AlignmentSpecifier::_AlignasConstExpr(
                    flattened.expressions.len() - 1,
                ))
            }
        }
        None => Err("Unexpected end of tokens".to_string()),
    }
}

pub fn parse_specifiers_qualifiers(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<SpecifierQualifierList, String> {
    let mut specifier_qualifier = SpecifierQualifierList::new();
    loop {
        if let Some(t) = tokens.get(*index) {
            if is_type_specifier_token(*t) {
                let specifiers = parse_type_specifiers(tokens, index, flattened, str_maps)?;
                specifier_qualifier
                    .type_specifiers
                    .extend_from_slice(&specifiers);
            } else if is_type_qualifier_token(tokens, index) {
                specifier_qualifier.type_qualifier_flags = parse_type_qualifiers(tokens, index);
            } else if is_alignment_specifier_token(*t) {
                *index += 1;
                consume_whitespace(tokens, index);
                specifier_qualifier
                    .alignment_specifier
                    .push(parse_alignment_specifier(
                        tokens, index, flattened, str_maps,
                    )?);
            } else {
                break;
            }
        } else {
            break;
        }
    }
    Ok(specifier_qualifier)
}

pub fn parse_type_qualifiers(tokens: &[lexer::Token], index: &mut usize) -> u8 {
    let mut type_qualifier_flags = 0;
    while *index < tokens.len() {
        match tokens[*index] {
            Token {
                r#type: TokenType::CONST,
                ..
            } => {
                type_qualifier_flags |= TypeQualifier::Const as u8;
            }
            Token {
                r#type: TokenType::RESTRICT,
                ..
            } => {
                type_qualifier_flags |= TypeQualifier::Restrict as u8;
            }
            Token {
                r#type: TokenType::VOLATILE,
                ..
            } => {
                type_qualifier_flags |= TypeQualifier::Volatile as u8;
            }
            Token {
                r#type: TokenType::_ATOMIC,
                ..
            } => {
                type_qualifier_flags |= TypeQualifier::_Atomic as u8;
            }
            _ => break,
        }
        *index += 1;
        consume_whitespace(tokens, index);
    }
    type_qualifier_flags
}

pub fn is_type_specifier_token(t: Token) -> bool {
    match t.r#type {
        TokenType::STRUCT
        | TokenType::UNION
        | TokenType::VOID
        | TokenType::CHAR
        | TokenType::_ATOMIC
        | TokenType::ENUM
        | TokenType::SHORT
        | TokenType::INT
        | TokenType::LONG
        | TokenType::FLOAT
        | TokenType::DOUBLE
        | TokenType::SIGNED
        | TokenType::UNSIGNED
        | TokenType::_BOOL
        | TokenType::_COMPLEX => true,
        _ => false,
    }
}

pub fn parse_type_specifiers(
    tokens: &[lexer::Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<Vec<TypeSpecifier>, String> {
    let mut type_specifiers = Vec::new();
    while *index < tokens.len() {
        match tokens[*index] {
            Token {
                r#type: TokenType::WHITESPACE { .. } | TokenType::NEWLINE,
                ..
            } => {}
            Token {
                r#type: TokenType::VOID,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::Void);
            }
            Token {
                r#type: TokenType::CHAR,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::Char);
            }
            Token {
                r#type: TokenType::SHORT,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::Short);
            }
            Token {
                r#type: TokenType::INT,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::Int);
            }
            Token {
                r#type: TokenType::LONG,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::Long);
            }
            Token {
                r#type: TokenType::FLOAT,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::Float);
            }
            Token {
                r#type: TokenType::DOUBLE,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::Double);
            }
            Token {
                r#type: TokenType::SIGNED,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::Signed);
            }
            Token {
                r#type: TokenType::UNSIGNED,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::Unsigned);
            }
            Token {
                r#type: TokenType::_BOOL,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::_Bool);
            }
            Token {
                r#type: TokenType::_COMPLEX,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::_Complex);
            }
            Token {
                r#type: TokenType::STRUCT { .. } | TokenType::UNION,
                ..
            } => {
                let struct_union_specifier =
                    parse_struct_union_specifier(tokens, index, flattened, str_maps)?;
                type_specifiers.push(TypeSpecifier::StructUnion(struct_union_specifier));
                continue;
            }
            Token {
                r#type: TokenType::_ATOMIC,
                ..
            } => todo!(),
            Token {
                r#type: TokenType::ENUM,
                ..
            } => {
                let enum_specifier =
                    parse_enumerator_specifier(tokens, index, flattened, str_maps)?;
                type_specifiers.push(TypeSpecifier::Enum(enum_specifier));
                continue;
            }
            // TODO: typedef identifiers
            Token {
                r#type: TokenType::IDENT { str_map_key, .. },
                ..
            } if type_specifiers.is_empty() => {
                type_specifiers.push(TypeSpecifier::IdentTypeDef(str_map_key));
                break;
            }
            _ => break,
        }
        *index += 1;
    }
    Ok(type_specifiers)
}

pub fn parse_declaration_specifiers(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<DeclarationSpecifiers, String> {
    let mut declaration_specifier = DeclarationSpecifiers::new();
    while *index < tokens.len() {
        match tokens[*index] {
            Token {
                r#type: TokenType::TYPEDEF,
                ..
            } => {
                declaration_specifier.storage_class_specifiers_flags |=
                    StorageClassSpecifier::TypeDef as u8
            }
            Token {
                r#type: TokenType::EXTERN,
                ..
            } => {
                declaration_specifier.storage_class_specifiers_flags |=
                    StorageClassSpecifier::Extern as u8
            }
            Token {
                r#type: TokenType::STATIC,
                ..
            } => {
                declaration_specifier.storage_class_specifiers_flags |=
                    StorageClassSpecifier::Static as u8
            }
            Token {
                r#type: TokenType::_THREAD_LOCAL,
                ..
            } => {
                declaration_specifier.storage_class_specifiers_flags |=
                    StorageClassSpecifier::ThreadLocal as u8
            }
            Token {
                r#type: TokenType::AUTO,
                ..
            } => {
                declaration_specifier.storage_class_specifiers_flags |=
                    StorageClassSpecifier::Auto as u8
            }
            Token {
                r#type: TokenType::REGISTER,
                ..
            } => {
                declaration_specifier.storage_class_specifiers_flags |=
                    StorageClassSpecifier::Register as u8
            }
            Token {
                r#type: TokenType::VOID,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Void),
            Token {
                r#type: TokenType::CHAR,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Char),
            Token {
                r#type: TokenType::SHORT,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Short),
            Token {
                r#type: TokenType::INT,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Int),
            Token {
                r#type: TokenType::LONG,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Long),
            Token {
                r#type: TokenType::FLOAT,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Float),
            Token {
                r#type: TokenType::DOUBLE,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Double),
            Token {
                r#type: TokenType::SIGNED,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Signed),
            Token {
                r#type: TokenType::UNSIGNED,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Unsigned),
            Token {
                r#type: TokenType::_BOOL,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::_Bool),
            Token {
                r#type: TokenType::_COMPLEX,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::_Complex),
            Token {
                r#type: TokenType::STRUCT { .. } | TokenType::UNION,
                ..
            } => todo!(),
            Token {
                r#type: TokenType::ENUM,
                ..
            } => todo!(),
            Token {
                r#type: TokenType::CONST,
                ..
            } => declaration_specifier.type_qualifier_flags |= TypeQualifier::Const as u8,
            Token {
                r#type: TokenType::RESTRICT,
                ..
            } => declaration_specifier.type_qualifier_flags |= TypeQualifier::Restrict as u8,
            Token {
                r#type: TokenType::VOLATILE,
                ..
            } => declaration_specifier.type_qualifier_flags |= TypeQualifier::Volatile as u8,
            Token {
                r#type: TokenType::_ATOMIC,
                ..
            } => {
                *index += 1;
                consume_whitespace(tokens, index);
                if let Some(Token {
                    r#type: TokenType::OPEN_PAR,
                    ..
                }) = tokens.get(*index)
                {
                    let type_name = parse_type_names(&tokens, index, flattened, str_maps)?;
                    expected_token(tokens, index, TokenType::CLOSE_PAR, "Expectedd ')'")?;
                    declaration_specifier
                        .type_specifiers
                        .push(TypeSpecifier::_Atomic(type_name));
                } else {
                    declaration_specifier.type_qualifier_flags |= TypeQualifier::_Atomic as u8
                }
            }
            Token {
                r#type: TokenType::INLINE,
                ..
            } => declaration_specifier.function_specifier = Some(FunctionSpecifier::Inline),
            Token {
                r#type: TokenType::_NORETURN,
                ..
            } => declaration_specifier.function_specifier = Some(FunctionSpecifier::_Noreturn),
            Token {
                r#type: TokenType::_ALIGNAS,
                ..
            } => todo!(),
            _ => break,
        }
        *index += 1;
    }
    Ok(declaration_specifier)
}

fn parse_parameter_declaration(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<ParameterDeclaration, String> {
    let declaration_specifiers = parse_declaration_specifiers(tokens, index, flattened, str_maps)?;
    let declarator = parse_declarator(tokens, index, flattened, str_maps)?;
    Ok(ParameterDeclaration {
        declaration_specifiers,
        declarator,
    })
}

fn parse_parameter_list(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<ParameterList, String> {
    let mut parameter_declarations = Vec::new();
    loop {
        let parameter_declaration =
            parse_parameter_declaration(tokens, index, flattened, str_maps)?;
        parameter_declarations.push(parameter_declaration);
        if let Some(Token {
            r#type: TokenType::COMMA,
            ..
        }) = tokens.get(*index)
        {
            *index += 1;
            consume_whitespace(tokens, index);
            if let Some(Token {
                r#type: TokenType::ELLIPSIS,
                ..
            }) = tokens.get(*index)
            {
                break;
            }
            continue;
        }
        break;
    }
    Ok(ParameterList {
        parameter_declarations,
    })
}

pub fn parse_parameter_type_list(
    tokens: &[lexer::Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<ParameterTypeList, String> {
    let parameter_list = parse_parameter_list(tokens, index, flattened, str_maps)?;
    let ellipsis = match tokens.get(*index) {
        Some(Token {
            r#type: TokenType::ELLIPSIS,
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            true
        }
        _ => {
            expected_token(tokens, index, TokenType::CLOSE_PAR, "Expected ')'")?;
            false
        }
    };
    Ok(ParameterTypeList {
        parameter_list,
        ellipsis,
    })
}

pub fn parse_type_names(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<TypeName, String> {
    let mut type_name = TypeName::new();
    type_name.specifier_qualifier_list =
        parse_specifiers_qualifiers(tokens, index, flattened, str_maps)?;
    let ad = parse_declarator(tokens, index, flattened, str_maps)?;
    type_name.abstract_declarator = Some(ad);
    Ok(type_name)
}

#[cfg(test)]
mod tests {
    use super::{
        parse_declarations, parse_declarator, parse_enumerator_specifier, parse_initializer,
        parse_parameter_list, parse_parameter_type_list, parse_struct_declarator,
        parse_struct_union_specifier, parse_type_names, Declaration, Declarator, DeclaratorForm,
        DeclaratorType, Designation, Designator, Enumerator, InitDeclarator, Initializer,
        InitializerList, TypeQualifier, TypeSpecifier,
    };
    use crate::lexer::*;
    use crate::parser::expressions::*;
    use crate::parser::*;
    #[test]
    fn parse_enumerator_specifier_test() -> Result<(), String> {
        {
            let src = r#"enum HI {
            YOUR_MOM,
            HEHE
        }"#
            .as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(src, false, &mut str_maps)?;
            let mut flattened = Flattened::new();
            let enum_specifier = parse_enumerator_specifier(
                tokens.as_slice(),
                &mut 0,
                &mut flattened,
                &mut str_maps,
            )?;
            assert_eq!(
                enum_specifier.identifier,
                Some(str_maps.add_byte_vec("HI".as_bytes()))
            );
            assert_eq!(
                enum_specifier.enumerator_list,
                vec![
                    Enumerator::Enum(str_maps.add_byte_vec("YOUR_MOM".as_bytes())),
                    Enumerator::Enum(str_maps.add_byte_vec("HEHE".as_bytes()))
                ],
                "failed 1"
            );
        }
        Ok(())
    }
    #[test]
    fn parse_initializer_test() -> Result<(), String> {
        {
            let src = r#"{ .hi = 4, .hi2 = 4 }"#.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer(src, false, &mut str_maps)?;
            let i = parse_initializer(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            let Initializer::InitializerList(ili) = i else {
                unreachable!()
            };
            let il = &flattened.initializer_lists[ili];
            assert_eq!(
                *il,
                InitializerList {
                    designation_initializers: vec![
                        DesignationInitializer {
                            designation: Some(
                                flattened
                                    .designations
                                    .iter()
                                    .position(|d| d.designator_list
                                        == vec![Designator::WithIdentifier(
                                            str_maps.add_byte_vec("hi".as_bytes())
                                        )])
                                    .unwrap()
                            ),
                            initializer: flattened
                                .initializers
                                .iter()
                                .position(|i| *i == Initializer::AssignmentExpression(0))
                                .expect("Expected to get an initializer that is in flattened")
                        },
                        DesignationInitializer {
                            designation: Some(
                                flattened
                                    .designations
                                    .iter()
                                    .position(|d| d.designator_list
                                        == vec![Designator::WithIdentifier(
                                            str_maps.add_byte_vec("hi2".as_bytes())
                                        )])
                                    .unwrap()
                            ),
                            initializer: flattened
                                .initializers
                                .iter()
                                .position(|i| *i == Initializer::AssignmentExpression(1))
                                .expect("Expected to get an initializer that is in flattened")
                        }
                    ]
                }
            );
        }
        {
            let src = r#"{ {[100] = 5}, 8, .baz = "" }"#.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer(src, false, &mut str_maps)?;
            let i = parse_initializer(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            let Initializer::InitializerList(ili) = i else {
                unreachable!()
            };
            let il = &flattened.initializer_lists[ili];
            assert_eq!(
                *il,
                InitializerList {
                    designation_initializers: vec![
                        DesignationInitializer {
                            designation: None,
                            initializer: flattened
                                .initializers
                                .iter()
                                .position(|i| *i
                                    == Initializer::InitializerList(
                                        flattened
                                            .initializer_lists
                                            .iter()
                                            .position(|ii| *ii
                                                == InitializerList {
                                                    designation_initializers: vec![
                                                        DesignationInitializer {
                                                            designation: Some(
                                                                flattened
                                                                    .designations
                                                                    .iter()
                                                                    .position(|d| d.designator_list
                                                                        == vec![
                                                                    Designator::WithConstantExpr(0)
                                                                ])
                                                                    .unwrap()
                                                            ),
                                                            initializer: flattened
                                                                .initializers
                                                                .iter()
                                                                .position(|ir| {
                                                                    *ir
                                                            == Initializer::AssignmentExpression(1)
                                                                })
                                                                .unwrap()
                                                        }
                                                    ]
                                                })
                                            .unwrap()
                                    ))
                                .unwrap()
                        },
                        DesignationInitializer {
                            designation: None,
                            initializer: flattened
                                .initializers
                                .iter()
                                .position(|i| *i == Initializer::AssignmentExpression(2))
                                .unwrap()
                        },
                        DesignationInitializer {
                            designation: Some(
                                flattened
                                    .designations
                                    .iter()
                                    .position(|d| d.designator_list
                                        == vec![Designator::WithIdentifier(
                                            str_maps.add_byte_vec("baz".as_bytes())
                                        )])
                                    .unwrap()
                            ),
                            initializer: flattened
                                .initializers
                                .iter()
                                .position(|ir| { *ir == Initializer::AssignmentExpression(3) })
                                .unwrap()
                        }
                    ]
                }
            );
        }
        Ok(())
    }
    #[test]
    fn parse_type_names_test_pointer_to_int_array_size_3() -> Result<(), String> {
        let src = r#"int (*)[3]"#.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let mut flattened = Flattened::new();
        let tokens = lexer(src, false, &mut str_maps)?;
        let type_name = parse_type_names(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
        assert!(matches!(
            type_name.specifier_qualifier_list.type_specifiers.first(),
            Some(TypeSpecifier::Int)
        ));
        assert_eq!(
            type_name.abstract_declarator,
            Some(Declarator {
                r#type: DeclaratorType::Abstract,
                pointer: Vec::new(),
                declarator: Some(Box::new(Declarator {
                    r#type: DeclaratorType::Abstract,
                    pointer: vec![Pointer {
                        type_qualifier_flags: 0
                    }],
                    declarator: None,
                    declarator_form: None,
                })),
                declarator_form: Some(DeclaratorForm::Array {
                    is_static: false,
                    variable_length: false,
                    type_qualifier_flags: 0,
                    assign_expr: Some(flattened.expressions.len() - 1)
                })
            })
        );
        let DeclaratorForm::Array { assign_expr, .. } = type_name
            .abstract_declarator
            .unwrap()
            .declarator_form
            .unwrap()
        else {
            unreachable!()
        };
        let expr_idx = assign_expr.unwrap();
        assert_eq!(
            flattened.expressions[expr_idx],
            Expr::Primary(PrimaryType::Token(Token {
                r#type: TokenType::CONSTANT_DEC_INT {
                    value_key: str_maps.add_byte_vec("3".as_bytes()),
                    suffix: None
                },
                location: Some(Location { line: 0, column: 8 })
            }))
        );
        Ok(())
    }
    #[test]
    fn parse_type_names_test_pointer_to_variable_length_int_array() -> Result<(), String> {
        let src = r##"int (*)[*]"##.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let mut flattened = Flattened::new();
        let tokens = lexer(src, false, &mut str_maps)?;
        let type_name = parse_type_names(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
        assert_eq!(
            type_name,
            TypeName {
                specifier_qualifier_list: SpecifierQualifierList {
                    type_qualifier_flags: 0,
                    type_specifiers: vec![TypeSpecifier::Int],
                    alignment_specifier: vec![],
                },
                abstract_declarator: Some(Declarator {
                    r#type: DeclaratorType::Abstract,
                    pointer: vec![],
                    declarator: Some(Box::new(Declarator {
                        r#type: DeclaratorType::Abstract,
                        declarator: None,
                        declarator_form: None,
                        pointer: vec![Pointer {
                            type_qualifier_flags: 0
                        }],
                    })),
                    declarator_form: Some(DeclaratorForm::Array {
                        type_qualifier_flags: 0,
                        is_static: false,
                        variable_length: true,
                        assign_expr: None,
                    })
                })
            }
        );
        Ok(())
    }
    #[test]
    fn parse_parameter_list_test() -> Result<(), String> {
        let src = r#"int hi, int hi2, int hi3"#.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let mut flattened = Flattened::new();
        let tokens = lexer(src, false, &mut str_maps)?;
        let ptl = parse_parameter_list(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
        assert_eq!(
            ptl,
            ParameterList {
                parameter_declarations: vec![
                    ParameterDeclaration {
                        declaration_specifiers: DeclarationSpecifiers {
                            type_specifiers: vec![TypeSpecifier::Int],
                            type_qualifier_flags: 0,
                            storage_class_specifiers_flags: 0,
                            alignment_specifiers: vec![],
                            function_specifier: None,
                        },
                        declarator: Declarator {
                            r#type: DeclaratorType::Direct {
                                identifier: Some(str_maps.add_byte_vec("hi".as_bytes()))
                            },
                            pointer: vec![],
                            declarator: None,
                            declarator_form: None,
                        }
                    },
                    ParameterDeclaration {
                        declaration_specifiers: DeclarationSpecifiers {
                            type_specifiers: vec![TypeSpecifier::Int],
                            type_qualifier_flags: 0,
                            storage_class_specifiers_flags: 0,
                            alignment_specifiers: vec![],
                            function_specifier: None,
                        },
                        declarator: Declarator {
                            r#type: DeclaratorType::Direct {
                                identifier: Some(str_maps.add_byte_vec("hi2".as_bytes()))
                            },
                            declarator: None,
                            pointer: vec![],
                            declarator_form: None
                        }
                    },
                    ParameterDeclaration {
                        declaration_specifiers: DeclarationSpecifiers {
                            type_specifiers: vec![TypeSpecifier::Int],
                            type_qualifier_flags: 0,
                            storage_class_specifiers_flags: 0,
                            alignment_specifiers: vec![],
                            function_specifier: None,
                        },
                        declarator: Declarator {
                            r#type: DeclaratorType::Direct {
                                identifier: Some(str_maps.add_byte_vec("hi3".as_bytes()))
                            },
                            declarator: None,
                            declarator_form: None,
                            pointer: vec![],
                        }
                    }
                ]
            }
        );
        Ok(())
    }
    #[test]
    fn parse_struct_declarator_test() -> Result<(), String> {
        {
            let src = r#"hi : 4"#.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer(src, false, &mut str_maps)?;
            let struct_declarator =
                parse_struct_declarator(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(
                struct_declarator,
                StructDeclarator::WithConstExpr {
                    declarator: Declarator {
                        r#type: DeclaratorType::Direct { .. },
                        ..
                    },
                    ..
                }
            ));
        }
        Ok(())
    }
    #[test]
    fn parse_struct_union_specifier_test() -> Result<(), String> {
        {
            let src = r#"struct {
int : 4;
};"#
            .as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer(src, false, &mut str_maps)?;
            let struct_union_specifier =
                parse_struct_union_specifier(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(struct_union_specifier.struct_declaration_list.len() == 1);
            assert!(
                struct_union_specifier.struct_declaration_list[0]
                    .specifier_qualifier_list
                    .type_specifiers
                    .len()
                    == 1
            );
            assert!(matches!(
                struct_union_specifier.struct_declaration_list[0].struct_declarator_list[0],
                StructDeclarator::WithConstExpr { .. }
            ));
        }
        {
            let src = r#"struct HEHE {
int hi;
};"#
            .as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer(src, false, &mut str_maps)?;
            let struct_union_specifier =
                parse_struct_union_specifier(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(struct_union_specifier.struct_declaration_list.len() == 1);
            assert!(
                struct_union_specifier.identifier.unwrap()
                    == str_maps.add_byte_vec("HEHE".as_bytes())
            );
            assert!(
                struct_union_specifier.struct_declaration_list[0]
                    .specifier_qualifier_list
                    .type_specifiers
                    .len()
                    == 1
            );
            assert!(matches!(
                struct_union_specifier.struct_declaration_list[0].struct_declarator_list[0],
                StructDeclarator::Declarator(_)
            ));
        }
        Ok(())
    }
    #[test]
    fn parse_declarations_test() -> Result<(), String> {
        {
            let src = r#"int hi = 4;"#;
            let mut str_maps = ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer(src.as_bytes(), false, &mut str_maps)?;
            let declaration = parse_declarations(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            let Declaration {
                declaration_specifiers,
                init_declarator_list,
            } = declaration;
            assert!(
                matches!(
                    declaration_specifiers.type_specifiers.get(0),
                    Some(TypeSpecifier::Int)
                ),
                "int was not added to the type specifier list"
            );
            assert!(
                matches!(
                    init_declarator_list.get(0),
                    Some(InitDeclarator::DeclaratorWithInitializer(_))
                ),
                "declarator and initializer not added to list",
            );
            let Some(InitDeclarator::DeclaratorWithInitializer(di)) = init_declarator_list.get(0)
            else {
                unreachable!()
            };
            assert!(matches!(
                di.initializer,
                Initializer::AssignmentExpression(_)
            ));
        }
        {
            // gnarly
            let src = r#"void (*(*f[])())();"#;
            let mut str_maps = ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer(src.as_bytes(), false, &mut str_maps)?;
            let declaration = parse_declarations(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            let Declaration {
                declaration_specifiers,
                init_declarator_list,
            } = declaration;
            assert!(
                matches!(
                    declaration_specifiers.type_specifiers.get(0),
                    Some(TypeSpecifier::Void)
                ),
                "int was not added to the type specifier list"
            );
            assert!(
                matches!(
                    init_declarator_list.get(0),
                    Some(InitDeclarator::Declarator(_))
                ),
                "declarator and initializer not added to list",
            );
            let Some(InitDeclarator::Declarator(d)) = init_declarator_list.get(0) else {
                unreachable!()
            };
            assert_eq!(
                *d,
                Declarator {
                    r#type: DeclaratorType::Direct { identifier: None },
                    declarator: Some(Box::new(Declarator {
                        r#type: DeclaratorType::Direct { identifier: None },
                        declarator: Some(Box::new(Declarator {
                            r#type: DeclaratorType::Direct {
                                identifier: Some(str_maps.add_byte_vec("f".as_bytes()))
                            },
                            declarator: None,
                            pointer: vec![Pointer {
                                type_qualifier_flags: 0
                            }],
                            declarator_form: Some(DeclaratorForm::Array {
                                type_qualifier_flags: 0,
                                is_static: false,
                                variable_length: false,
                                assign_expr: None
                            })
                        })),
                        pointer: vec![Pointer {
                            type_qualifier_flags: 0
                        }],
                        declarator_form: Some(DeclaratorForm::Function {
                            parameter_type_list: None,
                            identifier_list: None
                        }),
                    })),
                    pointer: vec![],
                    declarator_form: Some(DeclaratorForm::Function {
                        parameter_type_list: None,
                        identifier_list: None
                    }),
                }
            );
        }
        Ok(())
    }
}
