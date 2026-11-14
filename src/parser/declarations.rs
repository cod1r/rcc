use crate::error::*;
use crate::lexer;
use crate::lexer::*;
use crate::parser::expressions::*;
use crate::parser::statements::*;
use crate::parser::*;
use crate::*;
use std::fmt::Display;

pub type TypeNameIndex = usize;
pub type DeclarationIndex = usize;
#[derive(Copy, Clone)]
pub enum StorageClassSpecifier {
    TypeDef,
    Extern,
    Static,
    ThreadLocal,
    Auto,
    Register,
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
#[derive(Clone)]
pub struct ParameterDeclaration {
    declaration_specifiers: DeclarationSpecifiers,
    declarator: Declarator,
}
#[derive(Clone)]
pub struct ParameterTypeList {
    pub parameter_list: ParameterList,
    pub ellipsis: bool,
}
#[derive(Clone)]
pub struct ParameterList {
    pub parameter_declarations: Vec<ParameterDeclaration>,
}

#[derive(Clone, Copy)]
enum DeclaratorType {
    Abstract,
    Direct { identifier: usize },
}

#[derive(Clone)]
pub struct Declarator {
    r#type: DeclaratorType,
    pub declarator: Option<Box<Declarator>>,
    pub type_qualifier_flags: u8,
    pub is_static: bool,
    pub pointer: Vec<Pointer>,
    pub parameter_type_list: Option<ParameterTypeList>,
    pub assign_expr: Option<ExpressionIndex>,
}
#[derive(Clone)]
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
#[derive(Copy, Clone)]
pub enum StructOrUnion {
    Struct,
    Union,
}
#[derive(Clone)]
pub struct Pointer {
    type_qualifier_flags: u8,
}
#[derive(Clone)]
pub enum StructDeclarator {
    Declarator(Declarator),
    WithConstExpr {
        declarator: Declarator,
        const_expr: ExpressionIndex,
    },
}
#[derive(Clone)]
pub struct SpecifierQualifierList {
    pub type_qualifiers_flags: u8,
    pub type_specifiers: Vec<TypeSpecifier>,
    pub alignment_specifier: Vec<AlignmentSpecifier>,
}
impl SpecifierQualifierList {
    fn new() -> Self {
        Self {
            type_qualifiers_flags: 0,
            type_specifiers: Vec::new(),
            alignment_specifier: Vec::new(),
        }
    }
}
#[derive(Clone)]
pub struct StructDeclaration {
    specifier_qualifier_list: SpecifierQualifierList,
    struct_declarator_list: Vec<StructDeclarator>,
}
#[derive(Clone)]
pub struct StructUnionSpecifier {
    pub struct_or_union: StructOrUnion,
    pub identifier: Option<usize>,
    pub struct_declaration_list: Vec<StructDeclaration>,
}
#[derive(Debug, PartialEq, Clone)]
pub enum Enumerator {
    Enum(usize),
    EnumWithConstantExpr(usize, i128),
}
#[derive(Clone)]
pub struct EnumSpecifier {
    identifier: Option<usize>,
    enumerator_list: Vec<Enumerator>,
}
#[derive(Clone)]
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
#[derive(Copy, Clone)]
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
#[derive(Clone, Copy)]
pub enum AlignmentSpecifier {
    _Alignas(TypeNameIndex),
    _AlignasConstExpr(ExpressionIndex),
}
#[derive(Clone)]
pub struct DeclarationSpecifiers {
    pub storage_class_specifiers: Vec<StorageClassSpecifier>,
    pub type_specifiers: Vec<TypeSpecifier>,
    pub type_qualifiers_flags: u8,
    pub function_specifiers: Vec<FunctionSpecifier>,
    pub alignment_specifiers: Vec<AlignmentSpecifier>,
}
impl DeclarationSpecifiers {
    fn new() -> Self {
        Self {
            storage_class_specifiers: Vec::new(),
            type_specifiers: Vec::new(),
            type_qualifiers_flags: 0,
            function_specifiers: Vec::new(),
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
pub struct InitializerList {
    designation: Option<DesignationIndex>,
    initializer: InitializerIndex,
    next: Option<InitializerListIndex>,
}
#[derive(Copy, Clone)]
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
            Some(Token { .. }) => {
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
    let declaration_specifier = parse_declaration_specifiers(tokens, index, flattened, str_maps)?;
    declaration.declaration_specifiers = declaration_specifier;
    declaration.init_declarator_list =
        parse_init_declarator_list(tokens, index, flattened, str_maps)?;
    consume_whitespace(tokens, index);
    expected_token(tokens, index, TokenType::SEMI_COLON, "Expected ';'")?;
    Ok(declaration)
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
            let il = parse_initializer_list(&tokens, index, flattened, str_maps)?;
            flattened.initializer_lists.push(il);
            Ok(Initializer::InitializerList(
                flattened.initializer_lists.len() - 1,
            ))
        }
        _ => {
            let expr = parse_expressions(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(expr);
            Ok(Initializer::AssignmentExpression(
                flattened.expressions.len() - 1,
            ))
        }
    }
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
            let expr = parse_expressions(&tokens, index, flattened, str_maps)?;
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
            }) = tokens.get(*index)
            else {
                unreachable!()
            };
            return Ok(Designator::WithIdentifier(*str_map_key));
        }
        _ => {}
    }
    match tokens.get(*index) {
        Some(Token {
            location: Some(Location { line, column }),
            ..
        }) => {
            return Err(error("Unexpected =, expected . or [", *line, *column));
        }
        _ => {}
    }
    return Err("Unexpected =, expected . or [".to_string());
}

fn parse_designator_list(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Vec<Designator>, String> {
    let mut designators = Vec::new();
    while !matches!(
        tokens.get(*index),
        Some(Token {
            r#type: TokenType::ASSIGNMENT,
            ..
        })
    ) {
        designators.push(parse_designator(tokens, index, flattened, str_maps)?);
    }
    Ok(designators)
}

fn parse_designation(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut ByteVecMaps,
) -> Result<Designation, String> {
    let mut designation = Designation {
        designator_list: parse_designator_list(tokens, index, flattened, str_maps)?,
    };
    consume_whitespace(tokens, index);
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
    let mut first_initializer_list = None;
    loop {
        if *index >= tokens.len() {
            break;
        }
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
        let designation_initializer = InitializerList {
            designation,
            initializer: flattened.initializers.len() - 1,
            next: None,
        };
        if first_initializer_list.is_none() {
            // i dont push to flattened.initializer_lists here bc once this function returns first_initializer_list, it'll be pushed to flattened by the caller.
            first_initializer_list = Some(designation_initializer);
        } else {
            flattened.initializer_lists.push(designation_initializer);
            if first_initializer_list.unwrap().next.is_none() {
                let Some(il) = &mut first_initializer_list else {
                    unreachable!()
                };
                il.next = Some(flattened.initializer_lists.len() - 1);
            } else {
                let mut il = flattened.initializer_lists[flattened.initializer_lists.len() - 2];
                il.next = Some(flattened.initializer_lists.len() - 1);
            }
        }
        if !matches!(
            tokens.get(*index),
            Some(Token {
                r#type: TokenType::COMMA,
                ..
            })
        ) {
            break;
        }
        *index += 1;
    }
    consume_token(tokens, index, TokenType::COMMA);
    expected_token(tokens, index, TokenType::CLOSE_CURLY, "Expected '}'")?;
    Ok(first_initializer_list.unwrap())
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

pub fn parse_declarator(
    tokens: &[Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<Declarator, String> {
    let mut declarator = Declarator {
        r#type: DeclaratorType::Abstract,
        declarator: None,
        type_qualifier_flags: 0,
        is_static: false,
        pointer: Vec::new(),
        parameter_type_list: None,
        assign_expr: None,
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
                identifier: *str_map_key,
            };
        }
        Some(Token {
            r#type: TokenType::OPEN_PAR,
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            let inner_declarator = parse_declarator(&tokens, index, flattened, str_maps)?;
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
        None => {}
    }
    match tokens.get(*index) {
        Some(Token {
            r#type: TokenType::OPEN_SQR,
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            match tokens.get(*index) {
                Some(Token {
                    r#type: TokenType::STATIC,
                    ..
                }) => {
                    *index += 1;
                    declarator.is_static = true;
                    declarator.type_qualifier_flags = parse_type_qualifiers(tokens, index);
                    let expr = parse_expressions(&tokens, index, flattened, str_maps)?;
                    flattened.expressions.push(expr);
                    declarator.assign_expr = Some(flattened.expressions.len() - 1);
                    expected_token(tokens, index, TokenType::CLOSE_SQR, "Expected ']'")?;
                }
                Some(Token {
                    r#type: TokenType::CLOSE_SQR,
                    ..
                }) => {
                    *index += 1;
                }
                _ => {
                    declarator.type_qualifier_flags = parse_type_qualifiers(tokens, index);
                    consume_whitespace(tokens, index);
                    if let Some(Token {
                        r#type: TokenType::STATIC,
                        location,
                    }) = tokens.get(*index)
                    {
                        let location = location.unwrap();
                        *index += 1;
                        if declarator.type_qualifier_flags == 0 {
                            return Err(error(
                                "Expected type qualifiers",
                                location.line,
                                location.column,
                            ));
                        }
                        declarator.is_static = true;
                    }
                    let expr = parse_expressions(&tokens, index, flattened, str_maps)?;
                    flattened.expressions.push(expr);
                    declarator.assign_expr = Some(flattened.expressions.len() - 1);
                    expected_token(tokens, index, TokenType::CLOSE_SQR, "Expected ']'")?;
                }
            }
        }
        Some(Token {
            r#type: TokenType::OPEN_PAR,
            ..
        }) => {
            *index += 1;
            let ptl = parse_parameter_type_list(&tokens, index, flattened, str_maps)?;
            expected_token(tokens, index, TokenType::CLOSE_PAR, "Expected ')'")?;
            declarator.parameter_type_list = Some(ptl);
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
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<EnumSpecifier, String> {
    let mut enum_specifier = EnumSpecifier {
        identifier: None,
        enumerator_list: Vec::new(),
    };
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
    while *index < tokens.len() {
        match tokens[*index] {
            Token {
                r#type: TokenType::WHITESPACE | TokenType::NEWLINE,
                ..
            } => {}
            Token {
                r#type: TokenType::CLOSE_CURLY,
                ..
            } => break,
            Token {
                r#type: TokenType::IDENT { str_map_key, .. },
                ..
            } => {
                if matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: TokenType::ASSIGNMENT,
                        ..
                    })
                ) {
                    // TODO: probably call parse_expressions here instead of evaluating
                    //let expr = parse_conditional_expression(tokens, index, str_maps, ...)?;
                    enum_specifier
                        .enumerator_list
                        .push(Enumerator::EnumWithConstantExpr(str_map_key, 0));
                } else {
                    enum_specifier
                        .enumerator_list
                        .push(Enumerator::Enum(str_map_key));
                }
                if !matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: TokenType::CLOSE_CURLY,
                        ..
                    })
                ) {
                    expected_token(tokens, index, TokenType::COMMA, "Expected ','")?;
                } else if !matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: TokenType::COMMA,
                        ..
                    }),
                ) {
                    expected_token(tokens, index, TokenType::CLOSE_CURLY, "Expected '}'")?;
                    break;
                }
            }
            _ => return Err(format!("Unexpected token: {:?}", tokens[*index])),
        }
        *index += 1;
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
                if let Some(Token {
                    r#type: OPEN_PAR, ..
                }) = tokens.get(*index + 1)
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
                specifier_qualifier.type_qualifiers_flags = parse_type_qualifiers(tokens, index);
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
    let mut type_qualifiers_flags = 0;
    while *index < tokens.len() {
        match tokens[*index] {
            Token {
                r#type: TokenType::CONST,
                ..
            } => {
                type_qualifiers_flags |= TypeQualifier::Const as u8;
            }
            Token {
                r#type: TokenType::RESTRICT,
                ..
            } => {
                type_qualifiers_flags |= TypeQualifier::Restrict as u8;
            }
            Token {
                r#type: TokenType::VOLATILE,
                ..
            } => {
                type_qualifiers_flags |= TypeQualifier::Volatile as u8;
            }
            Token {
                r#type: TokenType::_ATOMIC,
                ..
            } => {
                type_qualifiers_flags |= TypeQualifier::_Atomic as u8;
            }
            _ => break,
        }
        *index += 1;
        consume_whitespace(tokens, index);
    }
    type_qualifiers_flags
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
                let enum_specifier = parse_enumerator_specifier(tokens, index, str_maps)?;
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
            } => declaration_specifier
                .storage_class_specifiers
                .push(StorageClassSpecifier::TypeDef),
            Token {
                r#type: TokenType::EXTERN,
                ..
            } => declaration_specifier
                .storage_class_specifiers
                .push(StorageClassSpecifier::Extern),
            Token {
                r#type: TokenType::STATIC,
                ..
            } => declaration_specifier
                .storage_class_specifiers
                .push(StorageClassSpecifier::Static),
            Token {
                r#type: TokenType::_THREAD_LOCAL,
                ..
            } => declaration_specifier
                .storage_class_specifiers
                .push(StorageClassSpecifier::ThreadLocal),
            Token {
                r#type: TokenType::AUTO,
                ..
            } => declaration_specifier
                .storage_class_specifiers
                .push(StorageClassSpecifier::Auto),
            Token {
                r#type: TokenType::REGISTER,
                ..
            } => declaration_specifier
                .storage_class_specifiers
                .push(StorageClassSpecifier::Register),
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
            } => declaration_specifier.type_qualifiers_flags |= TypeQualifier::Const as u8,
            Token {
                r#type: TokenType::RESTRICT,
                ..
            } => declaration_specifier.type_qualifiers_flags |= TypeQualifier::Restrict as u8,
            Token {
                r#type: TokenType::VOLATILE,
                ..
            } => declaration_specifier.type_qualifiers_flags |= TypeQualifier::Volatile as u8,
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
                    declaration_specifier.type_qualifiers_flags |= TypeQualifier::_Atomic as u8
                }
            }
            Token {
                r#type: TokenType::INLINE,
                ..
            } => declaration_specifier
                .function_specifiers
                .push(FunctionSpecifier::Inline),
            Token {
                r#type: TokenType::_NORETURN,
                ..
            } => declaration_specifier
                .function_specifiers
                .push(FunctionSpecifier::_Noreturn),
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
        parse_parameter_type_list, parse_struct_declarator, parse_struct_union_specifier,
        parse_type_names, Declaration, Declarator, Designation, Designator, Enumerator,
        InitDeclarator, Initializer, InitializerList, TypeQualifier, TypeSpecifier,
    };
    use crate::lexer::*;
    use crate::parser::expressions::*;
    use crate::parser::*;
    #[test]
    fn parse_enumerator_specifier_test() -> Result<(), String> {
        {
            let src = r#"
        enum HI {
            YOUR_MOM,
            HEHE,
        }
"#
            .as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(src, false, &mut str_maps)?;
            let mut start_index = {
                let mut index = 0;
                while !matches!(
                    tokens.get(index),
                    Some(Token {
                        r#type: TokenType::ENUM,
                        ..
                    })
                ) {
                    index += 1;
                }
                index + 1
            };
            let enum_specifier =
                parse_enumerator_specifier(tokens.as_slice(), &mut start_index, &mut str_maps)?;
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
        {
            let src = r#"
        enum HI {
            YOUR_MOM,
            HEHE,
            THIS_PIGGY = 4,
        }
"#
            .as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let tokens = lexer(src, false, &mut str_maps)?;
            let mut start_index = {
                let mut index = 0;
                while !matches!(
                    tokens.get(index),
                    Some(Token {
                        r#type: TokenType::ENUM,
                        ..
                    })
                ) {
                    index += 1;
                }
                index + 1
            };
            let enum_specifier =
                parse_enumerator_specifier(tokens.as_slice(), &mut start_index, &mut str_maps)?;
            assert_eq!(
                enum_specifier.identifier,
                Some(str_maps.add_byte_vec("HI".as_bytes()))
            );
            assert_eq!(
                enum_specifier.enumerator_list,
                vec![
                    Enumerator::Enum(str_maps.add_byte_vec("YOUR_MOM".as_bytes())),
                    Enumerator::Enum(str_maps.add_byte_vec("HEHE".as_bytes())),
                    Enumerator::EnumWithConstantExpr(
                        str_maps.add_byte_vec("THIS_PIGGY".as_bytes()),
                        4
                    ),
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
            let il = flattened.initializer_lists[ili].clone();
            let il1 = flattened.initializer_lists[il.next.unwrap()];
            let il2 = flattened.initializer_lists[il1.next.unwrap()];
            let Some(des1_idx) = il1.designation else {
                unreachable!()
            };
            let des1 = flattened.designations[des1_idx].clone();
            assert_eq!(
                Designation {
                    designator_list: vec![Designator::WithIdentifier(
                        str_maps.add_byte_vec("hi".as_bytes()),
                    )],
                },
                des1
            );
            let ini1_idx = il1.initializer else {
                unreachable!()
            };
            let ini1 = flattened.initializers[ini1_idx].clone();
            let Initializer::AssignmentExpression(key) = ini1 else {
                unreachable!()
            };
            let expr = flattened.expressions[key];
            assert!(matches!(expr, Expr::Primary(PrimaryType::Token(_))));

            let Some(des2_idx) = il2.designation else {
                unreachable!()
            };
            let des2 = flattened.designations[des2_idx].clone();
            assert_eq!(
                Designation {
                    designator_list: vec![Designator::WithIdentifier(
                        str_maps.add_byte_vec("hi2".as_bytes()),
                    )],
                },
                des2
            );
            let ini2_idx = il2.initializer else {
                unreachable!()
            };
            let ini2 = flattened.initializers[ini2_idx].clone();
            let Initializer::AssignmentExpression(key) = ini2 else {
                unreachable!()
            };
            let expr = flattened.expressions[key];
            assert!(matches!(expr, Expr::Primary(PrimaryType::Token(_))));
        }
        {
            let src = r#"{}"#.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer(src, false, &mut str_maps)?;
            assert!(parse_initializer(&tokens, &mut 0, &mut flattened, &mut str_maps).is_err());
        }
        {
            let src = r#"{ {[100] = 5}, 8, .baz = "" }"#.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer(src, false, &mut str_maps)?;
            let i = parse_initializer(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
        }
        {
            let src = r#"{ 8, 8, .baz = "" }"#.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer(src, false, &mut str_maps)?;
            let i = parse_initializer(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
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
        assert!(type_name.abstract_declarator.is_some());
        let Some(abd) = type_name.abstract_declarator else {
            unreachable!()
        };
        assert!(abd.pointer.is_empty());
        assert!(abd.direct_abstract_declarator.is_some());
        let Some(dad) = abd.direct_abstract_declarator else {
            unreachable!()
        };
        assert!(dad.abstract_declarator.is_some());
        let Some(abd) = &dad.abstract_declarator else {
            unreachable!()
        };
        assert!(!abd.pointer.is_empty());
        assert!(matches!(
            dad,
            DirectAbstractDeclarator {
                is_static: false,
                mult: false,
                parameter_type_list: None,
                ..
            }
        ));
        let Some(expr_idx) = dad.assign_expr else {
            unreachable!()
        };
        assert!(matches!(flattened.expressions[expr_idx], Expr::Primary(_)));
        let Expr::Primary(PrimaryType::Token(t)) = flattened.expressions[expr_idx] else {
            unreachable!()
        };
        let TokenType::CONSTANT_DEC_INT { value_key, .. } = t.r#type else {
            unreachable!()
        };
        assert_eq!(value_key, str_maps.add_byte_vec(b"3"));
        Ok(())
    }
    #[test]
    fn parse_type_names_test_pointer_to_variable_length_int_array() -> Result<(), String> {
        let src = r#"int (*)[*]"#.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let mut flattened = Flattened::new();
        let tokens = lexer(src, false, &mut str_maps)?;
        let type_name = parse_type_names(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
        assert!(matches!(
            type_name.specifier_qualifier_list.type_specifiers.first(),
            Some(TypeSpecifier::Int)
        ));
        assert!(type_name.abstract_declarator.is_some());
        let Some(abd) = type_name.abstract_declarator else {
            unreachable!()
        };
        assert!(abd.pointer.is_empty());
        assert!(abd.direct_abstract_declarator.is_some());
        let Some(dad) = abd.direct_abstract_declarator else {
            unreachable!()
        };
        assert!(dad.abstract_declarator.is_some());
        let Some(abd) = &dad.abstract_declarator else {
            unreachable!()
        };
        assert!(!abd.pointer.is_empty());
        assert!(matches!(
            dad,
            DirectAbstractDeclarator {
                is_static: false,
                mult: true,
                parameter_type_list: None,
                ..
            }
        ));
        Ok(())
    }
    #[test]
    fn parse_declarators_test_simple() -> Result<(), String> {
        {
            let src = r#"* hi"#.as_bytes();
            let mut str_maps = ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer(src, false, &mut str_maps)?;
            let dec = parse_declarator(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
            assert!(matches!(dec, Declarator { .. }));
            let Declarator {
                pointer,
                direct_declarator,
            } = dec;
            assert!(direct_declarator.is_some());
            let Some(direct_declarator) = direct_declarator else {
                unreachable!()
            };
            let Some(key) = direct_declarator.identifier else {
                unreachable!()
            };
            assert_eq!(key, str_maps.add_byte_vec("hi".as_bytes()));
            assert!(!pointer.is_empty());
        }
        Ok(())
    }
    #[test]
    fn parse_parameter_type_list_test() -> Result<(), String> {
        let src = r#"int hi, int hi2, int hi3;"#.as_bytes();
        let mut str_maps = ByteVecMaps::new();
        let mut flattened = Flattened::new();
        let tokens = lexer(src, false, &mut str_maps)?;
        let ptl = parse_parameter_type_list(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
        assert!(ptl.parameter_declarations.len() == 3);
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
                struct_declarator.declarator,
                Some(Declarator {
                    direct_declarator: Some(DirectDeclarator { .. }),
                    ..
                })
            ));
            assert_eq!(
                struct_declarator
                    .declarator
                    .clone()
                    .unwrap()
                    .direct_declarator
                    .unwrap()
                    .identifier
                    .unwrap(),
                str_maps.add_byte_vec("hi".as_bytes())
            );
            assert!(struct_declarator.const_expr.is_some());
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
            assert!(
                struct_union_specifier.struct_declaration_list[0].struct_declarator_list[0]
                    .const_expr
                    .is_some()
            );
        }
        {
            let src = r#"struct {
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
                struct_union_specifier.struct_declaration_list[0]
                    .specifier_qualifier_list
                    .type_specifiers
                    .len()
                    == 1
            );
            assert!(
                struct_union_specifier.struct_declaration_list[0].struct_declarator_list[0]
                    .declarator
                    .clone()
                    .unwrap()
                    .direct_declarator
                    .unwrap()
                    .identifier
                    .is_some()
            );
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
            assert!(
                struct_union_specifier.struct_declaration_list[0].struct_declarator_list[0]
                    .declarator
                    .clone()
                    .unwrap()
                    .direct_declarator
                    .unwrap()
                    .identifier
                    .is_some()
            );
        }
        Ok(())
    }
    #[test]
    fn parse_declarations_test() -> Result<(), String> {
        //{
        //    let src = r#"int hi = 4;"#;
        //    let mut str_maps = ByteVecMaps::new();
        //    let mut flattened = Flattened::new();
        //    let tokens = lexer(src.as_bytes(), false, &mut str_maps)?;
        //    let declaration = parse_declarations(&tokens, &mut 0, &mut flattened, &mut str_maps)?;
        //    let Declaration {
        //        declaration_specifiers,
        //        init_declarator_list,
        //    } = declaration;
        //    assert!(
        //        matches!(
        //            declaration_specifiers.type_specifiers.get(0),
        //            Some(TypeSpecifier::Int)
        //        ),
        //        "int was not added to the type specifier list"
        //    );
        //    assert!(
        //        matches!(
        //            init_declarator_list.get(0),
        //            Some(InitDeclarator::DeclaratorWithInitializer(_))
        //        ),
        //        "declarator and initializer not added to list",
        //    );
        //    let Some(InitDeclarator::DeclaratorWithInitializer(di)) = init_declarator_list.get(0)
        //    else {
        //        unreachable!()
        //    };
        //    assert!(matches!(
        //        di.initializer,
        //        Initializer::AssignmentExpression(_)
        //    ));
        //}
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
            assert!(d.pointer.is_empty());
            assert!(d.direct_declarator.is_some());
            let Some(dd) = &d.direct_declarator else {
                unreachable!()
            };
            assert!(dd.declarator.is_some());
            assert!(dd.parameter_type_list.is_some());
            let Some(ddd) = &dd.declarator else {
                unreachable!()
            };
            assert!(!ddd.pointer.is_empty());
            assert!(ddd.direct_declarator.is_some());
            let Some(ddddd) = &ddd.direct_declarator else {
                unreachable!()
            };
            assert!(ddddd.parameter_type_list.is_some());
            assert!(ddddd.declarator.is_some());
            let Some(dddddd) = &ddddd.declarator else {
                unreachable!()
            };
            assert!(!dddddd.pointer.is_empty());
            assert!(dddddd.direct_declarator.is_some());
            let Some(ddddddd) = &dddddd.direct_declarator else {
                unreachable!()
            };
            assert!(ddddddd.identifier.is_some());
            let Some(key) = ddddddd.identifier else {
                unreachable!()
            };
            assert_eq!(str_maps.key_to_byte_vec[key], b"f");
        }
        Ok(())
    }
}
