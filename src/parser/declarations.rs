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
pub enum ParameterDeclaration {
    WithOptionalAbstractDeclarator {
        declaration_specifier: DeclarationSpecifier,
        abstract_declarator: Option<AbstractDeclarator>,
    },
    WithDeclarator {
        declaration_specifier: DeclarationSpecifier,
        declarator: Declarator,
    },
}
#[derive(Clone)]
pub struct ParameterTypeList {
    pub parameter_declarations: Vec<ParameterDeclaration>,
    pub ellipsis: bool,
}
#[derive(Clone)]
pub struct DirectAbstractDeclarator {
    abstract_declarator: Option<Box<AbstractDeclarator>>,
    type_qualifiers: Vec<TypeQualifier>,
    is_static: bool,
    mult: bool,
    parameter_type_list: Option<ParameterTypeList>,
    assign_expr: Option<ExpressionIndex>,
}
#[derive(Clone)]
pub struct AbstractDeclarator {
    pointer: Vec<Pointer>,
    direct_abstract_declarator: Option<DirectAbstractDeclarator>,
}
#[derive(Clone)]
pub struct TypeName {
    pub specifier_qualifier_list: SpecifierQualifierList,
    pub abstract_declarator: Option<AbstractDeclarator>,
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
    type_qualifier_list: Vec<TypeQualifier>,
}
impl Pointer {
    fn new() -> Self {
        Self {
            type_qualifier_list: Vec::new(),
        }
    }
}
#[derive(Clone)]
pub struct DirectDeclarator {
    pub identifier: Option<usize>,
    pub declarator: Option<Box<Declarator>>,
    pub type_qualifier_list: Vec<TypeQualifier>,
    pub is_static: bool,
    pub mult: bool,
    pub parameter_type_list: Option<ParameterTypeList>,
    pub assign_expr: Option<ExpressionIndex>,
}
#[derive(Clone)]
pub struct Declarator {
    pub pointer: Vec<Pointer>,
    pub direct_declarator: Option<DirectDeclarator>,
}
#[derive(Clone)]
pub struct StructDeclarator {
    declarator: Option<Declarator>,
    const_expr: Option<ExpressionIndex>,
}
#[derive(Clone)]
pub struct SpecifierQualifierList {
    pub type_qualifiers: Vec<TypeQualifier>,
    pub type_specifiers: Vec<TypeSpecifier>,
    pub alignment_specifiers: Vec<AlignmentSpecifier>,
}
impl SpecifierQualifierList {
    fn new() -> Self {
        Self {
            type_qualifiers: Vec::new(),
            type_specifiers: Vec::new(),
            alignment_specifiers: Vec::new(),
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
    Const,
    Restrict,
    Volatile,
    _Atomic,
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
#[derive(Clone)]
pub enum AlignmentSpecifier {
    _Alignas(TypeNameIndex),
    _AlignasConstExpr(ExpressionIndex),
}
#[derive(Clone)]
pub struct DeclarationSpecifier {
    pub storage_class_specifiers: Vec<StorageClassSpecifier>,
    pub type_specifiers: Vec<TypeSpecifier>,
    pub type_qualifiers: Vec<TypeQualifier>,
    pub function_specifiers: Vec<FunctionSpecifier>,
    pub alignment_specifiers: Vec<AlignmentSpecifier>,
}
impl DeclarationSpecifier {
    fn new() -> Self {
        Self {
            storage_class_specifiers: Vec::new(),
            type_specifiers: Vec::new(),
            type_qualifiers: Vec::new(),
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
    initializer: Option<InitializerIndex>,
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
    pub declaration_specifiers: DeclarationSpecifier,
    pub init_declarator_list: Vec<InitDeclarator>,
}
impl Declaration {
    fn new() -> Declaration {
        Declaration {
            declaration_specifiers: DeclarationSpecifier::new(),
            init_declarator_list: Vec::new(),
        }
    }
}
pub fn is_declaration_token(t: Token) -> bool {
    match t {
        Token {
            r#type: TokenType::KEYWORD_TYPEDEF,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD_EXTERN,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD_STATIC,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD__THREAD_LOCAL,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD_AUTO,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD_REGISTER,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD_VOID,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD_CHAR,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD_SHORT,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD_INT,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD_LONG,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD_FLOAT,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD_DOUBLE,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD_SIGNED,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD_UNSIGNED,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD__BOOL,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD__COMPLEX,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD_STRUCT | TokenType::KEYWORD_UNION,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD_ENUM,
            ..
        } => todo!(),
        Token {
            r#type: TokenType::KEYWORD_CONST,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD_RESTRICT,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD_VOLATILE,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD__ATOMIC,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD_INLINE,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD__NORETURN,
            ..
        } => true,
        Token {
            r#type: TokenType::KEYWORD__ALIGNAS,
            ..
        } => true,
        _ => false,
    }
}
pub fn parse_declarations(
    tokens: &[lexer::Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<Declaration, String> {
    let mut declaration = Declaration::new();
    let declaration_specifier = parse_declaration_specifiers(tokens, index, flattened, str_maps)?;
    declaration.declaration_specifiers = declaration_specifier;
    if !matches!(
        tokens.get(*index),
        Some(Token {
            r#type: TokenType::PUNCT_SEMI_COLON,
            ..
        })
    ) && tokens.get(*index).is_some()
    {
        loop {
            let declarator = parse_declarator(tokens, index, flattened, str_maps)?;
            if !matches!(
                tokens.get(*index),
                Some(Token {
                    r#type: TokenType::PUNCT_ASSIGNMENT,
                    ..
                })
            ) {
                declaration
                    .init_declarator_list
                    .push(InitDeclarator::Declarator(declarator));
            } else {
                loop {
                    *index += 1;
                    if matches!(
                        tokens.get(*index),
                        Some(Token {
                            r#type: TokenType::PUNCT_COMMA | TokenType::PUNCT_SEMI_COLON,
                            ..
                        }) | None
                    ) {
                        break;
                    }
                }
                if !matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: TokenType::PUNCT_COMMA,
                        ..
                    })
                ) {
                    if !matches!(
                        tokens.get(*index),
                        Some(Token {
                            r#type: TokenType::PUNCT_SEMI_COLON,
                            ..
                        })
                    ) {
                        todo!("ERROR HERE")
                    }
                }
                let initializer = parse_initializer(&tokens, index, flattened, str_maps)?;
                declaration
                    .init_declarator_list
                    .push(InitDeclarator::DeclaratorWithInitializer(
                        DeclaratorWithInitializer {
                            declarator,
                            initializer,
                        },
                    ));
            }
            if matches!(
                tokens.get(*index),
                Some(Token {
                    r#type: TokenType::PUNCT_SEMI_COLON,
                    ..
                })
            ) {
                *index += 1;
                break;
            }
            *index += 1;
        }
    }
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
            r#type: TokenType::PUNCT_OPEN_CURLY,
            ..
        }) => {
            *index += 1;
            let il = parse_initializer_list(&tokens, index, flattened, str_maps)?;
            if il.is_empty() {
                return Err(format!("initializer list is empty"));
            }
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

fn parse_initializer_list(
    tokens: &[lexer::Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<Vec<InitializerList>, String> {
    consume_whitespace(tokens, index);
    let mut initializer_lists = Vec::new();
    loop {
        if *index >= tokens.len() {
            break;
        }
        let mut designation = Designation {
            designator_list: Vec::new(),
        };
        loop {
            match tokens.get(*index) {
                Some(Token {
                    r#type: TokenType::PUNCT_OPEN_SQR,
                    ..
                }) => {
                    *index += 1;
                    let expr = parse_expressions(&tokens, index, flattened, str_maps)?;
                    expected_token(tokens, str_maps, index, TokenType::PUNCT_CLOSE_SQR)?;
                    flattened.expressions.push(expr);
                    designation
                        .designator_list
                        .push(Designator::WithConstantExpr(
                            flattened.expressions.len() - 1,
                        ));
                }
                Some(Token {
                    r#type: TokenType::PUNCT_DOT,
                    ..
                }) => {
                    *index += 1;
                    consume_whitespace(tokens, index);
                    expected_identifier(tokens, str_maps, index)?;
                    let Some(Token {
                        r#type: TokenType::IDENT { str_map_key, .. },
                        ..
                    }) = tokens.get(*index)
                    else {
                        unreachable!()
                    };
                    designation
                        .designator_list
                        .push(Designator::WithIdentifier(*str_map_key));
                }
                Some(Token {
                    r#type: TokenType::WHITESPACE | TokenType::NEWLINE,
                    ..
                }) => *index += 1,
                Some(Token {
                    r#type: TokenType::PUNCT_ASSIGNMENT,
                    location: Some(Location { column, line }),
                }) => {
                    if designation.designator_list.is_empty() {
                        return Err(error_msg("Unexpected =, expected . or [", *line, *column));
                    }
                    *index += 1;
                    break;
                }
                _ => {
                    break;
                }
            }
        }
        consume_whitespace(tokens, index);
        expected_token(tokens, str_maps, index, TokenType::PUNCT_COMMA)?;
        let init = parse_initializer(&tokens, index, flattened, str_maps)?;
        flattened.initializers.push(init);
        flattened.designations.push(designation);
        let initializer_list = InitializerList {
            designation: Some(flattened.designations.len() - 1),
            initializer: Some(flattened.initializers.len() - 1),
        };
        initializer_lists.push(initializer_list);
        *index += 1;
    }
    Ok(initializer_lists)
}

fn parse_pointer(tokens: &[lexer::Token], index: &mut usize) -> Option<Vec<Pointer>> {
    let mut pointer_stack = Vec::new();
    while matches!(
        tokens.get(*index),
        Some(Token {
            r#type: TokenType::PUNCT_MULT,
            ..
        })
    ) {
        pointer_stack.push(Pointer::new());
        *index += 1;
        let parsed_type_qualified = parse_type_qualifiers(tokens, index);
        if let Some(qualifiers) = parsed_type_qualified {
            let Some(pointer) = pointer_stack.last_mut() else {
                unreachable!()
            };
            pointer
                .type_qualifier_list
                .extend_from_slice(qualifiers.as_slice());
        }
        consume_whitespace(tokens, index);
    }
    if !pointer_stack.is_empty() {
        Some(pointer_stack)
    } else {
        None
    }
}

fn parse_direct_declarator(
    tokens: &[lexer::Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<DirectDeclarator, String> {
    let mut direct_declarator = DirectDeclarator {
        identifier: None,
        declarator: None,
        type_qualifier_list: Vec::new(),
        is_static: false,
        mult: false,
        assign_expr: None,
        parameter_type_list: None,
    };
    match tokens.get(*index) {
        Some(Token {
            r#type: TokenType::IDENT { str_map_key, .. },
            ..
        }) => {
            *index += 1;
            direct_declarator.identifier = Some(*str_map_key);
            consume_whitespace(tokens, index);
        }
        Some(Token {
            r#type: TokenType::PUNCT_OPEN_PAR,
            ..
        }) => {
            *index += 1;
            let inner_declarator = parse_declarator(&tokens, index, flattened, str_maps)?;
            expected_token(tokens, str_maps, index, TokenType::PUNCT_CLOSE_PAR)?;
            direct_declarator.declarator = Some(Box::new(inner_declarator));
        }
        _ => {
            return Err(format!(
                "Expected identifier or open parentheses, got {:?}",
                tokens.get(*index)
            ));
        }
    }
    match tokens.get(*index) {
        Some(Token {
            r#type: TokenType::PUNCT_OPEN_SQR,
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            match tokens.get(*index) {
                Some(Token {
                    r#type: TokenType::PUNCT_MULT,
                    ..
                }) => {
                    *index += 1;
                    direct_declarator.mult = true;
                }
                Some(Token {
                    r#type: TokenType::KEYWORD_STATIC,
                    ..
                }) => {
                    *index += 1;
                    direct_declarator.is_static = true;
                    if let Some(type_qualifiers) = parse_type_qualifiers(tokens, index) {
                        direct_declarator.type_qualifier_list = type_qualifiers;
                    }
                    let expr = parse_expressions(&tokens, index, flattened, str_maps)?;
                    flattened.expressions.push(expr);
                    direct_declarator.assign_expr = Some(flattened.expressions.len() - 1);
                    expected_token(tokens, str_maps, index, TokenType::PUNCT_CLOSE_SQR)?;
                }
                _ => {
                    if let Some(type_qualifiers) = parse_type_qualifiers(tokens, index) {
                        direct_declarator.type_qualifier_list = type_qualifiers;
                    }
                    consume_whitespace(tokens, index);
                    if matches!(
                        tokens.get(*index),
                        Some(Token {
                            r#type: TokenType::KEYWORD_STATIC,
                            ..
                        })
                    ) {
                        *index += 1;
                        if direct_declarator.type_qualifier_list.is_empty() {
                            return Err(format!(
                                "Expected type qualifiers, got {:?}",
                                tokens.get(*index)
                            ));
                        }
                        direct_declarator.is_static = true;
                    }
                    let expr = parse_expressions(&tokens, index, flattened, str_maps)?;
                    flattened.expressions.push(expr);
                    direct_declarator.assign_expr = Some(flattened.expressions.len() - 1);
                    expected_token(tokens, str_maps, index, TokenType::PUNCT_CLOSE_SQR)?;
                }
            }
        }
        Some(Token {
            r#type: TokenType::PUNCT_OPEN_PAR,
            ..
        }) => {
            *index += 1;
            let ptl = parse_parameter_type_list(&tokens, index, flattened, str_maps)?;
            expected_token(tokens, str_maps, index, TokenType::PUNCT_CLOSE_PAR)?;
            direct_declarator.parameter_type_list = Some(ptl);
        }
        _ => {}
    }
    Ok(direct_declarator)
}

pub fn parse_declarator(
    tokens: &[lexer::Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<Declarator, String> {
    let mut declarator = Declarator {
        pointer: Vec::new(),
        direct_declarator: None,
    };
    if let Some(pointers) = parse_pointer(tokens, index) {
        declarator.pointer = pointers;
    }
    consume_whitespace(tokens, index);
    let direct_declarator = parse_direct_declarator(tokens, index, flattened, str_maps)?;
    declarator.direct_declarator = Some(direct_declarator);
    Ok(declarator)
}

fn parse_struct_declarator(
    tokens: &[lexer::Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<StructDeclarator, String> {
    let mut struct_declarator = StructDeclarator {
        declarator: None,
        const_expr: None,
    };
    consume_whitespace(tokens, index);
    if matches!(
        tokens.get(*index),
        Some(Token {
            r#type: TokenType::PUNCT_COLON,
            ..
        })
    ) {
        *index += 1;
        let const_expr = parse_expressions(&tokens, index, flattened, str_maps)?;
        flattened.expressions.push(const_expr);
        struct_declarator.const_expr = Some(flattened.expressions.len() - 1);
    } else {
        let declarator = parse_declarator(tokens, index, flattened, str_maps)?;
        struct_declarator.declarator = Some(declarator);
        consume_whitespace(tokens, index);
        if matches!(
            tokens.get(*index),
            Some(Token {
                r#type: TokenType::PUNCT_COLON,
                ..
            })
        ) {
            *index += 1;
            let const_expr = parse_expressions(&tokens, index, flattened, str_maps)?;
            flattened.expressions.push(const_expr);
            struct_declarator.const_expr = Some(flattened.expressions.len() - 1);
        }
    }
    Ok(struct_declarator)
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
    expected_token(tokens, str_maps, index, TokenType::PUNCT_SEMI_COLON)?;
    loop {
        let struct_declarator = parse_struct_declarator(&tokens, index, flattened, str_maps)?;
        struct_declaration
            .struct_declarator_list
            .push(struct_declarator);
        if matches!(
            tokens.get(*index),
            Some(Token {
                r#type: TokenType::PUNCT_SEMI_COLON,
                ..
            })
        ) {
            *index += 1;
            return Ok(struct_declaration);
        }
        expected_token(tokens, str_maps, index, TokenType::PUNCT_COMMA)?;
    }
    expected_token(tokens, str_maps, index, TokenType::PUNCT_SEMI_COLON)?;
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
                r#type: TokenType::KEYWORD_STRUCT,
                ..
            }) => StructOrUnion::Struct,
            Some(Token {
                r#type: TokenType::KEYWORD_UNION,
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
    expected_token(tokens, str_maps, index, TokenType::PUNCT_OPEN_CURLY)?;
    while *index < tokens.len() {
        let struct_declaration = parse_struct_declaration(tokens, index, flattened, str_maps)?;
        struct_union_specifier
            .struct_declaration_list
            .push(struct_declaration);
        consume_whitespace(tokens, index);
        if matches!(
            tokens.get(*index),
            Some(Token {
                r#type: TokenType::PUNCT_CLOSE_CURLY,
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
    return Err(error_msg(
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
    expected_token(tokens, str_maps, index, TokenType::PUNCT_OPEN_CURLY)?;
    while *index < tokens.len() {
        match tokens[*index] {
            Token {
                r#type: TokenType::WHITESPACE | TokenType::NEWLINE,
                ..
            } => {}
            Token {
                r#type: TokenType::PUNCT_CLOSE_CURLY,
                ..
            } => break,
            Token {
                r#type: TokenType::IDENT { str_map_key, .. },
                ..
            } => {
                if matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: TokenType::PUNCT_ASSIGNMENT,
                        ..
                    })
                ) {
                    // TODO: probably call parse_expressions here instead of evaluating
                    let constant_val =
                        eval_constant_expression_integer_when_preprocess(tokens, index, str_maps)?;
                    enum_specifier
                        .enumerator_list
                        .push(Enumerator::EnumWithConstantExpr(str_map_key, constant_val));
                } else {
                    enum_specifier
                        .enumerator_list
                        .push(Enumerator::Enum(str_map_key));
                }
                if !matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: TokenType::PUNCT_CLOSE_CURLY,
                        ..
                    })
                ) {
                    expected_token(tokens, str_maps, index, TokenType::PUNCT_COMMA)?;
                } else if !matches!(
                    tokens.get(*index),
                    Some(Token {
                        r#type: TokenType::PUNCT_COMMA,
                        ..
                    }),
                ) {
                    expected_token(tokens, str_maps, index, TokenType::PUNCT_CLOSE_CURLY)?;
                    break;
                }
            }
            _ => return Err(format!("Unexpected token: {:?}", tokens[*index])),
        }
        *index += 1;
    }
    Ok(enum_specifier)
}

pub fn parse_specifiers_qualifiers(
    tokens: &[lexer::Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<SpecifierQualifierList, String> {
    let mut specifier_qualifier = SpecifierQualifierList::new();
    loop {
        if let Ok(mut specifiers) = parse_type_specifiers(tokens, index, flattened, str_maps) {
            // Avoids cloning
            while let Some(type_specifier) = specifiers.pop() {
                specifier_qualifier.type_specifiers.push(type_specifier);
            }
        }
        if let Some(mut qualifiers) = parse_type_qualifiers(tokens, index) {
            while let Some(type_qualifier) = qualifiers.pop() {
                specifier_qualifier.type_qualifiers.push(type_qualifier);
            }
        } else {
            break;
        }
    }
    Ok(specifier_qualifier)
}

pub fn parse_type_qualifiers(
    tokens: &[lexer::Token],
    index: &mut usize,
) -> Option<Vec<TypeQualifier>> {
    let mut type_qualifiers = Vec::new();
    while *index < tokens.len() {
        match tokens[*index] {
            Token {
                r#type: TokenType::WHITESPACE { .. } | TokenType::NEWLINE,
                ..
            } => {}
            Token {
                r#type: TokenType::KEYWORD_CONST,
                ..
            } => {
                type_qualifiers.push(TypeQualifier::Const);
            }
            Token {
                r#type: TokenType::KEYWORD_RESTRICT,
                ..
            } => {
                type_qualifiers.push(TypeQualifier::Restrict);
            }
            Token {
                r#type: TokenType::KEYWORD_VOLATILE,
                ..
            } => {
                type_qualifiers.push(TypeQualifier::Volatile);
            }
            Token {
                r#type: TokenType::KEYWORD__ATOMIC,
                ..
            } => {
                type_qualifiers.push(TypeQualifier::_Atomic);
            }
            _ => break,
        }
        *index += 1;
    }
    if type_qualifiers.is_empty() {
        None
    } else {
        Some(type_qualifiers)
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
                r#type: TokenType::KEYWORD_VOID,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::Void);
            }
            Token {
                r#type: TokenType::KEYWORD_CHAR,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::Char);
            }
            Token {
                r#type: TokenType::KEYWORD_SHORT,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::Short);
            }
            Token {
                r#type: TokenType::KEYWORD_INT,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::Int);
            }
            Token {
                r#type: TokenType::KEYWORD_LONG,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::Long);
            }
            Token {
                r#type: TokenType::KEYWORD_FLOAT,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::Float);
            }
            Token {
                r#type: TokenType::KEYWORD_DOUBLE,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::Double);
            }
            Token {
                r#type: TokenType::KEYWORD_SIGNED,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::Signed);
            }
            Token {
                r#type: TokenType::KEYWORD_UNSIGNED,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::Unsigned);
            }
            Token {
                r#type: TokenType::KEYWORD__BOOL,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::_Bool);
            }
            Token {
                r#type: TokenType::KEYWORD__COMPLEX,
                ..
            } => {
                type_specifiers.push(TypeSpecifier::_Complex);
            }
            Token {
                r#type: TokenType::KEYWORD_STRUCT { .. } | TokenType::KEYWORD_UNION,
                ..
            } => {
                let struct_union_specifier =
                    parse_struct_union_specifier(tokens, index, flattened, str_maps)?;
                type_specifiers.push(TypeSpecifier::StructUnion(struct_union_specifier));
                continue;
            }
            Token {
                r#type: TokenType::KEYWORD__ATOMIC,
                ..
            } => todo!(),
            Token {
                r#type: TokenType::KEYWORD_ENUM,
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
    tokens: &[lexer::Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<DeclarationSpecifier, String> {
    let mut declaration_specifier = DeclarationSpecifier::new();
    while *index < tokens.len() {
        match tokens[*index] {
            Token {
                r#type: TokenType::KEYWORD_TYPEDEF,
                ..
            } => declaration_specifier
                .storage_class_specifiers
                .push(StorageClassSpecifier::TypeDef),
            Token {
                r#type: TokenType::KEYWORD_EXTERN,
                ..
            } => declaration_specifier
                .storage_class_specifiers
                .push(StorageClassSpecifier::Extern),
            Token {
                r#type: TokenType::KEYWORD_STATIC,
                ..
            } => declaration_specifier
                .storage_class_specifiers
                .push(StorageClassSpecifier::Static),
            Token {
                r#type: TokenType::KEYWORD__THREAD_LOCAL,
                ..
            } => declaration_specifier
                .storage_class_specifiers
                .push(StorageClassSpecifier::ThreadLocal),
            Token {
                r#type: TokenType::KEYWORD_AUTO,
                ..
            } => declaration_specifier
                .storage_class_specifiers
                .push(StorageClassSpecifier::Auto),
            Token {
                r#type: TokenType::KEYWORD_REGISTER,
                ..
            } => declaration_specifier
                .storage_class_specifiers
                .push(StorageClassSpecifier::Register),
            Token {
                r#type: TokenType::KEYWORD_VOID,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Void),
            Token {
                r#type: TokenType::KEYWORD_CHAR,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Char),
            Token {
                r#type: TokenType::KEYWORD_SHORT,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Short),
            Token {
                r#type: TokenType::KEYWORD_INT,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Int),
            Token {
                r#type: TokenType::KEYWORD_LONG,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Long),
            Token {
                r#type: TokenType::KEYWORD_FLOAT,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Float),
            Token {
                r#type: TokenType::KEYWORD_DOUBLE,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Double),
            Token {
                r#type: TokenType::KEYWORD_SIGNED,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Signed),
            Token {
                r#type: TokenType::KEYWORD_UNSIGNED,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Unsigned),
            Token {
                r#type: TokenType::KEYWORD__BOOL,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::_Bool),
            Token {
                r#type: TokenType::KEYWORD__COMPLEX,
                ..
            } => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::_Complex),
            Token {
                r#type: TokenType::KEYWORD_STRUCT { .. } | TokenType::KEYWORD_UNION,
                ..
            } => todo!(),
            Token {
                r#type: TokenType::KEYWORD_ENUM,
                ..
            } => todo!(),
            Token {
                r#type: TokenType::KEYWORD_CONST,
                ..
            } => declaration_specifier
                .type_qualifiers
                .push(TypeQualifier::Const),
            Token {
                r#type: TokenType::KEYWORD_RESTRICT,
                ..
            } => declaration_specifier
                .type_qualifiers
                .push(TypeQualifier::Restrict),
            Token {
                r#type: TokenType::KEYWORD_VOLATILE,
                ..
            } => declaration_specifier
                .type_qualifiers
                .push(TypeQualifier::Volatile),
            Token {
                r#type: TokenType::KEYWORD__ATOMIC,
                ..
            } => {
                consume_whitespace(tokens, index);
                if let Some(Token {
                    r#type: TokenType::PUNCT_OPEN_PAR,
                    ..
                }) = tokens.get(*index)
                {
                    let _type_name = parse_type_names(&tokens, index, flattened, str_maps)?;
                    expected_token(tokens, str_maps, index, TokenType::PUNCT_CLOSE_PAR)?;
                }
            }
            Token {
                r#type: TokenType::KEYWORD_INLINE,
                ..
            } => declaration_specifier
                .function_specifiers
                .push(FunctionSpecifier::Inline),
            Token {
                r#type: TokenType::KEYWORD__NORETURN,
                ..
            } => declaration_specifier
                .function_specifiers
                .push(FunctionSpecifier::_Noreturn),
            Token {
                r#type: TokenType::KEYWORD__ALIGNAS,
                ..
            } => todo!(),
            _ => break,
        }
    }
    Ok(declaration_specifier)
}

pub fn parse_parameter_type_list(
    tokens: &[lexer::Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<ParameterTypeList, String> {
    let mut ptl = ParameterTypeList {
        parameter_declarations: Vec::new(),
        ellipsis: false,
    };
    while *index < tokens.len() {
        let declaration_specifier =
            parse_declaration_specifiers(tokens, index, flattened, str_maps)?;
        let has_identifier = {
            consume_whitespace(tokens, index);
            if matches!(
                tokens.get(*index),
                Some(Token {
                    r#type: TokenType::PUNCT_MULT,
                    ..
                })
            ) {
                todo!("this should do something");
            }
            consume_whitespace(tokens, index);
            match tokens.get(*index) {
                Some(Token {
                    r#type: TokenType::IDENT { .. },
                    ..
                }) => true,
                Some(Token {
                    r#type: TokenType::PUNCT_OPEN_PAR,
                    ..
                }) => {
                    let mut has_ident = false;
                    expected_token(tokens, str_maps, index, TokenType::PUNCT_CLOSE_PAR)?;
                    has_ident
                }
                _ => todo!("error herebrah"),
            }
        };
        if has_identifier {
            let declarator = parse_declarator(&tokens, index, flattened, str_maps)?;
            ptl.parameter_declarations
                .push(ParameterDeclaration::WithDeclarator {
                    declaration_specifier,
                    declarator,
                });
        } else {
            let ab = parse_abstract_declarator(&tokens, index, flattened, str_maps)?;
            ptl.parameter_declarations
                .push(ParameterDeclaration::WithOptionalAbstractDeclarator {
                    abstract_declarator: Some(ab),
                    declaration_specifier,
                });
        }
        *index += 1;
        consume_whitespace(tokens, index);
        if matches!(
            tokens.get(*index),
            Some(Token {
                r#type: TokenType::PUNCT_ELLIPSIS,
                ..
            })
        ) {
            ptl.ellipsis = true;
        }
        consume_whitespace(tokens, index);
        if ptl.ellipsis && *index < tokens.len() {
            todo!("ellipsis can only be at end of parameter type list");
        }
    }
    Ok(ptl)
}

pub fn parse_direct_abstract_declarator(
    tokens: &[lexer::Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<DirectAbstractDeclarator, String> {
    let mut dad = DirectAbstractDeclarator {
        type_qualifiers: Vec::new(),
        abstract_declarator: None,
        is_static: false,
        mult: false,
        parameter_type_list: None,
        assign_expr: None,
    };
    if matches!(
        tokens.get(*index),
        Some(Token {
            r#type: TokenType::PUNCT_OPEN_PAR,
            ..
        })
    ) {
        *index += 1;
        consume_whitespace(tokens, index);
        let abstract_declarator = parse_abstract_declarator(&tokens, index, flattened, str_maps)?;
        expected_token(tokens, str_maps, index, TokenType::PUNCT_CLOSE_PAR)?;
        dad.abstract_declarator = Some(Box::new(abstract_declarator));
        return Ok(dad);
    }
    match tokens.get(*index) {
        Some(Token {
            r#type: TokenType::PUNCT_OPEN_SQR,
            ..
        }) => {
            *index += 1;
            consume_whitespace(tokens, index);
            match tokens.get(*index) {
                Some(Token {
                    r#type: TokenType::PUNCT_MULT,
                    ..
                }) => {
                    *index += 1;
                    dad.mult = true;
                }
                Some(Token {
                    r#type: TokenType::KEYWORD_STATIC,
                    ..
                }) => {
                    dad.is_static = true;
                    *index += 1;
                    if let Some(type_qualifiers) = parse_type_qualifiers(tokens, index) {
                        dad.type_qualifiers = type_qualifiers;
                    }
                    let expr = parse_expressions(&tokens, index, flattened, str_maps)?;
                    flattened.expressions.push(expr);
                    dad.assign_expr = Some(flattened.expressions.len() - 1);
                    *index += 1;
                    expected_token(tokens, str_maps, index, TokenType::PUNCT_CLOSE_SQR);
                }
                _ => {
                    if let Some(type_qualifiers) = parse_type_qualifiers(tokens, index) {
                        dad.type_qualifiers = type_qualifiers;
                    }
                    consume_whitespace(tokens, index);
                    if matches!(
                        tokens.get(*index),
                        Some(Token {
                            r#type: TokenType::KEYWORD_STATIC,
                            ..
                        })
                    ) {
                        if dad.type_qualifiers.is_empty() {
                            return Err(format!(
                                "Expected type qualifiers, got {:?}",
                                tokens.get(*index)
                            ));
                        }
                        dad.is_static = true;
                        *index += 1;
                        expected_token(tokens, str_maps, index, TokenType::PUNCT_CLOSE_SQR)?;
                        let expr = parse_expressions(&tokens, index, flattened, str_maps)?;
                        flattened.expressions.push(expr);
                        dad.assign_expr = Some(flattened.expressions.len() - 1);
                        *index += 1;
                    } else {
                        let expr = parse_expressions(&tokens, index, flattened, str_maps)?;
                        flattened.expressions.push(expr);
                        dad.assign_expr = Some(flattened.expressions.len() - 1);
                        expected_token(tokens, str_maps, index, TokenType::PUNCT_CLOSE_SQR)?;
                    }
                }
            }
        }
        Some(Token {
            r#type: TokenType::PUNCT_OPEN_PAR,
            ..
        }) if dad.abstract_declarator.is_some() => {
            *index += 1;
            let ptl = parse_parameter_type_list(&tokens, index, flattened, str_maps)?;
            dad.parameter_type_list = Some(ptl);
            expected_token(tokens, str_maps, index, TokenType::PUNCT_CLOSE_PAR)?;
        }
        _ => {}
    }
    Ok(dad)
}

pub fn parse_abstract_declarator(
    tokens: &[lexer::Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<AbstractDeclarator, String> {
    let mut ad = AbstractDeclarator {
        pointer: Vec::new(),
        direct_abstract_declarator: None,
    };
    if let Some(pointers) = parse_pointer(tokens, index) {
        ad.pointer = pointers;
    }
    let dad = parse_direct_abstract_declarator(tokens, index, flattened, str_maps)?;
    ad.direct_abstract_declarator = Some(dad);
    Ok(ad)
}

pub fn parse_type_names(
    tokens: &[lexer::Token],
    index: &mut usize,
    flattened: &mut Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<TypeName, String> {
    let mut type_name = TypeName::new();
    let specifier_qualifier_list = parse_specifiers_qualifiers(tokens, index, flattened, str_maps)?;
    type_name.specifier_qualifier_list = specifier_qualifier_list;
    if *index < tokens.len() {
        let ad = parse_abstract_declarator(tokens, index, flattened, str_maps)?;
        type_name.abstract_declarator = Some(ad);
    }
    Ok(type_name)
}

#[cfg(test)]
mod tests {
    use super::{
        parse_declarations, parse_declarator, parse_enumerator_specifier, parse_initializer,
        parse_parameter_type_list, parse_struct_declarator, parse_struct_union_specifier,
        parse_type_names, Declaration, Declarator, Designation, Designator,
        DirectAbstractDeclarator, DirectDeclarator, Enumerator, InitDeclarator, Initializer,
        InitializerList, TypeQualifier, TypeSpecifier,
    };
    use crate::{lexer, parser};
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
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(src, false, &mut str_maps)?;
            let start_index = {
                let mut index = 0;
                while !matches!(
                    tokens.get(index),
                    Some(Token {
                        r#type: TokenType::KEYWORD_ENUM,
                        ..
                    })
                ) {
                    index += 1;
                }
                index + 1
            };
            let (_, enum_specifier) =
                parse_enumerator_specifier(tokens.as_slice(), start_index, &mut str_maps)?;
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
            let mut str_maps = lexer::ByteVecMaps::new();
            let tokens = lexer::lexer(src, false, &mut str_maps)?;
            let start_index = {
                let mut index = 0;
                while !matches!(
                    tokens.get(index),
                    Some(Token {
                        r#type: TokenType::KEYWORD_ENUM,
                        ..
                    })
                ) {
                    index += 1;
                }
                index + 1
            };
            let (_, enum_specifier) =
                parse_enumerator_specifier(tokens.as_slice(), start_index, &mut str_maps)?;
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
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer::lexer(src, false, &mut str_maps)?;
            let (_, i) = parse_initializer(&tokens, 0, &mut flattened, &mut str_maps)?;
            let Initializer::InitializerList(ili) = i else {
                unreachable!()
            };
            let il = flattened.initializer_lists[ili].clone();
            let il1 = il[0];
            let il2 = il[1];
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
            let Some(ini1_idx) = il1.initializer else {
                unreachable!()
            };
            let ini1 = flattened.initializers[ini1_idx].clone();
            let Initializer::AssignmentExpression(key) = ini1 else {
                unreachable!()
            };
            let expr = flattened.expressions[key];
            assert!(matches!(expr, Expr::Primary(Some(PrimaryInner::Token(_)))));

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
            let Some(ini2_idx) = il2.initializer else {
                unreachable!()
            };
            let ini2 = flattened.initializers[ini2_idx].clone();
            let Initializer::AssignmentExpression(key) = ini2 else {
                unreachable!()
            };
            let expr = flattened.expressions[key];
            assert!(matches!(expr, Expr::Primary(Some(PrimaryInner::Token(_)))));
        }
        {
            let src = r#"{}"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer::lexer(src, false, &mut str_maps)?;
            assert!(parse_initializer(&tokens, 0, &mut flattened, &mut str_maps).is_err());
        }
        {
            let src = r#"{ {[100] = 5}, 8, .baz = "" }"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer::lexer(src, false, &mut str_maps)?;
            let (_, i) = parse_initializer(&tokens, 0, &mut flattened, &mut str_maps)?;
        }
        {
            let src = r#"{ 8, 8, .baz = "" }"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer::lexer(src, false, &mut str_maps)?;
            let (_, i) = parse_initializer(&tokens, 0, &mut flattened, &mut str_maps)?;
        }
        Ok(())
    }
    #[test]
    fn parse_type_names_test_pointer_to_int_array_size_3() -> Result<(), String> {
        let src = r#"int (*)[3]"#.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut flattened = Flattened::new();
        let tokens = lexer::lexer(src, false, &mut str_maps)?;
        let (_, type_name) = parse_type_names(&tokens, 0, &mut flattened, &mut str_maps)?;
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
        let Expr::Primary(Some(PrimaryInner::Token(t))) = flattened.expressions[expr_idx] else {
            unreachable!()
        };
        let TokenType::CONSTANT_DEC_INT { value_key, .. } = t else {
            unreachable!()
        };
        assert_eq!(value_key, str_maps.add_byte_vec(b"3"));
        Ok(())
    }
    #[test]
    fn parse_type_names_test_pointer_to_variable_length_int_array() -> Result<(), String> {
        let src = r#"int (*)[*]"#.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut flattened = Flattened::new();
        let tokens = lexer::lexer(src, false, &mut str_maps)?;
        let (_, type_name) = parse_type_names(&tokens, 0, &mut flattened, &mut str_maps)?;
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
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer::lexer(src, false, &mut str_maps)?;
            let (_, dec) = parse_declarator(&tokens, 0, &mut flattened, &mut str_maps)?;
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
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut flattened = Flattened::new();
        let tokens = lexer::lexer(src, false, &mut str_maps)?;
        let ptl = parse_parameter_type_list(&tokens, &mut flattened, &mut str_maps)?;
        assert!(ptl.parameter_declarations.len() == 3);
        Ok(())
    }
    #[test]
    fn parse_struct_declarator_test() -> Result<(), String> {
        {
            let src = r#"hi : 4"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer::lexer(src, false, &mut str_maps)?;
            let (_, struct_declarator) =
                parse_struct_declarator(&tokens, 0, &mut flattened, &mut str_maps)?;
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
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer::lexer(src, false, &mut str_maps)?;
            let (_, struct_union_specifier) =
                parse_struct_union_specifier(&tokens, 0, &mut flattened, &mut str_maps)?;
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
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer::lexer(src, false, &mut str_maps)?;
            let (_, struct_union_specifier) =
                parse_struct_union_specifier(&tokens, 0, &mut flattened, &mut str_maps)?;
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
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer::lexer(src, false, &mut str_maps)?;
            let (_, struct_union_specifier) =
                parse_struct_union_specifier(&tokens, 0, &mut flattened, &mut str_maps)?;
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
        {
            let src = r#"int hi = 4;"#;
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer::lexer(src.as_bytes(), false, &mut str_maps)?;
            let (declaration, _) = parse_declarations(&tokens, 0, &mut flattened, &mut str_maps)?;
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
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut flattened = Flattened::new();
            let tokens = lexer::lexer(src.as_bytes(), false, &mut str_maps)?;
            let (declaration, _) = parse_declarations(&tokens, 0, &mut flattened, &mut str_maps)?;
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
