use crate::cpp::{self};
use crate::lexer::{self};
use crate::parser::expressions::{self};
pub enum StorageClassSpecifier {
    TypeDef,
    Extern,
    Static,
    ThreadLocal,
    Auto,
    Register,
}
pub enum DirectAbstractDeclarator {
    AbstractDeclarator(Box<AbstractDeclarator>),
    DirectAbstractDeclaratorWithStaticWithOptionalQualifiers(
        StaticDirectDeclaratorWithOptionalQualifiers,
    ),
    DirectAbstractDeclaratorWithStaticWithQualifiers(StaticDirectDeclaratorWithQualifiers),
    DirectAbstractDeclaratorWithPointer,
    DirectAbstractDeclaratorWithParameterTypeList(Option<DirectDeclaratorWithParameterTypeList>),
}
pub enum AbstractDeclarator {
    AbstractDeclaratorWithPointer(Pointer),
    AbstractDeclaratorWithDirectAbstractDeclarator {
        pointer: Option<Pointer>,
        direct_abstract_declarator: DirectAbstractDeclarator,
    },
}
pub struct TypeName {
    specifier_qualifier_list: SpecifierQualifierList,
    abstract_declarator: Option<AbstractDeclarator>,
}
pub enum StructOrUnion {
    Struct,
    Union,
}
pub struct Pointer {
    type_qualifier_list: Vec<TypeQualifier>,
    pointer: Option<Box<Pointer>>,
}
pub struct StaticDirectDeclaratorWithOptionalQualifiers {
    is_static: bool,
    type_qualifiers: Vec<TypeQualifier>,
}
impl StaticDirectDeclaratorWithOptionalQualifiers {
    fn new() -> Self {
        Self {
            is_static: true,
            type_qualifiers: Vec::new(),
        }
    }
}
pub struct StaticDirectDeclaratorWithQualifiers {
    is_static: bool,
    type_qualifiers: Vec<TypeQualifier>,
}
impl StaticDirectDeclaratorWithQualifiers {
    fn new() -> Self {
        Self {
            is_static: true,
            type_qualifiers: Vec::new(),
        }
    }
}
pub struct DirectDeclaratorWithPointer {
    type_qualifiers: Vec<TypeQualifier>,
}
pub struct ParameterType {
    ellipsis: bool,
    parameter_list: Vec<ParameterDeclaration>,
}
pub struct ParameterDeclaration {}
pub struct DirectDeclaratorWithParameterTypeList {
    parameter_type_list: Vec<ParameterType>,
}
pub struct DirectDeclaratorWithIdentifierList {
    identifier_list: Vec<usize>,
}
pub enum DirectDeclarator {
    Ident(usize),
    Declarator(Box<Declarator>),
    DirectDeclarator {
        type_qualifier_list: Vec<TypeQualifier>,
        assignment_expr: Option<expressions::Expr>,
    },
    DirectDeclaratorWithStaticWithOptionalQualifiers(StaticDirectDeclaratorWithOptionalQualifiers),
    DirectDeclaratorWithStaticWithQualifiers(StaticDirectDeclaratorWithQualifiers),
    DirectDeclaratorWithPointer(DirectDeclaratorWithPointer),
    DirectDeclaratorWithParameterTypeList(DirectDeclaratorWithParameterTypeList),
    DirectDeclaratorWithIdentifierList(Option<DirectDeclaratorWithIdentifierList>),
}
pub struct Declarator {
    pointer: Option<Pointer>,
    direct_declarator: DirectDeclarator,
}
pub enum StructDeclarator {
    Declarator(Declarator),
    DeclaratorBitField {
        declarator: Option<Declarator>,
        const_expr: expressions::Expr,
    },
}
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
pub struct StructDeclaration {
    specifier_qualifier_list: SpecifierQualifierList,
    struct_declarator_list: Vec<StructDeclarator>,
}
pub struct StructUnionSpecifier {
    pub struct_or_union: StructOrUnion,
    pub identifier: Option<usize>,
    pub struct_declaration_list: Vec<StructDeclaration>,
}
pub enum Enumerator {
    Enum(usize),
    EnumWithConstantExpr(usize, i128),
}
pub struct EnumSpecifier {
    enumerator_list: Vec<Enumerator>,
}
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
}
pub enum TypeQualifier {
    Const,
    Restrict,
    Volatile,
    _Atomic,
}
pub enum FunctionSpecifier {
    Inline,
    _Noreturn,
}
pub enum AlignmentSpecifier {
    _Alignas(TypeName),
    _AlignasConstExpr(expressions::Expr),
}
impl Declarator {
    fn new() -> Declarator {
        todo!()
    }
}
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
pub enum Designator {
    DesignatorWithConstantExpr(expressions::Expr),
    DesignatorWithIdentifier(usize),
}
pub struct Designation {
    designator_list: Vec<Designator>,
}
pub struct InitializerList {
    designation: Option<Designation>,
    initializer_list: Option<Box<InitializerList>>,
    initializer: Initializer,
}
pub struct Initializer {
    assignment_expression: expressions::Expr,
    initializer_list: Vec<Initializer>,
}
pub struct DeclaratorWithInitializer {
    declarator: Declarator,
    initializer: Initializer,
}
pub enum InitDeclarator {
    Declarator(Declarator),
    DeclaratorWithInitializer(DeclaratorWithInitializer),
}
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
fn parse_declarations(
    tokens: &[lexer::Token],
    start_index: usize,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<Option<(Declaration, usize)>, String> {
    let mut declaration_index = start_index;
    let mut declaration = Declaration::new();
    while declaration_index < tokens.len() {
        match tokens[declaration_index] {
            lexer::Token::KEYWORD_TYPEDEF => declaration
                .declaration_specifiers
                .storage_class_specifiers
                .push(StorageClassSpecifier::TypeDef),
            lexer::Token::KEYWORD_EXTERN => declaration
                .declaration_specifiers
                .storage_class_specifiers
                .push(StorageClassSpecifier::Extern),
            lexer::Token::KEYWORD_STATIC => declaration
                .declaration_specifiers
                .storage_class_specifiers
                .push(StorageClassSpecifier::Static),
            lexer::Token::KEYWORD__THREAD_LOCAL => declaration
                .declaration_specifiers
                .storage_class_specifiers
                .push(StorageClassSpecifier::ThreadLocal),
            lexer::Token::KEYWORD_AUTO => declaration
                .declaration_specifiers
                .storage_class_specifiers
                .push(StorageClassSpecifier::Auto),
            lexer::Token::KEYWORD_REGISTER => declaration
                .declaration_specifiers
                .storage_class_specifiers
                .push(StorageClassSpecifier::Register),
            lexer::Token::KEYWORD_VOID => declaration
                .declaration_specifiers
                .type_specifiers
                .push(TypeSpecifier::Void),
            lexer::Token::KEYWORD_CHAR => declaration
                .declaration_specifiers
                .type_specifiers
                .push(TypeSpecifier::Char),
            lexer::Token::KEYWORD_SHORT => declaration
                .declaration_specifiers
                .type_specifiers
                .push(TypeSpecifier::Short),
            lexer::Token::KEYWORD_INT => declaration
                .declaration_specifiers
                .type_specifiers
                .push(TypeSpecifier::Int),
            lexer::Token::KEYWORD_LONG => declaration
                .declaration_specifiers
                .type_specifiers
                .push(TypeSpecifier::Long),
            lexer::Token::KEYWORD_FLOAT => declaration
                .declaration_specifiers
                .type_specifiers
                .push(TypeSpecifier::Float),
            lexer::Token::KEYWORD_DOUBLE => declaration
                .declaration_specifiers
                .type_specifiers
                .push(TypeSpecifier::Double),
            lexer::Token::KEYWORD_SIGNED => declaration
                .declaration_specifiers
                .type_specifiers
                .push(TypeSpecifier::Signed),
            lexer::Token::KEYWORD_UNSIGNED => declaration
                .declaration_specifiers
                .type_specifiers
                .push(TypeSpecifier::Unsigned),
            lexer::Token::KEYWORD__BOOL => declaration
                .declaration_specifiers
                .type_specifiers
                .push(TypeSpecifier::_Bool),
            lexer::Token::KEYWORD__COMPLEX => declaration
                .declaration_specifiers
                .type_specifiers
                .push(TypeSpecifier::_Complex),
            lexer::Token::KEYWORD_STRUCT | lexer::Token::KEYWORD_UNION => todo!(),
            lexer::Token::KEYWORD_ENUM => todo!(),
            lexer::Token::KEYWORD_CONST => declaration
                .declaration_specifiers
                .type_qualifiers
                .push(TypeQualifier::Const),
            lexer::Token::KEYWORD_RESTRICT => declaration
                .declaration_specifiers
                .type_qualifiers
                .push(TypeQualifier::Restrict),
            lexer::Token::KEYWORD_VOLATILE => declaration
                .declaration_specifiers
                .type_qualifiers
                .push(TypeQualifier::Volatile),
            lexer::Token::KEYWORD__ATOMIC => {
                let mut next_index = declaration_index + 1;
                while matches!(
                    tokens.get(next_index),
                    Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                ) {
                    next_index += 1;
                }
                if let Some(lexer::Token::PUNCT_OPEN_PAR) = tokens.get(next_index) {
                    let start = next_index + 1;
                    while !matches!(tokens.get(next_index), Some(lexer::Token::PUNCT_CLOSE_PAR))
                        && next_index < tokens.len()
                    {
                        next_index += 1;
                    }
                    if !matches!(tokens.get(next_index), Some(lexer::Token::PUNCT_CLOSE_PAR)) {
                        return Err(format!("Missing closing parenthesis"));
                    }
                    let type_name = parse_type_names(&tokens[start..next_index], str_maps)?;
                }
            }
            lexer::Token::KEYWORD_INLINE => declaration
                .declaration_specifiers
                .function_specifiers
                .push(FunctionSpecifier::Inline),
            lexer::Token::KEYWORD__NORETURN => declaration
                .declaration_specifiers
                .function_specifiers
                .push(FunctionSpecifier::_Noreturn),
            lexer::Token::KEYWORD__ALIGNAS => todo!(),
            _ => todo!(),
        }
        todo!()
    }
    todo!()
}

fn parse_struct_union_specifier(tokens: &[lexer::Token]) -> Result<StructUnionSpecifier, String> {
    todo!()
}

fn parse_enumerator_specifier(
    tokens: &[lexer::Token],
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<EnumSpecifier, String> {
    let mut enum_specifier = EnumSpecifier {
        enumerator_list: Vec::new(),
    };
    let mut index = 0;
    while index < tokens.len() {
        match tokens[index] {
            lexer::Token::WHITESPACE | lexer::Token::NEWLINE => {}
            lexer::Token::IDENT(key) => {
                let mut assignment_token_index = index + 1;
                while matches!(
                    tokens.get(assignment_token_index),
                    Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                ) {
                    assignment_token_index += 1;
                }
                if matches!(
                    tokens.get(assignment_token_index),
                    Some(lexer::Token::PUNCT_ASSIGNMENT)
                ) {
                    let mut ending_index = assignment_token_index + 1;
                    while !matches!(tokens.get(ending_index), Some(lexer::Token::PUNCT_COMMA))
                        && ending_index < tokens.len()
                    {
                        ending_index += 1;
                    }
                    let constant_val = expressions::eval_constant_expression(
                        &tokens[assignment_token_index + 1..ending_index],
                        str_maps,
                    )?;
                    enum_specifier
                        .enumerator_list
                        .push(Enumerator::EnumWithConstantExpr(key, constant_val));
                    assignment_token_index = ending_index;
                } else {
                    enum_specifier.enumerator_list.push(Enumerator::Enum(key));
                }
                if assignment_token_index < tokens.len()
                    && !matches!(
                        tokens.get(assignment_token_index),
                        Some(lexer::Token::PUNCT_COMMA)
                    )
                {
                    return Err(format!(
                        "Unexpected token: {:?}",
                        tokens[assignment_token_index]
                    ));
                }
                index = assignment_token_index;
                continue;
            }
            _ => todo!(),
        }
        index += 1;
    }
    Ok(enum_specifier)
}

fn parse_type_names(
    tokens: &[lexer::Token],
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<TypeName, String> {
    let mut index = 0;
    let mut specifiers_qualifiers = SpecifierQualifierList::new();
    while index < tokens.len() {
        match tokens[index] {
            lexer::Token::KEYWORD_VOID => {
                specifiers_qualifiers
                    .type_specifiers
                    .push(TypeSpecifier::Void);
            }
            lexer::Token::KEYWORD_CHAR => {
                specifiers_qualifiers
                    .type_specifiers
                    .push(TypeSpecifier::Char);
            }
            lexer::Token::KEYWORD_SHORT => {
                specifiers_qualifiers
                    .type_specifiers
                    .push(TypeSpecifier::Short);
            }
            lexer::Token::KEYWORD_INT => {
                specifiers_qualifiers
                    .type_specifiers
                    .push(TypeSpecifier::Int);
            }
            lexer::Token::KEYWORD_LONG => {
                specifiers_qualifiers
                    .type_specifiers
                    .push(TypeSpecifier::Long);
            }
            lexer::Token::KEYWORD_FLOAT => {
                specifiers_qualifiers
                    .type_specifiers
                    .push(TypeSpecifier::Float);
            }
            lexer::Token::KEYWORD_DOUBLE => {
                specifiers_qualifiers
                    .type_specifiers
                    .push(TypeSpecifier::Double);
            }
            lexer::Token::KEYWORD_SIGNED => {
                specifiers_qualifiers
                    .type_specifiers
                    .push(TypeSpecifier::Signed);
            }
            lexer::Token::KEYWORD_UNSIGNED => {
                specifiers_qualifiers
                    .type_specifiers
                    .push(TypeSpecifier::Unsigned);
            }
            lexer::Token::KEYWORD__BOOL => {
                specifiers_qualifiers
                    .type_specifiers
                    .push(TypeSpecifier::_Bool);
            }
            lexer::Token::KEYWORD__COMPLEX => {
                specifiers_qualifiers
                    .type_specifiers
                    .push(TypeSpecifier::_Complex);
            }
            lexer::Token::KEYWORD_STRUCT | lexer::Token::KEYWORD_UNION => {
                // TODO: parse_struct_union_specifier()?;
            }
            lexer::Token::KEYWORD__ATOMIC => {}
            lexer::Token::KEYWORD_ENUM => {
                let mut enum_index = index + 1;
                while matches!(
                    tokens.get(enum_index),
                    Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                ) {
                    enum_index += 1;
                }
                if !matches!(tokens.get(enum_index), Some(lexer::Token::PUNCT_OPEN_CURLY)) {
                    let start = enum_index + 1;
                    while !matches!(
                        tokens.get(enum_index),
                        Some(lexer::Token::PUNCT_CLOSE_CURLY)
                    ) {
                        enum_index += 1;
                    }
                    if !matches!(
                        tokens.get(enum_index),
                        Some(lexer::Token::PUNCT_CLOSE_CURLY)
                    ) {
                        return Err(format!("Missing closing curly"));
                    }
                    let enum_specifier =
                        parse_enumerator_specifier(&tokens[start..enum_index], str_maps)?;
                    specifiers_qualifiers.type_specifiers.push(enum_specifier);
                }
            }
            lexer::Token::KEYWORD_TYPEDEF => {}
            _ => {}
        }
    }
    todo!()
}
