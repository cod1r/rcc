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
pub struct AbstractDeclarator {}
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
pub struct DirectDeclaratorWithIdentifierList {}
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
    DirectDeclaratorWithIdentifierList(DirectDeclaratorWithIdentifierList),
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
pub struct StructDeclaration {
    specifier_qualifier_list: SpecifierQualifierList,
    struct_declarator_list: Vec<StructDeclarator>,
}
pub struct StructUnionSpecifier {
    pub struct_or_union: StructOrUnion,
    pub identifier: Option<usize>,
    pub struct_declaration_list: Vec<StructDeclaration>,
}
pub struct EnumSpecifier {}
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
                todo!()
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
