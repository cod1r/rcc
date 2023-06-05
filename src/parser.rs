use crate::lexer::{self};

#[derive(Clone)]
pub enum PrimaryInner {
    Token(lexer::Token),
    Expr(Box<Expr>),
}

impl PrimaryInner {
    pub fn new_p_token(t: lexer::Token) -> Result<Self, String> {
        if matches!(
            t,
            lexer::Token::IDENT(_)
                | lexer::Token::StringLiteral { .. }
                | lexer::Token::CONSTANT_DEC_INT { .. }
                | lexer::Token::CONSTANT_HEXA_INT { .. }
                | lexer::Token::CONSTANT_DEC_FLOAT { .. }
                | lexer::Token::CONSTANT_HEXA_FLOAT { .. }
                | lexer::Token::CONSTANT_CHAR { .. }
                | lexer::Token::CONSTANT_OCTAL_INT { .. }
                | lexer::Token::CONSTANT_ENUM(_)
        ) {
            return Ok(Self::Token(t));
        }
        Err(format!("not allowed token passed in"))
    }
    pub fn new_p_expr(e: Expr) -> Self {
        Self::Expr(Box::new(e))
    }
}

pub enum Type {
    Void,
    SignedChar,
    UnsignedChar,
    ShortInt,
    UnsignedShortInt,
    Int,
    UnsignedInt,
    LongInt,
    LongLongInt,
    UnsignedLongLongInt,
}

#[derive(Clone)]
pub struct Conditional {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
    pub third: Option<Box<Expr>>,
}

#[derive(Clone)]
pub struct LogicalOR {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

#[derive(Clone)]
pub struct LogicalAND {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

#[derive(Clone)]
pub struct BitOR {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

#[derive(Clone)]
pub struct BitXOR {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

#[derive(Clone)]
pub struct BitAND {
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

#[derive(Clone)]
pub enum EqualityOp {
    Equal,
    NotEqual,
}

#[derive(Clone)]
pub struct Equality {
    pub op: EqualityOp,
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

#[derive(Clone)]
pub enum RelationalOp {
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
}

#[derive(Clone)]
pub struct Relational {
    pub op: RelationalOp,
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

#[derive(Clone)]
pub enum BitShiftOp {
    Left,
    Right,
}

#[derive(Clone)]
pub struct BitShift {
    pub op: BitShiftOp,
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}

#[derive(Clone)]
pub enum AdditiveOps {
    Add,
    Sub,
}

#[derive(Clone)]
pub struct Additive {
    pub op: AdditiveOps,
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}
#[derive(Clone)]
pub enum MultiplicativeOps {
    Mult,
    Div,
    Mod,
}
#[derive(Clone)]
pub struct Multiplicative {
    pub op: MultiplicativeOps,
    pub first: Option<Box<Expr>>,
    pub second: Option<Box<Expr>>,
}
#[derive(Clone)]
pub enum UnaryOp {
    Ampersand,
    Sub,
    Add,
    Deref,
    BitNOT,
    LogicalNOT,
}
#[derive(Clone)]
pub struct Unary {
    pub op: UnaryOp,
    pub first: Option<Box<Expr>>,
}
#[derive(Clone)]
pub struct Cast {}
#[derive(Clone)]
pub struct PostFix {}

#[derive(Clone)]
pub enum Expr {
    Conditional(Conditional),
    LogicalOR(LogicalOR),
    LogicalAND(LogicalAND),
    BitOR(BitOR),
    BitXOR(BitXOR),
    BitAND(BitAND),
    Equality(Equality),
    Relational(Relational),
    BitShift(BitShift),
    Additive(Additive),
    Multiplicative(Multiplicative),
    Unary(Unary),
    Cast(Cast),
    PostFix(PostFix),
    Primary(Option<PrimaryInner>),
}

impl Expr {
    pub fn priority(&self) -> u8 {
        match self {
            Expr::Primary(_) => u8::MAX,
            Expr::PostFix(_) => u8::MAX - 1,
            Expr::Unary(_) => u8::MAX - 2,
            Expr::Cast(_) => u8::MAX - 3,
            Expr::Multiplicative(_) => u8::MAX - 4,
            Expr::Additive(_) => u8::MAX - 5,
            Expr::BitShift(_) => u8::MAX - 6,
            Expr::Relational(_) => u8::MAX - 7,
            Expr::Equality(_) => u8::MAX - 8,
            Expr::BitAND(_) => u8::MAX - 9,
            Expr::BitXOR(_) => u8::MAX - 10,
            Expr::BitOR(_) => u8::MAX - 11,
            Expr::LogicalAND(_) => u8::MAX - 12,
            Expr::LogicalOR(_) => u8::MAX - 13,
            Expr::Conditional(_) => u8::MAX - 14,
        }
    }
}
pub enum StorageClassSpecifier {
    TypeDef,
    Extern,
    Static,
    ThreadLocal,
    Auto,
    Register,
}
pub struct TypeName {}
pub enum StructOrUnion {
    Struct,
    Union,
}
pub struct Declarator {}
pub enum StructDeclarator {
    Declarator(Declarator),
    DeclaratorBitField {
        declarator: Option<Declarator>,
        const_expr: Expr,
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
    _AlignasConstExpr(Expr),
}
impl Declarator {
    fn new() -> Declarator {
        Declarator {}
    }
}
pub struct Declaration {
    pub storage_class_specifiers: Vec<StorageClassSpecifier>,
    pub type_specifiers: Vec<TypeSpecifier>,
    pub type_qualifiers: Vec<TypeQualifier>,
    pub function_specifiers: Vec<FunctionSpecifier>,
    pub alignment_specifiers: Vec<AlignmentSpecifier>,
    pub declarator: Option<Declarator>,
}
impl Declaration {
    fn new() -> Declaration {
        Declaration {
            storage_class_specifiers: Vec::new(),
            type_specifiers: Vec::new(),
            type_qualifiers: Vec::new(),
            function_specifiers: Vec::new(),
            alignment_specifiers: Vec::new(),
            declarator: None,
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
                .storage_class_specifiers
                .push(StorageClassSpecifier::TypeDef),
            lexer::Token::KEYWORD_EXTERN => declaration
                .storage_class_specifiers
                .push(StorageClassSpecifier::Extern),
            lexer::Token::KEYWORD_STATIC => declaration
                .storage_class_specifiers
                .push(StorageClassSpecifier::Static),
            lexer::Token::KEYWORD__THREAD_LOCAL => declaration
                .storage_class_specifiers
                .push(StorageClassSpecifier::ThreadLocal),
            lexer::Token::KEYWORD_AUTO => declaration
                .storage_class_specifiers
                .push(StorageClassSpecifier::Auto),
            lexer::Token::KEYWORD_REGISTER => declaration
                .storage_class_specifiers
                .push(StorageClassSpecifier::Register),
            lexer::Token::KEYWORD_VOID => declaration.type_specifiers.push(TypeSpecifier::Void),
            lexer::Token::KEYWORD_CHAR => declaration.type_specifiers.push(TypeSpecifier::Char),
            lexer::Token::KEYWORD_SHORT => declaration.type_specifiers.push(TypeSpecifier::Short),
            lexer::Token::KEYWORD_INT => declaration.type_specifiers.push(TypeSpecifier::Int),
            lexer::Token::KEYWORD_LONG => declaration.type_specifiers.push(TypeSpecifier::Long),
            lexer::Token::KEYWORD_FLOAT => declaration.type_specifiers.push(TypeSpecifier::Float),
            lexer::Token::KEYWORD_DOUBLE => declaration.type_specifiers.push(TypeSpecifier::Double),
            lexer::Token::KEYWORD_SIGNED => declaration.type_specifiers.push(TypeSpecifier::Signed),
            lexer::Token::KEYWORD_UNSIGNED => {
                declaration.type_specifiers.push(TypeSpecifier::Unsigned)
            }
            lexer::Token::KEYWORD__BOOL => declaration.type_specifiers.push(TypeSpecifier::_Bool),
            lexer::Token::KEYWORD__COMPLEX => {
                declaration.type_specifiers.push(TypeSpecifier::_Complex)
            }
            lexer::Token::KEYWORD_STRUCT | lexer::Token::KEYWORD_UNION => todo!(),
            lexer::Token::KEYWORD_ENUM => todo!(),
            lexer::Token::KEYWORD_CONST => declaration.type_qualifiers.push(TypeQualifier::Const),
            lexer::Token::KEYWORD_RESTRICT => {
                declaration.type_qualifiers.push(TypeQualifier::Restrict)
            }
            lexer::Token::KEYWORD_VOLATILE => {
                declaration.type_qualifiers.push(TypeQualifier::Volatile)
            }
            lexer::Token::KEYWORD__ATOMIC => {
                todo!()
            }
            lexer::Token::KEYWORD_INLINE => declaration
                .function_specifiers
                .push(FunctionSpecifier::Inline),
            lexer::Token::KEYWORD__NORETURN => declaration
                .function_specifiers
                .push(FunctionSpecifier::_Noreturn),
            lexer::Token::KEYWORD__ALIGNAS => todo!(),
            _ => todo!(),
        }
        todo!()
    }
    todo!()
}

pub fn parser(tokens: &[lexer::Token]) -> Result<(), String> {
    let mut parser_index = 0;
    while parser_index < tokens.len() {
        parser_index += 1;
    }
    todo!()
}
