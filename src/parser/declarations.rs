use crate::lexer;
use crate::parser;

use super::expressions;
pub type TypeNameIndex = usize;
pub type DeclarationIndex = usize;
pub enum StorageClassSpecifier {
    TypeDef,
    Extern,
    Static,
    ThreadLocal,
    Auto,
    Register,
}
#[derive(Clone)]
pub enum DirectAbstractDeclarator {
    AbstractDeclarator(Box<AbstractDeclarator>),
    StaticTypeQualifiersAssignment(StaticTypeQualifiersAssignment),
    Pointer,
    ParameterTypeList(Option<DirectDeclaratorWithParameterTypeList>),
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
pub struct StaticTypeQualifiersAssignment {
    is_static: bool,
    type_qualifiers: Vec<TypeQualifier>,
    assignment: Option<parser::expressions::ExpressionIndex>,
}
#[derive(Clone)]
pub struct DirectDeclaratorWithPointer {
    type_qualifiers: Vec<TypeQualifier>,
}
#[derive(Clone)]
pub struct ParameterType {
    ellipsis: bool,
    parameter_list: Vec<ParameterDeclaration>,
}
#[derive(Clone)]
pub struct ParameterDeclaration {}
#[derive(Clone)]
pub struct DirectDeclaratorWithParameterTypeList {
    parameter_type_list: Vec<ParameterType>,
}
#[derive(Clone)]
pub struct DirectDeclaratorWithIdentifierList {
    identifier_list: Vec<usize>,
}
#[derive(Clone)]
pub enum DirectDeclarator {
    Ident(usize),
    Declarator(Box<Declarator>),
    DirectDeclarator {
        type_qualifier_list: Vec<TypeQualifier>,
        assignment_expr: Option<parser::expressions::ExpressionIndex>,
    },
    WithStaticTypeQualifiers(StaticTypeQualifiersAssignment),
    WithPointer(DirectDeclaratorWithPointer),
    WithParameterTypeList(DirectDeclaratorWithParameterTypeList),
    WithIdentifierList(Option<DirectDeclaratorWithIdentifierList>),
}
#[derive(Clone)]
pub struct Declarator {
    pointer: Vec<Pointer>,
    direct_declarator: DirectDeclarator,
}
impl Declarator {
    fn new(dd: DirectDeclarator) -> Self {
        Self {
            pointer: Vec::new(),
            direct_declarator: dd,
        }
    }
}
#[derive(Clone)]
pub enum StructDeclarator {
    Declarator(Declarator),
    DeclaratorBitField {
        declarator: Option<Declarator>,
        const_expr: Option<parser::expressions::ExpressionIndex>,
    },
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
}
#[derive(Copy, Clone)]
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
#[derive(Clone)]
pub enum AlignmentSpecifier {
    _Alignas(TypeNameIndex),
    _AlignasConstExpr(parser::expressions::ExpressionIndex),
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
#[derive(Debug, PartialEq, Clone)]
pub enum Designator {
    WithConstantExpr(parser::expressions::ExpressionIndex),
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
    AssignmentExpression(parser::expressions::ExpressionIndex),
    InitializerList(InitializerListIndex),
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
    let declaration_index = start_index;
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
                    let (new_index, _type_name) =
                        parse_type_names(&tokens, next_index + 1, str_maps)?;
                    next_index = new_index;
                    while !matches!(tokens.get(next_index), Some(lexer::Token::PUNCT_CLOSE_PAR))
                        && next_index < tokens.len()
                    {
                        next_index += 1;
                    }
                    if !matches!(tokens.get(next_index), Some(lexer::Token::PUNCT_CLOSE_PAR)) {
                        return Err(format!("Missing closing parenthesis"));
                    }
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

fn parse_initializer(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(usize, Initializer), String> {
    let mut index = start_index;
    while matches!(
        tokens.get(index),
        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
    ) && index < tokens.len()
    {
        index += 1;
    }
    match tokens.get(index) {
        Some(lexer::Token::PUNCT_OPEN_CURLY) => {
            index += 1;
            let (new_index, il) = parse_initializer_list(tokens, index, flattened, str_maps)?;
            flattened.initializer_lists.push(il);
            index = new_index;
            while matches!(
                tokens.get(index),
                Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
            ) && index < tokens.len()
            {
                index += 1;
            }
            Ok((
                index,
                Initializer::InitializerList(flattened.initializer_lists.len() - 1),
            ))
        }
        _ => {
            let (new_index, expr) =
                expressions::parse_expressions(tokens, index, flattened, str_maps)?;
            flattened.expressions.push(expr);
            Ok((
                new_index,
                Initializer::AssignmentExpression(flattened.expressions.len() - 1),
            ))
        }
    }
}

fn parse_initializer_list(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(usize, Vec<InitializerList>), String> {
    let mut index = start_index;
    while matches!(
        tokens.get(index),
        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
    ) && index < tokens.len()
    {
        index += 1;
    }
    let mut initializer_lists = Vec::new();
    loop {
        let mut designation = Designation {
            designator_list: Vec::new(),
        };
        loop {
            match tokens.get(index) {
                Some(lexer::Token::PUNCT_OPEN_SQR) => {
                    index += 1;
                    let starting = index;
                    let mut sqr_br_counter = 1;
                    while sqr_br_counter > 0 {
                        match tokens.get(index) {
                            Some(lexer::Token::PUNCT_OPEN_SQR) => sqr_br_counter += 1,
                            Some(lexer::Token::PUNCT_CLOSE_SQR) => sqr_br_counter -= 1,
                            None => return Err(format!("No closing sqr bracket")),
                            _ => {}
                        }
                        index += 1;
                    }
                    let (_, expr) = parser::expressions::parse_expressions(
                        &tokens[starting..index - 1],
                        0,
                        flattened,
                        str_maps,
                    )?;
                    flattened.expressions.push(expr);
                    designation
                        .designator_list
                        .push(Designator::WithConstantExpr(
                            flattened.expressions.len() - 1,
                        ));
                }
                Some(lexer::Token::PUNCT_DOT) => {
                    index += 1;
                    while matches!(
                        tokens.get(index),
                        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                    ) && index < tokens.len()
                    {
                        index += 1;
                    }
                    if !matches!(tokens.get(index), Some(lexer::Token::IDENT(_))) {
                        return Err(format!("Expected identifier, got {:?}", tokens.get(index)));
                    }
                    let Some(lexer::Token::IDENT(key)) = tokens.get(index) else { unreachable!() };
                    designation
                        .designator_list
                        .push(Designator::WithIdentifier(*key));
                    index += 1;
                }
                Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE) => index += 1,
                Some(lexer::Token::PUNCT_ASSIGNMENT | lexer::Token::PUNCT_COMMA) => {
                    index += 1;
                    break;
                }
                _ => return Err(format!("Expected '.' or '[', got {:?}", tokens.get(index))),
            }
        }
        let starting = index;
        while !matches!(
            tokens.get(index),
            Some(lexer::Token::PUNCT_COMMA | lexer::Token::PUNCT_CLOSE_CURLY)
        ) && index < tokens.len()
        {
            index += 1;
        }
        if starting != index {
            let (_, init) = parse_initializer(&tokens[starting..index], 0, flattened, str_maps)?;
            flattened.initializers.push(init);
            flattened.designations.push(designation);
            let initializer_list = InitializerList {
                designation: Some(flattened.designations.len() - 1),
                initializer: Some(flattened.initializers.len() - 1),
            };
            initializer_lists.push(initializer_list);
            index += 1;
        }
        if starting == index || matches!(tokens.get(index), None) {
            break;
        }
    }
    Ok((index, initializer_lists))
}

fn parse_pointer(tokens: &[lexer::Token], start_index: usize) -> Option<(usize, Vec<Pointer>)> {
    let mut index = start_index;
    let mut pointer_stack = Vec::new();
    while matches!(tokens.get(index), Some(lexer::Token::PUNCT_MULT)) {
        pointer_stack.push(Pointer::new());
        index += 1;
        let parsed_type_qualified = parse_type_qualifiers(tokens, index);
        if let Some((new_index, qualifiers)) = parsed_type_qualified {
            let Some(pointer) = pointer_stack.last_mut() else { unreachable!() };
            pointer
                .type_qualifier_list
                .extend_from_slice(qualifiers.as_slice());
            index = new_index;
        }
        while matches!(
            tokens.get(index),
            Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
        ) && index < tokens.len()
        {
            index += 1;
        }
    }
    if !pointer_stack.is_empty() {
        Some((index, pointer_stack))
    } else {
        None
    }
}

fn parse_declarator(
    tokens: &[lexer::Token],
    start_index: usize,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(usize, Declarator), String> {
    let mut index = start_index;
    if let Some((new_index, _pointers)) = parse_pointer(tokens, index) {
        index = new_index;
    }
    let mut declarator = None;
    loop {
        match tokens[index] {
            lexer::Token::WHITESPACE | lexer::Token::NEWLINE => {}
            lexer::Token::IDENT(key) => {
                declarator = Some(Declarator::new(DirectDeclarator::Ident(key)));
            }
            lexer::Token::PUNCT_OPEN_PAR => {
                // we use recursion because we are lazy. Most likely rewrite later.
                let (new_index, inner_declarator) = parse_declarator(tokens, index, str_maps)?;
                declarator = Some(Declarator::new(DirectDeclarator::Declarator(Box::new(
                    inner_declarator,
                ))));
                index = new_index;
                while matches!(
                    tokens.get(index),
                    Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                ) && index < tokens.len()
                {
                    index += 1;
                }
                if !matches!(tokens.get(index), Some(lexer::Token::PUNCT_CLOSE_PAR)) {
                    return Err(format!("Expected ')', got {:?}", tokens.get(index)));
                }
                index += 1;
                break;
            }
            _ => return Err("Expected identifier or open parentheses".to_string()),
        }
        index += 1;
    }
    while matches!(
        tokens.get(index),
        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
    ) && index < tokens.len()
    {
        index += 1;
    }
    if !matches!(tokens.get(index), Some(lexer::Token::PUNCT_OPEN_SQR)) {
        return Err(format!("Expected '[', got: {:?}", tokens.get(index)));
    }
    index += 1;
    while matches!(
        tokens.get(index),
        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
    ) && index < tokens.len()
    {
        index += 1;
    }
    match tokens.get(index) {
        Some(lexer::Token::IDENT(key)) => {
            let ident = &str_maps.key_to_byte_vec[*key];
            if *ident == *b"static" {}
        }
        Some(_) => {
            if let Some((new_index, _qualifiers)) = parse_type_qualifiers(tokens, index) {
                index = new_index;
                while matches!(
                    tokens.get(index),
                    Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                ) {
                    index += 1;
                }
                if let Some(lexer::Token::IDENT(key)) = tokens.get(index) {
                    let ident = &str_maps.key_to_byte_vec[*key];
                    if *ident == *b"static" {
                        //let _static_direct_declarator_with_qualifiers =
                        //    StaticDirectDeclaratorWithQualifiers::new();
                        //index += 1;
                        todo!("let assign_expr = parse_assignment_expr(tokens, index)?");
                    }
                }
            }
        }
        None => {}
    }
    let Some(declarator) = declarator else { unreachable!() };
    Ok((index, declarator))
}

fn parse_struct_union_specifier(
    tokens: &[lexer::Token],
    start_index: usize,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(usize, StructUnionSpecifier), String> {
    let mut index = start_index + 1;
    let mut struct_union_specifier = StructUnionSpecifier {
        struct_or_union: match tokens.get(start_index) {
            Some(lexer::Token::KEYWORD_STRUCT) => StructOrUnion::Struct,
            Some(lexer::Token::KEYWORD_UNION) => StructOrUnion::Union,
            _ => unreachable!(),
        },
        identifier: None,
        struct_declaration_list: Vec::new(),
    };
    while !matches!(
        tokens.get(index),
        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
    ) {
        index += 1;
    }
    if let Some(lexer::Token::IDENT(key)) = tokens.get(index) {
        struct_union_specifier.identifier = Some(*key);
        index += 1;
    }
    while !matches!(
        tokens.get(index),
        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
    ) {
        index += 1;
    }
    if !matches!(tokens.get(index), Some(lexer::Token::PUNCT_OPEN_CURLY)) {
        return Err(format!("Expected open curly bracket"));
    }
    index += 1;
    while !matches!(tokens.get(index), Some(lexer::Token::PUNCT_CLOSE_CURLY))
        && index < tokens.len()
    {
        while !matches!(tokens.get(index), Some(lexer::Token::PUNCT_SEMI_COLON))
            && index < tokens.len()
        {
            let (new_index, _specifier_qualifier_list) =
                parse_specifiers_qualifiers(tokens, index, str_maps)?;
            index = new_index;
            loop {
                //let (new_index, declarator) = parse_declarators()?;
                //index = new_index;
                //if next thing is not comma {
                //    break;
                //}
            }
        }
    }
    todo!()
}

fn parse_enumerator_specifier(
    tokens: &[lexer::Token],
    start_index: usize,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(usize, EnumSpecifier), String> {
    let mut enum_specifier = EnumSpecifier {
        identifier: None,
        enumerator_list: Vec::new(),
    };
    let mut index = start_index + 1;
    while matches!(
        tokens.get(index),
        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
    ) {
        index += 1;
    }
    if let Some(lexer::Token::IDENT(key)) = tokens.get(index) {
        enum_specifier.identifier = Some(*key);
        index += 1;
    }
    while matches!(
        tokens.get(index),
        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
    ) {
        index += 1;
    }
    if !matches!(tokens.get(index), Some(lexer::Token::PUNCT_OPEN_CURLY)) {
        return Err(format!("Expected opening curly bracket"));
    }
    index += 1;
    while index < tokens.len() {
        match tokens[index] {
            lexer::Token::WHITESPACE | lexer::Token::NEWLINE => {}
            lexer::Token::PUNCT_CLOSE_CURLY => break,
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
                    while !matches!(
                        tokens.get(ending_index),
                        Some(lexer::Token::PUNCT_COMMA | lexer::Token::PUNCT_CLOSE_CURLY)
                    ) && ending_index < tokens.len()
                    {
                        ending_index += 1;
                    }
                    // TODO: probably call parse_expressions here instead of evaluating
                    let constant_val = parser::expressions::eval_constant_expression_integer(
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
                if !matches!(
                    tokens.get(assignment_token_index),
                    Some(lexer::Token::PUNCT_COMMA | lexer::Token::PUNCT_CLOSE_CURLY)
                ) {
                    return Err(format!(
                        "Unexpected token: {:?}, expected a comma or closing curly bracket",
                        tokens[assignment_token_index]
                    ));
                }
                index = assignment_token_index + 1;
                if let Some(lexer::Token::PUNCT_CLOSE_CURLY) = tokens.get(assignment_token_index) {
                    break;
                }
                continue;
            }
            _ => return Err(format!("Unexpected token: {:?}", tokens[index])),
        }
        index += 1;
    }
    Ok((index, enum_specifier))
}

fn parse_specifiers_qualifiers(
    tokens: &[lexer::Token],
    start_index: usize,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(usize, SpecifierQualifierList), String> {
    let mut index = start_index;
    let mut specifier_qualifier = SpecifierQualifierList::new();
    loop {
        let (next_index, mut specifiers) = parse_type_specifiers(tokens, index, str_maps)?;
        // Avoids cloning
        while let Some(type_specifier) = specifiers.pop() {
            specifier_qualifier.type_specifiers.push(type_specifier);
        }
        index = next_index;
        if let Some((next_index, mut qualifiers)) = parse_type_qualifiers(tokens, index) {
            while let Some(type_qualifier) = qualifiers.pop() {
                specifier_qualifier.type_qualifiers.push(type_qualifier);
            }
            index = next_index;
        } else if specifiers.is_empty() {
            break;
        }
    }
    Ok((index, specifier_qualifier))
}

fn parse_type_qualifiers(
    tokens: &[lexer::Token],
    start_index: usize,
) -> Option<(usize, Vec<TypeQualifier>)> {
    let mut index = start_index;
    let mut type_qualifiers = Vec::new();
    while index < tokens.len() {
        match tokens[index] {
            lexer::Token::WHITESPACE | lexer::Token::NEWLINE => {}
            lexer::Token::KEYWORD_CONST => {
                type_qualifiers.push(TypeQualifier::Const);
            }
            lexer::Token::KEYWORD_RESTRICT => {
                type_qualifiers.push(TypeQualifier::Restrict);
            }
            lexer::Token::KEYWORD_VOLATILE => {
                type_qualifiers.push(TypeQualifier::Volatile);
            }
            lexer::Token::KEYWORD__ATOMIC => {
                type_qualifiers.push(TypeQualifier::_Atomic);
            }
            _ => break,
        }
        index += 1;
    }
    if type_qualifiers.is_empty() {
        None
    } else {
        Some((index, type_qualifiers))
    }
}

fn parse_type_specifiers(
    tokens: &[lexer::Token],
    start_index: usize,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(usize, Vec<TypeSpecifier>), String> {
    let mut index = start_index;
    let mut type_specifiers = Vec::new();
    while index < tokens.len() {
        match tokens[index] {
            lexer::Token::WHITESPACE | lexer::Token::NEWLINE => {}
            lexer::Token::KEYWORD_VOID => {
                type_specifiers.push(TypeSpecifier::Void);
            }
            lexer::Token::KEYWORD_CHAR => {
                type_specifiers.push(TypeSpecifier::Char);
            }
            lexer::Token::KEYWORD_SHORT => {
                type_specifiers.push(TypeSpecifier::Short);
            }
            lexer::Token::KEYWORD_INT => {
                type_specifiers.push(TypeSpecifier::Int);
            }
            lexer::Token::KEYWORD_LONG => {
                type_specifiers.push(TypeSpecifier::Long);
            }
            lexer::Token::KEYWORD_FLOAT => {
                type_specifiers.push(TypeSpecifier::Float);
            }
            lexer::Token::KEYWORD_DOUBLE => {
                type_specifiers.push(TypeSpecifier::Double);
            }
            lexer::Token::KEYWORD_SIGNED => {
                type_specifiers.push(TypeSpecifier::Signed);
            }
            lexer::Token::KEYWORD_UNSIGNED => {
                type_specifiers.push(TypeSpecifier::Unsigned);
            }
            lexer::Token::KEYWORD__BOOL => {
                type_specifiers.push(TypeSpecifier::_Bool);
            }
            lexer::Token::KEYWORD__COMPLEX => {
                type_specifiers.push(TypeSpecifier::_Complex);
            }
            lexer::Token::KEYWORD_STRUCT | lexer::Token::KEYWORD_UNION => {
                todo!()
                // TODO: parse_struct_union_specifier()?;
            }
            lexer::Token::KEYWORD__ATOMIC => todo!(),
            lexer::Token::KEYWORD_ENUM => {
                let (new_index, enum_specifier) =
                    parse_enumerator_specifier(tokens, index, str_maps)?;
                index = new_index;
                type_specifiers.push(TypeSpecifier::Enum(enum_specifier));
            }
            // TODO: typedef identifiers
            lexer::Token::IDENT(_) => todo!(),
            _ => {}
        }
        index += 1;
    }
    Ok((index, type_specifiers))
}

fn parse_direct_abstract_declarator(
    tokens: &[lexer::Token],
    start_index: usize,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(usize, DirectAbstractDeclarator), String> {
    let mut index = start_index;
    let mut dad: Option<DirectAbstractDeclarator> = None;
    loop {
        if index >= tokens.len() {
            break;
        }
        match tokens.get(index) {
            Some(lexer::Token::PUNCT_OPEN_PAR) => {
                index += 1;
                let (new_index, abstract_declarator) =
                    parse_abstract_declarator(tokens, index, str_maps)?;
                index = new_index;
                if !matches!(tokens.get(index), Some(lexer::Token::PUNCT_CLOSE_PAR)) {
                    return Err(format!("Missing closing parenth"));
                }
                dad = Some(DirectAbstractDeclarator::AbstractDeclarator(Box::new(
                    abstract_declarator,
                )));
                while matches!(
                    tokens.get(index),
                    Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                ) && index < tokens.len()
                {
                    index += 1;
                }
            }
            Some(lexer::Token::PUNCT_OPEN_SQR) => {
                index += 1;
                while matches!(
                    tokens.get(index),
                    Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                ) {
                    index += 1;
                }
                if let Some(lexer::Token::KEYWORD_STATIC) = tokens.get(index) {
                    let s = StaticTypeQualifiersAssignment {
                        is_static: true,
                        type_qualifiers: Vec::new(),
                        assignment: None,
                    };
                    dad = Some(DirectAbstractDeclarator::StaticTypeQualifiersAssignment(s));
                    index += 1;
                }
                while matches!(
                    tokens.get(index),
                    Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                ) {
                    index += 1;
                }
                if let Some((new_index, type_qualifiers)) = parse_type_qualifiers(tokens, index) {
                    let mut s = StaticTypeQualifiersAssignment {
                        is_static: false,
                        type_qualifiers: Vec::new(),
                        assignment: None,
                    };
                    s.type_qualifiers = type_qualifiers;
                    dad = Some(DirectAbstractDeclarator::StaticTypeQualifiersAssignment(s));
                    index = new_index;
                }
                while matches!(
                    tokens.get(index),
                    Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                ) {
                    index += 1;
                }
                if matches!(tokens.get(index), Some(lexer::Token::KEYWORD_STATIC)) {
                    index += 1;
                    if let Some(DirectAbstractDeclarator::StaticTypeQualifiersAssignment(s)) =
                        &mut dad
                    {
                        s.is_static = true;
                    }
                }
                while matches!(
                    tokens.get(index),
                    Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                ) {
                    index += 1;
                }
                todo!("let assign_expr = parse_assignment_expr(tokens, index)?");
            }
            _ => return Err(format!("Expected '(' or '[', got: {:?}", tokens[index])),
        }
    }
    if let Some(dad) = dad {
        Ok((index, dad))
    } else {
        Err("FUCK".to_string())
    }
}

fn parse_abstract_declarator(
    tokens: &[lexer::Token],
    start_index: usize,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(usize, AbstractDeclarator), String> {
    let mut index = start_index;
    let mut ad = AbstractDeclarator {
        pointer: Vec::new(),
        direct_abstract_declarator: None,
    };
    if let Some((new_index, pointers)) = parse_pointer(tokens, index) {
        index = new_index;
        ad.pointer = pointers;
    }
    if let Ok((new_index, dad)) = parse_direct_abstract_declarator(tokens, index, str_maps) {
        index = new_index;
        ad.direct_abstract_declarator = Some(dad);
    }
    Ok((index, ad))
}

pub fn parse_type_names(
    tokens: &[lexer::Token],
    start_index: usize,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(usize, TypeName), String> {
    let mut index = start_index;
    let mut type_name = TypeName::new();
    let (new_index, specifier_qualifier_list) =
        parse_specifiers_qualifiers(tokens, index, str_maps)?;
    type_name.specifier_qualifier_list = specifier_qualifier_list;
    index = new_index;
    if let Ok((new_index, ad)) = parse_abstract_declarator(tokens, index, str_maps) {
        index = new_index;
        type_name.abstract_declarator = Some(ad);
    }
    Ok((index, type_name))
}

#[cfg(test)]
mod tests {
    use super::{
        parse_enumerator_specifier, parse_initializer, Designation, Designator, Enumerator,
        Initializer, InitializerList,
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
                while !matches!(tokens.get(index), Some(lexer::Token::KEYWORD_ENUM)) {
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
                while !matches!(tokens.get(index), Some(lexer::Token::KEYWORD_ENUM)) {
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
            let mut flattened = parser::Flattened::new();
            let tokens = lexer::lexer(src, false, &mut str_maps)?;
            let (_, i) = parse_initializer(&tokens, 0, &mut flattened, &mut str_maps)?;
            let Initializer::InitializerList(ili) = i else { unreachable!() };
            let il = flattened.initializer_lists[ili].clone();
            let il1 = il[0];
            let il2 = il[1];
            let Some(des1_idx) = il1.designation else { unreachable!() };
            let des1 = flattened.designations[des1_idx].clone();
            assert_eq!(
                Designation {
                    designator_list: vec![Designator::WithIdentifier(
                        str_maps.add_byte_vec("hi".as_bytes()),
                    )],
                },
                des1
            );
            let Some(ini1_idx) = il1.initializer else { unreachable!() };
            let ini1 = flattened.initializers[ini1_idx].clone();
            let Initializer::AssignmentExpression(key) = ini1 else { unreachable!() };
            let expr = flattened.expressions[key];
            assert!(matches!(
                expr,
                parser::expressions::Expr::Primary(Some(parser::expressions::PrimaryInner::Token(
                    _
                )))
            ));

            let Some(des2_idx) = il2.designation else { unreachable!() };
            let des2 = flattened.designations[des2_idx].clone();
            assert_eq!(
                Designation {
                    designator_list: vec![Designator::WithIdentifier(
                        str_maps.add_byte_vec("hi2".as_bytes()),
                    )],
                },
                des2
            );
            let Some(ini2_idx) = il2.initializer else { unreachable!() };
            let ini2 = flattened.initializers[ini2_idx].clone();
            let Initializer::AssignmentExpression(key) = ini2 else { unreachable!() };
            let expr = flattened.expressions[key];
            assert!(matches!(
                expr,
                parser::expressions::Expr::Primary(Some(parser::expressions::PrimaryInner::Token(
                    _
                )))
            ));
        }
        //{
        //    let src = r#"{}"#.as_bytes();
        //    let mut str_maps = lexer::ByteVecMaps::new();
        //    let mut flattened = parser::Flattened::new();
        //    let tokens = lexer::lexer(src, false, &mut str_maps)?;
        //    let (_, i) = parse_initializer(&tokens, 0, &mut flattened, &mut str_maps)?;
        //}
        {
            let src = r#"{[100] = 5}, 8, .baz = "" }"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut flattened = parser::Flattened::new();
            let tokens = lexer::lexer(src, false, &mut str_maps)?;
            let (_, i) = parse_initializer(&tokens, 0, &mut flattened, &mut str_maps)?;
        }
        Ok(())
    }
}
