use crate::error;
use crate::lexer;
use crate::parser;

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
    parameter_declarations: Vec<ParameterDeclaration>,
    ellipsis: bool,
}
#[derive(Clone)]
pub struct DirectAbstractDeclarator {
    abstract_declarator: Option<Box<AbstractDeclarator>>,
    type_qualifiers: Vec<TypeQualifier>,
    is_static: bool,
    mult: bool,
    parameter_type_list: Option<ParameterTypeList>,
    assign_expr: Option<parser::expressions::ExpressionIndex>,
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
    identifier: Option<usize>,
    declarator: Option<Box<Declarator>>,
    type_qualifier_list: Vec<TypeQualifier>,
    is_static: bool,
    mult: bool,
    parameter_type_list: Option<ParameterTypeList>,
    assign_expr: Option<parser::expressions::ExpressionIndex>,
}
#[derive(Clone)]
pub struct Declarator {
    pointer: Vec<Pointer>,
    direct_declarator: Option<DirectDeclarator>,
}
#[derive(Clone)]
pub struct StructDeclarator {
    declarator: Option<Declarator>,
    const_expr: Option<parser::expressions::ExpressionIndex>,
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
#[derive(Copy, Clone)]
pub enum TypeQualifier {
    Const,
    Restrict,
    Volatile,
    _Atomic,
}
#[derive(Copy, Clone)]
pub enum FunctionSpecifier {
    Inline,
    _Noreturn,
}
#[derive(Clone)]
pub enum AlignmentSpecifier {
    _Alignas(TypeNameIndex),
    _AlignasConstExpr(parser::expressions::ExpressionIndex),
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
pub fn is_declaration_token(t: lexer::Token) -> bool {
    match t {
        lexer::Token::KEYWORD_TYPEDEF => true,
        lexer::Token::KEYWORD_EXTERN => true,
        lexer::Token::KEYWORD_STATIC => true,
        lexer::Token::KEYWORD__THREAD_LOCAL => true,
        lexer::Token::KEYWORD_AUTO => true,
        lexer::Token::KEYWORD_REGISTER => true,
        lexer::Token::KEYWORD_VOID => true,
        lexer::Token::KEYWORD_CHAR => true,
        lexer::Token::KEYWORD_SHORT => true,
        lexer::Token::KEYWORD_INT => true,
        lexer::Token::KEYWORD_LONG => true,
        lexer::Token::KEYWORD_FLOAT => true,
        lexer::Token::KEYWORD_DOUBLE => true,
        lexer::Token::KEYWORD_SIGNED => true,
        lexer::Token::KEYWORD_UNSIGNED => true,
        lexer::Token::KEYWORD__BOOL => true,
        lexer::Token::KEYWORD__COMPLEX => true,
        lexer::Token::KEYWORD_STRUCT | lexer::Token::KEYWORD_UNION => true,
        lexer::Token::KEYWORD_ENUM => todo!(),
        lexer::Token::KEYWORD_CONST => true,
        lexer::Token::KEYWORD_RESTRICT => true,
        lexer::Token::KEYWORD_VOLATILE => true,
        lexer::Token::KEYWORD__ATOMIC => true,
        lexer::Token::KEYWORD_INLINE => true,
        lexer::Token::KEYWORD__NORETURN => true,
        lexer::Token::KEYWORD__ALIGNAS => true,
        _ => false,
    }
}
pub fn parse_declarations(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(Declaration, usize), String> {
    let mut declaration_index = start_index;
    let mut declaration = Declaration::new();
    let (declaration_specifier, new_index) =
        parse_declaration_specifiers(tokens, start_index, flattened, str_maps)?;
    declaration.declaration_specifiers = declaration_specifier;
    declaration_index = new_index;
    loop {
        declaration_index += 1;
        if !matches!(
            tokens.get(declaration_index),
            Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
        ) {
            break;
        }
    }
    if !matches!(
        tokens.get(declaration_index),
        Some(lexer::Token::PUNCT_SEMI_COLON)
    ) && !tokens.get(declaration_index).is_none()
    {
        loop {
            let (new_index, declarator) =
                parse_declarator(tokens, declaration_index, flattened, str_maps)?;
            declaration_index = new_index;
            loop {
                if !matches!(
                    tokens.get(declaration_index),
                    Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                ) {
                    break;
                }
                declaration_index += 1;
            }
            if !matches!(
                tokens.get(declaration_index),
                Some(lexer::Token::PUNCT_ASSIGNMENT)
            ) {
                declaration
                    .init_declarator_list
                    .push(InitDeclarator::Declarator(declarator));
                declaration_index += 1;
            } else {
                let start = declaration_index + 1;
                loop {
                    declaration_index += 1;
                    if matches!(
                        tokens.get(declaration_index),
                        Some(lexer::Token::PUNCT_COMMA | lexer::Token::PUNCT_SEMI_COLON) | None
                    ) {
                        break;
                    }
                }
                if !matches!(
                    tokens.get(declaration_index),
                    Some(lexer::Token::PUNCT_COMMA)
                ) {
                    if !matches!(
                        tokens.get(declaration_index),
                        Some(lexer::Token::PUNCT_SEMI_COLON)
                    ) {
                        return Err(error::RccErrorInfo::new(
                            error::RccError::Custom("Expected semi colon".to_string()),
                            start_index..declaration_index,
                            tokens,
                            str_maps,
                        )
                        .to_string());
                    }
                }
                let (_, initializer) =
                    parse_initializer(&tokens[start..declaration_index], 0, flattened, str_maps)?;
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
                tokens.get(declaration_index),
                Some(lexer::Token::PUNCT_SEMI_COLON)
            ) {
                declaration_index += 1;
                break;
            }
            declaration_index += 1;
        }
    }
    Ok((declaration, declaration_index))
}

pub fn parse_initializer(
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
            let starting = index;
            let mut curly_bal_counter = 1;
            loop {
                match tokens.get(index) {
                    Some(lexer::Token::PUNCT_OPEN_CURLY) => curly_bal_counter += 1,
                    Some(lexer::Token::PUNCT_CLOSE_CURLY) => curly_bal_counter -= 1,
                    None => return Err(format!("Missing curly, {:?}", tokens)),
                    _ => {}
                }
                if curly_bal_counter == 0
                    && matches!(tokens.get(index), Some(lexer::Token::PUNCT_CLOSE_CURLY))
                {
                    break;
                }
                index += 1;
            }
            let (new_index, il) =
                parse_initializer_list(&tokens[starting..index], 0, flattened, str_maps)?;
            if il.is_empty() {
                return Err(format!("initializer list is empty"));
            }
            flattened.initializer_lists.push(il);
            Ok((
                new_index,
                Initializer::InitializerList(flattened.initializer_lists.len() - 1),
            ))
        }
        _ => {
            let (new_index, expr) =
                parser::expressions::parse_expressions(tokens, index, flattened, str_maps)?;
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
        if index >= tokens.len() {
            break;
        }
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
                    if !matches!(tokens.get(index - 1), Some(lexer::Token::PUNCT_CLOSE_SQR)) {
                        return Err(format!("No closing sqr bracket"));
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
                    let Some(lexer::Token::IDENT(key)) = tokens.get(index) else {
                        unreachable!()
                    };
                    designation
                        .designator_list
                        .push(Designator::WithIdentifier(*key));
                    index += 1;
                }
                Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE) => index += 1,
                Some(lexer::Token::PUNCT_ASSIGNMENT) => {
                    if designation.designator_list.is_empty() {
                        return Err(format!("Unexpected =, expected . or ["));
                    }
                    index += 1;
                    break;
                }
                _ => {
                    break;
                }
            }
        }
        let starting = index;
        while !matches!(tokens.get(index), Some(lexer::Token::PUNCT_COMMA)) && index < tokens.len()
        {
            index += 1;
        }
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
            let Some(pointer) = pointer_stack.last_mut() else {
                unreachable!()
            };
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

fn parse_direct_declarator(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(usize, DirectDeclarator), String> {
    let mut index = start_index;
    let mut direct_declarator = DirectDeclarator {
        identifier: None,
        declarator: None,
        type_qualifier_list: Vec::new(),
        is_static: false,
        mult: false,
        assign_expr: None,
        parameter_type_list: None,
    };
    match tokens.get(index) {
        Some(lexer::Token::IDENT(key)) => {
            index += 1;
            direct_declarator.identifier = Some(*key);
            while matches!(
                tokens.get(index),
                Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
            ) && index < tokens.len()
            {
                index += 1;
            }
        }
        Some(lexer::Token::PUNCT_OPEN_PAR) => {
            index += 1;
            let starting = index;
            let mut par_bal_counter = 1;
            while par_bal_counter > 0 {
                match tokens.get(index) {
                    Some(lexer::Token::PUNCT_OPEN_PAR) => par_bal_counter += 1,
                    Some(lexer::Token::PUNCT_CLOSE_PAR) => par_bal_counter -= 1,
                    None => return Err(format!("missing )")),
                    Some(_) => {}
                }
                index += 1;
            }
            if !matches!(tokens.get(index - 1), Some(lexer::Token::PUNCT_CLOSE_PAR)) {
                return Err(format!("missing )"));
            }
            let (_, inner_declarator) =
                parse_declarator(&tokens[starting..index - 1], 0, flattened, str_maps)?;
            direct_declarator.declarator = Some(Box::new(inner_declarator));
            while matches!(
                tokens.get(index),
                Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
            ) && index < tokens.len()
            {
                index += 1;
            }
        }
        _ => {
            return Err(format!(
                "Expected identifier or open parentheses, got {:?}",
                tokens.get(index)
            ));
        }
    }
    match tokens.get(index) {
        Some(lexer::Token::PUNCT_OPEN_SQR) => {
            index += 1;
            while matches!(
                tokens.get(index),
                Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
            ) {
                index += 1;
            }
            match tokens.get(index) {
                Some(lexer::Token::PUNCT_MULT) => {
                    index += 1;
                    direct_declarator.mult = true;
                    let mut sqr_balance = 1;
                    loop {
                        match tokens.get(index) {
                            Some(lexer::Token::PUNCT_OPEN_SQR) => sqr_balance += 1,
                            Some(lexer::Token::PUNCT_CLOSE_SQR) => sqr_balance -= 1,
                            None => return Err("Missing ]".to_string()),
                            Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE) => {}
                            Some(_) => return Err(format!("Unexpected {:?}", tokens.get(index))),
                        }
                        if sqr_balance == 0 {
                            break;
                        }
                        index += 1;
                    }
                    index += 1;
                }
                Some(lexer::Token::KEYWORD_STATIC) => {
                    direct_declarator.is_static = true;
                    index += 1;
                    if let Some((new_index, type_qualifiers)) = parse_type_qualifiers(tokens, index)
                    {
                        direct_declarator.type_qualifier_list = type_qualifiers;
                        index = new_index;
                    }
                    let starting = index;
                    let mut sqr_counter = 1;
                    while sqr_counter > 0 {
                        match tokens.get(index) {
                            Some(lexer::Token::PUNCT_OPEN_SQR) => sqr_counter += 1,
                            Some(lexer::Token::PUNCT_CLOSE_SQR) => sqr_counter -= 1,
                            Some(_) => {}
                            None => return Err("missing closing sqr bracket".to_string()),
                        }
                        index += 1;
                    }
                    if !matches!(tokens.get(index - 1), Some(lexer::Token::PUNCT_CLOSE_SQR)) {
                        return Err(format!("missing closing sqr bracket"));
                    }
                    if tokens[starting..index - 1]
                        .iter()
                        .filter(|t| !matches!(t, lexer::Token::WHITESPACE | lexer::Token::NEWLINE))
                        .count()
                        > 0
                    {
                        let (_, expr) = parser::expressions::parse_expressions(
                            &tokens[starting..index - 1],
                            0,
                            flattened,
                            str_maps,
                        )?;
                        flattened.expressions.push(expr);
                        direct_declarator.assign_expr = Some(flattened.expressions.len() - 1);
                        index += 1;
                    }
                }
                _ => {
                    if let Some((new_index, type_qualifiers)) = parse_type_qualifiers(tokens, index)
                    {
                        direct_declarator.type_qualifier_list = type_qualifiers;
                        index = new_index;
                    }
                    while matches!(
                        tokens.get(index),
                        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                    ) {
                        index += 1;
                    }
                    if matches!(tokens.get(index), Some(lexer::Token::KEYWORD_STATIC)) {
                        if direct_declarator.type_qualifier_list.is_empty() {
                            return Err(format!(
                                "Expected type qualifiers, got {:?}",
                                tokens.get(index)
                            ));
                        }
                        direct_declarator.is_static = true;
                        index += 1;
                        let starting = index;
                        let mut sqr_counter = 1;
                        while sqr_counter > 0 {
                            match tokens.get(index) {
                                Some(lexer::Token::PUNCT_OPEN_SQR) => sqr_counter += 1,
                                Some(lexer::Token::PUNCT_CLOSE_SQR) => sqr_counter -= 1,
                                Some(_) => {}
                                None => return Err("missing closing sqr bracket".to_string()),
                            }
                            index += 1;
                        }
                        if !matches!(tokens.get(index - 1), Some(lexer::Token::PUNCT_CLOSE_SQR)) {
                            return Err(format!("missing closing sqr bracket"));
                        }
                        let (_, expr) = parser::expressions::parse_expressions(
                            &tokens[starting..index - 1],
                            0,
                            flattened,
                            str_maps,
                        )?;
                        flattened.expressions.push(expr);
                        direct_declarator.assign_expr = Some(flattened.expressions.len() - 1);
                        index += 1;
                    } else {
                        let starting = index;
                        let mut sqr_counter = 1;
                        while sqr_counter > 0 {
                            match tokens.get(index) {
                                Some(lexer::Token::PUNCT_OPEN_SQR) => sqr_counter += 1,
                                Some(lexer::Token::PUNCT_CLOSE_SQR) => sqr_counter -= 1,
                                Some(_) => {}
                                None => return Err("missing closing sqr bracket".to_string()),
                            }
                            index += 1;
                        }
                        if !matches!(tokens.get(index - 1), Some(lexer::Token::PUNCT_CLOSE_SQR)) {
                            return Err(format!("missing closing sqr bracket"));
                        }
                        if tokens[starting..index - 1]
                            .iter()
                            .filter(|t| {
                                !matches!(t, lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                            })
                            .count()
                            > 0
                        {
                            let (_, expr) = parser::expressions::parse_expressions(
                                &tokens[starting..index - 1],
                                0,
                                flattened,
                                str_maps,
                            )?;
                            flattened.expressions.push(expr);
                            direct_declarator.assign_expr = Some(flattened.expressions.len() - 1);
                            index += 1;
                        }
                    }
                }
            }
        }
        Some(lexer::Token::PUNCT_OPEN_PAR) => {
            index += 1;
            let starting = index;
            let mut parenth_bal_counter = 1;
            loop {
                match tokens.get(index) {
                    Some(lexer::Token::PUNCT_OPEN_PAR) => parenth_bal_counter += 1,
                    Some(lexer::Token::PUNCT_CLOSE_PAR) => parenth_bal_counter -= 1,
                    None => return Err(format!("missing closing parenth")),
                    Some(_) => {}
                }
                if parenth_bal_counter == 0
                    && matches!(tokens.get(index), Some(lexer::Token::PUNCT_CLOSE_PAR))
                {
                    break;
                }
                index += 1;
            }
            let ptl = parse_parameter_type_list(&tokens[starting..index], flattened, str_maps)?;
            direct_declarator.parameter_type_list = Some(ptl);
        }
        _ => {}
    }
    Ok((index, direct_declarator))
}

fn parse_declarator(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(usize, Declarator), String> {
    let mut index = start_index;
    let mut declarator = Declarator {
        pointer: Vec::new(),
        direct_declarator: None,
    };
    if let Some((new_index, pointers)) = parse_pointer(tokens, index) {
        declarator.pointer = pointers;
        index = new_index;
    }
    while matches!(
        tokens.get(index),
        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
    ) && index < tokens.len()
    {
        index += 1;
    }
    let (new_index, direct_declarator) =
        parse_direct_declarator(tokens, index, flattened, str_maps)?;
    index = new_index;
    declarator.direct_declarator = Some(direct_declarator);
    Ok((index, declarator))
}

fn parse_struct_declarator(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(usize, StructDeclarator), String> {
    let mut index = start_index;
    let mut struct_declarator = StructDeclarator {
        declarator: None,
        const_expr: None,
    };
    while matches!(
        tokens.get(index),
        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
    ) {
        index += 1;
    }
    if matches!(tokens.get(index), Some(lexer::Token::PUNCT_COLON)) {
        index += 1;
        let (new_index, const_expr) =
            parser::expressions::parse_expressions(&tokens[index..], 0, flattened, str_maps)?;
        flattened.expressions.push(const_expr);
        struct_declarator.const_expr = Some(flattened.expressions.len() - 1);
        index += new_index;
    } else {
        let (new_index, declarator) = parse_declarator(tokens, index, flattened, str_maps)?;
        index = new_index;
        struct_declarator.declarator = Some(declarator);
        while matches!(
            tokens.get(index),
            Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
        ) {
            index += 1;
        }
        if matches!(tokens.get(index), Some(lexer::Token::PUNCT_COLON)) {
            index += 1;
            let (new_index, const_expr) =
                parser::expressions::parse_expressions(&tokens[index..], 0, flattened, str_maps)?;
            flattened.expressions.push(const_expr);
            struct_declarator.const_expr = Some(flattened.expressions.len() - 1);
            index = new_index;
        }
    }
    Ok((index, struct_declarator))
}

fn parse_struct_declaration(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(usize, StructDeclaration), String> {
    let mut struct_declaration = StructDeclaration {
        specifier_qualifier_list: SpecifierQualifierList::new(),
        struct_declarator_list: Vec::new(),
    };
    let mut index = start_index;
    while matches!(
        tokens.get(index),
        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
    ) {
        index += 1;
    }
    let (new_index, specifier_qualifier_list) =
        parse_specifiers_qualifiers(tokens, index, flattened, str_maps)?;
    struct_declaration.specifier_qualifier_list = specifier_qualifier_list;
    index = new_index;
    let starting = index;
    while !matches!(tokens.get(index), Some(lexer::Token::PUNCT_SEMI_COLON)) && index < tokens.len()
    {
        index += 1;
    }
    if !matches!(tokens.get(index), Some(lexer::Token::PUNCT_SEMI_COLON)) {
        return Err(format!("Expected ;, got {:?}", tokens.get(index)));
    }
    let mut parse_struct_declarator_idx = starting;
    if tokens[starting..index]
        .iter()
        .filter(|t| !matches!(t, lexer::Token::WHITESPACE | lexer::Token::NEWLINE))
        .count()
        == 0
    {
        return Ok((index, struct_declaration));
    }
    while parse_struct_declarator_idx < index {
        let (new_index, struct_declarator) = parse_struct_declarator(
            &tokens[0..index],
            parse_struct_declarator_idx,
            flattened,
            str_maps,
        )?;
        parse_struct_declarator_idx = new_index;
        struct_declaration
            .struct_declarator_list
            .push(struct_declarator);
    }
    Ok((index + 1, struct_declaration))
}

fn parse_struct_union_specifier(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
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
    while matches!(
        tokens.get(index),
        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
    ) {
        index += 1;
    }
    if let Some(lexer::Token::IDENT(key)) = tokens.get(index) {
        struct_union_specifier.identifier = Some(*key);
        index += 1;
    }
    while matches!(
        tokens.get(index),
        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
    ) {
        index += 1;
    }
    if matches!(tokens.get(index), None) {
        return Ok((index, struct_union_specifier));
    }
    if !matches!(tokens.get(index), Some(lexer::Token::PUNCT_OPEN_CURLY)) {
        return Err(format!(
            "Expected open curly bracket, got {:?}",
            tokens.get(index)
        ));
    }
    index += 1;
    let starting = index;
    let mut curly_counter = 1;
    while curly_counter > 0 {
        match tokens.get(index) {
            Some(lexer::Token::PUNCT_OPEN_CURLY) => curly_counter += 1,
            Some(lexer::Token::PUNCT_CLOSE_CURLY) => curly_counter -= 1,
            Some(_) => {}
            None => return Err("missing closing curly".to_string()),
        }
        index += 1;
    }
    if !matches!(tokens.get(index - 1), Some(lexer::Token::PUNCT_CLOSE_CURLY)) {
        return Err("missing closing curly".to_string());
    }
    let mut parse_struct_declarator_idx = starting;
    while parse_struct_declarator_idx < index - 1 {
        let (new_index, struct_declaration) =
            parse_struct_declaration(tokens, parse_struct_declarator_idx, flattened, str_maps)?;
        parse_struct_declarator_idx = new_index;
        struct_union_specifier
            .struct_declaration_list
            .push(struct_declaration);
        while matches!(
            tokens.get(parse_struct_declarator_idx),
            Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
        ) {
            parse_struct_declarator_idx += 1;
        }
    }
    Ok((index, struct_union_specifier))
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
    Ok((index + 1, enum_specifier))
}

pub fn parse_specifiers_qualifiers(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(usize, SpecifierQualifierList), String> {
    let mut index = start_index;
    let mut specifier_qualifier = SpecifierQualifierList::new();
    loop {
        if let Ok((next_index, mut specifiers)) =
            parse_type_specifiers(tokens, index, flattened, str_maps)
        {
            // Avoids cloning
            while let Some(type_specifier) = specifiers.pop() {
                specifier_qualifier.type_specifiers.push(type_specifier);
            }
            index = next_index;
        }
        if let Some((next_index, mut qualifiers)) = parse_type_qualifiers(tokens, index) {
            while let Some(type_qualifier) = qualifiers.pop() {
                specifier_qualifier.type_qualifiers.push(type_qualifier);
            }
            index = next_index;
        } else {
            break;
        }
    }
    Ok((index, specifier_qualifier))
}

pub fn parse_type_qualifiers(
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

pub fn parse_type_specifiers(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
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
                let (new_index, struct_union_specifier) =
                    parse_struct_union_specifier(tokens, index, flattened, str_maps)?;
                index = new_index;
                type_specifiers.push(TypeSpecifier::StructUnion(struct_union_specifier));
                continue;
            }
            lexer::Token::KEYWORD__ATOMIC => todo!(),
            lexer::Token::KEYWORD_ENUM => {
                let (new_index, enum_specifier) =
                    parse_enumerator_specifier(tokens, index, str_maps)?;
                index = new_index;
                type_specifiers.push(TypeSpecifier::Enum(enum_specifier));
                continue;
            }
            // TODO: typedef identifiers
            lexer::Token::IDENT(key) if type_specifiers.is_empty() => {
                type_specifiers.push(TypeSpecifier::IdentTypeDef(key));
                break;
            }
            _ => break,
        }
        index += 1;
    }
    Ok((index, type_specifiers))
}

pub fn parse_declaration_specifiers(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(DeclarationSpecifier, usize), String> {
    let mut declaration_specifier = DeclarationSpecifier::new();
    let mut declaration_specifier_idx = start_index;
    while declaration_specifier_idx < tokens.len() {
        match tokens[declaration_specifier_idx] {
            lexer::Token::KEYWORD_TYPEDEF => declaration_specifier
                .storage_class_specifiers
                .push(StorageClassSpecifier::TypeDef),
            lexer::Token::KEYWORD_EXTERN => declaration_specifier
                .storage_class_specifiers
                .push(StorageClassSpecifier::Extern),
            lexer::Token::KEYWORD_STATIC => declaration_specifier
                .storage_class_specifiers
                .push(StorageClassSpecifier::Static),
            lexer::Token::KEYWORD__THREAD_LOCAL => declaration_specifier
                .storage_class_specifiers
                .push(StorageClassSpecifier::ThreadLocal),
            lexer::Token::KEYWORD_AUTO => declaration_specifier
                .storage_class_specifiers
                .push(StorageClassSpecifier::Auto),
            lexer::Token::KEYWORD_REGISTER => declaration_specifier
                .storage_class_specifiers
                .push(StorageClassSpecifier::Register),
            lexer::Token::KEYWORD_VOID => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Void),
            lexer::Token::KEYWORD_CHAR => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Char),
            lexer::Token::KEYWORD_SHORT => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Short),
            lexer::Token::KEYWORD_INT => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Int),
            lexer::Token::KEYWORD_LONG => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Long),
            lexer::Token::KEYWORD_FLOAT => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Float),
            lexer::Token::KEYWORD_DOUBLE => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Double),
            lexer::Token::KEYWORD_SIGNED => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Signed),
            lexer::Token::KEYWORD_UNSIGNED => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::Unsigned),
            lexer::Token::KEYWORD__BOOL => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::_Bool),
            lexer::Token::KEYWORD__COMPLEX => declaration_specifier
                .type_specifiers
                .push(TypeSpecifier::_Complex),
            lexer::Token::KEYWORD_STRUCT | lexer::Token::KEYWORD_UNION => todo!(),
            lexer::Token::KEYWORD_ENUM => todo!(),
            lexer::Token::KEYWORD_CONST => declaration_specifier
                .type_qualifiers
                .push(TypeQualifier::Const),
            lexer::Token::KEYWORD_RESTRICT => declaration_specifier
                .type_qualifiers
                .push(TypeQualifier::Restrict),
            lexer::Token::KEYWORD_VOLATILE => declaration_specifier
                .type_qualifiers
                .push(TypeQualifier::Volatile),
            lexer::Token::KEYWORD__ATOMIC => {
                let mut next_index = declaration_specifier_idx + 1;
                while matches!(
                    tokens.get(next_index),
                    Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                ) {
                    next_index += 1;
                }
                if let Some(lexer::Token::PUNCT_OPEN_PAR) = tokens.get(next_index) {
                    let (new_index, _type_name) =
                        parse_type_names(&tokens, next_index + 1, flattened, str_maps)?;
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
            lexer::Token::KEYWORD_INLINE => declaration_specifier
                .function_specifiers
                .push(FunctionSpecifier::Inline),
            lexer::Token::KEYWORD__NORETURN => declaration_specifier
                .function_specifiers
                .push(FunctionSpecifier::_Noreturn),
            lexer::Token::KEYWORD__ALIGNAS => todo!(),
            _ => break,
        }
        declaration_specifier_idx += 1;
    }
    Ok((declaration_specifier, declaration_specifier_idx))
}

pub fn parse_parameter_type_list(
    tokens: &[lexer::Token],
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<ParameterTypeList, String> {
    let mut index = 0;
    let mut ptl = ParameterTypeList {
        parameter_declarations: Vec::new(),
        ellipsis: false,
    };
    while index < tokens.len() {
        let (declaration_specifier, new_index) =
            parse_declaration_specifiers(tokens, index, flattened, str_maps)?;
        index = new_index;
        let starting = index;
        let has_identifier = {
            let mut identifier_idx = starting;
            while matches!(
                tokens.get(identifier_idx),
                Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
            ) {
                identifier_idx += 1;
            }
            if matches!(tokens.get(identifier_idx), Some(lexer::Token::PUNCT_MULT)) {
                identifier_idx += 1;
            }
            while matches!(
                tokens.get(identifier_idx),
                Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
            ) {
                identifier_idx += 1;
            }
            match tokens.get(identifier_idx) {
                Some(lexer::Token::IDENT(_)) => true,
                Some(lexer::Token::PUNCT_OPEN_PAR) => {
                    let mut has_ident = false;
                    let mut parenth_counter = 1;
                    let mut parenth_idx = identifier_idx + 1;
                    while parenth_counter > 0 {
                        match tokens.get(parenth_idx) {
                            Some(lexer::Token::PUNCT_OPEN_PAR) => parenth_counter += 1,
                            Some(lexer::Token::PUNCT_CLOSE_PAR) => parenth_counter -= 1,
                            Some(lexer::Token::IDENT(_)) => {
                                has_ident = true;
                            }
                            Some(_) => {}
                            None => return Err("unbalanced parenths".to_string()),
                        }
                        parenth_idx += 1;
                    }
                    if !matches!(
                        tokens.get(parenth_idx - 1),
                        Some(lexer::Token::PUNCT_CLOSE_PAR)
                    ) {
                        return Err("unbalanced parenths".to_string());
                    }
                    has_ident
                }
                Some(_) => {
                    return Err(format!("unexpected token {:?}", tokens.get(identifier_idx)))
                }
                None => return Err("unexpected end of tokens".to_string()),
            }
        };
        while !matches!(tokens.get(index), Some(lexer::Token::PUNCT_COMMA)) && index < tokens.len()
        {
            index += 1;
        }
        if has_identifier {
            let (_, declarator) =
                parse_declarator(&tokens[starting..index], 0, flattened, str_maps)?;
            ptl.parameter_declarations
                .push(ParameterDeclaration::WithDeclarator {
                    declaration_specifier,
                    declarator,
                });
        } else {
            let (_, ab) =
                parse_abstract_declarator(&tokens[starting..index], 0, flattened, str_maps)?;
            ptl.parameter_declarations
                .push(ParameterDeclaration::WithOptionalAbstractDeclarator {
                    abstract_declarator: Some(ab),
                    declaration_specifier,
                });
        }
        index += 1;
        while matches!(
            tokens.get(index),
            Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
        ) {
            index += 1;
        }
        if matches!(tokens.get(index), Some(lexer::Token::PUNCT_ELLIPSIS)) {
            ptl.ellipsis = true;
        }
        while matches!(
            tokens.get(index),
            Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
        ) {
            index += 1;
        }
        if ptl.ellipsis && index < tokens.len() {
            return Err(format!(
                "ellipsis can only be at end of parameter type list"
            ));
        }
    }
    Ok(ptl)
}

pub fn parse_direct_abstract_declarator(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(usize, DirectAbstractDeclarator), String> {
    let mut index = start_index;
    let mut dad = DirectAbstractDeclarator {
        type_qualifiers: Vec::new(),
        abstract_declarator: None,
        is_static: false,
        mult: false,
        parameter_type_list: None,
        assign_expr: None,
    };
    if matches!(tokens.get(index), Some(lexer::Token::PUNCT_OPEN_PAR)) {
        index += 1;
        let starting = index;
        let mut parenth_bal_counter = 1;
        loop {
            match tokens.get(index) {
                Some(lexer::Token::PUNCT_OPEN_PAR) => parenth_bal_counter += 1,
                Some(lexer::Token::PUNCT_CLOSE_PAR) => parenth_bal_counter -= 1,
                None => return Err(format!("missing closing parenth")),
                Some(_) => {}
            }
            if parenth_bal_counter == 0
                && matches!(tokens.get(index), Some(lexer::Token::PUNCT_CLOSE_PAR))
            {
                break;
            }
            index += 1;
        }
        let mut first_token_inside_idx = starting;
        while matches!(
            tokens.get(first_token_inside_idx),
            Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
        ) && first_token_inside_idx < tokens.len()
        {
            first_token_inside_idx += 1;
        }
        if matches!(
            tokens.get(first_token_inside_idx),
            Some(
                lexer::Token::PUNCT_MULT
                    | lexer::Token::PUNCT_OPEN_PAR
                    | lexer::Token::PUNCT_OPEN_SQR
            )
        ) {
            let (_, abstract_declarator) =
                parse_abstract_declarator(&tokens[starting..index], 0, flattened, str_maps)?;
            dad.abstract_declarator = Some(Box::new(abstract_declarator));
        } else {
            let ptl = parse_parameter_type_list(&tokens[starting..index], flattened, str_maps)?;
            dad.parameter_type_list = Some(ptl);
        }
        index += 1;
        while matches!(
            tokens.get(index),
            Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
        ) && index < tokens.len()
        {
            index += 1;
        }
    }
    match tokens.get(index) {
        Some(lexer::Token::PUNCT_OPEN_SQR) => {
            index += 1;
            while matches!(
                tokens.get(index),
                Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
            ) {
                index += 1;
            }
            match tokens.get(index) {
                Some(lexer::Token::PUNCT_MULT) => {
                    index += 1;
                    dad.mult = true;
                    let mut sqr_balance = 1;
                    loop {
                        match tokens.get(index) {
                            Some(lexer::Token::PUNCT_OPEN_SQR) => sqr_balance += 1,
                            Some(lexer::Token::PUNCT_CLOSE_SQR) => sqr_balance -= 1,
                            None => return Err("Missing ]".to_string()),
                            Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE) => {}
                            Some(_) => return Err(format!("Unexpected {:?}", tokens.get(index))),
                        }
                        if sqr_balance == 0 {
                            break;
                        }
                        index += 1;
                    }
                    index += 1;
                }
                Some(lexer::Token::KEYWORD_STATIC) => {
                    dad.is_static = true;
                    index += 1;
                    if let Some((new_index, type_qualifiers)) = parse_type_qualifiers(tokens, index)
                    {
                        dad.type_qualifiers = type_qualifiers;
                        index = new_index;
                    }
                    let starting = index;
                    let mut sqr_counter = 1;
                    while sqr_counter > 0 {
                        match tokens.get(index) {
                            Some(lexer::Token::PUNCT_OPEN_SQR) => sqr_counter += 1,
                            Some(lexer::Token::PUNCT_CLOSE_SQR) => sqr_counter -= 1,
                            Some(_) => {}
                            None => return Err("missing closing sqr bracket".to_string()),
                        }
                        index += 1;
                    }
                    if !matches!(tokens.get(index - 1), Some(lexer::Token::PUNCT_CLOSE_SQR)) {
                        return Err("missing closing sqr bracket".to_string());
                    }
                    if tokens[starting..index - 1]
                        .iter()
                        .filter(|t| !matches!(t, lexer::Token::WHITESPACE | lexer::Token::NEWLINE))
                        .count()
                        > 0
                    {
                        let (_, expr) = parser::expressions::parse_expressions(
                            &tokens[starting..index - 1],
                            0,
                            flattened,
                            str_maps,
                        )?;
                        flattened.expressions.push(expr);
                        dad.assign_expr = Some(flattened.expressions.len() - 1);
                        index += 1;
                    }
                }
                _ => {
                    if let Some((new_index, type_qualifiers)) = parse_type_qualifiers(tokens, index)
                    {
                        dad.type_qualifiers = type_qualifiers;
                        index = new_index;
                    }
                    while matches!(
                        tokens.get(index),
                        Some(lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                    ) {
                        index += 1;
                    }
                    if matches!(tokens.get(index), Some(lexer::Token::KEYWORD_STATIC)) {
                        if dad.type_qualifiers.is_empty() {
                            return Err(format!(
                                "Expected type qualifiers, got {:?}",
                                tokens.get(index)
                            ));
                        }
                        dad.is_static = true;
                        index += 1;
                        let starting = index;
                        let mut sqr_counter = 1;
                        while sqr_counter > 0 {
                            match tokens.get(index) {
                                Some(lexer::Token::PUNCT_OPEN_SQR) => sqr_counter += 1,
                                Some(lexer::Token::PUNCT_CLOSE_SQR) => sqr_counter -= 1,
                                Some(_) => {}
                                None => return Err("missing closing sqr bracket".to_string()),
                            }
                            index += 1;
                        }
                        if !matches!(tokens.get(index - 1), Some(lexer::Token::PUNCT_CLOSE_SQR)) {
                            return Err("missing closing sqr bracket".to_string());
                        }
                        let (_, expr) = parser::expressions::parse_expressions(
                            &tokens[starting..index - 1],
                            0,
                            flattened,
                            str_maps,
                        )?;
                        flattened.expressions.push(expr);
                        dad.assign_expr = Some(flattened.expressions.len() - 1);
                        index += 1;
                    } else {
                        let starting = index;
                        let mut sqr_counter = 1;
                        while sqr_counter > 0 {
                            match tokens.get(index) {
                                Some(lexer::Token::PUNCT_OPEN_SQR) => sqr_counter += 1,
                                Some(lexer::Token::PUNCT_CLOSE_SQR) => sqr_counter -= 1,
                                Some(_) => {}
                                None => return Err("missing closing sqr bracket".to_string()),
                            }
                            index += 1;
                        }
                        if !matches!(tokens.get(index - 1), Some(lexer::Token::PUNCT_CLOSE_SQR)) {
                            return Err("missing closing sqr bracket".to_string());
                        }
                        if tokens[starting..index - 1]
                            .iter()
                            .filter(|t| {
                                !matches!(t, lexer::Token::WHITESPACE | lexer::Token::NEWLINE)
                            })
                            .count()
                            > 0
                        {
                            let (_, expr) = parser::expressions::parse_expressions(
                                &tokens[starting..index - 1],
                                0,
                                flattened,
                                str_maps,
                            )?;
                            flattened.expressions.push(expr);
                            dad.assign_expr = Some(flattened.expressions.len() - 1);
                            index += 1;
                        }
                    }
                }
            }
        }
        Some(lexer::Token::PUNCT_OPEN_PAR) if dad.abstract_declarator.is_some() => {
            index += 1;
            let starting = index;
            let mut parenth_bal_counter = 1;
            loop {
                match tokens.get(index) {
                    Some(lexer::Token::PUNCT_OPEN_PAR) => parenth_bal_counter += 1,
                    Some(lexer::Token::PUNCT_CLOSE_PAR) => parenth_bal_counter -= 1,
                    None => return Err(format!("missing closing parenth")),
                    Some(_) => {}
                }
                if parenth_bal_counter == 0
                    && matches!(tokens.get(index), Some(lexer::Token::PUNCT_CLOSE_PAR))
                {
                    break;
                }
                index += 1;
            }
            let ptl = parse_parameter_type_list(&tokens[starting..index], flattened, str_maps)?;
            dad.parameter_type_list = Some(ptl);
        }
        _ => {}
    }
    Ok((index, dad))
}

pub fn parse_abstract_declarator(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
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
    if index < tokens.len() {
        let (new_index, dad) =
            parse_direct_abstract_declarator(tokens, index, flattened, str_maps)?;
        index = new_index;
        ad.direct_abstract_declarator = Some(dad);
    }
    Ok((index, ad))
}

pub fn parse_type_names(
    tokens: &[lexer::Token],
    start_index: usize,
    flattened: &mut parser::Flattened,
    str_maps: &mut lexer::ByteVecMaps,
) -> Result<(usize, TypeName), String> {
    let mut index = start_index;
    let mut type_name = TypeName::new();
    let (new_index, specifier_qualifier_list) =
        parse_specifiers_qualifiers(tokens, index, flattened, str_maps)?;
    type_name.specifier_qualifier_list = specifier_qualifier_list;
    index = new_index;
    if index < tokens.len() {
        let (new_index, ad) = parse_abstract_declarator(tokens, index, flattened, str_maps)?;
        index = new_index;
        type_name.abstract_declarator = Some(ad);
    }
    Ok((index, type_name))
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
            assert!(matches!(
                expr,
                parser::expressions::Expr::Primary(Some(parser::expressions::PrimaryInner::Token(
                    _
                )))
            ));

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
            assert!(matches!(
                expr,
                parser::expressions::Expr::Primary(Some(parser::expressions::PrimaryInner::Token(
                    _
                )))
            ));
        }
        {
            let src = r#"{}"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut flattened = parser::Flattened::new();
            let tokens = lexer::lexer(src, false, &mut str_maps)?;
            assert!(parse_initializer(&tokens, 0, &mut flattened, &mut str_maps).is_err());
        }
        {
            let src = r#"{ {[100] = 5}, 8, .baz = "" }"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut flattened = parser::Flattened::new();
            let tokens = lexer::lexer(src, false, &mut str_maps)?;
            let (_, i) = parse_initializer(&tokens, 0, &mut flattened, &mut str_maps)?;
        }
        {
            let src = r#"{ 8, 8, .baz = "" }"#.as_bytes();
            let mut str_maps = lexer::ByteVecMaps::new();
            let mut flattened = parser::Flattened::new();
            let tokens = lexer::lexer(src, false, &mut str_maps)?;
            let (_, i) = parse_initializer(&tokens, 0, &mut flattened, &mut str_maps)?;
        }
        Ok(())
    }
    #[test]
    fn parse_type_names_test_pointer_to_int_array_size_3() -> Result<(), String> {
        let src = r#"int (*)[3]"#.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut flattened = parser::Flattened::new();
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
        assert!(matches!(
            flattened.expressions[expr_idx],
            parser::expressions::Expr::Primary(_)
        ));
        let parser::expressions::Expr::Primary(Some(parser::expressions::PrimaryInner::Token(t))) =
            flattened.expressions[expr_idx]
        else {
            unreachable!()
        };
        let lexer::Token::CONSTANT_DEC_INT { value_key, .. } = t else {
            unreachable!()
        };
        assert_eq!(value_key, str_maps.add_byte_vec(b"3"));
        Ok(())
    }
    #[test]
    fn parse_type_names_test_pointer_to_variable_length_int_array() -> Result<(), String> {
        let src = r#"int (*)[*]"#.as_bytes();
        let mut str_maps = lexer::ByteVecMaps::new();
        let mut flattened = parser::Flattened::new();
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
            let mut flattened = parser::Flattened::new();
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
        let mut flattened = parser::Flattened::new();
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
            let mut flattened = parser::Flattened::new();
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
            let mut flattened = parser::Flattened::new();
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
            let mut flattened = parser::Flattened::new();
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
            let mut flattened = parser::Flattened::new();
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
            let mut flattened = parser::Flattened::new();
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
        Ok(())
    }
}
