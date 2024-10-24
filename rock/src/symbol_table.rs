use std::collections::HashSet;
use std::collections::{hash_map::Entry, HashMap};
use std::fmt::Write;

// use log::info;

use crate::source_code::SourceFiles;
use crate::{
    ast,
    source_code::{LineCol, Span},
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ScopeId {
    Root,
    Block(ast::AstNodeId),
}

#[derive(Debug, Clone)]
pub struct ScopeInfo {
    symbols: HashMap<String, SymbolId>,
    parent: Option<ScopeId>,
}

impl ScopeInfo {
    pub fn new_root() -> Self {
        Self {
            symbols: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_child(parent: ScopeId) -> Self {
        Self {
            symbols: HashMap::new(),
            parent: Some(parent),
        }
    }
}

pub struct ScopeBuilder {
    pub table: SymbolTable,
    current_scope: ScopeId,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BlockId(ast::AstNodeId);

#[derive(Debug, Clone)]
pub struct BlockInfo {
    /// The returned type of this block. Should be the same as the type of the
    /// wrapping Block expression but sometimes we don't have a block epression.
    pub ty: TypeId,
    /// Set to true when this block will unconditionally diverge (i.e. return or
    /// panic) and will never yield a result.
    pub is_diverging: bool,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ExpressionId(ast::AstNodeId);

#[derive(Debug, Clone)]
pub struct ExpressionInfo {
    pub ty: TypeId,
    /// Filled during the typecheck pass. Indicates whether this expression is a
    /// place in memory that can be taken a reference to.
    pub is_lvalue: bool,
    /// Used when the expression is a method call, but the base type self is a
    /// pointer or Ref to the type that actually holds the method. This will
    /// make the code generator emit an extra deref.
    pub needs_deref_before_method_call: bool,
}

/// The name of a symbol is a stable unique identifier for a symbol, unlike the
/// symbol id. One can think of it as the fully qualified name of the symbol.
///
/// The need for a symbol name arises from the fact that multiple reloads of the
/// same module can lead to different symbol ids for what's conceptually the
/// same symbol. A symbol id only has meaning within a compilation, whereas a
/// SymbolName can be used to identify a symbol across compilations.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SymbolName {
    name: ustr::Ustr,
}

impl SymbolName {
    pub fn root() -> Self {
        Self {
            name: ustr::Ustr::from(""),
        }
    }

    pub fn child(&self, name: &str) -> Self {
        Self {
            name: ustr::Ustr::from(&format!("{}::{}", self.name, name)),
        }
    }

    pub fn unqualified_name(&self) -> String {
        self.name.as_str().split("::").last().unwrap().to_string()
    }

    pub fn qualified_name(&self) -> String {
        self.name.as_str().to_string()
    }
}

/// The symbol information is the main way a symbol table maps nodes of the AST
/// (mainly, identifiers) to symbol data, with different kinds of symbols
/// representing different language constructs.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SymbolInfo {
    /// The name of the symbol. This is a unique identifier for the symbol that
    /// is stable across reloads.
    ///
    /// This may be None for symbols that don't have a stable name, such as
    /// local variables.
    pub name: Option<SymbolName>,
    /// Stores the text of the identifier that this symbol corresponds to. This
    /// useful when emitting compiler diagnostics and in some very specific
    /// situations, but this value should not be relied upon to uniquely
    /// identify a symbol. Use `name` instead.
    pub ident_text: ustr::Ustr,
    pub ty: TypeId,
    pub kind: SymbolInfoKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SymbolInfoKind {
    /// A symbol representing a local variable declaration.
    LocalVariable,
    /// A symbol representing a global variable declaration.
    GlobalVariable,
    // /// A function symbol. Represents a function declaration. The `args` contain
    // /// symbol references to each of the function arguments.
    Function {
        args: Vec<SymbolId>,
    },
    // /// Same as a function symbol, but for method declarations. Note that `self`
    // /// is not a symbol and is thus not included in the `args` list. Any method
    // /// symbol is assumed to have a self first argument.
    // Method { args: Vec<SymbolId> },
    // /// A struct symbol. Represents a struct declaration. The inner field
    // /// symbols can be used to access the information of each field.
    // Struct { fields: Vec<SymbolId> },
    // /// The symbol representing a field in a declared struct.
    // StructField,
    // /// An enum symbol. Represents an enum declaration. The inner variant
    // /// symbols can be used to access the information of each variant.
    // Enum { variants: Vec<SymbolId> },
    // /// The symbol representing a variant in a declared struct.
    // EnumVariant { parent_enum: SymbolId, tag: usize },
}

impl SymbolInfoKind {
    pub fn is_named_type(&self) -> bool {
        false
    }
}

#[derive(Debug, Clone)]
pub struct TypeTable {
    type_id_gen: u32,
    types: HashMap<TypeId, TypeInfo>,
}

impl TypeTable {
    const TYPE_UNKNOWN: TypeId = TypeId::from_u32(0);

    fn new() -> Self {
        let mut types = HashMap::new();

        types.insert(TypeId(0), TypeInfo::Unknown);

        Self {
            type_id_gen: types.len() as u32,
            types,
        }
    }

    fn gen_type_id(&mut self) -> TypeId {
        let typeid = TypeId(self.type_id_gen);
        self.type_id_gen += 1;
        typeid
    }

    /// Registers the provided type and returns its type id. This method performs
    /// type deduplication. If the type already exists in the table, the existing
    /// type id is returned.
    pub fn get_or_register_type(
        &mut self,
        ty: TypeInfo,
        reserved_type_id: Option<TypeId>,
    ) -> TypeId {
        if let Some((typeid, _)) = self.types.iter().find(|(_, t)| *t == &ty) {
            *typeid
        } else {
            let id = reserved_type_id.unwrap_or_else(|| self.gen_type_id());
            self.types.insert(id, ty);
            id
        }
    }

    pub fn get_type_info(&self, id: TypeId) -> &TypeInfo {
        self.types.get(&id).expect("Typeid not found in type table")
    }

    pub fn type_to_string(&self, id: TypeId) -> String {
        let info = self.types.get(&id).expect("Typeid not found in type table");
        match info {
            // TypeInfo::Struct(info) => {
            //     info.name.unqualified_name()
            //
            //     /* NOTE: This will be useful once we add anonymous structs.
            //     let mut out = String::from("{");
            //     let mut first = true;
            //     for (field_name, field) in &info.fields {
            //         if !first {
            //             write!(out, ", ").unwrap();
            //         }
            //         first = false;
            //         write!(out, "{field_name}: {}", self.type_to_string(*field)).unwrap();
            //     }
            //     write!(out, "}}").unwrap();
            //     out*/
            // }
            // TypeInfo::Enum(info) => info.name.unqualified_name(),
            TypeInfo::Function(info) => {
                let mut out = String::new();
                write!(out, "fn(").unwrap();
                match &info.kind {
                    super::FunctionKind::Plain { args } => {
                        let mut first = true;
                        for arg in args {
                            if !first {
                                write!(out, ", ").unwrap();
                            }
                            first = false;
                            write!(out, "{}", self.type_to_string(*arg)).unwrap();
                        }
                    }
                    super::FunctionKind::Method { self_ty, args } => {
                        write!(out, "self: {}", self.type_to_string(*self_ty)).unwrap();
                        for arg in args {
                            write!(out, ", {}", self.type_to_string(*arg)).unwrap();
                        }
                    }
                }

                write!(out, ") -> {}", self.type_to_string(info.ret)).unwrap();
                out
            }
            TypeInfo::Primitive(info) => info.primitive.to_string(),
            TypeInfo::String => "String".to_string(),
            TypeInfo::Unit => "()".to_string(),
            TypeInfo::Never => "!".to_string(),
            TypeInfo::Unknown => "<unknown>".to_string(),
            TypeInfo::PartiallyResolved => "<partially_resolved>".to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypeInfo {
    // Struct(StructTypeInfo),
    // Enum(EnumTypeInfo),
    Function(FunctionTypeInfo),
    Primitive(PrimitiveTypeInfo),
    // RawPtr(RawPtrTypeInfo),
    // ScopedPtr(ScopedPtrTypeInfo),
    // Ref(RefTypeInfo),
    String,
    // Vec(VecTypeInfo),
    /// Used for expressions that yield nothing.
    Unit,
    /// Used for expressions that *never* yield, such as return or panic
    /// statements.
    Never,
    // This variant is only used during type resolution. After the type
    // resolution pass, no symbol should have this type.
    Unknown,
    // A type that is not fully resolved, such as an integer or floating point
    // literal.
    PartiallyResolved,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FunctionKind {
    Plain { args: Vec<TypeId> },
    Method { self_ty: TypeId, args: Vec<TypeId> },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionTypeInfo {
    pub ret: TypeId,
    pub kind: FunctionKind,
    pub text_name: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PrimitiveTypeInfo {
    pub primitive: ast::PrimitiveType,
}

#[derive(Debug, Clone)]
pub struct SymbolRef {
    pub symbol: SymbolId,
    pub span: Span,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SymbolRefId(ast::AstNodeId);

impl SymbolRefId {
    pub fn from_u32(id: u32) -> Self {
        Self(ast::AstNodeId::from_u32(id))
    }

    pub fn to_u32(&self) -> u32 {
        self.0.to_u32()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SymbolId(ast::AstNodeId);

impl SymbolId {
    pub fn from_u32(id: u32) -> Self {
        Self(ast::AstNodeId::from_u32(id))
    }

    pub fn to_u32(&self) -> u32 {
        self.0.to_u32()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeId(u32);

impl TypeId {
    pub const fn from_u32(id: u32) -> Self {
        Self(id)
    }

    pub const fn to_u32(&self) -> u32 {
        self.0
    }
}

pub struct SymbolTable {
    pub symbols: HashMap<SymbolId, SymbolInfo>,
    pub symbol_refs: HashMap<SymbolRefId, SymbolRef>,
    pub scopes: HashMap<ScopeId, ScopeInfo>,
    pub expressions: HashMap<ExpressionId, ExpressionInfo>,
    pub blocks: HashMap<BlockId, BlockInfo>,
    pub types: TypeTable,
    pub type_symbol_usages: HashMap<SymbolId, TypeId>,
    pub root_scope: ScopeId,
    // pub methods: HashMap<TypeId, HashMap<MethodName, SymbolId>>,
    // pub operator_overloads: OperatorOverloads,
    // pub codegen_info: CodegenInfo,
    pub symbol_id_to_name: HashMap<SymbolId, SymbolName>,
    // pub global_metadata: HashMap<SymbolId, GlobalMetadata>,
}

impl SymbolTable {
    // Create a new SymbolTable
    pub fn new() -> Self {
        let root_scope = ScopeInfo::new_root();
        let mut scopes = HashMap::new();
        scopes.insert(ScopeId::Root, root_scope);
        Self {
            symbols: HashMap::new(),
            symbol_refs: HashMap::new(),
            scopes,
            expressions: HashMap::new(),
            blocks: HashMap::new(),
            root_scope: ScopeId::Root,
            types: TypeTable::new(),
            type_symbol_usages: HashMap::new(),
            // methods: HashMap::new(),
            // operator_overloads: Default::default(),
            // codegen_info: Default::default(),
            symbol_id_to_name: HashMap::new(),
            // global_metadata: HashMap::new(),
        }
    }

    pub fn query_definition_at(&self, pos: LineCol) -> Option<SymbolId> {
        for symbol_ref in self.symbol_refs.values() {
            if symbol_ref.span.contains_loc(&pos) {
                return Some(symbol_ref.symbol);
            }
        }

        None
    }

    pub fn span_of_symbol(&self, symbol: SymbolId) -> Span {
        self.symbols.get(&symbol).as_ref().unwrap().span.clone()
    }

    /// Registers the given type info in the type table and returns its type id.
    /// The type table performs deduplication, so the same TypeInfo can be passed
    /// multiple times and the same type id will be returned.
    ///
    /// NOTE: Do not use this function to register struct or other named user
    /// types. Instead, use `register_named_type`.
    pub fn get_or_register_type(&mut self, ty: TypeInfo) -> TypeId {
        self.types.get_or_register_type(ty, None)
    }

    // pub fn get_or_register_option_type(
    //     &mut self,
    //     inner: TypeId,
    //     resolve_inner_ty_name: bool,
    // ) -> TypeId {
    //     self.get_or_register_type(TypeInfo::Enum(EnumTypeInfo {
    //         // HACK: We cannot always resolve the type using type_to_string just
    //         // here, because sometimes when this is called some types are not
    //         // yet registered.
    //         name: if resolve_inner_ty_name {
    //             SymbolName::root().child(&format!("Option<{}>", self.type_to_string(inner)))
    //         } else {
    //             SymbolName::root().child("Option<TODO!!!>")
    //         },
    //         variants: vec![
    //             EnumVariantInfo {
    //                 name: "None".into(),
    //                 tag: 0,
    //                 ty: None,
    //             },
    //             EnumVariantInfo {
    //                 name: "Some".into(),
    //                 tag: 1,
    //                 ty: Some(inner),
    //             },
    //         ],
    //     }))
    // }

    // pub fn patch_all_option_names(&mut self) {
    //     let mut deferred_name_set = vec![]; // Borrow checker...
    //                                         //
    //     for (type_id, ty) in self.types.types.iter() {
    //         if ty.is_option() {
    //             let TypeInfo::Enum(enum_info) = ty else {
    //                 panic!("Should be enum")
    //             };
    //             let inner_ty = enum_info.variants[1].ty.expect("Payload should have ty");
    //
    //             deferred_name_set.push((
    //                 *type_id,
    //                 SymbolName::root().child(&format!("Option<{}>", self.type_to_string(inner_ty))),
    //             ));
    //         }
    //     }
    //
    //     for (type_id, name) in deferred_name_set {
    //         let ty = self.types.types.get_mut(&type_id).unwrap();
    //         if let TypeInfo::Enum(enum_info) = ty {
    //             enum_info.name = name;
    //         } else {
    //             panic!("Should be enum")
    //         }
    //     }
    // }

    /// Returns the type id for the provided symbol. If the symbol is not yet
    /// registered, a new type id is reserved and returned.
    ///
    /// Requesting the type info for the returned type id before the symbol is
    /// registered using `register_named_type` will panic.
    pub fn type_id_for_named_type_symbol(&mut self, symbol: SymbolId) -> TypeId {
        assert!(
            self.symbols[&symbol].kind.is_named_type(),
            "Symbol should correspond to a named type"
        );

        match self.type_symbol_usages.entry(symbol) {
            Entry::Occupied(typeid) => *typeid.get(),
            Entry::Vacant(entry) => {
                let reserved_type_id = self.types.gen_type_id();
                entry.insert(reserved_type_id);
                reserved_type_id
            }
        }
    }

    /// Registers the type information for a struct or other named user type.
    /// Callers can obtain a symbol's type id before this function is called by
    /// using `type_id_for_named_type_symbol`.
    pub fn register_named_type(&mut self, symbol: SymbolId, ty: TypeInfo) -> TypeId {
        let type_id = self.type_id_for_named_type_symbol(symbol);
        let registered_id = self.types.get_or_register_type(ty, Some(type_id));
        if registered_id != type_id {
            panic!("A reserved type was already in the type table! This should never happen");
        }
        type_id
    }

    pub fn set_initial_expression_info(&mut self, expr: &ast::Expr, ty: TypeId) -> ExpressionId {
        let expr_id = ExpressionId(expr.id);
        self.expressions.insert(
            expr_id,
            ExpressionInfo {
                ty,
                is_lvalue: false, // filled out later
                needs_deref_before_method_call: false,
            },
        );
        expr_id
    }

    pub fn set_expression_type_for_id(&mut self, expr_id: ExpressionId, ty: TypeId) {
        let expr_info = self.expressions.get_mut(&expr_id).unwrap();
        expr_info.ty = ty;
    }

    pub fn expression_id_for_expr(&self, expr: &ast::Expr) -> ExpressionId {
        ExpressionId(expr.id)
    }

    pub fn type_to_string(&self, id: TypeId) -> String {
        self.types.type_to_string(id)
    }

    pub fn set_type_expression_info(&mut self, expr: &ast::TypeExpr, ty: TypeId) {
        // HACK: Reusing the expressions table for non-expressions is a bit of a
        // hack. But ids are unique, so as long as ExpressionInfo doesn't differ
        // much from a potential TypeExpressionInfo, we're good.
        let expr_id = ExpressionId(expr.id);
        self.expressions.insert(
            expr_id,
            ExpressionInfo {
                ty,
                is_lvalue: false,
                needs_deref_before_method_call: false,
            },
        );
    }

    pub fn mark_expression_as_lvalue(&mut self, expr: &ast::Expr) {
        let expr_id = ExpressionId(expr.id);
        let expr_info = self.expressions.get_mut(&expr_id).unwrap();
        expr_info.is_lvalue = true;
    }

    pub fn mark_expression_as_deref_method(&mut self, expr: &ast::Expr) {
        let expr_id = ExpressionId(expr.id);
        let expr_info = self.expressions.get_mut(&expr_id).unwrap();
        expr_info.needs_deref_before_method_call = true;
    }

    pub fn set_initial_block_info(
        &mut self,
        block: &ast::Block,
        ty: TypeId,
        is_diverging: bool,
    ) -> BlockId {
        let block_id = BlockId(block.id);
        self.blocks.insert(block_id, BlockInfo { ty, is_diverging });
        block_id
    }

    pub fn get_expression_info(&self, expr: &ast::Expr) -> &ExpressionInfo {
        self.expressions
            .get(&ExpressionId(expr.id))
            .expect("Expression not yet added to symbol table")
    }

    pub fn get_expression_id_and_info(&self, expr: &ast::Expr) -> (ExpressionId, &ExpressionInfo) {
        let expr_id = ExpressionId(expr.id);
        let expr_info = self
            .expressions
            .get(&expr_id)
            .expect("Expression not yet added to symbol table");
        (expr_id, expr_info)
    }

    pub fn get_block_info(&self, block: &ast::Block) -> &BlockInfo {
        self.blocks
            .get(&BlockId(block.id))
            .expect("Block not yet added to symbol table")
    }

    pub fn get_type_expression_info(&self, expr: &ast::TypeExpr) -> &ExpressionInfo {
        self.expressions
            .get(&ExpressionId(expr.id))
            .unwrap_or_else(|| panic!("Type expression not yet added to symbol table {expr:?}"))
    }

    pub fn get_symbol_id(&self, ident: &ast::Ident) -> SymbolId {
        self.try_get_symbol_id(ident).unwrap_or_else(|| {
            panic!(
                "Identifier '{}' does not reference a valid symbol. {}",
                ident.text, "Probably forgot to add a usage reference to it."
            )
        })
    }

    /// Generally, the symbol of a path is the symbol associated to the last
    /// identifier in the path. This function exists to document that convention
    /// and to make it a bit more convenient.
    pub fn get_path_symbol_id(&self, path: &ast::IdentPath) -> SymbolId {
        self.get_symbol_id(
            path.idents
                .last()
                .expect("A path should always have at least one identifier."),
        )
    }

    pub fn try_get_symbol_id(&self, ident: &ast::Ident) -> Option<SymbolId> {
        self.symbol_refs
            .get(&SymbolRefId(ident.id))
            .map(|x| x.symbol)
    }

    pub fn get_symbol_info(&self, symbol: SymbolId) -> &SymbolInfo {
        self.symbols
            .get(&symbol)
            .expect("Symbol info does not exist")
    }

    pub fn get_type_info(&self, typeid: TypeId) -> &TypeInfo {
        self.types.get_type_info(typeid)
    }

    pub fn get_function_symbol_type_info(&self, symbol: SymbolId) -> &FunctionTypeInfo {
        let typeid = self
            .symbols
            .get(&symbol)
            .expect("Symbol info does not exist")
            .ty;
        match self.get_type_info(typeid) {
            TypeInfo::Function(f) => f,
            _ => panic!("Symbol is not a function"),
        }
    }

    // pub fn get_struct_symbol_type_info(&self, symbol: SymbolId) -> &StructTypeInfo {
    //     let typeid = self
    //         .symbols
    //         .get(&symbol)
    //         .expect("Symbol info does not exist")
    //         .ty;
    //     match self.get_type_info(typeid) {
    //         TypeInfo::Struct(s) => s,
    //         _ => panic!("Symbol is not a struct"),
    //     }
    // }

    pub fn get_ident_symbol_info(&self, ident: &ast::Ident) -> &SymbolInfo {
        self.symbols
            .get(&self.get_symbol_id(ident))
            .expect("Symbol info does not exist")
    }

    pub fn root_scope(&self) -> ScopeId {
        self.root_scope
    }

    pub fn lookup_symbol_in_scope_by_string(
        &self,
        name: &str,
        starting_scope: ScopeId,
    ) -> Option<(SymbolId, ScopeId)> {
        let mut scope = starting_scope;
        loop {
            if let Some(sym_id) = self.scopes[&scope].symbols.get(name) {
                return Some((*sym_id, scope));
            } else {
                scope = match self.scopes[&scope].parent {
                    Some(parent) => parent,
                    None => return None,
                }
            }
        }
    }

    pub fn get_scope_for_function(&self, function: &ast::FunctionDeclaration) -> ScopeId {
        ScopeId::Block(function.id)
    }

    pub fn get_scope_for_block(&self, block: &ast::Block) -> ScopeId {
        ScopeId::Block(block.id)
    }

    // pub fn get_scope_for_match_case(&self, case: &ast::PatternMatchCase) -> ScopeId {
    //     ScopeId::Block(case.id)
    // }

    pub fn register_plain_function_usage(&mut self, ident: &ast::Ident, symbol: SymbolId) {
        assert!(
            matches!(
                self.symbols.get(&symbol).expect("Symbol should exist").kind,
                SymbolInfoKind::Function { .. }
            ),
            "Function usage should point to a function symbol"
        );
        self.symbol_refs.insert(
            SymbolRefId(ident.id),
            SymbolRef {
                symbol,
                span: ident.span.clone(),
            },
        );
    }

    // pub fn register_named_type_usage(&mut self, ident: &ast::Ident, symbol: SymbolId) {
    //     assert!(
    //         matches!(
    //             self.symbols.get(&symbol).expect("Symbol should exist").kind,
    //             SymbolInfoKind::Struct { .. } | SymbolInfoKind::Enum { .. }
    //         ),
    //         "Type usage should point to a struct or enum symbol"
    //     );
    //     self.symbol_refs.insert(
    //         SymbolRefId(ident.id),
    //         SymbolRef {
    //             symbol,
    //             span: ident.span.clone(),
    //         },
    //     );
    // }

    pub fn register_global_variable_usage(&mut self, id: &ast::Ident, symbol: SymbolId) {
        assert!(
            matches!(
                self.symbols.get(&symbol).expect("Symbol should exist").kind,
                SymbolInfoKind::GlobalVariable { .. }
            ),
            "Global variable usage should point to a global variable symbol"
        );
        self.symbol_refs.insert(
            SymbolRefId(id.id),
            SymbolRef {
                symbol,
                span: id.span.clone(),
            },
        );
    }

    // pub fn register_struct_field_usage(&mut self, id: &ast::Ident, symbol: SymbolId) {
    //     assert!(
    //         matches!(
    //             self.symbols.get(&symbol).expect("Symbol should exist").kind,
    //             SymbolInfoKind::StructField
    //         ),
    //         "Struct field usage should point to a struct field symbol"
    //     );
    //     self.symbol_refs.insert(
    //         SymbolRefId(id.id),
    //         SymbolRef {
    //             symbol,
    //             span: id.span.clone(),
    //         },
    //     );
    // }

    // pub fn register_enum_variant_usage(&mut self, id: &ast::Ident, symbol: SymbolId) {
    //     assert!(
    //         matches!(
    //             self.symbols.get(&symbol).expect("Symbol should exist").kind,
    //             SymbolInfoKind::EnumVariant { .. }
    //         ),
    //         "Enum variant usage should point to a enum variant symbol"
    //     );
    //     self.symbol_refs.insert(
    //         SymbolRefId(id.id),
    //         SymbolRef {
    //             symbol,
    //             span: id.span.clone(),
    //         },
    //     );
    // }

    pub fn set_variable_type(&mut self, ident: &ast::Ident, ty: TypeId) {
        let symbol = self.get_symbol_id(ident);
        let symbol_info = self.symbols.get_mut(&symbol).unwrap();
        assert_eq!(
            symbol_info.ty,
            TypeTable::TYPE_UNKNOWN,
            "Type should be unkown"
        );
        symbol_info.ty = ty;
    }

    // pub fn set_struct_type(&mut self, symbol: SymbolId, ty: TypeId) {
    //     let symbol_info = self.symbols.get_mut(&symbol).unwrap();
    //     match &mut symbol_info.kind {
    //         SymbolInfoKind::Struct { fields: _ } => {}
    //         _ => panic!("Symbol is not a struct"),
    //     }
    //     assert_eq!(
    //         symbol_info.ty,
    //         TypeTable::TYPE_UNKNOWN,
    //         "Type should be unkown"
    //     );
    //     symbol_info.ty = ty;
    // }

    // pub fn set_enum_type(&mut self, symbol: SymbolId, ty: TypeId) {
    //     let symbol_info = self.symbols.get_mut(&symbol).unwrap();
    //     match &mut symbol_info.kind {
    //         SymbolInfoKind::Enum { variants: _ } => {}
    //         _ => panic!("Symbol is not an enum"),
    //     }
    //     assert_eq!(
    //         symbol_info.ty,
    //         TypeTable::TYPE_UNKNOWN,
    //         "Type should be unkown"
    //     );
    //     symbol_info.ty = ty;
    // }

    pub fn set_function_info(
        &mut self,
        debug_text_name: &str,
        function: &ast::FunctionDeclaration,
        kind: FunctionKind,
        return_type: TypeId,
        // extern_kind: FunctionExternKind,
    ) {
        // Collect some info to do error handling later -- borrowck limitation
        let (non_self_arg_count, is_method) = match &kind {
            FunctionKind::Plain { args } => (args.len(), false),
            FunctionKind::Method { self_ty: _, args } => (args.len(), true),
        };

        // Register the type of the function
        let fn_type_id = self.get_or_register_type(TypeInfo::Function(FunctionTypeInfo {
            text_name: debug_text_name.to_string(),
            kind,
            ret: return_type,
            // extern_kind,
        }));

        let function_symbol = self.get_symbol_id(&function.name);
        let function_symbol_info = self.symbols.get_mut(&function_symbol).unwrap();

        // Sanity checks
        match &function_symbol_info.kind {
            SymbolInfoKind::Function { args } => {
                assert_eq!(args.len(), non_self_arg_count, "Arg types length mismatch");
                assert!(!is_method, "");
            }
            // SymbolInfoKind::Method { args } => {
            //     assert_eq!(args.len(), non_self_arg_count, "Arg types length mismatch");
            //     assert!(
            //         is_method,
            //         "Registering non-method function for method symbol"
            //     );
            // }
            _ => panic!("Symbol is not a function"),
        }

        // Register the type for the function symbol
        function_symbol_info.ty = fn_type_id;
    }

    // /// If the method was special, returns its special method name.
    // pub fn register_method(
    //     &mut self,
    //     ty: TypeId,
    //     symbol: SymbolId,
    //     method_ident: &ast::Ident,
    // ) -> Result<(), CompilerError> {
    //     let methods = self.methods.entry(ty).or_insert_with(HashMap::new);
    //
    //     let method_name = MethodName::from_ident(method_ident);
    //
    //     let old = methods.insert(method_name, symbol);
    //     if old.is_some() {
    //         return Err(
    //             CompilerError::new(method_ident.span.start(), "Duplicate method".into()).label(
    //                 method_ident.span.clone(),
    //                 format!(
    //                     "Method '{}' is already declared for type '{}'.",
    //                     method_ident.text,
    //                     self.type_to_string(ty)
    //                 ),
    //             ),
    //         );
    //     }
    //     Ok(())
    // }

    // pub fn get_method_symbol_id_and_register_usage(
    //     &mut self,
    //     ty: TypeId,
    //     method: &ast::Ident,
    // ) -> Option<SymbolId> {
    //     let method_name = MethodName::from_ident(method);
    //     let symbol_id = self.methods.get(&ty)?.get(&method_name).copied()?;
    //
    //     // Register the method usage. This is mainly used by the LSP
    //     let symbol_ref_id = SymbolRefId(method.id);
    //     self.symbol_refs.insert(
    //         symbol_ref_id,
    //         SymbolRef {
    //             symbol: symbol_id,
    //             span: method.span.clone(),
    //         },
    //     );
    //
    //     Some(symbol_id)
    // }

    // pub fn register_operator_overload(
    //     &mut self,
    //     op: OverloadableOperator,
    //     symbol: SymbolId,
    //     span: Span,
    // ) -> Result<(), CompilerError> {
    //     let fn_info = self.get_function_symbol_type_info(symbol);
    //     let (self_ty, args) = match &fn_info.kind {
    //         FunctionKind::Plain { .. } => panic!("Operator overloads should be methods"),
    //         FunctionKind::Method { self_ty, args } => (*self_ty, args.clone()),
    //     };
    //
    //     let overloads_for_ty = self
    //         .operator_overloads
    //         .overloads
    //         .entry(self_ty)
    //         .or_insert_with(HashMap::new);
    //     let overloads_for_op = overloads_for_ty.entry(op).or_insert_with(Vec::new).clone();
    //
    //     for overload in overloads_for_op {
    //         let other_fn_info = self.get_function_symbol_type_info(overload);
    //         let other_args = match &other_fn_info.kind {
    //             FunctionKind::Plain { args } => args,
    //             FunctionKind::Method { self_ty: _, args } => args,
    //         };
    //
    //         if other_args == &args {
    //             return Err(
    //                 CompilerError::new(span.start(), "Duplicate operator overload".into()).label(
    //                     self.symbols[&symbol].span.clone(),
    //                     format!(
    //                         "Operator overload for '{}' with arguments ({}) is already declared for type '{}'.",
    //                         op.to_string(),
    //                         args.iter().map(|x| self.type_to_string(*x)).collect::<Vec<_>>().join(", "),
    //                         self.type_to_string(self_ty)
    //                     ),
    //                 ),
    //             );
    //         }
    //     }
    //
    //     // NOTE: Borrow checker requires clone above and borrowing again here.
    //     self.operator_overloads
    //         .overloads
    //         .get_mut(&self_ty)
    //         .unwrap()
    //         .get_mut(&op)
    //         .unwrap()
    //         .push(symbol);
    //
    //     Ok(())
    // }

    // pub fn get_operator_overload(
    //     &self,
    //     operator: OverloadableOperator,
    //     lhs_ty: TypeId,
    //     args: &[TypeId],
    // ) -> Option<SymbolId> {
    //     let overloads = self
    //         .operator_overloads
    //         .overloads
    //         .get(&lhs_ty)?
    //         .get(&operator)?;
    //     for overload in overloads {
    //         let fn_info = self.get_function_symbol_type_info(*overload);
    //         let fn_args = match &fn_info.kind {
    //             FunctionKind::Plain { .. } => panic!("Operator overloads should be methods"),
    //             FunctionKind::Method { self_ty: _, args } => args,
    //         };
    //
    //         if fn_args == args {
    //             return Some(*overload);
    //         }
    //     }
    //     return None;
    // }

    // pub fn set_global_metadata(&mut self, symbol_id: SymbolId, metadata: GlobalMetadata) {
    //     assert!(
    //         self.global_metadata.get(&symbol_id).is_none(),
    //         "Global metadata already set"
    //     );
    //     self.global_metadata.insert(symbol_id, metadata);
    // }
    //
    // pub fn get_global_metadata(&self, symbol_id: SymbolId) -> Option<GlobalMetadata> {
    //     self.global_metadata.get(&symbol_id).cloned()
    // }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl ScopeBuilder {
    pub fn new() -> Self {
        let table = SymbolTable::new();
        let root_scope = table.root_scope();
        Self {
            table,
            current_scope: root_scope,
        }
    }

    /// Sets the current scope
    pub fn set_current_scope(&mut self, scope: ScopeId) {
        self.current_scope = scope;
    }

    fn enter_scope(&mut self, node_id: ast::AstNodeId) {
        let child_scope_id = ScopeId::Block(node_id);
        self.table
            .scopes
            .insert(child_scope_id, ScopeInfo::new_child(self.current_scope));
        self.current_scope = child_scope_id;
    }

    pub fn enter_impl_scope(&mut self, impl_block: &ast::ImplBlock) {
        self.enter_scope(impl_block.id);
    }

    /// Enter a new scope for the given block
    pub fn enter_block_scope(&mut self, block: &ast::Block) {
        self.enter_scope(block.id)
    }

    // pub fn enter_match_case_scope(&mut self, case: &ast::PatternMatchCase) {
    //     self.enter_scope(case.id)
    // }

    /// Enter a new scope to declare the arguments for the given function.
    fn enter_function_scope(&mut self, fn_decl: &ast::FunctionDeclaration) {
        self.enter_scope(fn_decl.id)
    }

    /// Exit the current scope
    pub fn exit_scope(&mut self) {
        let parent = self.table.scopes[&self.current_scope]
            .parent
            .expect("Should never exit the root scope!");
        self.current_scope = parent;
    }

    fn register_symbol_in_scope(&mut self, name: &str, symbol: SymbolId) {
        self.table
            .scopes
            .get_mut(&self.current_scope)
            .unwrap()
            .symbols
            .insert(name.to_string(), symbol);
    }

    pub fn declare_local_variable(&mut self, ident: &ast::Ident) -> SymbolId {
        // info!("DECLARE LOCAL VAR!!! {}", ident.text);
        // info!("DECLARE LOCAL VAR!!! {}", ident.text);
        // info!("DECLARE LOCAL VAR!!! {}", ident.text);

        let var_id = SymbolId(ident.id);
        let var_info = SymbolInfo {
            // Local variables don't have symbol name, because shadowing and
            // control flow / scopes make them too complex to track.
            name: None,
            ident_text: ident.text,
            ty: TypeTable::TYPE_UNKNOWN,
            kind: SymbolInfoKind::LocalVariable,
            span: ident.span.clone(),
        };
        self.table.symbols.insert(var_id, var_info);
        self.table.symbol_refs.insert(
            SymbolRefId(ident.id),
            SymbolRef {
                symbol: var_id,
                span: ident.span.clone(),
            },
        );
        self.register_symbol_in_scope(&ident.text, var_id);
        var_id
    }

    pub fn start_function_declaration(
        &mut self,
        parent_name: &SymbolName,
        decl: &ast::FunctionDeclaration,
        // TODO: remove
        _is_method: bool,
    ) -> SymbolId {
        let ident = &decl.name;
        let func_id = SymbolId(ident.id);

        let fn_symbol_name = parent_name.child(&ident.text);
        let func_info = SymbolInfo {
            name: Some(fn_symbol_name),
            ident_text: ident.text,
            // kind: if is_method {
            //     SymbolInfoKind::Method { args: vec![] }
            // } else {
            //     SymbolInfoKind::Function { args: vec![] }
            // },
            kind: SymbolInfoKind::Function { args: vec![] },
            ty: TypeTable::TYPE_UNKNOWN,
            span: decl.name.span.clone(),
        };
        self.table.symbols.insert(func_id, func_info);
        self.table.symbol_refs.insert(
            SymbolRefId(ident.id),
            SymbolRef {
                symbol: func_id,
                span: decl.name.span.clone(),
            },
        );
        self.table.symbol_id_to_name.insert(func_id, fn_symbol_name);
        self.register_symbol_in_scope(&ident.text, func_id);
        self.enter_function_scope(decl);
        func_id
    }

    pub fn declare_function_argument(
        &mut self,
        func_decl: &ast::FunctionDeclaration,
        param: &ast::FunctionParam,
    ) -> SymbolId {
        let func_id = SymbolId(func_decl.name.id);
        let arg_sym = self.declare_local_variable(param.ident());
        let func_info = self.table.symbols.get_mut(&func_id).unwrap();

        match &mut func_info.kind {
            SymbolInfoKind::Function { args } => {
                args.push(arg_sym);
            }
            // SymbolInfoKind::Method { args } => args.push(arg_sym),
            _ => unreachable!(),
        }
        arg_sym
    }

    // fn declare_struct_field(&mut self, parent_name: &SymbolName, name: &ast::Ident) -> SymbolId {
    //     let field_id = SymbolId(name.id);
    //     let field_info = SymbolInfo {
    //         ident_text: name.text.clone(),
    //         name: Some(parent_name.child(&name.text)),
    //         ty: TypeTable::TYPE_UNKNOWN,
    //         kind: SymbolInfoKind::StructField,
    //         span: name.span.clone(),
    //     };
    //     self.table.symbols.insert(field_id, field_info);
    //     self.table.symbol_refs.insert(
    //         SymbolRefId(name.id),
    //         SymbolRef {
    //             symbol: field_id,
    //             span: name.span.clone(),
    //         },
    //     );
    //     self.table
    //         .symbol_id_to_name
    //         .insert(field_id, parent_name.child(&name.text));
    //     // NOTE: Struct fields are never in a scope. So we don't register them
    //     // in the scope unlike other types of symbols.
    //     field_id
    // }

    // pub fn declare_struct<'a>(
    //     &mut self,
    //     parent_name: &SymbolName,
    //     name: &ast::Ident,
    //     fields: impl Iterator<Item = &'a ast::Ident>,
    // ) -> SymbolId {
    //     let struct_id = SymbolId(name.id);
    //     let struct_sym_name = parent_name.child(&name.text);
    //     let struct_fields = fields
    //         .map(|x| self.declare_struct_field(&struct_sym_name, x))
    //         .collect();
    //     self.table.symbols.insert(
    //         struct_id,
    //         SymbolInfo {
    //             name: Some(struct_sym_name),
    //             ident_text: name.text.clone(),
    //             ty: TypeTable::TYPE_UNKNOWN,
    //             kind: SymbolInfoKind::Struct {
    //                 fields: struct_fields,
    //             },
    //             span: name.span.clone(),
    //         },
    //     );
    //     self.table.symbol_refs.insert(
    //         SymbolRefId(name.id),
    //         SymbolRef {
    //             symbol: struct_id,
    //             span: name.span.clone(),
    //         },
    //     );
    //     self.table
    //         .symbol_id_to_name
    //         .insert(struct_id, struct_sym_name);
    //     self.register_symbol_in_scope(&name.text, struct_id);
    //     struct_id
    // }
    //
    // fn declare_enum_variant(
    //     &mut self,
    //     parent_enum_name: &SymbolName,
    //     parent_enum: SymbolId,
    //     name: &ast::Ident,
    //     tag: usize,
    // ) -> SymbolId {
    //     let variant_id = SymbolId(name.id);
    //     let variant_info = SymbolInfo {
    //         name: Some(parent_enum_name.child(&name.text)),
    //         ident_text: name.text.clone(),
    //         ty: TypeTable::TYPE_UNKNOWN,
    //         kind: SymbolInfoKind::EnumVariant { parent_enum, tag },
    //         span: name.span.clone(),
    //     };
    //     self.table.symbols.insert(variant_id, variant_info);
    //     self.table.symbol_refs.insert(
    //         SymbolRefId(name.id),
    //         SymbolRef {
    //             symbol: variant_id,
    //             span: name.span.clone(),
    //         },
    //     );
    //     self.table
    //         .symbol_id_to_name
    //         .insert(variant_id, parent_enum_name.child(&name.text));
    //     // NOTE: Enum variants are never in a scope. So we don't register them
    //     // in the scope unlike other types of symbols.
    //     variant_id
    // }
    //
    // pub fn declare_enum<'a>(
    //     &mut self,
    //     parent_name: &SymbolName,
    //     name: &ast::Ident,
    //     variants: impl Iterator<Item = &'a ast::Ident>,
    // ) -> SymbolId {
    //     let enum_id = SymbolId(name.id);
    //     let enum_sym_name = parent_name.child(&name.text);
    //     let enum_variants = variants
    //         .enumerate()
    //         // NOTE: This assumes sequential tags from 0 to N-1
    //         .map(|(tag, id)| self.declare_enum_variant(&enum_sym_name, enum_id, id, tag))
    //         .collect();
    //     self.table.symbols.insert(
    //         enum_id,
    //         SymbolInfo {
    //             ident_text: name.text.clone(),
    //             name: Some(enum_sym_name),
    //             ty: TypeTable::TYPE_UNKNOWN,
    //             kind: SymbolInfoKind::Enum {
    //                 variants: enum_variants,
    //             },
    //             span: name.span.clone(),
    //         },
    //     );
    //     self.table.symbol_refs.insert(
    //         SymbolRefId(name.id),
    //         SymbolRef {
    //             symbol: enum_id,
    //             span: name.span.clone(),
    //         },
    //     );
    //     self.table.symbol_id_to_name.insert(enum_id, enum_sym_name);
    //     self.register_symbol_in_scope(&name.text, enum_id);
    //     enum_id
    // }

    pub fn declare_global(&mut self, parent_name: &SymbolName, name: &ast::Ident) -> SymbolId {
        let global_id = SymbolId(name.id);
        self.table.symbols.insert(
            global_id,
            SymbolInfo {
                ident_text: name.text,
                name: Some(parent_name.child(&name.text)),
                ty: TypeTable::TYPE_UNKNOWN,
                kind: SymbolInfoKind::GlobalVariable,
                span: name.span.clone(),
            },
        );
        self.table.symbol_refs.insert(
            SymbolRefId(name.id),
            SymbolRef {
                symbol: global_id,
                span: name.span.clone(),
            },
        );
        self.table
            .symbol_id_to_name
            .insert(global_id, parent_name.child(&name.text));
        self.register_symbol_in_scope(&name.text, global_id);
        global_id
    }

    pub fn finish_function_declaration(&mut self, _func_decl: &ast::FunctionDeclaration) {
        self.exit_scope();
    }

    pub fn register_identifier_symbol_usage(&mut self, ident: &ast::Ident) -> Option<SymbolId> {
        let symbol_id = self
            .table
            .lookup_symbol_in_scope_by_string(&ident.text, self.current_scope)?
            .0;

        assert!(
            matches!(
                self.table
                    .symbols
                    .get(&symbol_id)
                    .expect("Symbol should exist")
                    .kind,
                SymbolInfoKind::LocalVariable | SymbolInfoKind::GlobalVariable,
            ),
            "Variable usage should point to a variable symbol"
        );

        self.table.symbol_refs.insert(
            SymbolRefId(ident.id),
            SymbolRef {
                symbol: symbol_id,
                span: ident.span.clone(),
            },
        );

        Some(symbol_id)
    }

    pub fn current_scope(&self) -> ScopeId {
        self.current_scope
    }

    // pub fn declare_fundamental_named_type(&mut self, symbol: SymbolId, ty_name: &str) {
    //     for (ty_id, ty_info) in self.table.types.types.iter() {
    //         match ty_info {
    //             TypeInfo::Struct(StructTypeInfo { name, .. })
    //             | TypeInfo::Enum(EnumTypeInfo { name, .. }) => {
    //                 if name.qualified_name() == ty_name {
    //                     self.table.type_symbol_usages.insert(symbol, *ty_id);
    //                     return;
    //                 }
    //             }
    //             _ => (),
    //         }
    //     }
    //     panic!("Fundamental type not found in type table");
    // }
}

impl Default for ScopeBuilder {
    fn default() -> Self {
        Self::new()
    }
}

pub fn print_symbol_table(table: &SymbolTable, sources: &SourceFiles, query_loc: Option<&LineCol>) {
    let mut report = ariadne::Report::build(ariadne::ReportKind::Advice, "gud.rock", 0);

    let file = &sources.files[0];

    let mut seen_syms = HashSet::new();

    if let Some(query_loc) = query_loc {
        let span = Span {
            file: query_loc.file,
            line_range: (query_loc.line, query_loc.line + 1),
            col_range: (query_loc.col, query_loc.col + 1),
            offset_range: (query_loc.offset, query_loc.offset + 1),
        };

        report.add_label(span.to_label(file.path(), ">>> Query location <<<".to_string()));
    }

    for (sym_id, sym) in table.symbols.iter() {
        seen_syms.insert(sym_id.to_u32());

        report.add_label(sym.span.to_label(
            file.path(),
            format!("symbol[#{}]: '{}'", sym_id.to_u32(), sym.ident_text),
        ));
    }

    for (ref_id, sym_ref) in table.symbol_refs.iter() {
        if seen_syms.contains(&ref_id.to_u32()) {
            continue;
        }

        report.add_label(sym_ref.span.to_label(
            file.path(),
            format!("ref[#{}] -> #{}", ref_id.to_u32(), sym_ref.symbol.to_u32()),
        ));
    }

    report
        .finish()
        .eprint((
            file.path().to_string(),
            ariadne::Source::from(file.contents()),
        ))
        .unwrap();
}
