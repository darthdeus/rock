use std::collections::HashMap;

use anyhow::Result;
use source_code::{LineCol, Span};

use crate::parser::Source;
use ast::*;

pub mod ast;
pub mod debug;
pub mod format;
pub mod parser;
pub mod source_code;

pub fn parse(source: &str) -> Result<Vec<TopLevel>> {
    let mut parser = parser::Parser::new();

    let top_level = parser.parse(&Source {
        code: source.to_string(),
        file: Some("file.rock".into()),
    })?;

    Ok(top_level)
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
    // Function { args: Vec<SymbolId> },
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

#[derive(Default)]
pub struct CompilerContext {
    pub symbols: HashMap<SymbolId, SymbolInfo>,
}

impl CompilerContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn query_definition_at(&self, pos: LineCol) -> Option<SymbolId> {
        None
    }

    pub fn span_of_symbol(&self, symbol: SymbolId) -> Span {
        self.symbols.get(&symbol).as_ref().unwrap().span.clone()
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
