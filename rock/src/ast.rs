use anyhow::Result;
use std::sync::atomic::AtomicU32;

use crate::{debug::NodeExt, parser::Source, source_code::*};
use tree_sitter::Node;
use ustr::Ustr;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AstNodeId(u32);

impl AstNodeId {
    pub fn from_u32(id: u32) -> Self {
        Self(id)
    }

    pub fn to_u32(&self) -> u32 {
        self.0
    }
}

pub struct AstNodeIdGen {
    next_id: AtomicU32,
}

impl AstNodeIdGen {
    pub fn new() -> Self {
        Self {
            next_id: AtomicU32::new(0),
        }
    }

    pub fn id_gen(&self) -> AstNodeId {
        AstNodeId(
            self.next_id
                .fetch_add(1, std::sync::atomic::Ordering::SeqCst),
        )
    }
}

impl Default for AstNodeIdGen {
    fn default() -> Self {
        Self::new()
    }
}

pub struct AstBuilder {
    id_gen: AstNodeIdGen,
}

impl AstBuilder {
    pub fn new() -> Self {
        Self {
            id_gen: AstNodeIdGen::new(),
        }
    }

    pub fn id_gen(&mut self) -> AstNodeId {
        self.id_gen.id_gen()
    }

    // pub fn build_module(&self, items: Vec<Item>) -> Module {
    //     Module {
    //         id: self.id_gen.id_gen(),
    //         items,
    //     }
    // }
    //
    // pub fn build_function_declaration(
    //     &self,
    //     name: Ident,
    //     span: Span,
    //     args: Vec<(Ident, TypeExpr)>,
    //     ret: TypeExpr,
    //     implementation: FunctionImpl,
    //     kind: FunctionDeclKind,
    // ) -> FunctionDeclaration {
    //     FunctionDeclaration {
    //         id: self.id_gen.id_gen(),
    //         span,
    //         name,
    //         args,
    //         ret_ty_expr: ret,
    //         implementation,
    //         kind,
    //     }
    // }

    pub fn build_block(
        &self,
        span: Span,
        statements: Vec<Statement>,
        return_expr: Option<Expr>,
    ) -> Block {
        Block {
            id: self.id_gen.id_gen(),
            span,
            statements,
            return_expr,
        }
    }

    pub fn build_statement(&self, span: Span, kind: StatementKind) -> Statement {
        Statement {
            id: self.id_gen.id_gen(),
            span,
            kind,
        }
    }

    pub fn build_expr(&self, span: Span, kind: ExprKind) -> Expr {
        Expr {
            id: self.id_gen.id_gen(),
            span,
            kind,
        }
    }

    pub fn build_type_expr(&self, span: Span, kind: TypeExprKind) -> TypeExpr {
        TypeExpr {
            id: self.id_gen.id_gen(),
            span,
            kind,
        }
    }

    pub fn build_ident(&self, span: Span, name: impl Into<ustr::Ustr>) -> Ident {
        Ident {
            id: self.id_gen.id_gen(),
            span,
            text: name.into(),
        }
    }

    pub fn build_ident_path(&self, span: Span, idents: Vec<Ident>) -> IdentPath {
        IdentPath {
            id: self.id_gen.id_gen(),
            span,
            idents,
        }
    }

    pub fn ident_path(&mut self, node: Node, source: &Source) -> Result<IdentPath> {
        let span = node.to_source_span(source);

        Ok(IdentPath {
            id: self.id_gen(),
            span: span.clone(),
            idents: vec![self.build_ident(span, node.text(source)?)],
        })
    }

    // pub fn build_struct_declaration(
    //     &self,
    //     name: Ident,
    //     span: Span,
    //     fields: Vec<StructFieldDeclaration>,
    // ) -> StructDeclaration {
    //     StructDeclaration {
    //         id: self.id_gen.id_gen(),
    //         span,
    //         name,
    //         fields,
    //     }
    // }
    //
    // pub fn build_enum_declaration(
    //     &self,
    //     name: Ident,
    //     span: Span,
    //     variants: Vec<(Ident, Option<TypeExpr>)>,
    // ) -> EnumDeclaration {
    //     EnumDeclaration {
    //         id: self.id_gen.id_gen(),
    //         span,
    //         name,
    //         variants,
    //     }
    // }

    pub fn build_item(&self, span: Span, kind: ItemKind, annotations: Vec<ItemAnnotation>) -> Item {
        Item {
            id: self.id_gen.id_gen(),
            span,
            kind,
            annotations,
        }
    }

    // pub fn build_global_declaration(
    //     &self,
    //     name: Ident,
    //     span: Span,
    //     ty: TypeExpr,
    //     val: Expr,
    // ) -> GlobalDeclaration {
    //     GlobalDeclaration {
    //         id: self.id_gen.id_gen(),
    //         span,
    //         name,
    //         ty_expr: ty,
    //         value_expr: val,
    //     }
    // }
    //
    // pub fn build_macro_body(&self, start: Span, parts: Vec<MacroBodyPart>) -> MacroBody {
    //     MacroBody {
    //         id: self.id_gen.id_gen(),
    //         span: start,
    //         parts,
    //     }
    // }
    //
    // pub fn build_macro_expr(&self, span: Span, kind: MacroExprKind) -> MacroExpr {
    //     MacroExpr {
    //         id: self.id_gen.id_gen(),
    //         span,
    //         kind,
    //     }
    // }
    //
    // pub fn build_macro_definition(
    //     &self,
    //     span: Span,
    //     name: Ident,
    //     args: Vec<(Ident, MacroType)>,
    //     return_type: MacroType,
    //     body: MacroBody,
    // ) -> MacroDefinition {
    //     MacroDefinition {
    //         id: self.id_gen.id_gen(),
    //         span,
    //         name,
    //         params: args,
    //         return_type,
    //         body,
    //     }
    // }

    pub fn build_impl_block(&self, span: Span, ty: TypeExpr, items: Vec<Item>) -> ImplBlock {
        ImplBlock {
            id: self.id_gen.id_gen(),
            span,
            ty_expr: ty,
            inner: items,
        }
    }

    // pub fn build_pattern_match_case(
    //     &self,
    //     span: Span,
    //     kind: PatternMatchKind,
    //     body: Expr,
    // ) -> PatternMatchCase {
    //     PatternMatchCase {
    //         id: self.id_gen.id_gen(),
    //         span,
    //         kind,
    //         body,
    //     }
    // }
    //
    // pub fn build_pattern_match(&self, span: Span, cases: Vec<PatternMatchCase>) -> PatternMatch {
    //     PatternMatch {
    //         id: self.id_gen.id_gen(),
    //         span,
    //         cases,
    //     }
    // }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IdentPath {
    pub id: AstNodeId,
    pub span: Span,
    pub idents: Vec<Ident>,
}

impl IdentPath {
    pub fn text(&self) -> String {
        self.idents
            .iter()
            .map(|ident| ident.text.to_string())
            .collect::<Vec<_>>()
            .join("::")
    }

    pub fn is_single_ident(&self, other: &str) -> bool {
        self.idents.len() == 1 && self.idents[0].text == other
    }

    pub fn to_single_ident(self) -> Ident {
        assert_eq!(self.idents.len(), 1);
        self.idents.into_iter().next().unwrap()
    }

    pub fn as_single_ident(&self) -> &Ident {
        assert_eq!(self.idents.len(), 1);
        &self.idents[0]
    }

    pub fn last_ident(&self) -> &Ident {
        self.idents.last().unwrap()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Ident {
    pub id: AstNodeId,
    pub span: Span,
    pub text: Ustr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Statement {
    pub id: AstNodeId,
    pub span: Span,
    pub kind: StatementKind,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub id: AstNodeId,
    pub span: Span,
    pub kind: ExprKind,
}

// TODO: rename to FunctionDef
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDeclaration {
    pub id: AstNodeId,
    pub span: Span,
    pub name: Ident,
    // TODO: Rename to params
    pub args: Vec<FunctionParam>,
    pub return_type: Option<TypeExpr>,
    pub body: Block,
    pub kind: FunctionDefKind,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub id: AstNodeId,
    pub span: Span,
    pub statements: Vec<Statement>,
    pub return_expr: Option<Expr>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum FunctionDefKind {
    Standalone,
    Method,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FunctionParam {
    Typed(Ident, TypeExpr),
    Untyped(Ident),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeExpr {
    pub id: AstNodeId,
    pub span: Span,
    pub kind: TypeExprKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeExprKind {
    Named(Ident),
    Unit,
    String,
}

#[derive(Debug, Clone)]
pub enum TopLevel {
    Statement(Statement),
    Function(FunctionDeclaration),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Comment {
    pub id: AstNodeId,
    pub span: Span,
    pub text: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StatementKind {
    Comment(Comment),
    BlankLine,
    Nothing,
    Expression(Expr),
    Return(Option<Expr>),
    Break,
    Continue,
    Let {
        ident: Ident,
        ty_expr: Option<TypeExpr>,
        expr: Expr,
    },
    Assign {
        lhs: Expr,
        rhs: Expr,
    },
    If {
        cond: Expr,
        then_block: Block,
        else_block: Option<Block>,
    },
    For {
        var: Ident,
        iterable: Expr,
        body: Block,
    },
    While {
        cond: Expr,
        body: Block,
    },
}
#[derive(Debug, Clone, PartialEq)]
pub struct ImplBlock {
    pub id: AstNodeId,
    pub span: Span,
    pub ty_expr: TypeExpr,
    pub inner: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub id: AstNodeId,
    pub span: Span,
    pub kind: ItemKind,
    pub annotations: Vec<ItemAnnotation>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    Function(FunctionDeclaration),
    // Struct(StructDeclaration),
    // Enum(EnumDeclaration),
    // Global(GlobalDeclaration),
    // MacroDefinition(MacroDefinition),
    // Macro(MacroExpr),
    ImplBlock(ImplBlock),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ItemAnnotation {
    pub span: Span,
    pub name: Ident,
    // pub tokens: Vec<TokenKind>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Path(IdentPath),
    NumLiteral(f64),
    SelfIdent,
    BoolLiteral(bool),
    StringLiteral(String),
    NullLiteral,
    OptionSomeLiteral(Box<Expr>),
    OptionNoneLiteral,
    // StringInterpolation {
    //     parts: Vec<StringInterpolationPart>,
    // },
    FunctionCall {
        ident: Ident,
        args: Vec<Expr>,
    },
    // VectorBuiltin {
    //     kind: VectorBuiltinKind,
    //     args: Vec<Expr>,
    // },
    // UnaryOp {
    //     op: UnOp,
    //     operand: Box<Expr>,
    // },
    BinaryOp {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    // Typecast {
    //     ty_expr: TypeExpr,
    //     sub_expr: Box<Expr>,
    // },
    FieldAccess {
        field: Ident,
        base: Box<Expr>,
    },
    // MethodCall {
    //     base: Box<Expr>,
    //     args: Vec<Expr>,
    //     method: Ident,
    // },
    Index {
        base: Box<Expr>,
        index: Box<Expr>,
    },
    ParenExpr(Box<Expr>),
    // StructLiteral {
    //     struct_ident: Ident,
    //     fields: Vec<(Ident, Expr)>,
    // },
    // VecLiteral {
    //     elements: Vec<Expr>,
    // },
    // RefAlloc(Box<Expr>),
    Block(Box<Block>),
    // TODO: not actuallly parsed as expr atm
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
    },
    // Match {
    //     test_expr: Box<Expr>,
    //     match_: PatternMatch,
    // },
    // TypeOf {
    //     inner: Box<Expr>,
    // },
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Eq,
    Neq,
    Le,
    Ge,
    Lt,
    Gt,
    BitAnd,
    BitOr,
    BitXor,
    BitNot,
    Shl,
    Shr,
    Modulo,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnOp {
    AddrOf,
    Deref,
    Not,
}

impl ToString for BinOp {
    fn to_string(&self) -> String {
        match self {
            BinOp::Add => "+".to_string(),
            BinOp::Sub => "-".to_string(),
            BinOp::Mul => "*".to_string(),
            BinOp::Div => "/".to_string(),
            BinOp::And => "&&".to_string(),
            BinOp::Or => "||".to_string(),
            BinOp::Eq => "==".to_string(),
            BinOp::Neq => "!=".to_string(),
            BinOp::Le => "<=".to_string(),
            BinOp::Ge => ">=".to_string(),
            BinOp::Lt => "<".to_string(),
            BinOp::Gt => ">".to_string(),
            BinOp::BitAnd => "&".to_string(),
            BinOp::BitOr => "|".to_string(),
            BinOp::BitXor => "^".to_string(),
            BinOp::BitNot => "!".to_string(),
            BinOp::Shl => "<<".to_string(),
            BinOp::Shr => ">>".to_string(),
            BinOp::Modulo => "%".to_string(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PrimitiveType {
    Num,
    Bool,
    String,
}

impl PrimitiveType {
    pub fn from_string(s: &str) -> Option<Self> {
        match s {
            "num" => Some(Self::Num),
            "bool" => Some(Self::Bool),
            "string" => Some(Self::String),
            _ => None,
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            PrimitiveType::Num => "num".to_string(),
            PrimitiveType::Bool => "bool".to_string(),
            PrimitiveType::String => "string".to_string(),
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            PrimitiveType::Num => true,
            PrimitiveType::Bool => false,
            PrimitiveType::String => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            PrimitiveType::Num => true,
            PrimitiveType::Bool => false,
            PrimitiveType::String => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            PrimitiveType::Bool => true,
            _ => false,
        }
    }
}
