use std::sync::atomic::AtomicU32;

use crate::source_code::*;
use ustr::Ustr;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AstNodeId(u32);

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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Ident {
    pub id: AstNodeId,
    pub span: Span,
    pub text: Ustr,
}

#[derive(Clone, Debug)]
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

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub id: AstNodeId,
    pub span: Span,
    pub name: Ident,
    pub params: Vec<FunctionParam>,
    pub return_type: Option<TypeExpr>,
    pub body: Block,
    pub kind: FunctionDefKind,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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
}

#[derive(Debug, Clone)]
pub enum TopLevel {
    Statement(Statement),
    Function(FunctionDef),
}

#[derive(Clone, Debug, PartialEq)]
pub enum StatementKind {
    Expression(Expr),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    NumLiteral(f64),
}
