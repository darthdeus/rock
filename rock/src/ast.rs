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

impl Default for AstNodeIdGen {
    fn default() -> Self {
        Self::new()
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

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDef {
    pub id: AstNodeId,
    pub span: Span,
    pub name: Ident,
    pub params: Vec<FunctionParam>,
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
}

#[derive(Debug, Clone)]
pub enum TopLevel {
    Statement(Statement),
    Function(FunctionDef),
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

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    // TODO: change to ident path?
    Path(Ident),
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
    // FieldAccess {
    //     base: Box<Expr>,
    //     field: Ident,
    // },
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
    // If {
    //     condition: Box<Expr>,
    //     then_branch: Box<Expr>,
    //     else_branch: Option<Box<Expr>>,
    // },
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
