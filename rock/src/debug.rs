use crate::*;

pub trait AstKind {
    fn kind(&self) -> &'static str;
}

impl AstKind for Statement {
    fn kind(&self) -> &'static str {
        "Statement"
    }
}

impl AstKind for FunctionDef {
    fn kind(&self) -> &'static str {
        "FunctionDef"
    }
}

impl AstKind for Expr {
    fn kind(&self) -> &'static str {
        "Expr"
    }
}

impl AstKind for Block {
    fn kind(&self) -> &'static str {
        "Block"
    }
}

impl AstKind for Ident {
    fn kind(&self) -> &'static str {
        "Ident"
    }
}

impl AstKind for TypeExpr {
    fn kind(&self) -> &'static str {
        "TypeExpr"
    }
}

impl AstKind for FunctionParam {
    fn kind(&self) -> &'static str {
        "FunctionParam"
    }
}

impl AstKind for TypeExprKind {
    fn kind(&self) -> &'static str {
        "TypeExprKind"
    }
}

impl AstKind for FunctionDefKind {
    fn kind(&self) -> &'static str {
        "FunctionDefKind"
    }
}

impl AstKind for TopLevel {
    fn kind(&self) -> &'static str {
        match self {
            TopLevel::Statement(_) => "Statement",
            TopLevel::Function(_) => "Function",
        }
    }
}
