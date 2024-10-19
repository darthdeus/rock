use ariadne::{Label, Report, ReportKind, Source};
use tree_sitter::Node;

use crate::*;

pub trait NodeExt {
    fn report_error(&self, source: &str, message: &str);
    fn report_unexpected_kind(&self, source: &str, ty: &str);
}

impl<'a> NodeExt for Node<'a> {
    fn report_error(&self, source: &str, message: &str) {
        Report::build(ReportKind::Error, "file.rock", 0)
            .with_label(
                Label::new(("file.rock", self.start_byte()..self.end_byte())).with_message(message),
            )
            .finish()
            .print(("file.rock", Source::from(source)))
            .unwrap();
    }

    fn report_unexpected_kind(&self, source: &str, ty: &str) {
        self.report_error(source, &format!("unexpected {} kind: {}", ty, self.kind()));
    }
}

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
