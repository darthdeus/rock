use ariadne::{Label, Report, ReportKind};
use compiler_error::CompilerError;
use source_code::Span;
use tree_sitter::Node;

use crate::*;

use crate::parser::Source;

pub trait NodeExt {
    fn report_error(&self, source: &Source, message: &str);
    fn report_unexpected_kind(&self, source: &Source, ty: &str);
    fn to_span_nofile(&self) -> Span;
    fn to_span(&self, file: ustr::Ustr) -> Span;
    fn to_source_span(&self, source: &Source) -> Span;
    fn text<'a>(&self, source: &'a Source) -> Result<&'a str, CompilerError>;
}

impl<'a> NodeExt for Node<'a> {
    fn report_error(&self, source: &Source, message: &str) {
        let fname = source.file.as_deref().unwrap_or("<unknown>");

        Report::build(ReportKind::Error, fname, 0)
            .with_label(
                Label::new((fname, self.start_byte()..self.end_byte())).with_message(message),
            )
            .finish()
            .print((fname, ariadne::Source::from(&source.code)))
            .unwrap();
    }

    fn report_unexpected_kind(&self, source: &Source, ty: &str) {
        self.report_error(source, &format!("unexpected {} kind: {}", ty, self.kind()));
    }

    fn to_span_nofile(&self) -> Span {
        self.to_span("<unknown>".into())
    }

    fn to_source_span(&self, source: &Source) -> Span {
        self.to_span(source.file.unwrap_or_else(|| "<unknown>".into()))
    }

    fn to_span(&self, file: ustr::Ustr) -> Span {
        Span {
            file,
            line_range: (self.start_position().row, self.end_position().row),
            col_range: (self.start_position().column, self.end_position().column),
            offset_range: (self.start_byte(), self.end_byte()),
        }
    }

    fn text<'b>(&self, source: &'b Source) -> Result<&'b str, CompilerError> {
        self.utf8_text(source.code.as_bytes()).map_err(|e| {
            CompilerError::new(self.to_source_span(source).start(), format!("{:?}", e))
        })
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

impl AstKind for FunctionDeclaration {
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
