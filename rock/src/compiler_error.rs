use crate::source_code::{LineCol, SourceFiles, Span};

#[repr(u32)]
#[derive(Debug, Clone, Copy)]
pub enum CompilerErrorCode {
    Unknown,
    LexerError,
    UnexpectedEOF,
    SyntaxError,
    DuplicateSymbol,
    DeclarationNotFound,
    TypeMismatch,
    InferenceError,
    FunctionArgumentCountMismatch,
    ExpressionIsNotLvalue,
    InvalidDereference,
    InvalidTypeCast,
    DuplicateField,
    MissingFields,
    InvalidFieldAccess,
    FormattingUnsupported,
}

#[derive(Debug)]
pub enum CompilerErrorLevel {
    Advice,
    Error,
    Warning,
}

#[derive(Debug)]
pub struct CompilerErrorLabel {
    pub span: Span,
    pub message: String,
}

#[derive(Debug)]
pub struct CompilerErrorNote {
    pub message: String,
}

#[derive(Debug)]
pub struct CompilerError {
    pub location: LineCol,
    pub code: CompilerErrorCode,
    pub labels: Vec<CompilerErrorLabel>,
    pub note: Option<CompilerErrorNote>,
    pub level: CompilerErrorLevel,
    pub message: String,
}

impl CompilerError {
    pub fn new(location: LineCol, message: String) -> Self {
        Self {
            location,
            code: CompilerErrorCode::Unknown,
            level: CompilerErrorLevel::Error,
            labels: Vec::new(),
            note: None,
            message,
        }
    }

    pub fn code(mut self, code: CompilerErrorCode) -> Self {
        self.code = code;
        self
    }

    pub fn label(mut self, span: Span, message: String) -> Self {
        self.labels.push(CompilerErrorLabel { span, message });
        self
    }

    pub fn note(mut self, message: String) -> Self {
        self.note = Some(CompilerErrorNote { message });
        self
    }

    pub fn render(&self, sources: &SourceFiles) -> String {
        let mut builder: ariadne::ReportBuilder<'_, (String, std::ops::Range<usize>)> =
            ariadne::Report::build::<String>(
                match self.level {
                    CompilerErrorLevel::Advice => ariadne::ReportKind::Advice,
                    CompilerErrorLevel::Error => ariadne::ReportKind::Error,
                    CompilerErrorLevel::Warning => ariadne::ReportKind::Warning,
                },
                self.location.file.to_string(),
                self.location.offset,
            )
            .with_code(format!("E{:02}", self.code as u8))
            .with_message(self.message.clone());

        // Add an empty label if there are no labels, to highlight the error location.
        if self.labels.is_empty() {
            builder.add_label(ariadne::Label::new((
                self.location.file.to_string(),
                self.location.offset..self.location.offset + 1,
            )));
        }

        for label in &self.labels {
            builder.add_label(
                ariadne::Label::new((
                    label.span.file.to_string(),
                    label.span.offset_range.0..label.span.offset_range.1,
                ))
                .with_message(label.message.clone()),
            );
        }
        if let Some(note) = &self.note {
            builder.set_note(note.message.clone());
        }

        let mut buf = Vec::new();

        let sources_vec = sources
            .iter()
            .map(|src| (src.path().to_string(), src.contents()))
            .collect::<Vec<_>>();
        let sources = ariadne::sources(sources_vec);
        builder.finish().write(sources, &mut buf).unwrap();
        String::from_utf8(buf).unwrap()
    }
}

#[cfg(test)]
mod test {
    use crate::source_code::SourceFile;

    use super::*;
    #[test]
    pub fn test_errors() {
        let err = CompilerError::new(
            LineCol {
                file: "string.rbl".into(),
                line: 0,
                col: 0,
                offset: 37,
            },
            "Error message".into(),
        )
        .code(CompilerErrorCode::Unknown)
        .label(
            Span {
                file: "string.rbl".into(),
                line_range: (0, 0),
                col_range: (0, 0),
                offset_range: (37, 52),
            },
            format!("This is a test error"),
        );

        let sources = SourceFiles::new_with_stdlib(vec![SourceFile::test(
            "string.rbl".into(),
            "string.rbl".into(),
            include_str!("../../rebel_std/string.rbl").into(),
        )]);

        println!("{}", err.render(&sources));
    }
}
