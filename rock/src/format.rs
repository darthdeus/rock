use crate::*;

#[derive(Default)]
pub struct FormatStyle {}

pub fn format_file(file: &str) -> Result<String> {
    Ok(format_top_level(parse(file)?))
}

pub fn format_top_level(top_level: Vec<TopLevel>) -> String {
    top_level
        .into_iter()
        .map(|x| x.format(&FormatStyle::default()))
        .collect::<Vec<_>>()
        .join("\n")
}

pub trait Format {
    fn format(&self, style: &FormatStyle) -> String;
}

impl Format for TopLevel {
    fn format(&self, style: &FormatStyle) -> String {
        match self {
            TopLevel::Statement(s) => s.format(style),
            TopLevel::Function(f) => f.format(style),
        }
    }
}

impl Format for Statement {
    fn format(&self, _: &FormatStyle) -> String {
        format!("Statement")
    }
}

impl Format for FunctionDef {
    fn format(&self, style: &FormatStyle) -> String {
        let param_list = self
            .params
            .iter()
            .map(|x| x.format(style))
            .collect::<Vec<_>>()
            .join(", ");

        let body = self.body.format(style);

        format!("fn {}({}) {{\n{}\n}}", self.name.text, param_list, body)
    }
}

impl Format for FunctionParam {
    fn format(&self, style: &FormatStyle) -> String {
        match self {
            FunctionParam::Typed(ident, ty) => {
                format!("{}: {}", ident.format(style), ty.format(style))
            }
            FunctionParam::Untyped(ident) => ident.format(style),
        }
    }
}

impl Format for Ident {
    fn format(&self, _: &FormatStyle) -> String {
        self.text.to_owned()
    }
}

impl Format for TypeExpr {
    fn format(&self, style: &FormatStyle) -> String {
        match &self.kind {
            TypeExprKind::Named(ident) => ident.format(style),
        }
    }
}

impl Format for Block {
    fn format(&self, _: &FormatStyle) -> String {
        "".to_string()
    }
}
