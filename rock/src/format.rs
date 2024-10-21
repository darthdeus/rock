use compiler_error::CompilerError;
use parser::parse;

use crate::*;

#[derive(Default)]
pub struct FormatStyle {}

pub fn format_file(file: &str) -> Result<String, CompilerError> {
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

impl Format for IdentPath {
    fn format(&self, style: &FormatStyle) -> String {
        self.idents
            .iter()
            .map(|x| x.format(style))
            .collect::<Vec<_>>()
            .join("::")
    }
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
    fn format(&self, s: &FormatStyle) -> String {
        match &self.kind {
            StatementKind::Comment(text) => text.text.clone(),
            StatementKind::BlankLine => "".to_string(),
            StatementKind::Nothing => "".to_string(),
            StatementKind::Expression(expr) => expr.format(s),
            StatementKind::Return(None) => "return;".to_string(),
            StatementKind::Return(Some(expr)) => format!("return {};", expr.format(s)),
            StatementKind::Break => "break".to_string(),
            StatementKind::Continue => "continue".to_string(),
            StatementKind::Let {
                ident,
                ty_expr,
                expr,
            } => {
                let ty_expr = match ty_expr {
                    Some(t) => format!(": {}", t.format(s)),
                    None => "".to_string(),
                };
                format!("let {}{} = {};", ident.format(s), ty_expr, expr.format(s))
            }
            StatementKind::Assign { lhs, rhs } => format!("{} = {};", lhs.format(s), rhs.format(s)),
            StatementKind::If {
                cond,
                then_block,
                else_block,
            } => {
                let else_block = match else_block {
                    Some(b) => format!("else {{\n{}\n}}", b.format(s)),
                    None => "".to_string(),
                };
                format!(
                    "if {} {{\n{}\n}} {}",
                    cond.format(s),
                    then_block.format(s),
                    else_block
                )
            }
            StatementKind::For {
                var,
                iterable,
                body,
            } => {
                format!(
                    "for {} in {} {{\n{}\n}}",
                    var.format(s),
                    iterable.format(s),
                    body.format(s)
                )
            }
            StatementKind::While { cond, body } => {
                format!("while {} {{\n{}\n}}", cond.format(s), body.format(s))
            }
        }
    }
}

impl Format for Expr {
    fn format(&self, s: &FormatStyle) -> String {
        match &self.kind {
            ExprKind::Path(ident) => ident.format(s),
            ExprKind::NumLiteral(num) => num.to_string(),
            ExprKind::SelfIdent => "self".to_string(),
            ExprKind::BoolLiteral(val) => val.to_string(),
            ExprKind::StringLiteral(string) => string.clone(),
            ExprKind::NullLiteral => "null".to_string(),
            ExprKind::OptionSomeLiteral(expr) => {
                format!("Some({})", expr.format(s))
            }
            ExprKind::OptionNoneLiteral => "None".to_string(),
            ExprKind::FunctionCall { ident, args } => {
                let args = args
                    .iter()
                    .map(|x| x.format(s))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", ident.format(s), args)
            }
            ExprKind::BinaryOp { op, left, right } => {
                format!("{} {} {}", left.format(s), op.to_string(), right.format(s))
            }
            ExprKind::FieldAccess { field, base } => {
                format!("{}.{}", base.format(s), field.format(s))
            }
            ExprKind::Index { base, index } => {
                format!("{}[{}]", base.format(s), index.format(s))
            }

            ExprKind::ParenExpr(expr) => {
                format!("({})", expr.format(s))
            }
            ExprKind::Block(block) => block.format(s),
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let else_branch = match else_branch {
                    Some(b) => format!("else {{\n{}\n}}", b.format(s)),
                    None => "".to_string(),
                };
                format!(
                    "if {} {{\n{}\n}} {}",
                    condition.format(s),
                    then_branch.format(s),
                    else_branch
                )
            }
        }
    }
}

impl Format for FunctionDeclaration {
    fn format(&self, style: &FormatStyle) -> String {
        let param_list = self
            .args
            .iter()
            .map(|x| x.format(style))
            .collect::<Vec<_>>()
            .join(", ");

        let body = self.body.format(style);

        if self.return_type.is_none() {
            format!("fn {}({}) {}", self.name.text, param_list, body)
        } else {
            format!(
                "fn {}({}) -> {} {}",
                self.name.text,
                param_list,
                self.return_type.as_ref().unwrap().format(style),
                body
            )
        }
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
            TypeExprKind::Unit => "()".to_string(),
            TypeExprKind::String => "string".to_string(),
        }
    }
}

impl Format for Block {
    fn format(&self, s: &FormatStyle) -> String {
        let statements = self
            .statements
            .iter()
            .map(|x| format!("    {}", x.format(s)))
            .collect::<Vec<_>>()
            .join("\n");

        let return_expr = match &self.return_expr {
            Some(expr) => format!("return {};", expr.format(s)),
            None => "".to_string(),
        };

        format!(
            "{{\n{}\n}}",
            if return_expr.is_empty() {
                statements.to_string()
            } else {
                format!("{}\n{}", statements, return_expr)
            }
        )
    }
}
