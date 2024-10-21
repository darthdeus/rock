use anyhow::{anyhow, bail, Result};
use debug::NodeExt;
use tree_sitter::Node;

use crate::*;

macro_rules! bail_unexpected {
    ($node:expr, $source:expr, $ty:expr) => {
        $node.report_unexpected_kind($source, $ty);
        bail!("unexpected {} kind: '{}'", $ty, $node.kind());
    };
}

macro_rules! field_or_bail {
    ($node:expr, $field_name:expr, $source:expr) => {
        $node.child_by_field_name($field_name).ok_or_else(|| {
            $node.report_error(
                $source,
                &format!("No field '{}' on node {}", $field_name, $node.to_sexp()),
            );
            anyhow!("No field '{}' on node", $field_name)
        })?
    };
}

pub fn parse(source: &str) -> Result<Vec<TopLevel>> {
    let mut parser = parser::Parser::new();

    let top_level = parser.parse(&Source {
        code: source.to_string(),
        file: Some("file.rock".into()),
    })?;

    Ok(top_level)
}

pub struct Source {
    pub code: String,
    pub file: Option<ustr::Ustr>,
}

pub struct Parser {}

impl Parser {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse(&mut self, source: &Source) -> Result<Vec<TopLevel>> {
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_rock::LANGUAGE.into())?;

        let tree = parser.parse(&source.code, None).unwrap();

        let root = tree.root_node();
        let mut cursor = root.walk();

        let mut top_level = Vec::<TopLevel>::new();

        let mut builder = AstBuilder::new();

        for child in root.children(&mut cursor) {
            match child.kind() {
                "statement" => {
                    let statement = parse_statement(child, source, &mut builder)?;

                    if let StatementKind::Nothing = statement.kind {
                        continue;
                    }

                    top_level.push(TopLevel::Statement(statement));
                }

                "function_def" => {
                    top_level.push(TopLevel::Function(parse_function_def(
                        child,
                        source,
                        &mut builder,
                    )?));
                }

                _ => {
                    bail_unexpected!(child, source, "top_level");
                }
            }
        }

        Ok(top_level)
    }
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}

pub fn parse_statement(node: Node, source: &Source, builder: &mut AstBuilder) -> Result<Statement> {
    // if node.text(source)?.chars().all(|x| x.is_whitespace()) {
    //     return Ok(Statement {
    //         id: builder.id_gen(),
    //         span: node.to_source_span(source),
    //         kind: StatementKind::Nothing,
    //     });
    // }

    let node = node.child(0).ok_or_else(|| {
        node.report_error(
            source,
            &format!(
                "Statement must have a child, got '{}'",
                node.text(source).unwrap()
            ),
        );
        anyhow!("Statement must have a child")
    })?;

    let kind = match node.kind() {
        "comment" => StatementKind::Comment(Comment {
            id: builder.id_gen(),
            span: node.to_source_span(source),
            text: node.text(source)?.to_string(),
        }),

        "blank_line" => StatementKind::BlankLine,
        "newline" => StatementKind::Nothing,

        "expression" => {
            let expr = parse_expression(node, source, builder)?;
            StatementKind::Expression(expr)
        }

        "return" => {
            let expr_node = node
                .child_by_field_name("expr")
                .ok_or_else(|| anyhow!("No expr on return"))?;

            let expr = parse_expression(expr_node, source, builder)?;
            StatementKind::Return(Some(expr))
        }

        "let" => {
            let ident_node = node
                .child_by_field_name("ident")
                .ok_or_else(|| anyhow!("No ident on let"))?;

            let ident = Ident {
                id: builder.id_gen(),
                span: ident_node.to_source_span(source),
                text: ident_node.text(source)?.into(),
            };

            let ty_expr = if let Some(node) = node.child_by_field_name("type") {
                let type_expr_node = node
                    .child_by_field_name("type")
                    .ok_or_else(|| anyhow!("No type on let"))?;

                let ty_expr = TypeExpr {
                    id: builder.id_gen(),
                    span: type_expr_node.to_source_span(source),
                    kind: TypeExprKind::Named(Ident {
                        id: builder.id_gen(),
                        span: type_expr_node.to_source_span(source),
                        text: type_expr_node.text(source)?.into(),
                    }),
                };

                Some(ty_expr)
            } else {
                None
            };

            let expr_node = node
                .child_by_field_name("expr")
                .ok_or_else(|| anyhow!("No expr on let"))?;
            let expr = parse_expression(expr_node, source, builder)?;

            StatementKind::Let {
                ident,
                ty_expr,
                expr,
            }
        }

        _ => {
            bail_unexpected!(node, source, "statement");
        }
    };

    Ok(Statement {
        id: builder.id_gen(),
        span: node.to_source_span(source),
        kind,
    })
}

pub fn parse_function_def(
    child: Node,
    source: &Source,
    builder: &mut AstBuilder,
) -> Result<FunctionDeclaration> {
    let name_node = child
        .child_by_field_name("name")
        .ok_or_else(|| anyhow!("No name on function_def"))?;
    let fn_name = name_node.text(source)?;

    let params_node = child
        .child_by_field_name("parameters")
        .ok_or_else(|| anyhow!("No parameters on function_def"))?;

    // Extract parameters from the parameters_node
    let mut params = Vec::new();

    for i in 0..params_node.named_child_count() {
        if let Some(node) = params_node.named_child(i) {
            let param_text = node.text(source)?;

            let ident = Ident {
                id: builder.id_gen(),
                span: node.to_source_span(source),
                text: param_text.into(),
            };

            match node.kind() {
                "typed_param" => {
                    let type_expr_node = node
                        .child_by_field_name("type_expr")
                        .ok_or_else(|| anyhow!("No type on typed_param"))?;

                    let type_expr = TypeExpr {
                        id: builder.id_gen(),
                        span: type_expr_node.to_source_span(source),
                        kind: TypeExprKind::Named(Ident {
                            id: builder.id_gen(),
                            span: type_expr_node.to_source_span(source),
                            text: type_expr_node.text(source)?.into(),
                        }),
                    };

                    params.push(FunctionParam::Typed(ident, type_expr));
                }

                "untyped_param" => {
                    params.push(FunctionParam::Untyped(ident));
                }

                _ => {
                    bail_unexpected!(node, source, "param");
                }
            }
        }
    }

    let return_type_node = child.child_by_field_name("return_type");

    let return_type = if let Some(node) = return_type_node {
        let text = node.text(source)?;

        Some(TypeExpr {
            id: builder.id_gen(),
            span: node.to_source_span(source),
            kind: TypeExprKind::Named(Ident {
                id: builder.id_gen(),
                span: node.to_source_span(source),
                text: text.into(),
            }),
        })
    } else {
        None
    };

    let body_node = child
        .child_by_field_name("body")
        .ok_or_else(|| anyhow!("No body on function_def"))?;

    let body = parse_block(body_node, source, builder)?;

    Ok(FunctionDeclaration {
        id: builder.id_gen(),
        span: child.to_source_span(source),

        name: Ident {
            id: builder.id_gen(),
            span: child.to_source_span(source),
            text: fn_name.into(),
        },
        args: params,
        return_type,
        body,
        kind: FunctionDefKind::Standalone,
    })
}

pub fn parse_block(node: Node, source: &Source, builder: &mut AstBuilder) -> Result<Block> {
    let mut statements = Vec::new();
    let return_expr = None;

    for i in 0..node.named_child_count() {
        if let Some(node) = node.named_child(i) {
            let statement = parse_statement(node, source, builder)?;
            statements.push(statement);
        }
    }

    Ok(Block {
        id: builder.id_gen(),
        span: node.to_source_span(source),
        statements,
        return_expr,
    })
}

pub fn parse_expression(node: Node, source: &Source, builder: &mut AstBuilder) -> Result<Expr> {
    if node.child_count() == 0 {
        return Ok(Expr {
            id: builder.id_gen(),
            span: node.to_source_span(source),
            kind: ExprKind::Path(builder.ident_path(node, source)?),
        });
    }

    // println!("parsing expression: '{}'", node.to_sexp());

    let kind = match node.kind() {
        "identifier" => ExprKind::Path(builder.ident_path(node, source)?),

        "function_call" => {
            let ident_node = field_or_bail!(node, "ident", source);

            let ident = Ident {
                id: builder.id_gen(),
                span: node.to_source_span(source),
                text: ident_node.text(source)?.into(),
            };

            let mut args = Vec::new();

            // let args_node = node
            //     .child_by_field_name("args")
            //     .ok_or_else(|| anyhow!("No arguments on function_call"))?;

            // println!(
            //     "got args_node: {}\n\n{}\n\n",
            //     node.named_child_count(),
            //     node.to_sexp()
            // );

            // for i in 0..args_node.named_child_count() {
            //     if let Some(node) = args_node.named_child(i) {
            //         let expr = parse_expression(node, source, builder)?;
            //         args.push(expr);
            //     }
            // }

            for node in node.children_by_field_name("args", &mut node.walk()) {
                // println!("-- iterating child args: '{}'", node.to_sexp());

                if node.kind() == "expression" {
                    let expr = parse_expression(node, source, builder)?;
                    args.push(expr);
                }
            }

            ExprKind::FunctionCall { ident, args }
        }

        "number" => {
            let num_text = node.text(source)?;
            let num = num_text.parse::<f64>()?;
            ExprKind::NumLiteral(num)
        }

        "binary_op" => {
            let left_node = field_or_bail!(node, "left", source);
            let left = parse_expression(left_node, source, builder)?;

            let right_node = field_or_bail!(node, "right", source);
            let right = parse_expression(right_node, source, builder)?;

            let op_node = field_or_bail!(node, "op", source);

            let op = match op_node.text(source)? {
                "+" => BinOp::Add,
                "-" => BinOp::Sub,
                "*" => BinOp::Mul,
                "/" => BinOp::Div,
                "&&" => BinOp::And,
                "||" => BinOp::Or,
                "==" => BinOp::Eq,
                "!=" => BinOp::Neq,
                "<" => BinOp::Lt,
                ">" => BinOp::Gt,
                "<=" => BinOp::Le,
                ">=" => BinOp::Ge,
                "&" => BinOp::BitAnd,
                "|" => BinOp::BitOr,
                "^" => BinOp::BitXor,
                "!" => BinOp::BitNot,
                "<<" => BinOp::Shl,
                ">>" => BinOp::Shr,
                "%" => BinOp::Modulo,
                op => {
                    let msg = format!("unknown operator: '{}'", op);
                    node.report_error(source, &msg);
                    bail!(msg);
                }
            };

            ExprKind::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            }
        }

        "field_access" => {
            let base_node = field_or_bail!(node, "base", source);
            let base = parse_expression(base_node, source, builder)?;
            let field_node = field_or_bail!(node, "field", source);
            let field = field_node.text(source)?.into();

            ExprKind::FieldAccess {
                field: Ident {
                    id: builder.id_gen(),
                    span: field_node.to_source_span(source),
                    text: field,
                },
                base: Box::new(base),
            }
        }

        "index" => {
            let base_node = field_or_bail!(node, "base", source);
            let base = parse_expression(base_node, source, builder)?;

            let index_node = field_or_bail!(node, "index", source);
            let index = parse_expression(index_node, source, builder)?;

            ExprKind::Index {
                base: Box::new(base),
                index: Box::new(index),
            }
        }

        "string" => ExprKind::StringLiteral(node.text(source)?.into()),

        // Sometimes exprs are nested for some reason.
        // For now we don't care, this works.
        "expression" => {
            let node = node
                .child(0)
                .ok_or_else(|| anyhow!("Expression must have a child"))?;

            return parse_expression(node, source, builder);
        }

        _ => {
            println!("node.kind(): '{}'", node.to_sexp());
            bail_unexpected!(node, source, "expression");
        }
    };

    Ok(Expr {
        id: builder.id_gen(),
        span: node.to_source_span(source),
        kind,
    })
}
