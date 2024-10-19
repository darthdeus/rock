use anyhow::{anyhow, bail, Result};
use source_code::Span;
use tree_sitter::Node;

use crate::*;

pub struct Parser {
    id_gen: AstNodeIdGen,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            id_gen: AstNodeIdGen::new(),
        }
    }

    pub fn parse(&mut self, source: &str) -> Result<Vec<TopLevel>> {
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_rock::LANGUAGE.into())?;

        let tree = parser.parse(source, None).unwrap();

        let root = tree.root_node();
        let mut cursor = root.walk();

        let mut top_level = Vec::<TopLevel>::new();

        for child in root.children(&mut cursor) {
            match child.kind() {
                "statement" => {
                    top_level.push(TopLevel::Statement(parse_statement(
                        child,
                        source,
                        &mut self.id_gen,
                    )?));
                }

                "function_def" => {
                    top_level.push(TopLevel::Function(parse_function_def(
                        child,
                        source,
                        &mut self.id_gen,
                    )?));
                }

                _ => {
                    bail!("unexpected child: {:?}", child);
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

pub fn parse_statement(node: Node, source: &str, id_gen: &mut AstNodeIdGen) -> Result<Statement> {
    let node = node
        .child(0)
        .ok_or_else(|| anyhow!("Statement must have a child"))?;

    let kind = match node.kind() {
        "comment" => {
            let text = node.utf8_text(source.as_bytes())?;
            StatementKind::Comment(Comment {
                id: id_gen.id_gen(),
                span: Span::unknown(),
                text: text.to_string(),
            })
        }

        "expression" => {
            let expr = parse_expression(node, source, id_gen)?;
            StatementKind::Expression(expr)
        }

        _ => {
            bail!("unexpected statement kind: '{}'", node.kind())
        }
    };

    Ok(Statement {
        id: id_gen.id_gen(),
        span: Span::unknown(),
        kind,
    })
}

pub fn parse_function_def(
    child: Node,
    source: &str,
    id_gen: &mut AstNodeIdGen,
) -> Result<FunctionDef> {
    let name_node = child
        .child_by_field_name("name")
        .ok_or_else(|| anyhow!("No name on function_def"))?;
    let fn_name = name_node.utf8_text(source.as_bytes())?;

    let params_node = child
        .child_by_field_name("parameters")
        .ok_or_else(|| anyhow!("No parameters on function_def"))?;

    // Extract parameters from the parameters_node
    let mut params = Vec::new();

    for i in 0..params_node.named_child_count() {
        if let Some(node) = params_node.named_child(i) {
            let param_text = node.utf8_text(source.as_bytes())?;

            let ident = Ident {
                id: id_gen.id_gen(),
                span: Span::unknown(),
                text: param_text.into(),
            };

            let param_kind = node.kind();

            match param_kind {
                "typed_param" => {
                    let type_expr_node = node
                        .child_by_field_name("type_expr")
                        .ok_or_else(|| anyhow!("No type on typed_param"))?;

                    let type_expr = TypeExpr {
                        id: id_gen.id_gen(),
                        span: Span::unknown(),
                        kind: TypeExprKind::Named(Ident {
                            id: id_gen.id_gen(),
                            span: Span::unknown(),
                            text: type_expr_node.utf8_text(source.as_bytes())?.into(),
                        }),
                    };

                    params.push(FunctionParam::Typed(ident, type_expr));
                }

                "untyped_param" => {
                    params.push(FunctionParam::Untyped(ident));
                }

                _ => {
                    bail!("unexpected param kind: '{}'", param_kind);
                }
            }
        }
    }

    // println!("parsed params: {:#?}", params);

    let return_type_node = child.child_by_field_name("return_type");

    let return_type = if let Some(node) = return_type_node {
        let text = node.utf8_text(source.as_bytes())?;

        Some(TypeExpr {
            id: id_gen.id_gen(),
            span: Span::unknown(),
            kind: TypeExprKind::Named(Ident {
                id: id_gen.id_gen(),
                span: Span::unknown(),
                text: text.into(),
            }),
        })
    } else {
        None
    };

    let body_node = child
        .child_by_field_name("body")
        .ok_or_else(|| anyhow!("No body on function_def"))?;

    let body = parse_block(body_node, source, id_gen)?;

    Ok(FunctionDef {
        id: id_gen.id_gen(),
        span: Span::unknown(),

        name: Ident {
            id: id_gen.id_gen(),
            span: Span::unknown(),
            text: fn_name.into(),
        },
        params,
        return_type,
        body,
        kind: FunctionDefKind::Standalone,
    })
}

pub fn parse_block(node: Node, source: &str, id_gen: &mut AstNodeIdGen) -> Result<Block> {
    let mut statements = Vec::new();
    let return_expr = None;

    for i in 0..node.named_child_count() {
        if let Some(node) = node.named_child(i) {
            let statement = parse_statement(node, source, id_gen)?;
            statements.push(statement);
        }
    }

    Ok(Block {
        id: id_gen.id_gen(),
        span: Span::unknown(),
        statements,
        return_expr,
    })
}

pub fn parse_expression(node: Node, source: &str, id_gen: &mut AstNodeIdGen) -> Result<Expr> {
    if node.child_count() == 0 {
        return Ok(Expr {
            id: id_gen.id_gen(),
            span: Span::unknown(),
            kind: ExprKind::Path(Ident {
                id: id_gen.id_gen(),
                span: Span::unknown(),
                text: node.utf8_text(source.as_bytes())?.into(),
            }),
        });
    }

    let node = node
        .child(0)
        .ok_or_else(|| anyhow!("Expression must have a child"))?;

    let kind = match node.kind() {
        "function_call" => {
            let ident = Ident {
                id: id_gen.id_gen(),
                span: Span::unknown(),
                text: node.utf8_text(source.as_bytes())?.into(),
            };

            let mut args = Vec::new();

            let args_node = node
                .child_by_field_name("args")
                .ok_or_else(|| anyhow!("No arguments on function_call"))?;

            for i in 0..args_node.named_child_count() {
                if let Some(node) = args_node.named_child(i) {
                    let expr = parse_expression(node, source, id_gen)?;
                    args.push(expr);
                }
            }

            ExprKind::FunctionCall { ident, args }
        }

        _ => {
            bail!("unexpected expression kind: '{}'", node.kind());
        }
    };

    Ok(Expr {
        id: id_gen.id_gen(),
        span: Span::unknown(),
        kind,
    })
}
