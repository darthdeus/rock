use anyhow::{anyhow, bail, Result};
use source_code::Span;

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
                    // top_level.push(TopLevel::Statement(Statement::Yes));
                }

                "function_def" => {
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
                                id: self.id_gen.id_gen(),
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
                                        id: self.id_gen.id_gen(),
                                        span: Span::unknown(),
                                        kind: TypeExprKind::Named(Ident {
                                            id: self.id_gen.id_gen(),
                                            span: Span::unknown(),
                                            text: type_expr_node
                                                .utf8_text(source.as_bytes())?
                                                .into(),
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

                    println!("parsed params: {:#?}", params);

                    let return_type_node = child.child_by_field_name("return_type");

                    let return_type = if let Some(node) = return_type_node {
                        let text = node.utf8_text(source.as_bytes())?;

                        Some(TypeExpr {
                            id: self.id_gen.id_gen(),
                            span: Span::unknown(),
                            kind: TypeExprKind::Named(Ident {
                                id: self.id_gen.id_gen(),
                                span: Span::unknown(),
                                text: text.into(),
                            }),
                        })
                    } else {
                        None
                    };

                    let body = child
                        .child_by_field_name("body")
                        .ok_or_else(|| anyhow!("No body on function_def"))?;

                    top_level.push(TopLevel::Function(FunctionDef {
                        id: self.id_gen.id_gen(),
                        span: Span::unknown(),

                        name: Ident {
                            id: self.id_gen.id_gen(),
                            span: Span::unknown(),
                            text: fn_name.into(),
                        },
                        params: todo!(),
                        return_type,
                        body: todo!(),
                        kind: FunctionDefKind::Standalone,
                    }));
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
