use anyhow::Result;
use compiler_error::CompilerError;
use debug::NodeExt;
use log::error;
use source_code::{LineCol, SourceFile};
use tree_sitter::Node;

use crate::*;

macro_rules! bail_unexpected {
    ($node:expr, $source:expr, $ty:expr) => {
        $node.report_unexpected_kind($source, $ty);
        let span = $node.to_source_span($source);
        return Err(CompilerError::new(
            span.start(),
            format!("unexpected {} kind: '{}'", $ty, $node.kind()),
        ));
        // bail!("unexpected {} kind: '{}'", $ty, $node.kind());
    };
}

macro_rules! field_or_bail {
    ($node:expr, $field_name:expr, $source:expr) => {
        $node.child_by_field_name($field_name).ok_or_else(|| {
            let msg = format!("No field '{}' on node {}", $field_name, $node.to_sexp());

            $node.report_error($source, &msg);

            return CompilerError::new($node.to_source_span($source).start(), msg);
        })?
    };
}

pub fn parse_file(source: &SourceFile) -> Result<Vec<TopLevel>, Vec<CompilerError>> {
    let source = Source {
        code: source.contents().to_string(),
        file: Some(source.path().into()),
    };

    match parse_source(&source) {
        Ok(res) => Ok(res),
        Err(e) => Err(vec![e]),
    }
}

pub struct Source {
    pub code: String,
    pub file: Option<ustr::Ustr>,
}

pub fn tree_sitter_parse(source: &Source) -> Result<tree_sitter::Tree, CompilerError> {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_rock::LANGUAGE.into())
        .map_err(|e| {
            CompilerError::new(
                LineCol::unknown(),
                format!(
                    "Error setting treesitter parser language: '{}'.
                        This is a bug in the tree-sitter-rock parser, or treesitter itself.",
                    e
                ),
            )
        })?;

    let tree = parser.parse(&source.code, None).unwrap();

    Ok(tree)
}

pub fn parse_source(source: &Source) -> Result<Vec<TopLevel>, CompilerError> {
    let tree = tree_sitter_parse(source)?;
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

pub fn parse_statement(
    node: Node,
    source: &Source,
    builder: &mut AstBuilder,
) -> Result<Statement, CompilerError> {
    // if node.text(source)?.chars().all(|x| x.is_whitespace()) {
    //     return Ok(Statement {
    //         id: builder.id_gen(),
    //         span: node.to_source_span(source),
    //         kind: StatementKind::Nothing,
    //     });
    // }

    let node = node.child(0).ok_or_else(|| {
        let msg = format!(
            "Statement must have a child, got '{}'",
            node.text(source).unwrap()
        );
        node.report_error(source, &msg);
        // anyhow!("Statement must have a child")
        CompilerError::new(node.to_source_span(source).start(), msg)
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
            // let expr_node = node
            //     .child_by_field_name("expr")
            //     .ok_or_else(|| anyhow!("No expr on return"))?;

            let expr_node = field_or_bail!(node, "expr", source);

            let expr = parse_expression(expr_node, source, builder)?;
            StatementKind::Return(Some(expr))
        }

        "let" => {
            // let ident_node = node
            //     .child_by_field_name("ident")
            //     .ok_or_else(|| anyhow!("No ident on let"))?;

            let ident_node = field_or_bail!(node, "ident", source);

            let ident = Ident {
                id: builder.id_gen(),
                span: ident_node.to_source_span(source),
                text: ident_node.text(source)?.into(),
            };

            let ty_expr = if let Some(node) = node.child_by_field_name("type_annotation") {
                // let type_expr_node = node
                //     .child_by_field_name("type")
                //     .ok_or_else(|| anyhow!("No type on let"))?;

                let type_expr_node = field_or_bail!(node, "type", source);

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

            // let expr_node = node
            //     .child_by_field_name("expr")
            //     .ok_or_else(|| anyhow!("No expr on let"))?;

            let expr_node = field_or_bail!(node, "expr", source);

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
) -> Result<FunctionDeclaration, CompilerError> {
    let name_node = field_or_bail!(child, "name", source);
    // .child_by_field_name("name")
    // .ok_or_else(|| anyhow!("No name on function_def"))?;
    //
    let fn_name = name_node.text(source)?;

    let params_node = field_or_bail!(child, "parameters", source);

    // let params_node = child
    //     .child_by_field_name("parameters")
    //     .ok_or_else(|| anyhow!("No parameters on function_def"))?;

    // Extract parameters from the parameters_node
    let mut params = Vec::new();

    for i in 0..params_node.named_child_count() {
        if let Some(node) = params_node.named_child(i) {
            let param_text = node.text(source)?;

            match node.kind() {
                "typed_param" => {
                    let type_expr_node = field_or_bail!(node, "type_expr", source);
                    // let type_expr_node = node
                    //     .child_by_field_name("type_expr")
                    //     .ok_or_else(|| anyhow!("No type on typed_param"))?;

                    let ident_node = field_or_bail!(node, "ident", source);

                    let ident = Ident {
                        id: builder.id_gen(),
                        span: node.to_source_span(source),
                        text: ident_node.text(source)?.into(),
                    };

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
                    let ident = Ident {
                        id: builder.id_gen(),
                        span: node.to_source_span(source),
                        text: param_text.into(),
                    };

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

    // let body_node = child
    //     .child_by_field_name("body")
    //     .ok_or_else(|| anyhow!("No body on function_def"))?;

    let body_node = field_or_bail!(child, "body", source);
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

pub fn parse_block(
    node: Node,
    source: &Source,
    builder: &mut AstBuilder,
) -> Result<Block, CompilerError> {
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

pub fn parse_expression(
    node: Node,
    source: &Source,
    builder: &mut AstBuilder,
) -> Result<Expr, CompilerError> {
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
            let num = num_text.parse::<f64>().map_err(|_| {
                let msg = format!("Could not parse number: '{}'", num_text);
                node.report_error(source, &msg);
                CompilerError::new(node.to_source_span(source).start(), msg)
            })?;

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
                    return Err(CompilerError::new(node.to_source_span(source).start(), msg));
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
            let node = node.child(0).ok_or_else(|| {
                CompilerError::new(
                    node.to_source_span(source).start(),
                    "Expression must have a child".to_string(),
                )
            })?;

            return parse_expression(node, source, builder);
        }

        _ => {
            error!("node.kind(): '{}'", node.to_sexp());
            bail_unexpected!(node, source, "expression");
        }
    };

    Ok(Expr {
        id: builder.id_gen(),
        span: node.to_source_span(source),
        kind,
    })
}

#[test]
fn parse_ident_test() {
    let source = Source {
        code: "let x: num = 1;".to_string(),
        file: None,
    };
    let res = parse_source(&source);

    let res = res.unwrap();

    assert_eq!(res.len(), 1);
    match &res[0] {
        TopLevel::Statement(stmt) => match &stmt.kind {
            StatementKind::Let { ident, .. } => {
                assert_eq!(ident.text, "x");
            }
            _ => panic!("unexpected statement kind"),
        },
        _ => panic!("unexpected toplevel kind"),
    }
}

#[test]
fn parse_simple_function() {
    let source = Source {
        code: "fn foo(x: num, y) { }".to_string(),
        file: None,
    };

    let res = parse_source(&source).unwrap();

    assert_eq!(res.len(), 1);

    match &res[0] {
        TopLevel::Function(f) => {
            assert_eq!(f.name.text, "foo");
            assert_eq!(f.args.len(), 2);

            match &f.args[0] {
                FunctionParam::Typed(ident, _) => {
                    assert_eq!(ident.text, "x");
                }
                _ => panic!("unexpected function param kind"),
            }

            match &f.args[1] {
                FunctionParam::Untyped(ident) => {
                    assert_eq!(ident.text, "y");
                }
                _ => panic!("unexpected function param kind"),
            }
        }
        _ => panic!("unexpected toplevel kind"),
    }
}
