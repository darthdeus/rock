use ast_walker::{AstNode, AstWalkEvent};
use compiler_error::CompilerError;

use crate::compiler_error::CompilerErrorCode;

use super::*;

/// The first pass of the semantic analysis. This pass is responsible for the
/// following actions:
///
/// - Adding all the declared symbols (variables, function arguments, functions,
///   structs, ...) to the symbol table, with Unknown type.
/// - Creates and stores the scopes for functions and blocks.
/// - Register variable usages: Unlike functions and structs, variables can be
///   shadowed by other variables in the current scope, so we have to resolve
///   this as we walk the AST during this first pass.
pub fn declaration_pass(
    top_level: &Vec<TopLevel>,
    scope_builder: &mut ScopeBuilder,
) -> Result<(), CompilerError> {
    // profile_scope!("rock_compiler", "declaration_pass::declaration_pass");

    let mut is_on_impl_block = false;

    let mut symbol_stack = vec![SymbolName::root()];
    macro_rules! child_sym {
        ($name:expr) => {
            symbol_stack.last().unwrap().child($name)
        };
    }

    for item in top_level.into_iter() {
        ast_walker::AstWalker::walk_top_level(item, &mut |ev| match ev {
            AstWalkEvent::OnEnter(AstNode::Item(ast::Item {
                kind: ast::ItemKind::ImplBlock(impl_block),
                ..
            })) => {
                scope_builder.enter_impl_scope(impl_block);
                is_on_impl_block = true;

                symbol_stack.push(child_sym!(&impl_block.ty_expr.to_string()));

                Ok(())
            }
            AstWalkEvent::OnExit(AstNode::Item(ast::Item {
                kind: ast::ItemKind::ImplBlock(_),
                ..
            })) => {
                scope_builder.exit_scope();
                is_on_impl_block = false;

                symbol_stack.pop();

                Ok(())
            }

            AstWalkEvent::OnEnter(AstNode::FunctionDeclaration(fndecl)) => {
                // Unlike variables, functions cannot shadow each other if they're
                // in the same scope
                match scope_builder.table.lookup_symbol_in_scope_by_string(
                    &fndecl.name.text,
                    scope_builder.current_scope(),
                ) {
                    Some((_, scope)) if scope == scope_builder.current_scope() => {
                        return Err(CompilerError::new(
                            fndecl.span.start(),
                            format!("Function '{}' already exists", fndecl.name.text),
                        )
                        .code(CompilerErrorCode::DuplicateSymbol)
                        .label(
                            fndecl.name.span.clone(),
                            "This function is already declared".to_owned(),
                        ))
                    }
                    _ => (),
                }

                scope_builder.start_function_declaration(
                    symbol_stack.last().unwrap(),
                    fndecl,
                    is_on_impl_block, /* is_method */
                );

                // Add the function arguments to the symbol table
                for arg in &fndecl.args {
                    scope_builder.declare_function_argument(&fndecl, &arg);
                }

                Ok(())
            }

            AstWalkEvent::OnExit(AstNode::FunctionDeclaration(fndecl)) => {
                // This closes the scope where function arguments were defined
                scope_builder.finish_function_declaration(fndecl);
                Ok(())
            }
            AstWalkEvent::OnEnter(AstNode::Block(block)) => {
                scope_builder.enter_block_scope(block);
                Ok(())
            }
            AstWalkEvent::OnExit(AstNode::Block(_)) => {
                scope_builder.exit_scope();
                Ok(())
            }

            AstWalkEvent::OnEnter(AstNode::Statement(statement)) => {
                match &statement.kind {
                    ast::StatementKind::Comment(_) => {}
                    ast::StatementKind::BlankLine => {}
                    ast::StatementKind::Nothing => {}
                    ast::StatementKind::If {
                        cond: _,
                        then_block: _,
                        else_block: _,
                    } => {}
                    ast::StatementKind::Let {
                        ident, ty_expr: _, ..
                    } => {
                        scope_builder.declare_variable(ident);
                    }
                    ast::StatementKind::For {
                        var,
                        iterable: _,
                        body: _,
                    } => {
                        scope_builder.declare_variable(var);
                    }
                    ast::StatementKind::While { .. } => (),
                    ast::StatementKind::Expression(_) => (),
                    ast::StatementKind::Return { .. } => (),
                    ast::StatementKind::Break { .. } => (),
                    ast::StatementKind::Continue { .. } => (),
                    ast::StatementKind::Assign { .. } => (),
                    // ast::StatementKind::Macro(_) => {
                    //     unreachable!("macros should be expanded before semantic analysis")
                    // }
                }
                Ok(())
            }

            // AstWalkEvent::OnEnter(AstNode::PatternMatchCase(case)) => {
            //     scope_builder.enter_match_case_scope(case);
            //     match &case.kind {
            //         ast::PatternMatchKind::EnumVariant(variant) => {
            //             if let Some(binding) = &variant.binding {
            //                 scope_builder.declare_variable(&binding);
            //             }
            //         }
            //         ast::PatternMatchKind::Wildcard => (),
            //     }
            //     Ok(())
            // }
            // AstWalkEvent::OnExit(AstNode::PatternMatchCase(_)) => {
            //     scope_builder.exit_scope();
            //     Ok(())
            // }

            AstWalkEvent::OnEnter(AstNode::Expression(expr)) => {
                match &expr.kind {
                    ast::ExprKind::Path(path) => {
                        // We ignore "variable not found" errors here, because the
                        // variable might be a global, and globals support being
                        // declared after their usages.
                        //
                        // Note that we still have to declare usages here because in
                        // case it is a local variable, we don't want to accept this
                        // situation:
                        //
                        // x = x + 1; let x = 0;
                        //
                        // During the usage pass, we will try to find any undeclared
                        // variables in scope again, but only accept their usages if the
                        // variable is declared as global.
                        //
                        if path.idents.len() == 1 {
                            let _ = scope_builder
                                .register_identifier_symbol_usage(path.as_single_ident());
                        } else {
                            // Multi-segment paths are not local variable
                            // references. They are resolved in a different way.
                        }
                    }

                    // ast::ExprKind::StructLiteral {
                    //     struct_ident: _,
                    //     fields: _,
                    // } => {
                    //     // TODO: In the future we may want to register usages of struct
                    //     // field names here. It is not useful to the compiler right now,
                    //     // but it could be useful for an LSP.
                    // }
                    //
                    ast::ExprKind::SelfIdent => (),
                    ast::ExprKind::NumLiteral(_) => (),
                    ast::ExprKind::BoolLiteral(_) => (),
                    ast::ExprKind::StringLiteral(_) => (),
                    ast::ExprKind::OptionSomeLiteral(_) => (),
                    ast::ExprKind::OptionNoneLiteral => (),
                    ast::ExprKind::FunctionCall { .. } => (),
                    ast::ExprKind::BinaryOp { .. } => (),
                    // ast::ExprKind::UnaryOp { .. } => (),
                    ast::ExprKind::ParenExpr(_) => (),
                    // ast::ExprKind::Typecast { .. } => (),
                    ast::ExprKind::FieldAccess { .. } => (),
                    // ast::ExprKind::MethodCall { .. } => (),
                    ast::ExprKind::Block(_) => (),
                    ast::ExprKind::If { .. } => (),
                    // ast::ExprKind::StringInterpolation { .. } => (),
                    ast::ExprKind::Index { .. } => (),
                    ast::ExprKind::NullLiteral => (),
                    // ast::ExprKind::VectorBuiltin { .. } => (),
                    // ast::ExprKind::TypeOf { .. } => (),
                    // ast::ExprKind::Transmute { .. } => (),
                    // ast::ExprKind::LlvmIntrinsicCall { .. } => (),
                    // ast::ExprKind::Match { .. } => (),
                    // ast::ExprKind::Macro(_) => {
                    //     unreachable!("macros should be expanded before semantic analysis")
                    // }
                }
                Ok(())
            }

            // AstWalkEvent::OnEnter(AstNode::StructDeclaration(struct_)) => {
            //     scope_builder.declare_struct(
            //         symbol_stack.last().unwrap(),
            //         &struct_.name,
            //         struct_
            //             .fields
            //             .iter()
            //             .map(|StructFieldDeclaration { name: ident, .. }| ident),
            //     );
            //     Ok(())
            // }
            // AstWalkEvent::OnEnter(AstNode::EnumDeclaration(enum_)) => {
            //     scope_builder.declare_enum(
            //         symbol_stack.last().unwrap(),
            //         &enum_.name,
            //         enum_.variants.iter().map(|(ident, _)| ident),
            //     );
            //     Ok(())
            // }

            // AstWalkEvent::OnExit(AstNode::Item(item)) => {
            //     // Fundamental named types are structs that are implemented in the
            //     // language, but whose type id is also pre-populated in the type
            //     // table, because they are used by some language construct.
            //     //
            //     // One such example is the `TypeInfo` struct, which has both a
            //     // well-known type id and also a type declaration in an stdlib
            //     // source file.
            //     //
            //     // In those cases, the "pre-registration" mechanism we use for named
            //     // types may lead to trouble, because the registered id and the
            //     // well-known id would differ. This is why they need special
            //     // handling here.
            //     if let Some(ann) = item
            //         .annotations
            //         .iter()
            //         .find(|x| x.name.text == "fundamental_named_type")
            //     {
            //         if ann.tokens.len() != 1 {
            //             return Err(CompilerError::new(
            //                 ann.span.start(),
            //                 "Expected exactly one token in annotation".to_owned(),
            //             ));
            //         }
            //         let ty_name = match &ann.tokens[0] {
            //             crate::lexer::TokenKind::String(name) => name,
            //             _ => {
            //                 return Err(CompilerError::new(
            //                     ann.span.start(),
            //                     "Expected string literal in annotation".to_owned(),
            //                 ))
            //             }
            //         };
            //
            //         let symbol_id =
            //             match &item.kind {
            //                 ast::ItemKind::Struct(ast::StructDeclaration { name, .. })
            //                 | ast::ItemKind::Enum(ast::EnumDeclaration { name, .. }) => {
            //                     scope_builder
            //                         .table
            //                         .lookup_symbol_in_scope_by_string(
            //                             &name.text,
            //                             scope_builder.current_scope(),
            //                         )
            //                         .unwrap()
            //                         .0
            //                 }
            //                 _ => return Err(CompilerError::new(
            //                     item.span.start(),
            //                     "The @fundamental_named_type annotation only works for named types"
            //                         .to_owned(),
            //                 )),
            //             };
            //         scope_builder.declare_fundamental_named_type(symbol_id, ty_name);
            //     }
            //     Ok(())
            // }

            // AstWalkEvent::OnEnter(AstNode::GlobalDeclaration(global)) => {
            //     scope_builder.declare_global(symbol_stack.last().unwrap(), &global.name);
            //     Ok(())
            // }
            _ => Ok(()),
        })?;
    }

    Ok(())
}
