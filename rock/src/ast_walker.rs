//! # A note to implementors
//!
//! This module tries to provide a generic way to walk the AST. Because Rust
//! does not have an abstraction mechanism over mutability, this module uses the
//! `duplicate` crate to introduce normal and mut variants of each type.
//!
//! This makes the code a bit more verbose, but it's still much better than
//! having to write the same code twice. Especially in a phase where the AST is
//! still changing rapidly.
//!
//! Note that `duplicate` plays reasonably well with rust-analyzer, but some
//! features, most notably adding missing variants to a case or generating
//! methods won't work and will break the code.
//!
//! Also note this code uses structs (AstWalker, AstWalkerMut) when otherwise
//! plain functions would suffice. This is done because by having a struct in
//! which we attach the methods, we can have the same method names for the ref
//! and mut variants, i.e. `AstWalker::walk_module` and
//! `AstWalkerMut::walk_module`, instead fo `walk_module` and `walk_module_mut`.
//!
use compiler_error::CompilerError;
use duplicate::duplicate_item;

use super::*;

#[duplicate_item(
    AstNodeBorrow     borrow(t);
    [AstNode]    [&'a t];
    [AstNodeMut] [&'a mut t];
)]
pub enum AstNodeBorrow<'a> {
    // Module(borrow([ast::Module])),
    Item(borrow([ast::Item])),
    FunctionDeclaration(borrow([ast::FunctionDeclaration])),
    // StructDeclaration(borrow([ast::StructDeclaration])),
    // EnumDeclaration(borrow([ast::EnumDeclaration])),
    // GlobalDeclaration(borrow([ast::GlobalDeclaration])),
    Block(borrow([ast::Block])),
    Statement(borrow([ast::Statement])),
    Expression(borrow([ast::Expr])),
    TypeExpression(borrow([ast::TypeExpr])),
    // PatternMatchCase(borrow([ast::PatternMatchCase])),
}

#[duplicate_item(
    AstWalkEventBorrow  AstNodeBorrow;
    [AstWalkEvent]      [AstNode];
    [AstWalkEventMut]   [AstNodeMut];
)]
pub enum AstWalkEventBorrow<'a> {
    OnEnter(AstNodeBorrow<'a>),
    OnExit(AstNodeBorrow<'a>),
}

/// Provides several functions to walk an immutable borrow of the AST immutably
/// by emitting several `AstWalkEvent`s as it traverses.
pub struct AstWalker;

/// Same as `AstWalker`, but walks a mutable borrow of the AST.
pub struct AstWalkerMut;

#[duplicate_item(
    AstWalker        AstNode        AstWalkEvent        borrow(t);
    [AstWalker]      [AstNode]      [AstWalkEvent]      [&t];
    [AstWalkerMut]   [AstNodeMut]   [AstWalkEventMut]   [&mut t];
)]
impl AstWalker {
    // /// Walks the given module, emitting `AstWalkEvent`s as it traverses.
    // /// Will call the given callback for each event.
    // pub fn walk_module(
    //     module: borrow([ast::Module]),
    //     cb: &mut impl FnMut(AstWalkEvent) -> Result<(), CompilerError>,
    // ) -> Result<(), CompilerError> {
    //     cb(AstWalkEvent::OnEnter(AstNode::Module(module)))?;
    //     for item in borrow([module.items]) {
    //         Self::walk_item(item, cb)?;
    //     }
    //     cb(AstWalkEvent::OnExit(AstNode::Module(module)))?;
    //     Ok(())
    // }

    pub fn walk_item(
        item: borrow([ast::Item]),
        cb: &mut impl FnMut(AstWalkEvent) -> Result<(), CompilerError>,
    ) -> Result<(), CompilerError> {
        cb(AstWalkEvent::OnEnter(AstNode::Item(item)))?;
        match borrow([item.kind]) {
            ast::ItemKind::Function(function) => {
                Self::walk_function_declaration(function, cb)?;
            }
            // ast::ItemKind::Global(global) => {
            //     Self::walk_global_declaration(global, cb)?;
            // }
            // ast::ItemKind::Struct(struct_) => {
            //     Self::walk_struct_declaration(struct_, cb)?;
            // }
            // ast::ItemKind::Enum(enum_) => {
            //     Self::walk_enum_declaration(enum_, cb)?;
            // }
            ast::ItemKind::ImplBlock(impl_) => {
                Self::walk_type_expression(borrow([impl_.ty_expr]), cb)?;
                for item in borrow([impl_.inner]) {
                    Self::walk_item(item, cb)?;
                }
            } // ast::ItemKind::Macro(_) => {
              //     // We don't emit any events for macros expressions. Users
              //     // should match on the wrapper AST node instead.
              // }
              // ast::ItemKind::MacroDefinition(_) => (),
        }
        cb(AstWalkEvent::OnExit(AstNode::Item(item)))?;
        Ok(())
    }

    /// Walks the given function declaration, emitting `AstWalkEvent`s as it
    /// traverses. Will call the given callback for each event.
    pub fn walk_function_declaration(
        function: borrow([ast::FunctionDeclaration]),
        cb: &mut impl FnMut(AstWalkEvent) -> Result<(), CompilerError>,
    ) -> Result<(), CompilerError> {
        cb(AstWalkEvent::OnEnter(AstNode::FunctionDeclaration(
            function,
        )))?;
        for arg in borrow([function.args]) {
            match arg {
                FunctionParam::Typed(_, type_expr) => {
                    Self::walk_type_expression(type_expr, cb)?;
                }
                FunctionParam::Untyped(_) => {}
            }
        }

        // Self::walk_type_expression(borrow([function.ret_ty_expr]), cb)?;

        // match borrow([function.implementation]) {
        //     ast::FunctionImpl::External { conv: _ } => (),
        //     ast::FunctionImpl::Defined {
        //         block: body,
        //         conv: _,
        //     } => {
        //         Self::walk_block(body, cb)?;
        //     }
        // }

        cb(AstWalkEvent::OnExit(AstNode::FunctionDeclaration(function)))?;
        Ok(())
    }

    // /// Walks the given global declaration, emitting `AstWalkEvent`s as it
    // /// traverses. Will call the given callback for each event.
    // pub fn walk_global_declaration(
    //     global: borrow([ast::GlobalDeclaration]),
    //     cb: &mut impl FnMut(AstWalkEvent) -> Result<(), CompilerError>,
    // ) -> Result<(), CompilerError> {
    //     cb(AstWalkEvent::OnEnter(AstNode::GlobalDeclaration(global)))?;
    //     Self::walk_type_expression(borrow([global.ty_expr]), cb)?;
    //     Self::walk_expression(borrow([global.value_expr]), cb)?;
    //     cb(AstWalkEvent::OnExit(AstNode::GlobalDeclaration(global)))?;
    //     Ok(())
    // }

    // /// Walks the given struct declaration, emitting `AstWalkEvent`s as it
    // /// traverses. Will call the given callback for each event.
    // pub fn walk_struct_declaration(
    //     struct_: borrow([ast::StructDeclaration]),
    //     cb: &mut impl FnMut(AstWalkEvent) -> Result<(), CompilerError>,
    // ) -> Result<(), CompilerError> {
    //     cb(AstWalkEvent::OnEnter(AstNode::StructDeclaration(struct_)))?;
    //     for StructFieldDeclaration {
    //         ty: field_ty_expr,
    //         default_initializer,
    //         ..
    //     } in borrow([struct_.fields])
    //     {
    //         Self::walk_type_expression(field_ty_expr, cb)?;
    //         if let Some(default_initializer) = default_initializer {
    //             Self::walk_expression(default_initializer, cb)?;
    //         }
    //     }
    //     cb(AstWalkEvent::OnExit(AstNode::StructDeclaration(struct_)))?;
    //     Ok(())
    // }

    // /// Walks the given enum declaration, emitting `AstWalkEvent`s as it
    // /// traverses. Will call the given callback for each event.
    // pub fn walk_enum_declaration(
    //     enum_: borrow([ast::EnumDeclaration]),
    //     cb: &mut impl FnMut(AstWalkEvent) -> Result<(), CompilerError>,
    // ) -> Result<(), CompilerError> {
    //     cb(AstWalkEvent::OnEnter(AstNode::EnumDeclaration(enum_)))?;
    //     for (_, variant) in borrow([enum_.variants]) {
    //         if let Some(variant) = variant {
    //             Self::walk_type_expression(variant, cb)?;
    //         }
    //     }
    //     cb(AstWalkEvent::OnExit(AstNode::EnumDeclaration(enum_)))?;
    //     Ok(())
    // }

    /// Walks the given block, emitting `AstWalkEvent`s as it traverses.
    /// Will call the given callback for each event.
    pub fn walk_block(
        block: borrow([ast::Block]),
        cb: &mut impl FnMut(AstWalkEvent) -> Result<(), CompilerError>,
    ) -> Result<(), CompilerError> {
        cb(AstWalkEvent::OnEnter(AstNode::Block(block)))?;
        for statement in borrow([block.statements]) {
            Self::walk_statement(statement, cb)?;
        }
        if let Some(return_expr) = borrow([block.return_expr]) {
            Self::walk_expression(return_expr, cb)?;
        }
        cb(AstWalkEvent::OnExit(AstNode::Block(block)))?;
        Ok(())
    }

    /// Walks the given statement, emitting `AstWalkEvent`s as it traverses.
    /// Will call the given callback for each event.
    pub fn walk_statement(
        statement: borrow([ast::Statement]),
        cb: &mut impl FnMut(AstWalkEvent) -> Result<(), CompilerError>,
    ) -> Result<(), CompilerError> {
        cb(AstWalkEvent::OnEnter(AstNode::Statement(statement)))?;
        match borrow([statement.kind]) {
            ast::StatementKind::Comment(_) => (),
            ast::StatementKind::BlankLine => (),
            ast::StatementKind::Nothing => (),
            ast::StatementKind::Expression(expr) => {
                Self::walk_expression(expr, cb)?;
            }
            ast::StatementKind::Return(expr) => {
                if let Some(expr) = expr {
                    Self::walk_expression(expr, cb)?;
                }
            }
            ast::StatementKind::Break => (),
            ast::StatementKind::Continue => (),
            ast::StatementKind::Let { expr, ty_expr, .. } => {
                if let Some(ty_expr) = ty_expr {
                    Self::walk_type_expression(ty_expr, cb)?;
                }
                Self::walk_expression(expr, cb)?;
            }
            ast::StatementKind::Assign { lhs, rhs } => {
                Self::walk_expression(lhs, cb)?;
                Self::walk_expression(rhs, cb)?;
            }
            ast::StatementKind::For {
                var: _,
                iterable,
                body,
            } => {
                // Self::walk_expression(var, cb)?;
                Self::walk_expression(iterable, cb)?;
                Self::walk_block(body, cb)?;
            }
            ast::StatementKind::If {
                cond,
                then_block,
                else_block,
            } => {
                Self::walk_expression(cond, cb);
                Self::walk_block(then_block, cb);

                if let Some(else_block) = else_block {
                    Self::walk_block(else_block, cb)?;
                }
            }
            ast::StatementKind::While { cond, body } => {
                Self::walk_expression(cond, cb)?;
                Self::walk_block(body, cb)?;
            } // ast::StatementKind::Macro(_) => {
              //     // We don't emit any events for macros expressions. Users
              //     // should match on the wrapper AST node instead.
              // }
        }
        cb(AstWalkEvent::OnExit(AstNode::Statement(statement)))?;
        Ok(())
    }

    /// Walks the given type expression, emitting `AstWalkEvent`s as it
    /// traverses. Will call the given callback for each event.
    pub fn walk_type_expression(
        ty_expr: borrow([ast::TypeExpr]),
        cb: &mut impl FnMut(AstWalkEvent) -> Result<(), CompilerError>,
    ) -> Result<(), CompilerError> {
        cb(AstWalkEvent::OnEnter(AstNode::TypeExpression(ty_expr)))?;
        match borrow([ty_expr.kind]) {
            ast::TypeExprKind::Named(_) => (),
            // ast::TypeExprKind::Primitive(_) => (),
            // ast::TypeExprKind::RawPtr(pointee) => Self::walk_type_expression(pointee, cb)?,
            // ast::TypeExprKind::ScopedPtr(pointee) => Self::walk_type_expression(pointee, cb)?,
            // ast::TypeExprKind::Ref(referent) => Self::walk_type_expression(referent, cb)?,
            // ast::TypeExprKind::Vec(element) => Self::walk_type_expression(element, cb)?,
            // ast::TypeExprKind::Option(element) => Self::walk_type_expression(element, cb)?,
            ast::TypeExprKind::Unit => (),
            ast::TypeExprKind::String => (),
        }
        cb(AstWalkEvent::OnExit(AstNode::TypeExpression(ty_expr)))?;
        Ok(())
    }

    /// Walks the given expression, emitting `AstWalkEvent`s as it traverses.
    /// Will call the given callback for each event.
    pub fn walk_expression(
        expr: borrow([ast::Expr]),
        cb: &mut impl FnMut(AstWalkEvent) -> Result<(), CompilerError>,
    ) -> Result<(), CompilerError> {
        cb(AstWalkEvent::OnEnter(AstNode::Expression(expr)))?;
        match borrow([expr.kind]) {
            ast::ExprKind::Path(_) => (),
            ast::ExprKind::SelfIdent => (),
            // ast::ExprKind::IntegerLiteral(_) => (),
            // ast::ExprKind::FloatLiteral(_) => (),
            ast::ExprKind::BoolLiteral(_) => (),
            ast::ExprKind::StringLiteral(_) => (),
            ast::ExprKind::NullLiteral => (),
            ast::ExprKind::OptionSomeLiteral(inner_expr) => {
                Self::walk_expression(inner_expr, cb)?;
            }
            ast::ExprKind::OptionNoneLiteral => (),
            ast::ExprKind::FunctionCall { ident: _, args } => {
                for arg in args {
                    Self::walk_expression(arg, cb)?;
                }
            }
            ast::ExprKind::BinaryOp { op: _, left, right } => {
                Self::walk_expression(left, cb)?;
                Self::walk_expression(right, cb)?;
            }
            ast::ExprKind::ParenExpr(inner_expr) => {
                Self::walk_expression(inner_expr, cb)?;
            }
            // ast::ExprKind::UnaryOp { op: _, operand } => {
            //     Self::walk_expression(operand, cb)?;
            // }
            // ast::ExprKind::Typecast { ty_expr, sub_expr } => {
            //     Self::walk_type_expression(ty_expr, cb)?;
            //     Self::walk_expression(sub_expr, cb)?;
            // }
            // ast::ExprKind::StructLiteral {
            //     struct_ident: _,
            //     fields,
            // } => {
            //     for (_, field_expr) in fields {
            //         Self::walk_expression(field_expr, cb)?;
            //     }
            // }
            // ast::ExprKind::VecLiteral { elements } => {
            //     for element in elements {
            //         Self::walk_expression(element, cb)?;
            //     }
            // }
            // ast::ExprKind::RefAlloc(inner_expr) => {
            //     Self::walk_expression(inner_expr, cb)?;
            // }
            // ast::ExprKind::FieldAccess { base, field: _ } => {
            //     Self::walk_expression(base, cb)?;
            // }
            // ast::ExprKind::MethodCall {
            //     base,
            //     args,
            //     method: _,
            // } => {
            //     Self::walk_expression(base, cb)?;
            //     for arg in args {
            //         Self::walk_expression(arg, cb)?;
            //     }
            // }
            ast::ExprKind::Block(block) => {
                Self::walk_block(block, cb)?;
            }
            ast::ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                Self::walk_expression(condition, cb)?;
                Self::walk_expression(then_branch, cb)?;
                if let Some(else_branch) = else_branch {
                    Self::walk_expression(else_branch, cb)?;
                }
            }
            // ast::ExprKind::Match { test_expr, match_ } => {
            //     Self::walk_expression(test_expr, cb)?;
            //     for case in borrow([match_.cases]) {
            //         cb(AstWalkEvent::OnEnter(AstNode::PatternMatchCase(case)))?;
            //         Self::walk_expression(borrow([case.body]), cb)?;
            //         cb(AstWalkEvent::OnExit(AstNode::PatternMatchCase(case)))?;
            //     }
            // }
            // ast::ExprKind::StringInterpolation { parts } => {
            //     for part in parts {
            //         match part {
            //             ast::StringInterpolationPart::Literal(_) => (),
            //             ast::StringInterpolationPart::Expr(part_expr) => {
            //                 Self::walk_expression(part_expr, cb)?;
            //             }
            //         }
            //     }
            // }
            // ast::ExprKind::VectorBuiltin { kind: _, args } => {
            //     for arg in args {
            //         Self::walk_expression(arg, cb)?;
            //     }
            // }
            ast::ExprKind::Index { base, index } => {
                Self::walk_expression(base, cb)?;
                Self::walk_expression(index, cb)?;
            }
            ast::ExprKind::NumLiteral(_) => {}
            ast::ExprKind::FieldAccess { field: _, base } => {
                // Self::walk_expression(field, cb);
                Self::walk_expression(base, cb);
            }
            // ast::ExprKind::TypeOf { inner } => {
            //     Self::walk_expression(inner, cb)?;
            // }
            // ast::ExprKind::Macro(_) => {
            //     // We don't emit any events for macros expressions. Users
            //     // should match on the wrapper AST node instead.
            // }
        }
        cb(AstWalkEvent::OnExit(AstNode::Expression(expr)))?;
        Ok(())
    }
}
