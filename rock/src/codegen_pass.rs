use crate::ast::*;
use crate::{compiler_error::CompilerError, SymbolTable, TopLevel};

pub fn codegen_pass(
    top_level: &[TopLevel],
    symbol_table: &SymbolTable,
) -> Result<String, Vec<CompilerError>> {
    let mut lua_code = String::new();

    for item in top_level {
        let code = codegen_top_level(item, symbol_table)?;
        lua_code.push_str(&code);
        lua_code.push('\n');
    }

    Ok(lua_code)
}

fn codegen_top_level(
    top_level: &TopLevel,
    symbol_table: &SymbolTable,
) -> Result<String, Vec<CompilerError>> {
    match top_level {
        TopLevel::Statement(s) => codegen_statement(s, symbol_table),
        TopLevel::Function(f) => codegen_function(f, symbol_table),
    }
}

fn codegen_statement(
    statement: &Statement,
    symbol_table: &SymbolTable,
) -> Result<String, Vec<CompilerError>> {
    match &statement.kind {
        StatementKind::Comment(text) => Ok(text.text.clone()),
        StatementKind::BlankLine => Ok("".to_string()),
        StatementKind::Nothing => Ok("".to_string()),
        StatementKind::Expression(expr) => Ok(codegen_expr(expr, symbol_table)),
        StatementKind::Return(None) => Ok("return;".to_string()),
        StatementKind::Return(Some(expr)) => {
            let expr_code = codegen_expr(expr, symbol_table);
            Ok(format!("return {};", expr_code))
        }
        StatementKind::Break => Ok("break".to_string()),
        StatementKind::Continue => Ok("continue".to_string()),
        StatementKind::Let {
            ident,
            ty_expr: _,
            expr,
        } => {
            let ident_code = codegen_ident(ident, symbol_table);
            let expr_code = codegen_expr(expr, symbol_table);
            Ok(format!("local {} = {};", ident_code, expr_code))
        }
        StatementKind::Assign { lhs, rhs } => Ok(format!(
            "{} = {}",
            codegen_expr(lhs, symbol_table),
            codegen_expr(rhs, symbol_table)
        )),
        StatementKind::If {
            cond,
            then_block,
            else_block,
        } => {
            let mut code = String::new();
            code.push_str("if ");
            code.push_str(&codegen_expr(cond, symbol_table));
            code.push_str(" then\n");

            for statement in then_block.statements.iter() {
                code.push_str(&codegen_statement(statement, symbol_table)?);
                code.push('\n');
            }

            if let Some(else_block) = else_block {
                code.push_str("else\n");
                for statement in else_block.statements.iter() {
                    code.push_str(&codegen_statement(statement, symbol_table)?);
                    code.push('\n');
                }
            }
            code.push_str("end");
            Ok(code)
        }

        StatementKind::For {
            var,
            iterable,
            body,
        } => {
            let var_code = codegen_ident(var, symbol_table);
            let iterable_code = codegen_expr(iterable, symbol_table);
            let mut code = String::new();
            code.push_str("for ");
            code.push_str(var_code.as_str());
            code.push_str(" in ");
            code.push_str(iterable_code.as_str());
            code.push_str(" do\n");
            for statement in body.statements.iter() {
                code.push_str(&codegen_statement(statement, symbol_table)?);
                code.push('\n');
            }
            code.push_str("end");
            Ok(code)
        }

        StatementKind::While { cond, body } => {
            let cond_code = codegen_expr(cond, symbol_table);
            let mut code = String::new();
            code.push_str("while ");
            code.push_str(cond_code.as_str());
            code.push_str(" do\n");
            for statement in body.statements.iter() {
                code.push_str(&codegen_statement(statement, symbol_table)?);
                code.push('\n');
            }
            code.push_str("end");
            Ok(code)
        }
    }
}

fn codegen_ident(ident: &Ident, _: &SymbolTable) -> String {
    ident.text.to_string()
}

// fn codegen_ident_path(ident: &IdentPath, _: &SymbolTable) -> String {
//     ident.text()
// }

fn codegen_function(
    function: &FunctionDeclaration,
    symbol_table: &SymbolTable,
) -> Result<String, Vec<CompilerError>> {
    let mut code = String::new();
    code.push_str("function ");
    code.push_str(&function.name.text);
    code.push('(');

    for (i, param) in function.args.iter().enumerate() {
        if i != 0 {
            code.push_str(", ");
        }
        code.push_str(&codegen_ident(param.ident(), symbol_table));
    }
    code.push_str(")\n");

    for statement in function.body.statements.iter() {
        code.push_str(&codegen_statement(statement, symbol_table)?);
        code.push('\n');
    }
    code.push_str("end");
    Ok(code)
}

fn codegen_expr(expr: &Expr, symbol_table: &SymbolTable) -> String {
    match &expr.kind {
        ExprKind::Path(ident_path) => ident_path.text(),
        ExprKind::NumLiteral(val) => format!("{}", val),
        ExprKind::SelfIdent => "self".to_string(),
        ExprKind::BoolLiteral(val) => format!("{}", val),
        ExprKind::StringLiteral(val) => val.clone(),
        ExprKind::NullLiteral => "null".to_string(),
        ExprKind::OptionSomeLiteral(expr) => codegen_expr(expr, symbol_table),
        ExprKind::OptionNoneLiteral => "null".to_string(),

        ExprKind::FunctionCall { ident, args } => {
            let mut code = String::new();
            code.push_str(&ident.text);
            code.push('(');
            for (i, arg) in args.iter().enumerate() {
                if i != 0 {
                    code.push_str(", ");
                }
                code.push_str(&codegen_expr(arg, symbol_table));
            }
            code.push(')');
            code
        }

        ExprKind::BinaryOp { op, left, right } => {
            let left_code = codegen_expr(left, symbol_table);
            let right_code = codegen_expr(right, symbol_table);
            format!("{} {} {}", left_code, op.to_string(), right_code)
        }

        ExprKind::FieldAccess { field, base } => {
            let base_code = codegen_expr(base, symbol_table);
            format!("{}.{}", base_code, field.text)
        }
        ExprKind::Index { base, index } => {
            let base_code = codegen_expr(base, symbol_table);
            let index_code = codegen_expr(index, symbol_table);
            format!("{}[{}]", base_code, index_code)
        }
        ExprKind::ParenExpr(expr) => {
            let expr_code = codegen_expr(expr, symbol_table);
            format!("({})", expr_code)
        }
        ExprKind::Block(block) => {
            let mut code = String::new();
            code.push_str("{\n");
            for statement in block.statements.iter() {
                code.push_str(&codegen_statement(statement, symbol_table).unwrap());
                code.push('\n');
            }
            code.push('}');
            code
        }

        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let mut code = String::new();
            code.push_str("if ");
            code.push_str(&codegen_expr(condition, symbol_table));
            code.push_str(" then\n");
            code.push_str(&codegen_expr(then_branch, symbol_table));
            code.push('\n');

            if let Some(else_branch) = else_branch {
                code.push_str("else\n");
                code.push_str(&codegen_expr(else_branch, symbol_table));
                code.push('\n');
            }

            code.push_str("end");
            code
        }
    }
}
