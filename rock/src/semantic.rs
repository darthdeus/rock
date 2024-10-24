use codegen_pass::codegen_pass;
use compiler_error::CompilerError;
use declaration_pass::declaration_pass;

use source_code::SourceFiles;

use crate::*;

pub struct SemanticResult {
    pub lua_code: String,
    // pub ir_module: ir::IrModule,
    pub symbol_table: SymbolTable,
    // pub ir_reg: ir::IrTypeRegistry,
    // pub global_order: GlobalInitializerOrder,
}

pub fn compile(source_files: &SourceFiles) -> Result<SemanticResult, Vec<CompilerError>> {
    let mut top_level = vec![];

    for file in source_files.iter() {
        let ast = parser::parse(file)?;
        top_level.extend(ast);
    }

    // Fill scopes, register all declarations, and variable usages.
    let mut scope_builder = ScopeBuilder::new();
    declaration_pass(&top_level, &mut scope_builder).map_err(|x| vec![x])?;

    let lua_code = codegen_pass(&top_level, &scope_builder.table)?;

    Ok(SemanticResult {
        symbol_table: scope_builder.table,
        lua_code,
    })
}
