use source_code::SourceFiles;

use crate::*;

pub struct SemanticResult {
    // pub ir_module: ir::IrModule,
    pub symbol_table: SymbolTable,
    // pub ir_reg: ir::IrTypeRegistry,
    // pub global_order: GlobalInitializerOrder,
}

pub fn compile(source_files: &SourceFiles) -> Result<SemanticResult> {
    let mut top_level = vec![];

    for file in source_files.iter() {
        let ast = parser::parse(file.contents())?;

        top_level.extend(ast);
    }

    // Fill scopes, register all declarations, and variable usages.
    let mut scope_builder = ScopeBuilder::new();
    declaration_pass(&top_level, &mut scope_builder)?;

    Ok(SemanticResult {
        symbol_table: scope_builder.table,
    })
}
