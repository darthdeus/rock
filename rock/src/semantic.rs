use compiler_error::CompilerError;
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

    let mut symbol_table = SymbolTable::default();

    Ok(SemanticResult { symbol_table })
}
