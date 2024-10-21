use std::path::{Path, PathBuf};

use anyhow::Result;

use ast::*;
use compiler_error::CompilerError;
use log::info;
use semantic::SemanticResult;
use source_code::SourceFiles;
use symbol_table::*;

pub mod ast;
pub mod ast_walker;
pub mod compiler_error;
pub mod debug;
pub mod declaration_pass;
pub mod format;
pub mod parser;
pub mod semantic;
pub mod source_code;
pub mod symbol_table;
pub mod types;

pub struct CompiledModule {
    pub semantic: SemanticResult,
    // pub ee: Option<llvm::ExecutionEngineId>,
    // pub level: CompileLevel,
}

pub struct CompilerContext {
    pub compiled_module: Option<CompiledModule>,
}

impl CompilerContext {
    pub fn new() -> Self {
        Self {
            compiled_module: None,
        }
    }

    pub fn get_module(&self) -> Option<&CompiledModule> {
        self.compiled_module.as_ref()
    }

    pub fn compile_sources(&mut self, sources: &SourceFiles) -> Result<(), CompilerError> {
        let result = semantic::compile(sources)?;

        self.compiled_module = Some(CompiledModule { semantic: result });

        info!("Compilation OK");

        Ok(())
    }
}

impl Default for CompilerContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Walks the given directory tree, returning a list of paths for which
/// match_pattern returns true.
pub fn walk_dir_matching_paths<F>(
    path: impl AsRef<Path>,
    match_pattern: &F,
) -> Box<dyn Iterator<Item = PathBuf>>
where
    F: Fn(&Path) -> bool + 'static,
{
    use std::fs;
    let entries = fs::read_dir(path.as_ref()).unwrap();
    let mut paths = Vec::new();

    for entry in entries {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_file() && !match_pattern(&path) {
            continue;
        } else if path.is_dir() {
            let mut sub_paths: Vec<PathBuf> =
                walk_dir_matching_paths(&path, match_pattern).collect();
            paths.append(&mut sub_paths);
        } else {
            paths.push(path);
        }
    }

    Box::new(paths.into_iter())
}
