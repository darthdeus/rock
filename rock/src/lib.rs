use std::path::{Path, PathBuf};

use anyhow::Result;

use ast::*;
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

pub struct CompilerContext {
    pub symbol_table: SymbolTable,
}

impl CompilerContext {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
        }
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
