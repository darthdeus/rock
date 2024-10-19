pub mod ast;
use anyhow::{bail, Result};

use ast::*;

pub fn parse(source: &str) -> Result<Vec<TopLevel>> {
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&tree_sitter_rock::LANGUAGE.into())?;

    let tree = parser.parse(source, None).unwrap();

    let root = tree.root_node();
    let mut cursor = root.walk();

    let mut top_level = Vec::<TopLevel>::new();

    for child in root.children(&mut cursor) {
        match child.kind() {
            "statement" => {
                top_level.push(TopLevel::Statement(Statement::Yes));
            }

            "function" => {
                top_level.push(TopLevel::Function(FunctionDef {}));
            }

            _ => {
                bail!("unexpected child: {:?}", child);
            }
        }
    }

    Ok(top_level)
}
