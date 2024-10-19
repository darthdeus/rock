use anyhow::Result;

use crate::parser::Source;
use ast::*;

pub mod ast;
pub mod debug;
pub mod format;
pub mod parser;
pub mod source_code;

pub fn parse(source: &str) -> Result<Vec<TopLevel>> {
    let mut parser = parser::Parser::new();

    let top_level = parser.parse(&Source {
        code: source.to_string(),
        file: Some("file.rock".into()),
    })?;

    Ok(top_level)
}
