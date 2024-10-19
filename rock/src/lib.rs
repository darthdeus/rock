use anyhow::Result;

use ast::*;

pub mod ast;
pub mod parser;
pub mod source_code;

pub fn parse(source: &str) -> Result<Vec<TopLevel>> {
    let mut parser = parser::Parser::new();

    let top_level = parser.parse(source)?;

    Ok(top_level)
}
