use anyhow::Result;
use rock::{format::*, source_code::SourceFile};
use std::env;

fn main() -> Result<()> {
    let args: Vec<_> = env::args().collect();

    if args.len() != 2 {
        println!("Usage: {} <filename>", args[0]);
        return Ok(());
    }

    let input_file = SourceFile::from_path(&args[1])?;

    println!("==== INPUT ====\n{}", input_file.contents());

    let top_level = rock::parser::parse(&input_file).map_err(|e| e.anyhow())?;

    println!("==== Formatted ====\n{}", format_top_level(top_level));

    Ok(())
}
