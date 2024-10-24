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

    // TODO: error handling lul
    let top_level = rock::parser::parse_file(&input_file).unwrap();

    println!("==== Formatted ====\n{}", format_top_level(top_level));

    Ok(())
}
