use anyhow::Result;
use rock::{debug::AstKind, format::*};
use std::env;

fn main() -> Result<()> {
    let args: Vec<_> = env::args().collect();

    if args.len() != 2 {
        println!("Usage: {} <filename>", args[0]);
        return Ok(());
    }

    let input_file = std::fs::read_to_string(&args[1])?;

    println!("got file: {}", input_file);

    let top_level = rock::parse(&input_file)?;

    println!(
        "Top level parsed as:\n{:#?}",
        top_level.iter().map(|x| x.kind()).collect::<Vec<_>>()
    );

    println!("Formatted:\n\n{}", format_top_level(top_level));

    Ok(())
}
