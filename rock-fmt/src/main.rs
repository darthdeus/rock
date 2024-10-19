use anyhow::Result;
use std::env;

fn main() -> Result<()> {
    let args: Vec<_> = env::args().collect();

    if args.len() != 2 {
        println!("Usage: {} <filename>", args[0]);
        return Ok(());
    }

    let input_file = std::fs::read_to_string(&args[1])?;

    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&tree_sitter_rock::LANGUAGE.into())?;

    let tree = parser.parse(&input_file, None).unwrap();

    let root = tree.root_node();
    let mut cursor = root.walk();

    let mut top_level = Vec::<TopLevel>::new();

    println!("got file: {}", input_file);
    println!("tree: {:#?}", tree);

    for child in root.children(&mut cursor) {
        match child.kind() {
            "statement" => {
                top_level.push(TopLevel::Statement(Statement::Yes));
            }

            "function" => {
                top_level.push(TopLevel::Function(FunctionDef {}));
            }

            _ => {
                println!("unexpected child: {:?}", child);
            }
        }
        println!("got child {:?} of kind {}", child, child.kind());
    }

    println!("Top level parsed as:\n{:#?}", top_level);

    Ok(())
}
