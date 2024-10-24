use clap::{Parser, ValueEnum};
use rock::symbol_table::print_symbol_table;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Command {
    Compile,
}

#[derive(clap::Parser)]
#[command(version = env!("CARGO_PKG_VERSION"), about = "Rock Compiler")]
struct Args {
    #[arg()]
    command: Command,

    #[arg()]
    file: String,
}

fn main() {
    let args = Args::parse();

    match args.command {
        Command::Compile => {
            let source_file = rock::source_code::SourceFile::from_path(&args.file).unwrap();
            let sources = rock::source_code::SourceFiles::new(vec![source_file]);
            let result = rock::semantic::compile(&sources).unwrap();

            print_symbol_table(&result.symbol_table, &sources, None);

            println!("{}", result.lua_code);
        }
    }
}
