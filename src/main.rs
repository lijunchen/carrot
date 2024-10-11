use carrot::grammar;
use carrot::typer;
use clap::Parser;
use std::path::PathBuf;

#[derive(Parser)]
struct Cli {
    #[clap(long)]
    pub file: PathBuf,
}

fn main() {
    let cli = Cli::parse();
    let content = std::fs::read_to_string(cli.file).unwrap();
    let parser = grammar::FileParser::new();
    let ast = parser.parse(&content).unwrap();
    let tast = typer::Typer::new().infer(&ast);
    println!("{:#?}", ast);
    println!("{:#?}", tast);
}
