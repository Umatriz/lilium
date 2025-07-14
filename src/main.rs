use std::path::PathBuf;

use clap::{ArgAction, Command, arg, value_parser};
use lilium::lexer::Lexer;

fn main() {
    let matches = Command::new("lilium")
        .subcommand(
            Command::new("lex").arg(arg!("file": <FILE>).value_parser(value_parser!(PathBuf))),
        )
        .get_matches();

    if let Some(sub) = matches.subcommand_matches("lex") {
        let file_path = sub.get_one::<PathBuf>("file").unwrap();
        let content = std::fs::read_to_string(file_path).unwrap();
        println!("File content is:\n--- BEGIN FILE ---\n{content}\n--- END FILE ---");
        let lexer = Lexer::new(&content);
        println!("Parsed results are:\n{lexer}");
    }

    // let expr = args.nth(1).expect("No expression to parse");
    // let tokens = lilium::parser::parse_tokens(&expr);

    // println!("{tokens:#?}");
}
