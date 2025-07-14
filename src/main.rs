use std::path::PathBuf;

use clap::{ArgAction, Command, arg, value_parser};
use lilium::parser::Lexer;

fn main() {
    let matches = Command::new("lilium")
        .subcommand(
            Command::new("lex").arg(arg!("file": <FILE>).value_parser(value_parser!(PathBuf))),
        )
        .get_matches();

    if let Some(sub) = matches.subcommand_matches("lex") {
        let file_path = sub.get_one::<PathBuf>("file").unwrap();
        let content = std::fs::read_to_string(file_path).unwrap();
        println!("File content is:\n{content}");
        let lexer = Lexer::new(&content);

        let t1 = Lexer::parse_tokens(&content);
        let t2 = Lexer::parse_tokens2(&content);

        // assert_eq!(t1.len(), t2.len());

        for i in 0..(t1.len().max(t2.len())) {
            assert_eq!(t1.get(i), t2.get(i));
        }

        println!("Parsed result is:\n{lexer}");
    }

    // let expr = args.nth(1).expect("No expression to parse");
    // let tokens = lilium::parser::parse_tokens(&expr);

    // println!("{tokens:#?}");
}
