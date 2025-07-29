use std::path::PathBuf;

use clap::{ArgAction, Command, arg, value_parser};
use lilium::{
    ast::{Expr, expr},
    lexer::Lexer,
};

fn main() {
    let matches = Command::new("lilium")
        .subcommand(
            Command::new("lex").arg(arg!(file: <FILE>).value_parser(value_parser!(PathBuf))),
        )
        .subcommand(
            Command::new("tree").arg(arg!(code: <CODE>).value_parser(value_parser!(String))),
        )
        .get_matches();

    if let Some(sub) = matches.subcommand_matches("lex") {
        let file_path = sub.get_one::<PathBuf>("file").unwrap();
        let content = std::fs::read_to_string(file_path).unwrap();
        println!("File content is:\n--- BEGIN FILE ---\n{content}\n--- END FILE ---");
        let lexer = Lexer::new(&content);
        println!("Lexed results are:\n{lexer}");
    }

    if let Some(sub) = matches.subcommand_matches("tree") {
        let code = sub.get_one::<String>("code").unwrap();
        println!("{code}");
        println!();

        let lexer = Lexer::new(code);
        println!("Lexed results are:\n{lexer}");
        println!();

        let mut expr = expr(&mut lexer.tokens(), 0).unwrap();
        println!("Tree:");
        if matches!(expr, Expr::Sequence(_)) {
            expr = expr.flatten_sequence();
        }
        println!("{expr:#?}");
    }

    // let expr = args.nth(1).expect("No expression to parse");
    // let tokens = lilium::parser::parse_tokens(&expr);

    // println!("{tokens:#?}");
}
