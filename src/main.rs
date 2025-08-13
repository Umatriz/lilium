use std::path::PathBuf;

use clap::{ArgGroup, ArgMatches, Command, arg, value_parser};
use lilium::{
    ast::{Expr, Parse, Stmt, expr},
    lexer::Lexer,
};

fn create_command_sources(name: &'static str) -> Command {
    Command::new(name)
        .arg(arg!(code: -C --code <CODE>).value_parser(value_parser!(String)))
        .arg(arg!(file: -F --file <FILE>).value_parser(value_parser!(PathBuf)))
        .group(
            ArgGroup::new("sources")
                .args(["code", "file"])
                .required(true),
        )
}

fn get_sources(matches: &ArgMatches) -> String {
    if let Some(path) = matches.get_one::<PathBuf>("file") {
        let content = std::fs::read_to_string(path).unwrap();
        return content;
    }

    if let Some(code) = matches.get_one::<String>("code") {
        return code.clone();
    }

    unreachable!("No source argument was provided!");
}

fn main() {
    let matches = Command::new("lilium")
        .subcommand(create_command_sources("lex"))
        .subcommand(create_command_sources("tree"))
        .get_matches();

    if let Some(sub) = matches.subcommand_matches("lex") {
        let content = get_sources(sub);
        println!("--- BEGIN FILE ---\n{content}\n--- END FILE ---");
        let lexer = Lexer::new(&content).unwrap();
        let tokens = lexer.tokens();
        println!("--- BEGIN LEXING RESULTS ---\n{tokens}\n--- END LEXING RESULTS ---");
    }

    if let Some(sub) = matches.subcommand_matches("tree") {
        let content = get_sources(sub);
        println!("--- BEGIN CODE ---\n{content}\n--- END CODE ---");

        let lexer = Lexer::new(&content).unwrap();
        let mut tokens = lexer.tokens();
        println!("--- BEGIN LEXING RESULTS ---\n{tokens}\n--- END LEXING RESULTS ---");

        let mut expr = match Stmt::parse(&mut tokens) {
            Ok(e) => e,
            Err(lilium::ast::Error::UnexpectedToken { found, expected }) => {
                let span = found.span;
                println!("UNEXPECTED TOKEN: {}", &content[span.start..span.end]);
                panic!(
                    "ERROR: {}",
                    lilium::ast::Error::UnexpectedToken { found, expected }
                )
            }
            Err(err) => panic!("ERROR: {err}"),
        };
        println!("Tree:");
        // if matches!(expr, Expr::Sequence(_)) {
        //     expr = expr.flatten_sequence();
        // }
        println!("{expr:#?}");
    }

    // let expr = args.nth(1).expect("No expression to parse");
    // let tokens = lilium::parser::parse_tokens(&expr);

    // println!("{tokens:#?}");
}
