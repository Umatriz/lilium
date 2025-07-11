use clap::{ArgAction, Command, arg, value_parser};

fn main() {
    let matches = Command::new("lilium")
        .arg(arg!(-e --exec <CODE> "Execute the code").value_parser(value_parser!(String)))
        .get_matches();

    if let Some(code) = matches.get_one::<String>("exec") {
        let lexer = lilium::parser::Lexer::new(code);
        println!("{lexer}");
    }

    // let expr = args.nth(1).expect("No expression to parse");
    // let tokens = lilium::parser::parse_tokens(&expr);

    // println!("{tokens:#?}");
}
