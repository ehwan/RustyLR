use rusty_lr::*;

fn main() {
    let mut grammar = Grammar::new();

    let _ = grammar.set_reduce_type('0', ReduceType::Right);

    grammar.add_rule("Num", vec![Token::NonTerm("Digit"), Token::NonTerm("Num")]);
    grammar.add_rule("Num", vec![Token::NonTerm("Digit")]);

    grammar.add_rule("Digit", vec![Token::Term('0')]);

    grammar.add_rule("Token", vec![Token::NonTerm("Num")]);
    grammar.add_rule(
        "Tokens",
        vec![Token::NonTerm("Token"), Token::NonTerm("Tokens")],
    );
    grammar.add_rule("Tokens", vec![Token::NonTerm("Token")]);

    grammar.add_rule("Aug", vec![Token::NonTerm("Tokens"), Token::Term('\0')]);

    match grammar.build("Aug") {
        Ok(_) => {
            println!("Build successful");
        }
        Err(e) => {
            println!("Build failed: {:?}", e);
        }
    }
}
