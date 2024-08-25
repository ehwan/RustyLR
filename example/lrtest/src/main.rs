use rusty_lr::*;

fn main() {
    let mut grammar = builder::Grammar::new();

    let _ = grammar.set_reduce_type('0', ReduceType::Right);

    grammar.add_rule(
        "Num",
        vec![Token::NonTerm("Digit"), Token::NonTerm("Num")],
        0,
    );
    grammar.add_rule("Num", vec![Token::NonTerm("Digit")], 0);

    grammar.add_rule("Digit", vec![Token::Term('0')], 0);

    grammar.add_rule("Token", vec![Token::NonTerm("Num")], 0);
    grammar.add_rule(
        "Tokens",
        vec![Token::NonTerm("Token"), Token::NonTerm("Tokens")],
        0,
    );
    grammar.add_rule("Tokens", vec![Token::NonTerm("Token")], 0);

    grammar.add_rule("Aug", vec![Token::NonTerm("Tokens"), Token::Term('\0')], 0);

    match grammar.build("Aug") {
        Ok(_) => {
            println!("Build successful");
        }
        Err(_) => {
            println!("Build failed");
        }
    }
}
