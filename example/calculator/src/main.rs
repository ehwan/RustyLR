#![allow(unused_variables)]

use rusty_lr;

type Term = char;
type NonTerm = &'static str;

/// define callback struct to trace parser's action
struct ParserCallback {}

impl rusty_lr::CallbackStr<Term, NonTerm> for ParserCallback {
    /// terminal symbol shifted and state transitioned
    fn shift_and_goto(
        &mut self,
        parser: &rusty_lr::Parser<Term, NonTerm>,
        context: &rusty_lr::ContextStr<'_, Term, NonTerm>,
    ) {
        // println!("Shift {} and goto State{}", context.term(), context.state());
    }
    /// non-terminal symbol shifted and state transitioned
    fn shift_and_goto_nonterm(
        &mut self,
        parser: &rusty_lr::Parser<Term, NonTerm>,
        context: &rusty_lr::ContextStr<'_, Term, NonTerm>,
        nonterm: &NonTerm,
    ) {
        // println!("Shift {} and goto State{}", nonterm, context.state());
    }
    /// set of tokens reduced by rule
    /// you can access the actual rule struct by parser.rules[rule_id]
    fn reduce(
        &mut self,
        parser: &rusty_lr::Parser<Term, NonTerm>,
        context: &rusty_lr::ContextStr<'_, Term, NonTerm>,
        rule: usize,
    ) {
        // rule is display if term, nonterm is display
        println!("Reduce by {}", parser.rules[rule]);
        println!("{}", context.top_str());
    }

    /// error handling
    fn invalid_term(
        &mut self,
        parser: &rusty_lr::Parser<Term, NonTerm>,
        context: &rusty_lr::ContextStr<'_, Term, NonTerm>,
    ) {
        eprintln!(
            "Invalid terminal {} at state {}",
            context.term(),
            context.state()
        );
    }
    fn invalid_nonterm(
        &mut self,
        parser: &rusty_lr::Parser<Term, NonTerm>,
        context: &rusty_lr::ContextStr<'_, Term, NonTerm>,
        nonterm: &NonTerm,
    ) {
        eprintln!(
            "Invalid non-terminal {} at state {}",
            nonterm,
            context.state()
        );
    }
}

fn main() {
    type Token = rusty_lr::Token<Term, NonTerm>;
    let mut grammar = rusty_lr::Grammar::new();

    // A -> A + A | M ( reduce left )
    grammar.add_rule(
        "A",
        vec![Token::NonTerm("A"), Token::Term('+'), Token::NonTerm("A")],
        rusty_lr::ReduceType::Left, // reduce left
    );
    grammar.add_rule(
        "A",
        vec![Token::NonTerm("M")],
        rusty_lr::ReduceType::Error, // error on shift/reduce conflict
    );

    // M -> M * M | P ( reduce left )
    grammar.add_rule(
        "M",
        vec![Token::NonTerm("M"), Token::Term('*'), Token::NonTerm("M")],
        rusty_lr::ReduceType::Left, // reduce left
    );
    grammar.add_rule(
        "M",
        vec![Token::NonTerm("P")],
        rusty_lr::ReduceType::Error, // error on shift/reduce conflict
    );

    for digit in 0..10 {
        let ascii = digit + '0' as i32;
        let ascii = ascii as u8 as char;
        grammar.add_rule("Num", vec![Token::Term(ascii)], rusty_lr::ReduceType::Left);
    }
    grammar.add_rule(
        "Num",
        vec![Token::NonTerm("Num"), Token::NonTerm("Num")],
        rusty_lr::ReduceType::Left,
    );
    grammar.add_rule(
        "P",
        vec![Token::NonTerm("Num")],
        rusty_lr::ReduceType::Error,
    );
    grammar.add_rule(
        "P",
        vec![Token::Term('('), Token::NonTerm("E"), Token::Term(')')],
        rusty_lr::ReduceType::Error,
    );

    // E -> A
    grammar.add_rule("E", vec![Token::NonTerm("A")], rusty_lr::ReduceType::Error);

    // augmented rule
    // E' -> E $
    grammar.add_rule(
        "E'",
        vec![Token::NonTerm("E"), Token::Term(0 as char)],
        rusty_lr::ReduceType::Error, // error on shift/reduce conflict
    );

    // grammar is Display if Term, NonTerm is Display
    println!("Grammar:\n{}", grammar);

    // build DFA
    let parser = match grammar.build("E'") {
        Ok(parser) => parser,
        Err(err) => {
            // error is Display if Term, NonTerm is Display
            eprintln!("{}", err);
            return;
        }
    };

    // input terminal symbols
    let input = "11+22*(33+44)";

    // start parsing with callback
    let mut callback = ParserCallback {};
    match parser.parse_str_with_callback(input, &mut callback, 0 as char) {
        Ok(tree) => println!("Parse success {:?}", tree.str(input)),
        Err(err) => eprintln!("{}", err),
    }
}
