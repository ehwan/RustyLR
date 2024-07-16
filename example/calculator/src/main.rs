#![allow(unused_variables)]

use std::fmt::Display;

use rusty_lr::*;

/// define set of terminal symbols
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)] // must implement these traits
pub enum TermType {
    Num,
    Plus,
    Mul,
    LeftParen,
    RightParen,
    Eof,
}

/// define set of non-terminal symbols
#[derive(Debug, Clone, Hash, PartialEq, Eq)] // must implement these traits
pub enum NonTermType {
    E,
    A,
    M,
    P,
    Augmented,
}

/// impl Display for TermType, NonTermType will make related ProductionRule, error message Display-able
impl Display for TermType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TermType::Num => write!(f, "Num"),
            TermType::Plus => write!(f, "+"),
            TermType::Mul => write!(f, "*"),
            TermType::LeftParen => write!(f, "("),
            TermType::RightParen => write!(f, ")"),
            TermType::Eof => write!(f, "$"),
        }
    }
}
impl Display for NonTermType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NonTermType::E => write!(f, "E"),
            NonTermType::A => write!(f, "A"),
            NonTermType::M => write!(f, "M"),
            NonTermType::P => write!(f, "P"),
            NonTermType::Augmented => write!(f, "E'"),
        }
    }
}

/// define callback struct to track parser's action
struct ParserCallback {}

impl rusty_lr::callback::Callback<TermType, NonTermType> for ParserCallback {
    /// terminal symbol shifted and state transitioned to state_goto
    fn shift_and_goto(
        &mut self,
        parser: &parser::Parser<TermType, NonTermType>,
        state_stack: &[usize],
        term: &TermType,
        state_goto: usize,
    ) {
        // println!("Shift {} and goto State{}", term, state_goto);
    }
    /// non-terminal symbol shifted and state transitioned to state_goto
    fn shift_and_goto_nonterm(
        &mut self,
        parser: &parser::Parser<TermType, NonTermType>,
        state_stack: &[usize],
        nonterm: &NonTermType,
        state_goto: usize,
    ) {
        // println!("Shift {} and goto State{}", nonterm, state_goto);
    }
    /// set of tokens reduced by rule
    /// you can access the actual rule struct by parser.rules[rule_id]
    fn reduce(&mut self, parser: &parser::Parser<TermType, NonTermType>, rule: usize) {
        // rule is display if term, nonterm is display
        println!("Reduce by {}", parser.rules[rule]);
    }
}

fn main() {
    type Token = token::Token<TermType, NonTermType>;
    let mut grammar = grammar::Grammar::new();

    // A -> A + A | M ( reduce left )
    grammar.add_rule(
        NonTermType::A,
        vec![
            Token::NonTerm(NonTermType::A),
            Token::Term(TermType::Plus),
            Token::NonTerm(NonTermType::A),
        ],
        rule::ReduceType::Left, // reduce left
    );
    grammar.add_rule(
        NonTermType::A,
        vec![Token::NonTerm(NonTermType::M)],
        rule::ReduceType::Error, // error on shift/reduce conflict
    );

    // M -> M * M | P ( reduce left )
    grammar.add_rule(
        NonTermType::M,
        vec![
            Token::NonTerm(NonTermType::M),
            Token::Term(TermType::Mul),
            Token::NonTerm(NonTermType::M),
        ],
        rule::ReduceType::Left, // reduce left
    );
    grammar.add_rule(
        NonTermType::M,
        vec![Token::NonTerm(NonTermType::P)],
        rule::ReduceType::Error, // error on shift/reduce conflict
    );

    // P -> Num | ( E ) ( error on shift/reduce conflict )
    grammar.add_rule(
        NonTermType::P,
        vec![Token::Term(TermType::Num)],
        rule::ReduceType::Error, // error on shift/reduce conflict
    );
    grammar.add_rule(
        NonTermType::P,
        vec![
            Token::Term(TermType::LeftParen),
            Token::NonTerm(NonTermType::E),
            Token::Term(TermType::RightParen),
        ],
        rule::ReduceType::Error, // error on shift/reduce conflict
    );

    // E -> A
    grammar.add_rule(
        NonTermType::E,
        vec![Token::NonTerm(NonTermType::A)],
        rule::ReduceType::Error, // error on shift/reduce conflict
    );

    // augmented rule
    // E' -> E $
    grammar.add_rule(
        NonTermType::Augmented,
        vec![Token::NonTerm(NonTermType::E), Token::Term(TermType::Eof)],
        rule::ReduceType::Error, // error on shift/reduce conflict
    );

    // grammar is Display if Term, NonTerm is Display
    println!("Grammar:\n{}", grammar);

    // build DFA
    let parser = match grammar.build_main(NonTermType::Augmented) {
        Ok(result) => result,
        Err(err) => {
            // error is Display if Term, NonTerm is Display
            eprintln!("{}", err);
            return;
        }
    };

    // input terminal symbols
    // num + num * ( num + num )
    let terms = vec![
        TermType::Num,
        TermType::Plus,
        TermType::Num,
        TermType::Mul,
        TermType::LeftParen,
        TermType::Num,
        TermType::Plus,
        TermType::Num,
        TermType::RightParen,
    ];

    // start parsing with callback
    let mut callback = ParserCallback {};
    match parser.parse(terms.iter(), Some(TermType::Eof), &mut callback) {
        Ok(_) => println!("Parse success"),
        Err(err) => eprintln!("{:?}", err),
    }
}
