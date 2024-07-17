#![allow(unused_variables)]

use std::fmt::Display;

use rusty_lr;

/// define set of terminal symbols
/// Term must implement Clone, Eq, Ord, Hash ( for BTreeMap, HashMap ) for DFA generation
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Term {
    Num,
    Plus,
    Mul,
    LeftParen,
    RightParen,
    Eof,
}

/// define set of non-terminal symbols
/// NonTerm must implement Clone, Eq, Hash for DFA generation
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum NonTerm {
    E,
    A,
    M,
    P,
    Augmented,
}

/// impl Display for TermType, NonTermType will make related ProductionRule, error message Display-able
impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Num => write!(f, "Num"),
            Term::Plus => write!(f, "+"),
            Term::Mul => write!(f, "*"),
            Term::LeftParen => write!(f, "("),
            Term::RightParen => write!(f, ")"),
            Term::Eof => write!(f, "$"),
        }
    }
}
impl Display for NonTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NonTerm::E => write!(f, "E"),
            NonTerm::A => write!(f, "A"),
            NonTerm::M => write!(f, "M"),
            NonTerm::P => write!(f, "P"),
            NonTerm::Augmented => write!(f, "E'"),
        }
    }
}

/// define callback struct to trace parser's action
struct ParserCallback {}

impl rusty_lr::Callback<Term, NonTerm> for ParserCallback {
    /// terminal symbol shifted and state transitioned
    fn shift_and_goto(
        &mut self,
        parser: &rusty_lr::Parser<Term, NonTerm>,
        context: &rusty_lr::Context<'_, Term, NonTerm>,
    ) {
        // println!("Shift {} and goto State{}", context.term(), context.state());
    }
    /// non-terminal symbol shifted and state transitioned
    fn shift_and_goto_nonterm(
        &mut self,
        parser: &rusty_lr::Parser<Term, NonTerm>,
        context: &rusty_lr::Context<'_, Term, NonTerm>,
        nonterm: &NonTerm,
    ) {
        // println!("Shift {} and goto State{}", nonterm, context.state());
    }
    /// set of tokens reduced by rule
    /// you can access the actual rule struct by parser.rules[rule_id]
    fn reduce(
        &mut self,
        parser: &rusty_lr::Parser<Term, NonTerm>,
        context: &rusty_lr::Context<'_, Term, NonTerm>,
        rule: usize,
    ) {
        // rule is display if term, nonterm is display
        println!("Reduce by {}", parser.rules[rule]);
    }

    /// error handling
    fn invalid_term(
        &mut self,
        parser: &rusty_lr::Parser<Term, NonTerm>,
        context: &rusty_lr::Context<'_, Term, NonTerm>,
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
        context: &rusty_lr::Context<'_, Term, NonTerm>,
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
    let mut grammar = rusty_lr::Grammar::<Term, NonTerm>::new();

    // A -> A + A | M ( reduce left )
    grammar.add_rule(
        NonTerm::A,
        vec![
            Token::NonTerm(NonTerm::A),
            Token::Term(Term::Plus),
            Token::NonTerm(NonTerm::A),
        ],
        rusty_lr::ReduceType::Left, // reduce left
    );
    grammar.add_rule(
        NonTerm::A,
        vec![Token::NonTerm(NonTerm::M)],
        rusty_lr::ReduceType::Error, // error on shift/reduce conflict
    );

    // M -> M * M | P ( reduce left )
    grammar.add_rule(
        NonTerm::M,
        vec![
            Token::NonTerm(NonTerm::M),
            Token::Term(Term::Mul),
            Token::NonTerm(NonTerm::M),
        ],
        rusty_lr::ReduceType::Left, // reduce left
    );
    grammar.add_rule(
        NonTerm::M,
        vec![Token::NonTerm(NonTerm::P)],
        rusty_lr::ReduceType::Error, // error on shift/reduce conflict
    );

    // P -> Num | ( E ) ( error on shift/reduce conflict )
    grammar.add_rule(
        NonTerm::P,
        vec![Token::Term(Term::Num)],
        rusty_lr::ReduceType::Error, // error on shift/reduce conflict
    );
    grammar.add_rule(
        NonTerm::P,
        vec![
            Token::Term(Term::LeftParen),
            Token::NonTerm(NonTerm::E),
            Token::Term(Term::RightParen),
        ],
        rusty_lr::ReduceType::Error, // error on shift/reduce conflict
    );

    // E -> A
    grammar.add_rule(
        NonTerm::E,
        vec![Token::NonTerm(NonTerm::A)],
        rusty_lr::ReduceType::Error, // error on shift/reduce conflict
    );

    // augmented rule
    // E' -> E $
    grammar.add_rule(
        NonTerm::Augmented,
        vec![Token::NonTerm(NonTerm::E), Token::Term(Term::Eof)],
        rusty_lr::ReduceType::Error, // error on shift/reduce conflict
    );

    // grammar is Display if Term, NonTerm is Display
    println!("Grammar:\n{}", grammar);

    // build DFA
    let parser = match grammar.build(NonTerm::Augmented) {
        Ok(parser) => parser,
        Err(err) => {
            // error is Display if Term, NonTerm is Display
            eprintln!("{}", err);
            return;
        }
    };

    // input terminal symbols
    // num + num * ( num + num )
    let terms = vec![
        Term::Num,
        Term::Plus,
        Term::Num,
        Term::Mul,
        Term::LeftParen,
        Term::Num,
        Term::Plus,
        Term::Num,
        Term::RightParen,
    ];

    // start parsing with callback
    let mut callback = ParserCallback {};
    match parser.parse_with_callback(&terms, &mut callback, Term::Eof) {
        Ok(tree) => println!("Parse success {:?}", tree.slice(&terms)),
        Err(err) => eprintln!("{}", err),
    }
}
