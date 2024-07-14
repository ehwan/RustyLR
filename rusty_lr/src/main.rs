mod action;
mod grammar;
mod parser;
mod rule;
mod state;
mod term;
mod token;

#[derive(Debug, Hash, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Term {
    Id,
    Plus,
    Star,
    LeftParen,
    RightParen,
}
impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Id => write!(f, "id"),
            Term::Plus => write!(f, "+"),
            Term::Star => write!(f, "*"),
            Term::LeftParen => write!(f, "("),
            Term::RightParen => write!(f, ")"),
        }
    }
}

fn main() {
    type Token = token::Token<Term, &'static str>;
    let mut grammar = grammar::Grammar::new();

    grammar.add_rule(
        "A",
        vec![
            Token::NonTerm("A"),
            Token::Term(Term::Plus),
            Token::NonTerm("M"),
        ],
    );
    grammar.add_rule("A", vec![Token::NonTerm("M")]);

    grammar.add_rule(
        "M",
        vec![
            Token::NonTerm("M"),
            Token::Term(Term::Star),
            Token::NonTerm("P"),
        ],
    );
    grammar.add_rule("M", vec![Token::NonTerm("P")]);

    grammar.add_rule("P", vec![Token::Term(Term::Id)]);
    grammar.add_rule(
        "P",
        vec![
            Token::Term(Term::LeftParen),
            Token::NonTerm("E"),
            Token::Term(Term::RightParen),
        ],
    );
    grammar.add_rule("E", vec![Token::NonTerm("A")]);

    grammar.set_main("E");

    let parser = match grammar.build_main("Augmented") {
        Ok(result) => result,
        Err(err) => {
            eprintln!("{}", err);
            return;
        }
    };
    println!("{}", grammar);
    println!("Number of states: {}", parser.states.len());
    println!("Main state: {}", parser.main_state);

    for (state_id, state) in parser.states.iter().enumerate() {
        println!("State{}", state_id);
        println!("{}", state);
    }

    let tokens = vec![
        Term::Id,
        Term::Plus,
        Term::Id,
        Term::Star,
        Term::LeftParen,
        Term::Id,
        Term::Plus,
        Term::Id,
        Term::RightParen,
    ];
    match parser.parse(&tokens) {
        Ok(_) => println!("Parse success"),
        Err(err) => eprintln!("{}", err),
    }
}
