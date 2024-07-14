mod action;
mod grammar;
mod parser;
mod rule;
mod state;
mod term;
mod token;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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
    type Token = token::Token<Term>;
    let mut grammar = grammar::Grammar::new();

    grammar.add_rule(
        "A",
        vec![
            Token::NonTerm("A".to_string()),
            Token::Term(Term::Plus),
            Token::NonTerm("M".to_string()),
        ],
    );
    grammar.add_rule("A", vec![Token::NonTerm("M".to_string())]);

    grammar.add_rule(
        "M",
        vec![
            Token::NonTerm("M".to_string()),
            Token::Term(Term::Star),
            Token::NonTerm("P".to_string()),
        ],
    );
    grammar.add_rule("M", vec![Token::NonTerm("P".to_string())]);

    grammar.add_rule("P", vec![Token::Term(Term::Id)]);
    grammar.add_rule(
        "P",
        vec![
            Token::Term(Term::LeftParen),
            Token::NonTerm("E".to_string()),
            Token::Term(Term::RightParen),
        ],
    );
    grammar.add_rule("E", vec![Token::NonTerm("A".to_string())]);

    grammar.set_main("E");

    let parser = match grammar.build_main() {
        Ok(result) => result,
        Err(err) => {
            eprintln!("{}", err);
            return;
        }
    };
    println!("Number of states: {}", parser.states.len());
    println!("Main state: {}", parser.main_state);

    for (state_id, state) in parser.states.iter().enumerate() {
        println!("State{}", state_id);
        println!("{}", state);
    }

    let tokens = vec![
        Token::Term(Term::Id),
        Token::Term(Term::Plus),
        Token::Term(Term::Id),
        Token::Term(Term::Star),
        Token::Term(Term::LeftParen),
        Token::Term(Term::Id),
        Token::Term(Term::Plus),
        Token::Term(Term::Id),
        Token::Term(Term::RightParen),
    ];
    match parser.parse(&tokens) {
        Ok(result) => println!("Result: {}", result),
        Err(err) => eprintln!("{}", err),
    }
}
