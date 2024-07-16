# RustyLR
LR(1) Parser generator in Rust

## Usage
In [`example/calculator/src/main.rs`](example/calculator/src/main.rs)
```rust
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
impl Display for TermType { ... }
impl Display for NonTermType { ... }



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
        vec![Token::NonTerm(NonTermType::A), Token::Term(TermType::Plus), Token::NonTerm(NonTermType::A)],
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
        vec![Token::NonTerm(NonTermType::M), Token::Term(TermType::Mul), Token::NonTerm(NonTermType::M)],
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
        vec![Token::Term(TermType::LeftParen), Token::NonTerm(NonTermType::E), Token::Term(TermType::RightParen)],
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
    let parser = match grammar.build(NonTermType::Augmented) {
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
```


The output will be:
```
Grammar:
0: A -> A + A (Left)
1: A -> M
2: M -> M * M (Left)
3: M -> P
4: P -> Num
5: P -> ( E )
6: E -> A
7: E' -> E $

Reduce by P -> Num
Reduce by M -> P
Reduce by A -> M
Reduce by P -> Num
Reduce by M -> P
Reduce by P -> Num
Reduce by M -> P
Reduce by A -> M
Reduce by P -> Num
Reduce by M -> P
Reduce by A -> M
Reduce by A -> A + A (Left)
Reduce by E -> A
Reduce by P -> ( E )
Reduce by M -> P
Reduce by M -> M * M (Left)
Reduce by A -> M
Reduce by A -> A + A (Left)
Reduce by E -> A
Parse success
```

## Error Messages
An `BuildError` type returned from `Grammar::build()` will contain the error message.
```rust
let parser = match grammar.build(NonTermType::Augmented) {
    Ok(result) => result,
    Err(err) => {
        // error is Display if Term, NonTerm is Display
        eprintln!("{}", err);

        // error is Debug if Term, NonTerm is Debug
        eprintln!("{:?}", err);
    }
};
```

For reduce/reduce conflict, the error message will be:
```
Grammar:
0: A -> A + A
1: A -> A + A

Reduce/Reduce Conflict with lookahead: +
A -> A + A
and
A -> A + A
```

For shift/reduce conflict, the error message will be:
```
Grammar:
0: A -> A + A
1: A -> M

Shift/Reduce Conflict
This rule has ReduceType::Error:
A -> A + . A / +
and the reduce rule is:
A -> A + A
Try rearanging the rules or change ReduceType to Left or Right.
```


## Resolving Conflicts

For shift/reduce conflict, you can specify the reduce type in `Grammar::add_rule()`.
```rust
pub enum ReduceType {
    Left,
    Right,
    /// error when conflict occurs
    Error,
}
```

For example, the production rule `A -> A + A | M` has a shift/reduce conflict.
Passing `ReduceType::Left` will force the rule to be reduced left (reduce first) when there is a shift/reduce conflict.

```rust
grammar.add_rule(
    NonTermType::A,
    vec![Token::NonTerm(NonTermType::A), Token::Term(TermType::Plus), Token::NonTerm(NonTermType::A)],
    rule::ReduceType::Left, // reduce left
);
grammar.add_rule(
    NonTermType::A,
    vec![Token::NonTerm(NonTermType::M)],
    rule::ReduceType::Error,
);
```