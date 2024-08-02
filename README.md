# RustyLR
RustyLR will provide you a LR(1) and LALR(1) Deterministic Finite Automata (DFA) generator from Context Free Grammar (CFGs).

```
[dependencies]
rusty_lr = "0.6.0"
rusty_lr_derive = "0.6.0"
```

## Features
 - pure Rust implementation
 - compile-time DFA construction from CFGs ( with proc-macro )
 - customizable reducing action
 - resolving conflicts of ambiguous grammar
 - tracing parser action with callback, also error handling

## Sample

In [`example/calculator/parser.rs`](example/calculator/src/parser.rs),
```rust
use rusty_lr_derive::lr1;
use rusty_lr_derive::lalr1;

enum Token {
    // token definitions
    ...
}

// this define struct `EParser`
// where 'E' is the start symbol
lalr1! {
    // type of userdata
    %userdata i32;
    // typeof token
    %tokentype Token;

    // start symbol
    %start E;
    // eof symbol; for augmented rule generation
    %eof Token::Eof;

    // define tokens
    %token num Token::Num(0);
    %token plus Token::Plus;
    %token star Token::Star;
    %token lparen Token::LParen;
    %token rparen Token::RParen;

    // resolving shift/reduce conflict
    %left plus;
    %left star;

    // s{N} is slice of shifted terminal symbols captured by N'th symbol
    // v{N} is value of N'th symbol ( if it has value )
    // s is slice of shifted terminal symbols captured by current rule
    // userdata canbe accessed by `data` ( &mut i32, for current situation )
    A(i32) : A plus A {
            println!("{:?} {:?} {:?}", s0, s1, s2 );
            //                         ^   ^   ^
            //                         |   |   |- slice of 2nd 'A'
            //                         |   |- slice of 'plus'
            //                         |- slice of 1st 'A'
            println!( "{:?}", s );
            *data += 1;
            v0 + v2 // --> this will be new value of current 'A'
        //  ^    ^ 
        //  |    |- value of 2nd 'A'
        //  |- value of 1st 'A'
        }
      | M { v0 }
      ;

    M(i32) : M star M { v0 * v2 }
      | P { v0 }
      ;

    P(i32) : num { if let Token::Num(n) = v0 { *n } else { return Err(format!("{:?}", s0)); } }
      | lparen E rparen { v1 }
      ;

    E(i32) : A  { v0 };
}
```

In [`example/calculator/src/main.rs`](example/calculator/src/main.rs),
```rust
mod parser;

fn main() {
    use parser::Token;
    let input = vec![
        Token::Num(1),
        Token::Plus,
        Token::Num(2),
        Token::Star,
        Token::LParen,
        Token::Num(3),
        Token::Plus,
        Token::Num(4),
        Token::RParen,
    ];

    let parser = parser::EParser::new();
    let mut context = parser.begin();
    let mut userdata: i32 = 0;
    for token in input {
        match parser.feed(&mut context, token, &mut userdata) {
            Ok(_) => {}
            Err(e) => {
                println!("{:?}", e);
                return;
            }
        }
    }
    let res = context.accept();
    println!("{}", res);
    println!("userdata: {}", userdata);
}
```

The result will be:
```
[Num(3)] [Plus] [Num(4)]
[Num(3), Plus, Num(4)]
[Num(1)] [Plus] [Num(2), Star, LParen, Num(3), Plus, Num(4), RParen]
[Num(1), Plus, Num(2), Star, LParen, Num(3), Plus, Num(4), RParen]
15
userdata: 2
```

## Build Deterministic Finite Automata (DFA) from Context Free Grammar (CFG)
This section will describe how to build DFA from CFGs, on runtime.

### 1. Define terminal and non-terminal symbols

```rust
#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)] // must implement these traits
pub enum Term {
    Num,
    Plus,
    Mul,
    LeftParen,
    RightParen,
    Eof,
}

#[derive(Clone, Hash, PartialEq, Eq)] // must implement these traits
pub enum NonTerm {
    E,
    A,
    M,
    P,
    Augmented,
}

/// impl Display for TermType, NonTermType will make related ProductionRule, error message Display-able
impl Display for TermType { ... }
impl Display for NonTermType { ... }
```
Or simply, you can use `char` or `u8` as terminal, and `&'static str` or `String` as non-terminal.
***Any type*** that implements traits above can be used as terminal and non-terminal symbols.

### 2. Define production rules
Consider the following context free grammar:
```
A -> A + A  (reduce left)
A -> M
```
This grammar can be written as:
```rust
/// type alias
type Token = rusty_lr::Token<Term, NonTerm>;

/// create grammar
let mut grammar = rusty_lr::Grammar::<Term,NonTerm>::new();

grammar.add_rule(
    NonTerm::A,
    vec![Token::NonTerm(NonTerm::A), Token::Term(Term::Plus), Token::NonTerm(NonTerm::A)],
);
grammar.add_rule(
    NonTerm::A,
    vec![Token::NonTerm(NonTerm::M)],
);

/// set reduce type
grammar.set_reduce_type( Term::Plus, ReduceType::Left );
```

Note that the production rule `A -> A + A` has a shift/reduce conflict, and the reduce type is set to `ReduceType::Left` for terminal symbol `Plus` to resolve the conflict. Default will cause an error when a conflict occurs.

reduce/reduce conflict (e.g. duplicated rules) will be always an error.

### 3. Build DFA
Calling `grammar.build()` or `grammar.build_lalr()` will build the DFA from the CFGs.

```rust
let parser:rusty_lr::Parser<Term,NonTerm> = match grammar.build(NonTerm::Augmented) {
    Ok(parser) => parser,
    Err(err) => {
        // error is Display if Term, NonTerm is Display
        eprintln!("{}", err);
        return;
    }
};
```

You must explicitly specify the Augmented non-terminal symbol, and the production rule `Augmented -> StartSymbol $` must be defined in the grammar.

The `Error` type returned from `Grammar::build()` will contain the error information.
`Grammar` is `Display` if both `Term` and `NonTerm` is `Display`, and It is `Debug` if both `Term` and `NonTerm` is `Debug`.

The returned `Parser` struct contains the DFA and the production rules(cloned). It is completely independent from the `Grammar` struct, so you can drop the `Grammar` struct, or export the `Parser` struct to another module.

## Parse input sequence with generated DFA
For given input sequence, you can start parsing with `Parser::begin()` method. Once you get the `Context` from `begin()`, you will feed the input sequence to the parser with `parser.feed()` method.

```rust
let terms = vec![ Term::Num, Term::Plus, Term::Num, Term::Mul, Term::LeftParen, Term::Num, Term::Plus, Term::Num, Term::RightParen];

// start parsing
let mut context = parser.begin();

// feed input sequence
for term in terms {
    match parser.feed(&mut context, term) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("{:?}", err);
            return;
        }
    }
}

// end parsing
match parser.end(&mut context, &Term::Eof) {
    Ok(_) => (),
    Err(err) => {
        eprintln!("{:?}", err);
        return;
    }
}
```

Not that `EOF` token is not feeded, but passed to `end()` method.

### Parse with callback

For complex error handling and tracing parser action, you can implement `Callback` trait and pass it to `*_callback(...)` methods.

```rust
struct ParserCallback {}

impl rusty_lr::Callback<Term, NonTerm> for ParserCallback {
    /// Error type for callback
    type Error = String;

    fn reduce(
        &mut self,
        rules: &[rusty_lr::ProductionRule<char, String>],
        states: &[rusty_lr::State<char, String>],
        state_stack: &[usize],
        rule: usize,
    ) -> Result<(), Self::Error> {
        println!("Reduce by {}", rules[rule]);
        Ok(())
    }
    fn shift_and_goto(
        &mut self,
        rules: &[rusty_lr::ProductionRule<char, String>],
        states: &[rusty_lr::State<char, String>],
        state_stack: &[usize],
        term: &char,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
    fn shift_and_goto_nonterm(
        &mut self,
        rules: &[rusty_lr::ProductionRule<char, String>],
        states: &[rusty_lr::State<char, String>],
        state_stack: &[usize],
        nonterm: &String,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
}
```

```rust
let terms = vec![ Term::Num, Term::Plus, Term::Num, Term::Mul, Term::LeftParen, Term::Num, Term::Plus, Term::Num, Term::RightParen];

// start parsing
let mut context = parser.begin();
let mut callback = ParserCallback {};

// feed input sequence
for term in terms {
    match parser.feed_callback(&mut context, &mut callback, term) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("{:?}", err);
            return;
        }
    }
}

// end parsing
match parser.end_callback(&mut context, &mut callback, &Term::Eof) {
    Ok(_) => (),
    Err(err) => {
        eprintln!("{:?}", err);
        return;
    }
}
```

The result will be:
```
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
Reduce by A -> A + A
Reduce by E -> A
Reduce by P -> ( E )
Reduce by M -> P
Reduce by M -> M * M
Reduce by A -> M
Reduce by A -> A + A
Reduce by E -> A
```