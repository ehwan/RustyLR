# RustyLR
RustyLR will provide you a LR(1) and LALR(1) Deterministic Finite Automata (DFA) generator from Context Free Grammar (CFGs).

```
[dependencies]
rusty_lr = "0.7.3"
```

## Features
 - pure Rust implementation
 - readable error messages, both for grammar building and parsing
 - compile-time DFA construction from CFGs ( with proc-macro )
 - customizable reducing action
 - resolving conflicts of ambiguous grammar
 - tracing parser action with callback, also error handling

## Sample

In [`example/calculator/parser.rs`](example/calculator/src/parser.rs),
```rust
use rusty_lr::lr1;
use rusty_lr::lalr1;

enum Token {
    // token definitions
    ...
}

// this define struct `EParser`
// where 'E' is the start symbol
lalr1! {
    // type of userdata
    %userdata i32;
    // type of token ( as Terminal symbol )
    %tokentype Token;

    // start symbol
    %start E;
    // eof symbol; for augmented rule generation
    %eof Token::Eof;

    // define tokens
    %token num Token::Num(0); // `num` maps to `Token::Num(0)`
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

    P(i32) : num {
        if let Token::Num(n) = v0 { *n }
        else { return Err(format!("{:?}", s0)); }
        //            ^^^^^^^^^ reduce action returns Result<(), String>
    }
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
    // 1 + 2 * ( 3 + 4 ) = 15
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
        Token::Eof,
    ];

    let parser = parser::EParser::new();
    let mut context = parser.begin();
    let mut userdata: i32 = 0;
    for token in input {
        match parser.feed(&mut context, token, &mut userdata) {
            //                          ^^^^^   ^^^^^^^^^^^^ userdata passed here as `&mut i32`
            //                           |- feed token
            Ok(_) => {}
            Err(e) => {
                println!("{:?}", e);
                return;
            }
        }
    }
    // res = value of start symbol ( E(i32) )
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
Consider the following context free grammar.
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

Note that the production rule `A -> A + A` has a shift/reduce conflict. To resolve this conflict, the precedence of shift/reduce is given to terminal symbol `Plus`. `Left` means that for `Plus` token, the parser will reduce the rule instead of shifting the token.

reduce/reduce conflict (e.g. duplicated rules) will be always an error.

### 3. Build DFA
Calling `grammar.build()` for LR(1) or `grammar.build_lalr()` for LALR(1) will build the DFA from the CFGs.

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

You must explicitly specify the Augmented non-terminal symbol, and the Augmented production rule must be defined in the grammar.
```
Augmented -> StartSymbol $
```

The returned `Parser` struct contains the DFA states and the production rules(cloned).
It is completely independent from the `Grammar`, so you can drop the `Grammar` struct, or export the `Parser` struct to another module.

### 4. Error messages
The `Error` type returned from `Grammar::build()` will contain the error information.
You can manually `match` the error type for custom error message,
but for most cases, using `println!("{}", err)` will be enough to see the detailed errors.
Error is `Display` if both `Term` and `NonTerm` is `Display`, and It is `Debug` if both `Term` and `NonTerm` is `Debug`.

#### Sample error messages
For Shift/Reduce conflicts,
```
Build failed: Shift/Reduce Conflict
NextTerm: '0'
Reduce Rule:
"Num" -> "Digit"
Shift Rules:
"Digit" -> '0' • /Lookaheads: '\0', '0'
Try rearanging the rules or set ReduceType to Terminal '0' to resolve the conflict.
```
For Reduce/Reduce conflicts,
```
Build failed: Reduce/Reduce Conflict with lookahead: '\0'
Production Rule1:
"Num" -> "Digit"
Production Rule2:
"Num" -> "Digit"
```

## Parse input sequence with generated DFA
For given input sequence, you can start parsing with `Parser::begin()` method.
Once you get the `Context` from `begin()`,
you can feed the input sequence one by one with `Parser::feed()` method.

```rust
let terms = vec![ Term::Num, Term::Plus, Term::Num, Term::Mul, Term::LeftParen, Term::Num, Term::Plus, Term::Num, Term::RightParen, Term::Eof];

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
```

`EOF` token is feeded at the end of sequence, and the augmented rule `Augmented -> StartSymbol $` will not be reduced since there are no lookahead symbols.

### Parse with callback

For tracing parser action, you can implement `Callback` trait and pass it to `parser.feed_callback()`.

```rust
struct ParserCallback {}

impl rusty_lr::Callback<Term, NonTerm> for ParserCallback {
    /// Error type for callback
    type Error = String;

    fn reduce(
        &mut self,
        rules: &[rusty_lr::ProductionRule<char, String>],
        //                                  ^     |- NonTerm
        //                                  |- Term
        states: &[rusty_lr::State<char, String>],
        //                         ^     |- NonTerm
        //                         |- Term
        state_stack: &[usize],
        rule: usize,
    ) -> Result<(), Self::Error> {
        // `Rule` is Display if Term, NonTerm is Display
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
// Num + Num * ( Num + Num )
let terms = vec![ Term::Num, Term::Plus, Term::Num, Term::Mul, Term::LeftParen, Term::Num, Term::Plus, Term::Num, Term::RightParen, Term::Eof];

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

## proc-macro `lr1!` and `lalr1!`
`lr1!` and `lalr1!` are procedural macros that will build `Parser` struct from CFGs at compile time.
Please refer to the [Sample](#sample) section for actual usage.

Every line in the macro must follow the syntax below.

#### Token type <sub><sup>(must defined)</sup></sub>
```
'%tokentype' <RustType> ';'
```
Define the type of terminal symbols.
`<RustType>` must be accessible at the point where the macro is called.

#### Token definition <sub><sup>(must defined)</sup></sub>
```
'%token' <Ident> <RustExpr> ';'
```
Map terminal symbol's name `<Ident>` to the actual value `<RustExpr>`.
`<RustExpr>` must be accessible at the point where the macro is called.

#### Start symbol <sub><sup>(must defined)</sup></sub>
```
'%start' <Ident> ';'
```
Define the start symbol of the grammar.

#### Eof symbol <sub><sup>(must defined)</sup></sub>
```
'%eof' <RustExpr> ';'
```
Define the `eof` terminal symbol.
`<RustExpr>` must be accessible at the point where the macro is called.
'eof' terminal symbol will be automatically added to the grammar.

#### Userdata type <sub><sup>(optional)</sup></sub>
```
'%userdata' <RustType> ';'
```
Define the type of userdata passed to `feed()` function.

#### Reduce type <sub><sup>(optional)</sup></sub>
```
'%left' <Ident> ';'
'%right' <Ident> ';'
```
Set the shift/reduce precedence of terminal symbols. `<Ident>` must be defined in `%token`.

#### Production rules
```
<Ident><RuleType>
  ':' <Ident>* <ReduceAction> 
  '|' <Ident>* <ReduceAction> 
  ...
  ';'
```
Define the production rules.
`<Ident>` must be valid terminal or non-terminal symbols.

```
(optional)
<RuleType> : '(' <RustType> ')'
           |
           ;
```
`<RuleType>` is optional, this will define the type of value that this production rule holds.

```
(optional)
<ReduceAction> : '{' <RustExpr> '}'
               |
               ;
```
`<ReduceAction>` is optional,
this will define the action to be executed when the rule is matched and reduced.
If `<RuleType>` is defined, `<ReduceAction>` itself must be the value of `<RuleType>` (i.e. no semicolon at the end of the statement).

**Predefined variables** can be used in `<ReduceAction>`:
 - `s0`, `s1`, `s2`, ... : slice of shifted terminal symbols `&[<TermType>]` captured by N'th symbol
 - `s` : slice of shifted terminal symbols `&[<TermType>]` captured by current rule.
 - `v0`, `v1`, `v2`, ... : value of N'th symbol. 
 If N'th symbol is Terminal, it will be `&<TermType>`, 
 and if it is NonTerminal, it will be `mut <RuleType>`.
 - `data` : userdata passed to `feed()` function.

`Result<(), String>` can be returned from `<ReduceAction>`.
Returned `Err` will be delivered to the caller of `feed()` function.

## Start Parsing
`lr1!` and `lalr1!` will generate struct `<StartSymbol>Parser`.

The struct has the following functions:
 - `new()` : create new parser
 - `begin(&self)` : create new context
 - `feed(&self, &mut Context, TermType, &mut UserData) -> Result<(), ParseError>` : feed token to the parser
 - `feed_callback(&self, &mut Context, TermType, &mut UserData) -> Result<(), ParseError>` : feed token with callback

Note that the parameter `&mut UserData` is omitted if `%userdata` is not defined.
Once the input sequence (including `eof` token) is feeded, without errors, you can get the value of start symbol by calling `context.accept()`.

```rust
let parser = parser::EParser::new();
// create context
let mut context = parser.begin();
// define userdata
let mut userdata: i32 = 0;

// start feeding tokens
for token in input_sequence {
    match parser.feed(&mut context, token, &mut userdata) {
        //                          ^^^^^   ^^^^^^^^^^^^ userdata passed here as `&mut i32`
        //                           |- feed token
        Ok(_) => {}
        Err(e) => {
            println!("{:?}", e);
            return;
        }
    }
}
// res = value of start symbol
let res = context.accept();
println!("{}", res);
println!("userdata: {}", userdata);
```