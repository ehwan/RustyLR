# RustyLR
LR(1) and LALR(1) Deterministic Finite Automata (DFA) generator from Context Free Grammar (CFGs).

```
[dependencies]
rusty_lr = "0.11.1"
```

## Features
 - pure Rust implementation
 - readable error messages, both for grammar building and parsing
 - compile-time DFA construction from CFGs ( with proc-macro )
 - customizable reduce action
 - resolving conflicts of ambiguous grammar
 - tracing parser action with callback

#### Why proc-macro, not external executable?
 - Decent built-in lexer, with consideration of unicode and comments.
 - Can generate *pretty* error messages, by just passing `Span` data.
 - With modern IDE, can see errors in real-time with specific location.

## Sample

 - [Calculator](example/calculator): calculator with enum `Token`
 - [Calculator with `u8`](example/calculator_u8): calculator with `u8`
 - [Bootstrap](rusty_lr_parser/src/parser.rs): bootstrap of `lr1!` and `lalr1!` macro

Please refer to the [example](example) directory for the full example.

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

    // data that each token holds can be accessed by its name
    // s is slice of shifted terminal symbols captured by current rule
    // userdata can be accessed by `data` ( &mut i32, for this situation )
    A(i32) : A plus a2=A {
            println!("{:?} {:?} {:?}", A.slice, *plus, a2.slice );
            //                         ^        ^      ^
            //                         |        |      |- slice of 2nd 'A'
            //                         |        |- &Token
            //                         |- slice of 1st 'A'
            println!( "{:?}", s );
            *data += 1;
            A.value + a2.value // --> this will be new value of current 'A'
        //  ^         ^
        //  |         |- value of 2nd 'A'
        //  |- value of 1st 'A'
        }
      | M { M.value }
      ;

    M(i32) : M star m2=M { *M * *m2 }
      | P { *P }
      ;

    P(i32) : num {
        if let Token::Num(n) = *num { *n }
        else { return Err(format!("{:?}", num)); }
        //            ^^^^^^^^^^^^^^^^^^^^^^^^^^
        //             reduce action returns Result<(), String>
    }
      | lparen E rparen { *E }
      ;

    E(i32) : A  { *A };
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
[Num(3)] Plus [Num(4)]
[Num(3), Plus, Num(4)]
[Num(1)] Plus [Num(2), Star, LParen, Num(3), Plus, Num(4), RParen]
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

Every line in the macro must follow the syntax below. The syntax can be also found in the [bootstrap](rusty_lr_parser/src/parser.rs) file.

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
// reduce first
'%left' <Ident> ';'
'%l' <Ident> ';'
'%reduce' <Ident> ';'

// shift first
'%right' <Ident> ';'
'%r' <Ident> ';'
'%shift' <Ident> ';'
```
Set the shift/reduce precedence for terminal symbols. `<Ident>` must be defined in `%token`.

#### Production rules
```
<Ident><RuleType>
  ':' <Token>* <ReduceAction>
  '|' <Token>* <ReduceAction>
  ...
  ';'
```
Define the production rules.

```
<Token> : <Ident as Term or Non-Term>                                ... (1)
        | <Ident as variable name> '=' <Ident as Term or Non-Term>   ... (2)
        ;
```
For (1), `<Ident>` must be valid terminal or non-terminal symbols. In this case, the data of the token will be mapped to the variable with the same name as `<Ident>`.
For (2), the data of the token will be mapped to the variable on the left side of '='.
For more information about the token data and variable, refer to the [reduce action](#reduceaction) below.

#### RuleType <sub><sup>(optional)</sup></sub>
```
<RuleType> : '(' <RustType> ')'
           |
           ;
```
Define the type of value that this production rule holds.

#### ReduceAction
```
<ReduceAction> : '{' <RustExpr> '}'
               |
               ;
```
Define the action to be executed when the rule is matched and reduced.
If `<RuleType>` is defined, `<ReduceAction>` itself must be the value of `<RuleType>` (i.e. no semicolon at the end of the statement).

**predefined variables** can be used in `<ReduceAction>`:
 - `s` : slice of shifted terminal symbols `&[<TermType>]` captured by current rule.
 - `data` : userdata passed to `feed()` function.

To access the data of each token, you can directly use the name of the token as a variable.
For non-terminal symbols, the type of data is [`rusty_lr::NonTermData<'a, TermType, RuleType>`](rusty_lr_core/src/nontermdata.rs).
For terminal symbols, the type of data is [`rusty_lr::TermData<'a, TermType>`](rusty_lr_core/src/termdata.rs).
If multiple variables are defined with the same name, the variable on the front-most will be used.

For example, following code will print the value of each `A`, and the slice of each `A` and `plus` token in the production rule `E -> A plus A`.
```rust
%token plus ...;

E : A plus a2=A
  {
    println!("Value of 1st A: {}", A.value);  // A.value or *A
    println!("Slice of 1st A: {:?}", A.slice);
    println!("Value of 2nd A: {}", a2.value); // a2.value or *a2
    println!("Slice of 2nd A: {:?}", a2.slice);

    if let Token::Plus(plus) = *plus {
        println!( "Plus token: {:?}", plus );
    }
  }
  ;

A(i32): ... ;
```

`Result<(), String>` can be returned from `<ReduceAction>`.
Returned `Err` will be delivered to the caller of `feed()` function.

#### Error type <sub><sup>(optional)</sup></sub>
```
'%err' <RustType> ';'
'%error' <RustType> ';'
```
Define the type of `Err` variant in `Result<(), Err>` returned from `<ReduceAction>`. If not defined, `String` will be used.

## Start Parsing
`lr1!` and `lalr1!` will generate struct `<StartSymbol>Parser`.

The struct has the following functions:
 - `new()` : create new parser
 - `begin(&self)` : create new context
 - `feed(&self, &mut Context, TermType, &mut UserData) -> Result<(), ParseError>` : feed token to the parser
 - `feed_callback(&self, &mut Context, &mut C: Callback, TermType, &mut UserData) -> Result<(), ParseError>` : feed token with callback

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
