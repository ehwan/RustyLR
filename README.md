# RustyLR
yacc-like LR(1) and LALR(1) Deterministic Finite Automata (DFA) generator from Context Free Grammar (CFGs).

```
[dependencies]
rusty_lr = "0.12.1"
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
 - [Bootstrap](rusty_lr_parser/src/parser.rs): bootstrapped line parser of `lr1!` and `lalr1!` macro

Please refer to the [example](example) directory for the full example.

In [`example/calculator_u8/parser.rs`](example/calculator_u8/src/parser.rs),
```rust
use rusty_lr::lr1;
use rusty_lr::lalr1;

// this define struct `EParser`
// where 'E' is the start symbol
lr1! {
    %userdata i32;
    %tokentype u8;
    %start E;
    %eof b'\0';

    %token zero b'0';
    %token one b'1';
    %token two b'2';
    %token three b'3';
    %token four b'4';
    %token five b'5';
    %token six b'6';
    %token seven b'7';
    %token eight b'8';
    %token nine b'9';
    %token plus b'+';
    %token star b'*';
    %token lparen b'(';
    %token rparen b')';
    %token space b' ';

    %left plus;
    %left star;

    WS0: space*;

    Digit: zero | one | two | three | four | five | six | seven | eight | nine;

    Number(i32): WS0 Digit+ WS0 { std::str::from_utf8(Digit.slice).unwrap().parse().unwrap() };

    A(f32): A plus a2=A {
        *data += 1; // access userdata by `data`
        println!( "{:?} {:?} {:?}", A.slice, *plus, a2.slice );
        *A + *a2
    }
    | M { *M }
    ;

    M(f32): M star m2=M { *M * *m2 }
    | P { *P }
    ;

    P(f32): Number { *Number as f32 }
    | WS0 lparen E rparen WS0 { *E }
    ;

    E(f32) : A { *A };

}
```

In [`example/calculator_u8/src/main.rs`](example/calculator_u8/src/main.rs),
```rust
pub mod parser;

fn main() {
    let input = "  1 +  20 *   (3 + 4 )   ";

    let parser = parser::EParser::new();
    let mut context = parser.begin();
    let mut userdata: i32 = 0;
    for b in input.as_bytes().iter() {
        match parser.feed(&mut context, *b, &mut userdata) {
            // feed userdata here
            Ok(_) => {}
            Err(e) => {
                eprintln!("error: {:?}", e);
                return;
            }
        }
    }
    parser.feed(&mut context, 0, &mut userdata).unwrap(); // feed EOF

    let result = context.accept(); // get value of start 'E'
    println!("result: {}", result);
    println!("userdata: {}", userdata);
}
```

The result will be:
```
[51, 32] 43 [32, 52, 32]
[32, 32, 49, 32] 43 [32, 32, 50, 48, 32, 42, 32, 32, 32, 40, 51, 32, 43, 32, 52, 32, 41, 32, 32, 32]
result: 141
userdata: 2
```





## proc-macro `lr1!`, `lalr1!`, `lr1_runtime!`, and `lalr1_runtime!`
`lr1!`, `lalr1!`, `lr1_runtime!`, and `lalr1_runtime!` are procedural macros that will build `Parser` struct from CFGs.
Please refer to the [Sample](#sample) section for actual usage.

### Compile-time vs Runtime
Former two macros (those without '_runtime' suffix) will generate `Parser` struct at compile-time, which `build()` is called at compile-time and the generated code will be *TONS* of `insert` of tokens one by one.
Latter two (those with '_runtime' suffix) will generate `Parser` struct at runtime, the `build()` is called on `Parser::new()`.


### Syntax
Every line in the macro must follow the syntax below. The syntax can be also found in the [bootstrap](rusty_lr_parser/src/parser.rs) file.

### Token type <sub><sup>(must defined)</sup></sub>
```
'%tokentype' <RustType> ';'
```
Define the type of terminal symbols.
`<RustType>` must be accessible at the point where the macro is called.

### Token definition <sub><sup>(must defined)</sup></sub>
```
'%token' <Ident> <RustExpr> ';'
```
Map terminal symbol's name `<Ident>` to the actual value `<RustExpr>`.
`<RustExpr>` must be accessible at the point where the macro is called.

### Start symbol <sub><sup>(must defined)</sup></sub>
```
'%start' <Ident> ';'
```
Define the start symbol of the grammar.

### Eof symbol <sub><sup>(must defined)</sup></sub>
```
'%eof' <RustExpr> ';'
```
Define the `eof` terminal symbol.
`<RustExpr>` must be accessible at the point where the macro is called.
'eof' terminal symbol will be automatically added to the grammar.

### Userdata type <sub><sup>(optional)</sup></sub>
```
'%userdata' <RustType> ';'
```
Define the type of userdata passed to `feed()` function.

### Reduce type <sub><sup>(optional)</sup></sub>
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

### Production rules
```
<Ident><RuleType>
  ':' <TokenMapped>* <ReduceAction>
  '|' <TokenMapped>* <ReduceAction>
  ...
  ';'
```
Define the production rules.

```
<TokenMapped> : <Ident as var_name> '=' <TokenPattern>
              | <TokenPattern>
              ;
```
```
<TokenPattern> : <Ident as terminal or non-terminal>
               | <Ident as terminal or non-terminal> '*'  (zero or more)
               | <Ident as terminal or non-terminal> '+'  (one or more)
               | <Ident as terminal or non-terminal> '?'  (zero or one)
               ;
```

Example:
```
E: A plus* d=D ;
```
This production rule defines non-terminal `E` to be `A`, then zero or more `plus`, then `D` mapped to variable `d`.
For more information, please refer to the [Accessing token data in ReduceAction](#accessing-token-data-in-reduceaction) section.


### RuleType <sub><sup>(optional)</sup></sub>
```
<RuleType> : '(' <RustType> ')'
           |
           ;
```
Define the type of value that this production rule holds.

### ReduceAction <sub><sup>(optional)</sup></sub>
```
<ReduceAction> : '{' <RustExpr> '}'
               |
               ;
```

Define the action to be executed when the rule is matched and reduced.
If `<RuleType>` is defined, `<ReduceAction>` itself must be the value of `<RuleType>` (i.e. no semicolon at the end of the statement).

### Accessing token data in ReduceAction

**predefined variables** can be used in `<ReduceAction>`:
 - `s` : slice of shifted terminal symbols `&[<TermType>]` captured by current rule.
 - `data` : userdata passed to `feed()` function.

To access the data of each token, you can directly use the name of the token as a variable.
For non-terminal symbols, the type of data is [`rusty_lr::NonTermData`](rusty_lr_core/src/nontermdata.rs).
You can access the value of `<RuleType>` by `NonTerm.value`(by value) or `*NonTerm`(by `Deref`), slice by `NonTerm.slice`.
For terminal symbols, the type of data is [`rusty_lr::TermData`](rusty_lr_core/src/termdata.rs).
You can access the shifted terminal symbol by `Term.value`(`&TermType`) or `*Term`(`&TermType`).

If multiple variables are defined with the same name, the variable on the front-most will be used.

For regex-like pattern, `<RuleType>` will be modified by following:
 | Pattern | Non-Terminal<br/>`<RuleType>=T` | Non-Terminal<br/>`<RuleType>=(not defined)` | Terminal |
 |:-------:|:--------------:|:--------------------------:|:--------:|
 | '*'     | `Vec<T>`       | (not defined)              | (not defined) |
 | '+'     | `Vec<T>`       | (not defined)              | (not defined) |
 | '?'     | `Option<T>`    | (not defined)              | (not defined) |

For example, following code will print the value of each `A`, and the slice of each `A` and `plus` token in the production rule `E -> A plus A`.
```rust
%token plus ...;

E : A plus? a2=A*
  {
    println!("Value of 1st A: {}", A.value); // i32
    println!("Slice of 1st A: {:?}", A.slice);
    println!("Value of 2nd As: {:?}", a2.value); // Vec<i32>
    println!("Slice of 2nd As: {:?}", a2.slice);

    if let plus.slice.len() == 0 {
        // plus is not captured
    }else {
        // plus is captured
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



## Parse with callback

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

#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)] // must implement these traits
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