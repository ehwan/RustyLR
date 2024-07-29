# RustyLR
LR(1) and LALR(1) parser code generator for Rust

```
[dependencies]
rusty_lr = "0.5.1"
rusty_lr_derive = "0.5.1"
```

## Features
 - pure Rust implementation
 - compile-time DFA construction from CFG ( with yacc-like syntax )
 - customizable reducing action
 - resolving conflicts of ambiguous grammar
 - tracing parser action with callback, also error handling
 - construct Tree from parsing result

## Sample
In [`example/calculator/parser.rs`](example/calculator/src/parser.rs),
```rust
// for LR(1) parser, with &str
use rusty_lr_derive::lr1_str;
// for LALR(1) parser, with &str
use rusty_lr_derive::lalr1_str;

// this define struct `EParser`
// where 'E' is the start symbol
lalr1_str! {
        // define type of user data
    %userdata i32;

    // start symbol ( for final reduction )
    %start E;

    // set augmented production rule
    %augmented Augmented;

    // v{N} is the value of the N-th symbol in the production rule
    // s{N} is the &str(or &[Term]) of the N-th symbol
    // (%left|%reduce|%right|%shift) to resolve shift/reduce conflict
    // reduce action must be evaluated into type (`i32` in this case) you provided
    A(i32): A r"+" A %left { println!("A: {:?}+{:?}={:?}", s0, s2, s); v0 + v2 }
          | M { v0 }
          ;
    M(i32): M '*' M %left { v0 * v2 }
          | P { v0 }
          ;
    P(i32): Num { v0 }
          | WS '(' E ')' WS { v2 }
          ;
    Num(i32): WS Num0 WS { *data += 1; s1.parse().unwrap() }; // user data can be accessed by `data`
    Num0: Digit Num0
       | Digit
       ;
    Digit : '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
          ;
    E(i32): A { v0 }
          ;

    WS1: " " WS1
      | b" "
      ;
    WS: WS1
      |
      ;
    Augmented : E '\0'
              ;
}
```

In [`example/calculator/src/main.rs`](example/calculator/src/main.rs),
```rust
mod parser;

fn main() {
    let p = parser::EParser::new();

    let input = " 1  + 2*(3   + 4)  ";
    let mut number_of_num: i32 = 0;
    // `number_of_num` passed to parser as user_data
    let res = match p.parse_str(input, 0 as char, &mut number_of_num) {
        Ok(res) => res,
        Err(e) => {
            println!("Error: {}", e);
            return;
        }
    };
    println!("Result: {}", res);
    println!("Number of 'Num' in {}: {}", input, number_of_num);
}
```

The result will be:
```
"3   "+" 4"="3   + 4"
" 1  "+" 2*(3   + 4)  "=" 1  + 2*(3   + 4)  "
Result: 15
Number of 'Num' in  1  + 2*(3   + 4)  : 4
```

## Build Deterministic Finite Automata (DFA) from Context Free Grammar (CFG)
This section will describe how to build DFA from CFG, on runtime.

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
Or simply, you can use `char` or `u8` as terminal, and `&'static str` as non-terminal.
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
    rusty_lr::ReduceType::Left, // reduce left
);
grammar.add_rule(
    NonTerm::A,
    vec![Token::NonTerm(NonTerm::M)],
    rusty_lr::ReduceType::Error,
);
```

Note that the production rule `A -> A + A` has a shift/reduce conflict, and the reduce type is set to `ReduceType::Left` to resolve the conflict. Letting the reduce type to `ReduceType::Error` will cause an error when a conflict occurs. This is useful when you want to know unexpected conflicts exist in your grammar.

reduce/reduce conflict (e.g. duplicated rules) will be always an error.

### 3. Build DFA
Calling `grammar.build()` will build the DFA from the CFG.

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
### 1. Parse without callback
For given input sequence, you can parse it with `Parser::parse()` method. The input sequence must be slice of `Term`(`&[Term]`).

If `Term` is `char`, you can use `&str` as input sequence (via `Parser::parse_str()`).

```rust
let terms = vec![ Term::Num, Term::Plus, Term::Num, Term::Mul, Term::LeftParen, Term::Num, Term::Plus, Term::Num, Term::RightParen];

match parser.parse(&terms, Term::Eof) {
    Ok(tree) => println!("Parse success {:?}", tree.slice(&terms)),
    // error is Display if Term, NonTerm is Display
    Err(err) => eprintln!("{:?}", err),
}
```

Note that the given input sequence **must not** ends with `EOF`(the end symbol you provided in Augmented rule). And the final Augmented rule will not be reduced (`EOF` will be only used for lookahead/reduce check, not shifted).

### 2. Parse with callback
For complex error handling and tracing parser action, you can implement `Callback` trait and pass it to `Parser::parse_with_callback(...)`.

```rust
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
```

```rust
let mut callback = ParserCallback{};
match parser.parse_with_callback(&terms, &mut callback, Term::Eof) {
    Ok(tree) => println!("Parse success {:?}", tree.slice(&terms)),
    Err(err) => eprintln!("{:?}", err),
}
```

The output will be:
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
Reduce by A -> A + A (Left)
Reduce by E -> A
Reduce by P -> ( E )
Reduce by M -> P
Reduce by M -> M * M (Left)
Reduce by A -> M
Reduce by A -> A + A (Left)
Reduce by E -> A
Parse success [Num, Plus, Num, Mul, LeftParen, Num, Plus, Num, RightParen]
```

### 3. `Tree` struct as parsing result
`Tree` struct is constructed from parsing result. You can access the reduced rule as tree structure.

```rust
// get slice of input sequence that this tree holds
let slc = tree.slice(&terms);

match tree {
    Tree::Terminal( terminal_index:usize ) => { println!("Terminal {:?}", terms[terminal_index]); }
    Tree::NonTerminal( rule_index:usize, reduced_tokens:Vec<Tree>, begin_index:usize ) => {
        let rule = &parser.rules[rule_index];
        ...
    }
}
```