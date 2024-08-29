# RustyLR
[![crates.io](https://img.shields.io/crates/v/rusty_lr.svg)](https://crates.io/crates/rusty_lr)
[![docs.rs](https://docs.rs/rusty_lr/badge.svg)](https://docs.rs/rusty_lr)

GLR, LR(1) and LALR(1) parser generator for Rust.

RustyLR provides [procedural macros](#proc-macro) and [buildscript tools](#integrating-with-buildrs) to generate GLR, LR(1) and LALR(1) parser.
The generated parser will be a pure Rust code, and the calculation of building DFA will be done at compile time.
Reduce action can be written in Rust,
and the error messages are [readable and detailed](#readable-error-messages-with-codespan).
For huge and complex grammars, it is recommended to use the [buildscipt](#integrating-with-buildrs).

#### `features` in `Cargo.toml`
 - `build` : Enable buildscript tools.
 - `fxhash` : In parser table, replace `std::collections::HashMap` with `FxHashMap` from [`rustc-hash`](https://github.com/rust-lang/rustc-hash).
 - `tree` : Enable automatic Tree construction.
    This feature should be used on debug purpose only, since it will consume much more memory and time.

### Example
```rust
// this define `EParser` struct
// where `E` is the start symbol
lr1! {
    %userdata i32;           // userdata type
    %tokentype char;         // token type
    %start E;                // start symbol
    %eof '\0';               // eof token

    // token definition
    %token zero '0';
    %token one '1';
    %token two '2';
    %token three '3';
    %token four '4';
    %token five '5';
    %token six '6';
    %token seven '7';
    %token eight '8';
    %token nine '9';
    %token plus '+';
    %token star '*';
    %token lparen '(';
    %token rparen ')';
    %token space ' ';

    // conflict resolving
    %left [plus star];                  // reduce first for token 'plus', 'star'

    // context-free grammars
    Digit(char): [zero-nine];           // character set '0' to '9'

    Number(i32)                         // type assigned to production rule `Number`
        : space* Digit+ space*          // regex pattern
    { Digit.into_iter().collect::<String>().parse().unwrap() }; 
    //    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ this will be the value of `Number`
                                        // reduce action written in Rust code

    A(f32): A plus a2=A {
        *data += 1;                     // access userdata by `data`
        println!( "{:?} {:?} {:?}", A, plus, a2 );
        A + a2
    }
        | M
        ;

    M(f32): M star m2=M { M * m2 }
        | P
        ;

    P(f32): Number { Number as f32 }
        | space* lparen E rparen space* { E }
        ;

    E(f32) : A ;
}
```
```rust
let parser = EParser::new();         // generate `EParser`
let mut context = parser.begin();    // create context
let mut userdata: i32 = 0;           // define userdata

let input_sequence = "1 + 2 * ( 3 + 4 )";

// start feeding tokens
for token in input_sequence.chars() {
    match parser.feed(&mut context, token, &mut userdata) {
        //                          ^^^^^   ^^^^^^^^^^^^ userdata passed here as `&mut i32`
        //                          feed token
        Ok(_) => {}
        Err(e) => {
            match e {
                EParseError::InvalidTerminal(invalid_terminal) => {
                    ...
                }
                EParseError::ReduceAction(error_from_reduce_action) => {
                    ...
                }
            }
            println!("{}", e);
            // println!( "{}", e.long_message( &parser, &context ) );
            return;
        }
    }
}
parser.feed(&mut context, '\0', &mut userdata).unwrap();    // feed `eof` token

let res = context.accept();   // get the value of start symbol
println!("{}", res);
println!("userdata: {}", userdata);
```

### Readable error messages (with [codespan](https://github.com/brendanzab/codespan))
![images/error1.png](images/error1.png)
![images/error2.png](images/error2.png)
 - This error message is generated by the buildscript tool, not the procedural macros.

## Features
 - pure Rust implementation
 - readable error messages, both for grammar building and parsing
 - compile-time DFA construction from CFGs
 - customizable reduce action
 - resolving conflicts of ambiguous grammar
 - regex patterns partially supported
 - tools for integrating with `build.rs`

## Contents
 - [Proc-macro](#proc-macro)
 - [Integrating with `build.rs`](#integrating-with-buildrs)
 - [Start Parsing](#start-parsing)
 - [Syntax Tree](#syntax-tree)
 - [GLR Parser](#glr-parser)
 - [Syntax](#syntax)


## proc-macro
Below procedural macros are provided:
 - `lr1!` : generate LR(1) parser
 - `lalr1!` : generate LALR(1) parser

These macros will generate structs:
 - `Parser` : contains DFA tables and production rules
 - `ParseError` : type alias for `Error` returned from `feed()`
 - `Context` : contains current state and data stack
 - `enum NonTerminals` : a list of non-terminal symbols
 - `Rule` : type alias for production rules
 - `State` : type alias for DFA states

All structs above are prefixed by `<StartSymbol>`.
In most cases, what you want is the `Parser` and `ParseError` structs, and the others are used internally.

## Integrating with `build.rs`
This buildscripting tool will provide much more detailed, pretty-printed error messages than the procedural macros.
If you are writing a huge, complex grammar, it is recommended to use buildscript than the procedural macros.
Generated code will contain the same structs and functions as the procedural macros. In your actual source code, you can `include!` the generated file.

Unlike the procedural macros, the program searches for `%%` in the input file, not the `lr1!`, `lalr1!` macro.
The contents before `%%` will be copied into the output file as it is.
And the context-free grammar must be followed by `%%`.

```rust
// parser.rs
use some_crate::some_module::SomeStruct;

enum SomeTypeDef {
    A,
    B,
    C,
}

%% // <-- input file splitted here

%tokentype u8;
%start E;
%eof b'\0';

%token a b'a';
%token lparen b'(';
%token rparen b')';

E: lparen E rparen
 | P
 ;

P: a;
```

You must enable the feature `build` to use in the build script.
```toml
[build-dependencies]
rusty_lr = { version = "...", features = ["build"] }
```

```rust
// build.rs
use rusty_lr::build;

fn main() {
    println!("cargo::rerun-if-changed=src/parser.rs");

    let output = format!("{}/parser.rs", std::env::var("OUT_DIR").unwrap());
    build::Builder::new()
        .file("src/parser.rs") // path to the input file
    //  .lalr()                // to generate LALR(1) parser
        .build(&output);       // path to the output file
}
```

In your source code, include the generated file.
```rust
include!(concat!(env!("OUT_DIR"), "/parser.rs"));
```

## Start Parsing
The `Parser` struct has the following functions:
 - `new()` : create new parser
 - `begin(&self)` : create new context
 - `feed(&self, &mut Context, TerminalType, &mut UserData) -> Result<(), ParseError>` : feed token to the parser

Note that the parameter `&mut UserData` is omitted if [`%userdata`](#userdata-type-optional) is not defined.
All you need to do is to call `new()` to generate the parser, and `begin()` to create a context.
Then, you can feed the input sequence one by one with `feed()` function.
Once the input sequence is feeded (including `eof` token), without errors,
you can get the value of start symbol by calling `context.accept()`.

```rust
let parser = Parser::new();
let context = parser.begin();
for token in input_sequence {
    match parser.feed(&context, token) {
        Ok(_) => {}
        Err(e) => { // e: ParseError
            println!("{}", e);
            return;
        }
    }
}
let start_symbol_value = context.accept();
```

## Syntax Tree
With the `tree` feature, `feed()` function will automatically construct the parse tree.
By calling `context.to_tree_list()`,
you can get current syntax tree. Simply print the tree list with `Display` or `Debug` will give you the pretty-printed tree.

```rust
let parser = Parser::new();
let mut context = parser.begin();
/// feed tokens...
println!( "{:?}", context.to_tree_list() ); // print tree list with `Debug` trait
println!( "{}", context.to_tree_list() );   // print tree list with `Display` trait
```

```
TreeList
├─A
│ └─M
│   └─P
│     └─Number
│       ├─WS0
│       │ └─_space_Star1
│       │   └─_space_Plus0
│       │     ├─_space_Plus0
│       │     │ └─' '
│       │     └─' '
│       ├─_Digit_Plus3
│       │ └─Digit
│       │   └─_TerminalSet2
│       │     └─'1'
│       └─WS0
│         └─_space_Star1
│           └─_space_Plus0
│             └─' '
├─'+'
├─M
│ └─P
│   └─Number
│     ├─WS0
│     │ └─_space_Star1
│     │   └─_space_Plus0
│     │     ├─_space_Plus0
│     │     │ └─' '
│     │     └─' '
│     ├─_Digit_Plus3
│     │ ├─_Digit_Plus3
│     │ │ └─Digit
│     │ │   └─_TerminalSet2
│     │ │     └─'2'
│     │ └─Digit
│     │   └─_TerminalSet2
│     │     └─'0'
│     └─WS0
│       └─_space_Star1
│         └─_space_Plus0
│           └─' '
├─'*'
├─WS0
│ └─_space_Star1
│   └─_space_Plus0
│     ├─_space_Plus0
│     │ ├─_space_Plus0
│     │ │ └─' '
│     │ └─' '
│     └─' '
├─'('
├─E
│ └─A
│   ├─A
│   │ └─M
│   │   └─P
│   │     └─Number
│   │       ├─WS0
│   │       │ └─_space_Star1
│   │       ├─_Digit_Plus3
│   │       │ └─Digit
│   │       │   └─_TerminalSet2
│   │       │     └─'3'
│   │       └─WS0
│   │         └─_space_Star1
│   │           └─_space_Plus0
│   │             └─' '
│   ├─'+'
│   └─A
│     └─M
│       └─P
│         └─Number
│           ├─WS0
│           │ └─_space_Star1
│           │   └─_space_Plus0
│           │     └─' '
│           ├─_Digit_Plus3
│           │ └─Digit
│           │   └─_TerminalSet2
│           │     └─'4'
│           └─WS0
│             └─_space_Star1
│               └─_space_Plus0
│                 └─' '
├─')'
├─_space_Plus0
│ ├─_space_Plus0
│ │ └─' '
│ └─' '
└─' '
```

## GLR Parser
The GLR (Generalized LR parser) can be generated by `%glr;` directive in the grammar.
```
// generate GLR parser;
// from now on, shift/reduce, reduce/reduce conflicts will not be treated as errors
%glr; 
...
```
GLR parser can handle ambiguous grammars that LR(1) or LALR(1) parser cannot.
When it encounters any kind of conflict during parsing,
the parser will diverge into multiple states, and will try every paths until it fails.
Of course, there must be single unique path left at the end of parsing (the point where you feed `eof` token).

### Resolving Ambiguities
You can resolve the ambiguties through the [reduce action](#reduceaction-optional).
Simply, returning `Result::Err(Error)` from the reduce action will revoke current path.
The `Error` variant type can be defined by [`%err`](#error-type-optional) directive.


### Note on GLR Parser
 - Still in development, not have been tested enough (patches are welcome!).
 - Since there are multiple paths, the reduce action can be called multiple times, even if the result will be thrown away in the future.
    - Every `RuleType` and `Term` must implement `Clone` trait.
    - `clone()` will be called carefully, only when there are multiple paths.
 - User must be aware of the point where shift/reduce or reduce/reduce conflicts occur.
 Every time the parser diverges, the calculation cost will increase.

## Syntax
To start writing down a context-free grammar, you need to define necessary directives first.
This is the syntax of the procedural macros.

```rust
lr1! {
// %directives
// %directives
// ...
// %directives

// NonTerminalSymbol(RuleType): ProductionRules
// NonTerminalSymbol(RuleType): ProductionRules
// ...
}
```

`lr1!` macro will generate a parser struct with LR(1) DFA tables.
If you want to generate LALR(1) parser, use `lalr1!` macro.
Every line in the macro must follow the syntax below.

[Bootstrap](rusty_lr_parser/src/parser/parser.rs), [Expanded Bootstrap](rusty_lr_parser/src/parser/parser_expanded.rs) would be a good example to understand the syntax and generated code. It is RustyLR syntax parser written in RustyLR itself.


### Quick Reference
 - [Production rules](#production-rules)
 - [Regex pattern](#regex-pattern)
 - [RuleType](#ruletype-optional)
 - [ReduceAction](#reduceaction-optional)
 - [Accessing token data in ReduceAction](#accessing-token-data-in-reduceaction)
 - [Exclamation mark `!`](#exclamation-mark-)
 - [`%tokentype`](#token-type-must-defined)
 - [`%token`](#token-definition-must-defined)
 - [`%start`](#start-symbol-must-defined)
 - [`%eof`](#eof-symbol-must-defined)
 - [`%userdata`](#userdata-type-optional)
 - [`%left`, `%right`](#reduce-type-optional)
 - [`%err`, `%error`](#error-type-optional)
 - [`%glr`](#glr-parser-generation)


---


### Production rules
Every production rules have the base form:
```
NonTerminalName
    : Pattern1 Pattern2 ... PatternN { ReduceAction }
    | Pattern1 Pattern2 ... PatternN { ReduceAction }
   ...
    ;
```

Each `Pattern` follows the syntax:
 - `name` : Non-terminal or terminal symbol `name` defined in the grammar.
 - `[term1 term_start-term_last]`, `[^term1 term_start-term_last]` : Set of terminal symbols. [`eof`](#eof-symbol-must-defined) will be automatically removed from the terminal set.
 - `P*` : Zero or more repetition of `P`.
 - `P+` : One or more repetition of `P`.
 - `P?` : Zero or one repetition of `P`.
 - `(P1 P2 P3)` : Grouping of patterns.
 - `P / term`, `P / [term1 term_start-term_last]`, `P / [^term1 term_start-term_last]` :
 Lookaheads; `P` followed by one of given terminal set. Lookaheads are not consumed.

#### Notes
When using range pattern `[first-last]`,
the range is constructed by the order of the [`%token`](#token-definition-must-defined) directives,
not by the actual value of the token.
If you define tokens in the following order:
```
%token one '1';
%token two '2';
...
%token zero '0';
%token nine '9';
```
The range `[zero-nine]` will be `['0', '9']`, not `['0'-'9']`.

---


### RuleType <sub><sup>(optional)</sup></sub>
You can assign a value for each non-terminal symbol.
In [reduce action](#reduceaction-optional),
you can access the value of each pattern holds,
and can assign new value to current non-terminal symbol.
Please refer to the [ReduceAction](#reduceaction-optional) and [Accessing token data in ReduceAction](#accessing-token-data-in-reduceaction) section below.
At the end of parsing, the value of the start symbol will be the result of the parsing.
By default, terminal symbols hold the value of [`%tokentype`](#token-type-must-defined) passed by `feed()` function.

```rust
struct MyType<T> {
    ...
}
```
```
E(MyType<i32>) : ... Patterns ... { <This will be new value of E> } ;
```

---


### ReduceAction <sub><sup>(optional)</sup></sub>
Reduce action can be written in Rust code. It is executed when the rule is matched and reduced.

- If [`RuleType`](#ruletype-optional) is defined for current non-terminal symbol, `ReduceAction` itself must be the value of [`RuleType`](#ruletype-optional) (i.e. no semicolon at the end of the statement).

- `ReduceAction` can be omitted if:
  - [`RuleType`](#ruletype-optional) is not defined.
  - Only one token is holding value in the production rule.

- `Result<(),Error>` can be returned from `ReduceAction`.
  - Returned `Error` will be delivered to the caller of `feed()` function.
  - `ErrorType` can be defined by `%err` or `%error` directive. See [Error type](#error-type-optional) section.


```rust
NoRuleType: ... ;

RuleTypeI32(i32): ... { 0 } ;

// RuleTypeI32 will be chosen
E(i32): NoRuleType NoRuleType RuleTypeI32 NoRuleType;
```

```rust
// set Err variant type to String
%err String;

%token div '/';

E(i32): A div a2=A {
    if a2 == 0 {
        return Err("Division by zero".to_string());
    }

    A / a2
};

A(i32): ... ;
```

---

### Accessing token data in ReduceAction

**predefined variables** can be used in `ReduceAction`:
 - `data` ( `&mut UserData` ) : userdata passed to the `feed()` function.
 - `lookahead` ( `&Term` ) : lookahead token that caused the reduce action.

To access the data of each token, you can directly use the name of the token as a variable.
 - For non-terminal symbols, the type of variable is `RuleType`.
 - For terminal symbols, the type of variable is [`%tokentype`](#token-type-must-defined).
 - If multiple variables are defined with the same name, the variable on the front-most will be used.
 - You can remap the variable name by using `=` operator.

```rust
E(i32) : A plus a2=A {
    println!("Value of A: {:?}", A);
    println!("Value of plus: {:?}", plus);
    println!("Value of a2: {:?}", a2);

    A + a2 // new value of E
};
```

For some regex pattern, the type of variable will be modified as follows:
 - `P*` : `Vec<P>`
 - `P+` : `Vec<P>`
 - `P?` : `Option<P>`

You can still access the `Vec` or `Option` by using the base name of the pattern.
```rust
E(i32) : A* {
    println!( "Value of A: {:?}", A ); // Vec<A>
};
```

For terminal set `[term1 term_start-term_end]`, `[^term1 term_start-term_end]`, there is no predefined variable name. You must explicitly define the variable name.
```rust
E: digit=[zero-nine] {
    println!( "Value of digit: {:?}", digit ); // %tokentype
};
```

For group `(P1 P2 P3)`:
 - If none of the patterns hold value, the group itself will not hold any value.
 - If only one of the patterns holds value, the group will hold the value of the very pattern. And the variable name will be same as the pattern.
 (i.e. If `P1` holds value, and others don't, then `(P1 P2 P3)` will hold the value of `P1`, and can be accessed via name `P1`)
 - If there are multiple patterns holding value, the group will hold `Tuple` of the values. There is no default variable name for the group, you must define the variable name explicitly by `=` operator.

 ```rust
 NoRuleType: ... ;

 I(i32): ... ;

 // I will be chosen
 A: (NoRuleType I NoRuleType) {
     println!( "Value of I: {:?}", I ); // can access by 'I'
     I
 };

 // ( i32, i32 )
 B: i2=( I NoRuleType I ) {
     println!( "Value of I: {:?}", i2 ); // must explicitly define the variable name
 };

 ```

---


### Exclamation mark `!`
An exclamation mark `!` can be used right after the token to ignore the value of the token.
The token will be treated as if it is not holding any value.

```rust
A(i32) : ... ;

// A in the middle will be chosen, since other A's are ignored
E(i32) : A! A A!;
```

---


### Token type <sub><sup>(must defined)</sup></sub>
```
%tokentype <RustType> ;
```
Define the type of terminal symbols.
`<RustType>` must be accessible at the point where the macro is called.

```rust
enum MyTokenType<Generic> {
    Digit,
    Ident,
    ...
    VariantWithGeneric<Generic>
}

lr! {
...
%tokentype MyTokenType<i32>;
}
```

---

### Token definition <sub><sup>(must defined)</sup></sub>
```
%token name <RustExpr> ;
```
Map terminal symbol `name` to the actual value `<RustExpr>`.
`<RustExpr>` must be accessible at the point where the macro is called.

```rust
%tokentype u8;

%token zero b'0';
%token one b'1';

...

// 'zero' and 'one' will be replaced by b'0' and b'1' respectively
E: zero one;
```

---

### Start symbol <sub><sup>(must defined)</sup></sub>
```
%start NonTerminalName ;
```
Set the start symbol of the grammar as `NonTerminalName`.

```rust
%start E;
// this internally generate augmented rule <Augmented> -> E eof

E: ... ;
```

---


### Eof symbol <sub><sup>(must defined)</sup></sub>
```
%eof <RustExpr> ;
```
Define the `eof` terminal symbol.
`<RustExpr>` must be accessible at the point where the macro is called.
'eof' terminal symbol will be automatically added to the grammar.


```rust
%eof b'\0';
// you can access eof terminal symbol by 'eof' in the grammar
// without %token eof ...;
```

---

### Userdata type <sub><sup>(optional)</sup></sub>
```
%userdata <RustType> ;
```
Define the type of userdata passed to `feed()` function.


```rust
struct MyUserData { ... }

...

%userdata MyUserData;

...

fn main() {
    ...
    let mut userdata = MyUserData { ... };
    parser.feed( ..., token, &mut userdata); // <-- userdata feed here
}
```

---


### Reduce type <sub><sup>(optional)</sup></sub>
```
// reduce first
%left term1 ;
%left [term1 term_start-term_last] ;

// shift first
%right term1 ;
%right [term1 term_start-term_last] ;
```
Set the shift/reduce precedence for terminal symbols.
`%left` can be abbreviated as `%reduce` or `%l`, and `%right` can be abbreviated as `%shift` or `%r`.

```rust
// define tokens
%token plus '+';
%token hat '^';


// reduce first for token 'plus'
%left plus;

// shift first for token 'hat'
%right hat;
```

---


### Error type <sub><sup>(optional)</sup></sub>
```
%err <RustType> ;
%error <RustType> ;
```
Define the type of `Err` variant in `Result<(), Err>` returned from [`ReduceAction`](#reduceaction-optional). If not defined, `DefaultReduceActionError` will be used.


```rust
enum MyErrorType<T> {
    ErrVar1,
    ErrVar2,
    ErrVar3(T),
}

...


%err MyErrorType<GenericType> ;

...

match parser.feed( ... ) {
    Ok(_) => {}
    Err(err) => {
        match err {
            ParseError::ReduceAction( err ) => {
                // do something with err
            }
            _ => {}
        }
    }
}
```


---

### GLR parser generation
```
%glr;
```
Swith to GLR parser generation.

If you want to generate GLR parser, add `%glr;` directive in the grammar.
With this directive, any Shift/Reduce, Reduce/Reduce conflicts will not be treated as errors.

See [GLR Parser](#glr-parser) section for more details.
