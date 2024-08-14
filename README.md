# RustyLR
[![crates.io](https://img.shields.io/crates/v/rusty_lr.svg)](https://crates.io/crates/rusty_lr)
[![docs.rs](https://docs.rs/rusty_lr/badge.svg)](https://docs.rs/rusty_lr)
for [proc-macro](#proc-macro)

[![crates.io](https://img.shields.io/crates/v/rustylr.svg)](https://crates.io/crates/rustylr)
for [executable](#executable-rustylr)

yacc-like LR(1) and LALR(1) Deterministic Finite Automata (DFA) generator from Context Free Grammar (CFGs).

RustyLR provides both [executable](#executable-rustylr) and [procedural macros](#proc-macro) to generate LR(1) and LALR(1) parser.
The generated parser will be a pure Rust code, and the calculation of building DFA will be done at compile time.
Reduce action can be written in Rust code,
and the error messages are [readable and detailed](#readable-error-messages-with-codespan) with [executable](#executable-rustylr).
For huge and complex grammars, it is recommended to use the [executable](#executable-rustylr) version.

By default, RustyLR uses `std::collections::HashMap` for the parser tables.
If you want to use `FxHashMap` from [`rustc-hash`](https://github.com/rust-lang/rustc-hash), add `features=["fxhash"]` to your `Cargo.toml`.
```toml
[dependencies]
rusty_lr = { version = "...", features = ["fxhash"] }
```

### Example
```rust
// this define `EParser` struct
// where `E` is the start symbol
lr1! {
    // userdata type
    %userdata i32;
    // token type
    %tokentype char;
    // start symbol
    %start E;
    // eof symbol
    %eof '\0';

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
    %left [plus star]; // reduce first for token 'plus', 'star'

    // context-free grammars
    Digit(char): [zero-nine]; // character set '0' to '9'

    Number(i32) // type assigned to production rule `Number`
        : space* Digit+ space* // regex pattern
    { Digit.into_iter().collect::<String>().parse().unwrap() }; 
    //    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ this will be the value of `Number`
    // reduce action written in Rust code

    A(f32): A plus a2=A {
        *data += 1; // access userdata by `data`
        println!( "{:?} {:?} {:?}", A, plus, a2 );
        A + a2
    }
        | M
        ;

    M(f32): M star m2=M { M * m2 }
        | P
        ;

    P(f32): Number { Number as f32 }
        | WS0 lparen E rparen WS0 { E }
        ;

    E(f32) : A ;
}
```
```rust
// generate `EParser`
let parser = EParser::new();
// create context
let mut context = parser.begin();
// define userdata
let mut userdata: i32 = 0;

let input_sequence = "1 + 2 * ( 3 + 4 )";

// start feeding tokens
for token in input_sequence.chars() {
    match parser.feed(&mut context, token, &mut userdata) {
        //                          ^^^^^   ^^^^^^^^^^^^ userdata passed here as `&mut i32`
        //                           |- feed token
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
// feed `eof` token
parser.feed(&mut context, '\0', &mut userdata).unwrap();

// res = value of start symbol
let res = context.accept();
println!("{}", res);
println!("userdata: {}", userdata);
```

### Readable error messages (with [codespan](https://github.com/brendanzab/codespan))
![images/error1.png](images/error1.png)
![images/error2.png](images/error2.png)

## Contents
 - [Proc-macro](#proc-macro)
 - [Start Parsing](#start-parsing)
 - [Error Handling](#error-handling)
 - [Syntax](#syntax)
 - [Executable `rustylr`](#executable-rustylr)

## Features
 - pure Rust implementation
 - readable error messages, both for grammar building and parsing
 - compile-time DFA construction from CFGs
 - customizable reduce action
 - resolving conflicts of ambiguous grammar
 - regex patterns partially supported
 - executable for generating parser tables

## proc-macro
Below procedural macros are provided:
 - `lr1!` : LR(1) parser
 - `lalr1!` : LALR(1) parser

These macros will generate structs:
 - `Parser` : contains DFA tables and production rules
 - `ParseError` : type alias for `Error` returned from `feed()`
 - `Context` : contains current state and data stack
 - `enum NonTerminals` : a list of non-terminal symbols
 - `Rule` : type alias for production rules
 - `State` : type alias for DFA states

All structs above are prefixed by `<StartSymbol>`.
In most cases, what you want is the `Parser` and `ParseError` structs, and the others are used internally.

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

## Error Handling
There are two error variants returned from `feed()` function:
 - `InvalidTerminal(InvalidTerminalError)` : when invalid terminal symbol is fed
 - `ReduceAction(ReduceActionError)` : when the reduce action returns `Err(Error)`

For `ReduceActionError`, the error type can be defined by [`%err`](#error-type-optional) directive. If not defined, `String` will be used.

When printing the error message, there are two ways to get the error message:
 - `e.long_message( &parser, &context )` : get the error message as `String`, in a detailed format
 - `e as Display` : briefly print the short message through `Display` trait.

The `long_message` function requires the reference to the parser and the context.
It will make a detailed error message of what current state was trying to parse, and what the expected terminal symbols were.
### Example of long_message
```
Invalid Terminal: *
Expected one of:  , (, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
-------------------------------Backtracing state--------------------------------
WS0 -> • _RustyLRGenerated0
_RustyLRGenerated1 -> •  
_RustyLRGenerated1 -> • _RustyLRGenerated1  
_RustyLRGenerated0 -> • _RustyLRGenerated1
_RustyLRGenerated0 ->  •
Number -> • WS0 _RustyLRGenerated3 WS0
M -> • M * M
M -> M * • M
M -> • P
P -> • Number
P -> • WS0 ( E ) WS0
-----------------------------------Prev state-----------------------------------
M -> M • * M
-----------------------------------Prev state-----------------------------------
A -> • A + A
A -> A + • A
A -> • M
M -> • M * M
-----------------------------------Prev state-----------------------------------
A -> A • + A
-----------------------------------Prev state-----------------------------------
A -> • A + A
E -> • A
Augmented -> • E 
```


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


### Token type <sub><sup>(must defined)</sup></sub>
```
'%tokentype' <RustType> ';'
```
Define the type of terminal symbols.
`<RustType>` must be accessible at the point where the macro is called.

<details>
<summary>
Example
</summary>

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

</details>


### Token definition <sub><sup>(must defined)</sup></sub>
```
'%token' <Ident> <RustExpr> ';'
```
Map terminal symbol's name `<Ident>` to the actual value `<RustExpr>`.
`<RustExpr>` must be accessible at the point where the macro is called.

<details>

<summary>
Example
</summary>


```rust
lr1! {
%tokentype u8;

%token zero b'0';
%token one b'1';

...

// 'zero' and 'one' will be replaced by b'0' and b'1' respectively
E: zero one;
}
```

</details>

### Start symbol <sub><sup>(must defined)</sup></sub>
```
'%start' <Ident> ';'
```
Define the start symbol of the grammar.

<details>
<summary>
Example
</summary>

```rust
lr1! {
%start E;
// this internally generate augmented rule <Augmented> -> E eof

E: ... ;
}
```

</details>

### Eof symbol <sub><sup>(must defined)</sup></sub>
```
'%eof' <RustExpr> ';'
```
Define the `eof` terminal symbol.
`<RustExpr>` must be accessible at the point where the macro is called.
'eof' terminal symbol will be automatically added to the grammar.

<details>
<summary>
Example
</summary>

```rust
lr1! {
%eof b'\0';
// you can access eof terminal symbol by 'eof' in the grammar
// without %token eof ...;
}
```

</details>

### Userdata type <sub><sup>(optional)</sup></sub>
```
'%userdata' <RustType> ';'
```
Define the type of userdata passed to `feed()` function.

<details>
<summary>
Example
</summary>

```rust
struct MyUserData { ... }

lr1! {
...
%userdata MyUserData;
}

...

fn main() {
    ...
    let mut userdata = MyUserData { ... };
    parser.feed( ..., token, &mut userdata); // <-- userdata feed here
}
```

</details>


### Reduce type <sub><sup>(optional)</sup></sub>
```
// reduce first
'%left' <Ident> ';'
'%left' <TerminalSet> ';'

// shift first
'%right' <Ident> ';'
'%right' <TerminalSet> ';'
```
Set the shift/reduce precedence for terminal symbols. `<Ident>` must be defined in `%token`.
With `<TerminalSet>`, you can define reduce type to multiple terminals at once. Please refer to the [Regex Pattern](#regex-pattern) section below.
`%left` can be abbreviated as `%reduce` or `%l`, and `%right` can be abbreviated as `%shift` or `%r`.

<details>
<summary>
Example
</summary>

```rust
lr1! {
// define tokens
%token plus '+';
%token hat '^';


// reduce first for token 'plus'
%left plus;

// shift first for token 'hat'
%right hat;
}
```

```rust
lr1! {
// define tokens
%token zero b'0';
%token one b'1';
...
%token nine b'9';

// shift first for tokens in range 'zero' to 'nine'
%shift [zero-nine];
}
```

</details>

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
               | <TerminalSet>
               | <TokenPattern> '*'    (zero or more)
               | <TokenPattern> '+'    (one or more)
               | <TokenPattern> '?'    (zero or one)
               ;
```

<details>
<summary>
Example
</summary>

This production rule defines non-terminal `E` to be `A`, then zero or more `plus`, then `D` mapped to variable `d`.
For more information, please refer to the [Accessing token data in ReduceAction](#accessing-token-data-in-reduceaction) section below.
```rust
lr1! {
E: A plus* d=D;
}
```

</details>

### Regex pattern
Regex patterns are partially supported. You can use `*`, `+`, `?` to define the number of repetitions, and `[]` to define the set of terminal symbols.

```
%token lparen '(';
%token rparen ')';
%token zero '0';
...
%token nine '9';

A: [zero-nine]+; // zero to nine

B: [^lparen rparen]; // any token except lparen and rparen

C: [lparen rparen one-nine]*; // lparen and rparen, and one to nine
```

Note that when using range pattern `[first-last]`,
the range is constructed by the order of the `%token` directives,
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


### RuleType <sub><sup>(optional)</sup></sub>
```
<RuleType> : '(' <RustType> ')'
           |
           ;
```
Define the type of value that this production rule holds.

<details>
<summary>
Example
</summary>

```rust
lr1! {
E(MyType<...>): ... Tokens ... ;
}
```

</details>

### ReduceAction <sub><sup>(optional)</sup></sub>
```
<ReduceAction> : '{' <RustExpr> '}'
               |
               ;
```

Define the action to be executed when the rule is matched and reduced.

- If `<RuleType>` is defined, `<ReduceAction>` itself must be the value of `<RuleType>` (i.e. no semicolon at the end of the statement).

- `<ReduceAction>` can be omitted if:
  - `<RuleType>` is not defined
  - Only one token is holding value in the production rule ( Non-terminal symbol with `<RuleType>` defined, or terminal symbols are considered as holding value )

- `Result<(),Error>` can be returned from `<ReduceAction>`.
  - Returned `Error` will be delivered to the caller of `feed()` function.
  - `ErrorType` can be defined by `%err` or `%error` directive. See [Error type](#error-type-optional) section.

<details>
<summary>
Example
</summary>

Omitting `ReduceAction`:
```rust
lr1! {
NoRuleType: ... ;

RuleTypeI32(i32): ... { 0 } ;

// RuleTypeI32 will be chosen
E(i32): NoRuleType NoRuleType RuleTypeI32 NoRuleType;
}
```

Returning `Result<(),String>` from ReduceAction:
```rust
lr1! {
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
}
```

</details>

### Accessing token data in ReduceAction

**predefined variables** can be used in `<ReduceAction>`:
 - `data` : userdata passed to `feed()` function.

To access the data of each token, you can directly use the name of the token as a variable.
For non-terminal symbols, the type of variable is `<RuleType>`.
For terminal symbols, the type of variable is `%tokentype`.

If multiple variables are defined with the same name, the variable on the front-most will be used.

For regex pattern, type of variable will be modified by following:
 | Pattern | Non-Terminal<br/>`<RuleType>=T` | Non-Terminal<br/>`<RuleType>=(not defined)` | Terminal<br/>TerminalSet |
 |:-------:|:--------------:|:--------------------------:|:--------:|
 | '*'     | `Vec<T>`       | (not defined)              | `Vec<TermType>` |
 | '+'     | `Vec<T>`       | (not defined)              | `Vec<TermType>` |
 | '?'     | `Option<T>`    | (not defined)              | `Option<TermType>` |

<details>
<summary>
Example
</summary>

```rust
lr1! {
%token plus ...;

// one or more 'A', then optional 'plus', then zero or more 'B'
E(f32) : A+ plus? b=B* minus_or_star=[minus star]
  {
    println!("Value of A: {:?}", A);         // Vec<i32>
    println!("Value of plus: {:?}", plus); // Option<TermType>
    println!("Value of b: {:?}", b);       // Vec<f32>
    println!("Value of minus_or_star: {:?}", minus_or_star); // must explicitly define the variable name

    let first_A = A[0];
    let first_B = b.first(); // Option<&f32>


    // this will be the new value of E
    if let Some(first_B) = first_B {
        let value = first_A as f32 + *first_B;
        value
    } else {
        first_a as f32
    }
  }
  ;

A(i32): ... ;
B(f32): ... ;
}
```

</details>



### Error type <sub><sup>(optional)</sup></sub>
```
'%err' <RustType> ';'
'%error' <RustType> ';'
```
Define the type of `Err` variant in `Result<(), Err>` returned from `<ReduceAction>`. If not defined, `String` will be used.

<details>
<summary>
Example
</summary>

```rust
enum MyErrorType<T> {
    ErrVar1,
    ErrVar2,
    ErrVar3(T),
    ...
}

lr1! {

%err MyErrorType<GenericType> ;

}

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

</details>

### Exclamation mark `!`
An exclamation mark `!` can be used right after the token to ignore the value of the token.
The token will be treated as if it is not holding any value.

**Tip**
When combining with repeatance pattern `*`, `+`, `?`, use `!` first.
It can prevent `Vec<T>` built from the value of the token internally.

<details>
<summary>
Example
</summary>

```rust
lr1! {
%token plus ...;

A(i32) : ... ;

// A in the middle will be chosen, since other A's are ignored
E(i32) : A! A A!;

B: A*!; // Vec<i32> will be built from the value of A, and then ignored

C: A!*; // A will be ignored first, and then repeatance pattern will be applied
}
```

</details>






## executable `rustylr`
An executable version of `lr1!` and `lalr1!` macro.
Converts a context-free grammar into a deterministic finite automaton (DFA) tables,
and generates a Rust code that can be used as a parser for that grammar.

```
cargo install rustylr
```

This executable will provide much more detailed, pretty-printed error messages than the procedural macros.
If you are writing a huge, complex grammar, it is recommended to use this executable than the procedural macros.
`--verbose` option is useful for debugging the grammar. It will print where the auto-generated rules are originated from and the resolving process of shift/reduce conflicts. [like](images/example1.png) [this](images/example2.png)

Although it is convenient to use the proc-macros for small grammars,
since modern IDEs feature (rust-analyzer's auto completion, inline error messages) could be enabled.

This program searches for `%%` in the input file. ( Not the `lr1!`, `lalr1!` macro )

The contents before `%%` will be copied into the output file as it is.
Context-free grammar must be followed by `%%`.
Each line must follow the syntax of [rusty_lr#syntax](#syntax)

```rust
// my_grammar.rs
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

Calling the command will generate a Rust code `my_parser.rs`.
```
$ rustylr my_grammar.rs my_parser.rs --verbose
```


Possible options can be found by `--help`.
```
$ rustylr --help
Usage: rustylr [OPTIONS] <INPUT_FILE> [OUTPUT_FILE]

Arguments:
  <INPUT_FILE>
          input_file to read

  [OUTPUT_FILE]
          output_file to write

          [default: out.tab.rs]

Options:
      --no-format
          do not rustfmt the output

  -l, --lalr
          build LALR(1) parser

  -v, --verbose
          print debug information.
    
          print the auto-generated rules, and where they are originated from.
          print the shift/reduce conflicts, and the resolving process.
```