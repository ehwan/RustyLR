# rusty_lr
[![crates.io](https://img.shields.io/crates/v/rusty_lr.svg)](https://crates.io/crates/rusty_lr)
[![docs.rs](https://docs.rs/rusty_lr/badge.svg)](https://docs.rs/rusty_lr)

***A Yacc-like, procedural macro-based parser generator for Rust supporting LR(1), LALR(1), and GLR parsing strategies.***

RustyLR enables you to define context-free grammars (CFGs) directly in Rust using macros or build scripts. It constructs deterministic finite automata (DFA) at compile time, ensuring efficient and reliable parsing.​

Please refer to [docs.rs](https://docs.rs/rusty_lr) for detailed example and documentation.

## Features
 - **Multiple Parsing Strategies:** Supports LR(1), LALR(1), and GLR parsers.
 - **Procedural Macros:** Define grammars using lr1! and lalr1! macros for compile-time parser generation.
 - **Build Script Integration:** Generate parsers via build scripts for complex grammars with detailed error messages.​
 - **Custom Reduce Actions:** Define custom actions during reductions to build ASTs or perform computations.​
 - **Grammar Conflict Detection:** Automatically detects shift/reduce and reduce/reduce conflicts during parser generation, providing informative diagnostics to help resolve ambiguities.

 ## Installation
 Add RustyLR to your `Cargo.toml`:
 ```toml
 [dependencies]
 rusty_lr = "..."
 ```
 To use buildscript tools:
 ```toml
 [build-dependencies]
 rusty_lr = { version = "...", features = ["build"] }
 ```
 Or you want to use executable version (optional):
 ```sh
 cargo install rustylr
 ```

 ## Quick Start
 ### Using Procedural Macros
 Define your grammar using the `lr1!` or `lalr1!` macro:
 ```rust
// this define `EParser` struct
// where `E` is the start symbol
lr1! {
    %userdata i32;           // userdata type passed to parser
    %tokentype char;         // token type; sequence of `tokentype` is fed to parser
    %start E;                // start symbol; this is the final value of parser
    %eof '\0';               // eof token; this token is used to finish parsing

    // ================= Token definitions =================
    %token zero '0';
    %token one '1';
    ...
    %token nine '9';
    %token plus '+';
    %token star '*';
    %token space ' ';

    %left [plus star];                  // reduce-first for token 'plus', 'star'

    // ================= Production rules =================
    Digit(char): [zero-nine];           // character set '0' to '9'

    Number(i32)                         // production rule `Number` holds `i32` value
        : space* Digit+ space*          // `Number` is one or more `Digit` surrounded by zero or more spaces
        { Digit.into_iter().collect::<String>().parse().unwrap() }; // this will be the value of `Number` (i32) by this production rule

    A(f32)
        : A plus a2=A {
            *data += 1;                                 // access userdata by `data`
            println!( "{:?} {:?} {:?}", A, plus, a2 );  // any Rust code can be written here
            A + a2                                      // this will be the value of `A` (f32) by this production rule
        }
        | M
        ;

    M(f32): M star m2=M { M * m2 }
        | Number { Number as f32 } // Number is `i32`, so cast to `f32`
        ;

    E(f32) : A ; // start symbol
}
```
This defines a simple arithmetic expression parser.
### Using Build Script
For complex grammars, you can use a build script to generate the parser. This will provide more detailed error messages when conflicts occur.
**1. Create a grammar file** (e.g., `src/parser.rs`) with the following content:
```rust

// Rust code of `use` and type definitions


%% // start of grammar definition

%tokentype u8;
%start E;
%eof b'\0';

%token a b'a';
%token lparen b'(';
%token rparen b')';

E: lparen E rparen
 | a;

 ...

```

**2. Setup `build.rs`**:
```rust
// build.rs
use rusty_lr::build;

fn main() {
    println!("cargo::rerun-if-changed=src/parser.rs");

    let output = format!("{}/parser.rs", std::env::var("OUT_DIR").unwrap());
    build::Builder::new()
        .file("src/parser.rs") // path to the input file
        .build(&output);       // path to the output file
}
```

**3. Include the generated source code:**
```rust
include!(concat!(env!("OUT_DIR"), "/parser.rs"));
```

## Examples
 - [Calculator](examples/calculator_u8/src/parser.rs): A calculator using `u8` as token type.
 - [lua 5.4 syntax parser](https://github.com/ehwan/lua_rust/blob/main/parser/src/parser.rs)
 - [Bootstrap](rusty_lr_parser/src/parser/parser.rs): rusty_lr syntax parser is written in rusty_lr itself.

## Cargo Features
 - `build`: Enable build script tools.
 - `fxhash`: Use FXHashMap instead of `std::collections::HashMap` for parser tables.
 - `tree`: Enable automatic syntax tree construction (For debugging purposes).
 - `error`: Enable detailed parsing error messages (For debugging purposes).


## Cargo Features
 - `build` : Enable buildscript tools.
 - `fxhash` : In parser table, replace `std::collections::HashMap` with `FxHashMap` from [`rustc-hash`](https://github.com/rust-lang/rustc-hash).
 - `tree` : Enable automatic syntax tree construction.
    This feature should be used on debug purpose only, since it will consume much more memory and time.
 - `error` : Enable detailed parsing error messages, for `Display` and `Debug` trait.
    This feature should be used on debug purpose only, since it will consume much more memory and time.

## Syntax
RustyLR's grammar syntax is inspired by traditional Yacc/Bison formats. Here's a quick overview:​
See [SYNTAX.md](SYNTAX.md) for details of grammar-definition syntax.

## Contribution
 - Any contribution is welcome.
 - Please feel free to open an issue or pull request.

## License (Since 2.8.0)
Either of
 - MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)
 - Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)

### Images
It is highly recommended to use buildscipt tools or executable instead of procedural macros, to generate readable error messages.
#### -Reduce/Reduce conflicts
![images/error1.png](images/error1.png)
#### - Shift/Reduce conflicts
![images/error2.png](images/error2.png)