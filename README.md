# rusty_lr
[![crates.io](https://img.shields.io/crates/v/rusty_lr.svg)](https://crates.io/crates/rusty_lr)
[![docs.rs](https://docs.rs/rusty_lr/badge.svg)](https://docs.rs/rusty_lr)

***A Bison-like, parser generator for Rust supporting minimal-LR(1), LALR(1), and GLR parsing strategies.***

RustyLR enables you to define context-free grammars directly in Rust.
Highly inspired by tools like *bison*, it uses a similar syntax while integrating seamlessly with Rust's ecosystem.
It constructs optimized state machine, ensuring efficient and reliable parsing.​

#### Number of terminal symbols reduced to 32 (from 0x10FFFF!) by optimization
![images/optimize.png](images/title.png)

## Features
 - **Custom Reduce Actions:** Define custom actions in Rust, allowing you to build into custom data structures easily.
 - **Automatic Optimization:**: Reduces parser table size and improves performance by grouping terminals with identical behavior across parser states.
 - **Multiple Parsing Strategies:** Supports minimal-LR(1), LALR(1) parser table, and GLR parsing strategy.
 - **Detailed Diagnostics:** Detect grammar conflicts, verbose conflicts resolving stages, and optimization stages.

 ## Installation & How to Use
 Add RustyLR to your `Cargo.toml`:
 ```toml
 [dependencies]
 rusty_lr = "..."
 ```
 To work with `rusty_lr`, you need to generate parser code using one of the following methods:
  - **Procedural macros:** Use the built-in `lr1!` macro
  - **Build script:** Enable the `build` feature and generate parser code during the build process
    ```toml
    [build-dependencies]
    rusty_lr = { version = "...", features = ["build"] }
    ```
  - **Executable:** Use the standalone `rustylr` executable to generate parser code
    ```sh
    cargo install rustylr
    ```
Recommend using the `rustylr` executable. It's faster and provides helpful grammar diagnostics.
Ensure the version of the generated code targets the same version of `rusty_lr` in your `Cargo.toml`. Otherwise you may encounter build errors.

 ### Using Procedural Macros
 Define your grammar using the `lr1!` macro:
 ```rust
// this define `EParser` struct
// where `E` is the start symbol
lr1! {
    %userdata i32;           // userdata type passed to parser
    %tokentype char;         // token type; sequence of `tokentype` is fed to parser
    %start E;                // start symbol; this is the final value of parser
    %eof '\0';               // eof token; this token is used to finish parsing

    // left reduction for '+' and '*'
    %left '+';
    %left '*';
    // operator precedence '*' > '+'

    // ================= Production rules =================
    Digit(char): ['0'-'9'];           // character set '0' to '9'

    Number(i32)                       // production rule `Number` holds `i32` value
        : ' '* Digit+ ' '*            // `Number` is one or more `Digit` surrounded by zero or more spaces
        { Digit.into_iter().collect::<String>().parse().unwrap() }; // this will be the value of `Number` (i32) by this production rule

    E(f32): E '*' e2=E { E * e2 }
          | E '+' e2=E {
              *data += 1;                       // access userdata by `data`
              println!( "{:?} {:?}", E, e2 );   // any Rust code can be written here
              E + e2                            // this will be the value of `E` (f32) by this production rule
          }
          | Number { Number as f32 } // Number is `i32`, so cast to `f32`
          ;
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

E: b'(' E b')' 
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

**4. Use the parser in your code:**
```rust
let mut parser = parser::EParser::new(); // create <StartSymbol>Parser class
let mut context = parser::EContext::new(); // create <StartSymbol>Context class
let mut userdata: i32 = 0;
for b in input.chars() {
    match context.feed(&parser, b, &mut userdata) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("error: {}", e);
            return;
        }
    }
}
println!("{:?}", context);
context.feed(&parser, 0 as char, &mut userdata).unwrap(); // feed EOF

let result:i32 = context.accept(); // get value of start 'E'
```

### Using `rustylr` Executable
```
cargo install rustylr

rustylr parser.rs output.rs
```
See [Executable](rusty_lr_executable/README.md) for more details.

The generated code will include several structs and enums:
 - `<Start>Parser`: A struct that holds the whole parser table. [(docs-LR)](https://docs.rs/rusty_lr/latest/rusty_lr/lr/trait.Parser.html) [(docs-GLR)](https://docs.rs/rusty_lr/latest/rusty_lr/glr/trait.Parser.html)
 - `<Start>Context`: A struct that maintains the current state and the values associated with each symbol. [(docs-LR)](https://docs.rs/rusty_lr/latest/rusty_lr/lr/struct.Context.html)  [(docs-GLR)](https://docs.rs/rusty_lr/latest/rusty_lr/glr/struct.Context.html)
 - `<Start>State`: A type representing a single parser state and its associated table. 
 - `<Start>Rule`: A type representing a single production rule. [(docs)](https://docs.rs/rusty_lr/latest/rusty_lr/struct.ProductionRule.html)
 - `<Start>NonTerminals`: A enum representing all non-terminal symbols in the grammar. [(docs)](https://docs.rs/rusty_lr/latest/rusty_lr/trait.NonTerminal.html)


You can get useful information from `<Start>NonTerminals` enum.
 ```rust
 let non_terminal: <Start>NonTerminals = ...;

 non_terminal.is_auto_generated(); // true if this non-terminal is auto-generated
 non_terminal.is_trace();          // if this non-terminal is marked with `%trace`
 ```

You can also get contextual information from `<Start>Context` struct.
```rust
let mut context = <Start>Context::new();

// ... parsing ...

context.expected();         // get expected terminal symbols
context.expected_nonterm(); // get expected non-terminal symbols
context.can_feed( term );   // check if a terminal symbol can be fed
context.trace();            // get all `%trace` non-terminals that are currently being parsed
println!("{}", context.backtrace()); // print backtrace of the parser state
println!("{}", context);    // print tree-structure of the parser state (`tree` feature)
```

The generated code will also include a `feed` method that takes a token and a mutable reference to the user data. This method will return an `Ok(())` if the token was successfully parsed, or an `Err` if there was an error.

```rust
context.feed( &parser, term, &mut userdata ); // feed a terminal symbol and update the state machine
```

Note that the actual definitions are bit different if you are building GLR parser.

## GLR Parsing
RustyLR offers built-in support for Generalized LR (GLR) parsing, enabling it to handle ambiguous or nondeterministic grammars that traditional LR(1) or LALR(1) parsers cannot process.
See [GLR.md](GLR.md) for details.

## Semantic Error Handling, and Resolving Conflicts
RustyLR provides a mechanism for handling semantic errors during parsing.
 - using `error` token for panic-mode-error-recovery
 - setting reduce type `%left`, `%right`, `%precedence` to terminals
 - setting priority `%dprec` to production rules
 - return `Err` from the reduce action

See [SYNTAX.md - Resolving Conflicts](SYNTAX.md#resolving-conflicts) for details.

## Examples
 - [Calculator](examples/calculator_u8/src/parser.rs): A calculator using `u8` as token type.
 - [Json Validator](examples/json/src/parser.rs): A JSON validator
 - [lua 5.4 syntax parser](https://github.com/ehwan/lua_rust/blob/main/parser/src/parser.rs)
 - [Bootstrap](rusty_lr_parser/src/parser/parser.rs): rusty_lr syntax parser is written in rusty_lr itself.

## Lexer Capabilities
While RustyLR is primarily a parser generator, it also functions effectively as a lexer.
Its design allows for efficient tokenization of input streams,
addressing challenges like the "too-many-characters" problem.
By constructing optimized state automata, it ensures rapid and memory-efficient lexing,
making it suitable for processing large or complex inputs.

## Cargo Features
 - `build`: Enable build script tools.
 - `tree`: Enable automatic syntax tree construction (For debugging purposes). Make `Context` tobe `Display`-able.
 - `error`: Enable detailed parsing error messages with backtrace when displaying feed error (For debugging purposes).

## Syntax
RustyLR's grammar syntax is inspired by traditional Yacc/Bison formats.
See [SYNTAX.md](SYNTAX.md) for details of grammar-definition syntax.

## Contribution
 - Any contribution is welcome.
 - Please feel free to open an issue or pull request.

## License (Since 2.8.0)
Either of
 - MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)
 - Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
