# rusty_lr
[![crates.io](https://img.shields.io/crates/v/rusty_lr.svg)](https://crates.io/crates/rusty_lr)
[![docs.rs](https://docs.rs/rusty_lr/badge.svg)](https://docs.rs/rusty_lr)

GLR, LR(1) and LALR(1) parser generator for Rust.

Please refer to [docs.rs](https://docs.rs/rusty_lr) for detailed example and documentation.

## Features
 - GLR, LR(1) and LALR(1) parser generator
 - Can handle every possible paths with GLR parser
 - Procedural macros and buildscript tools
 - Customizable reduce action
 - Readable error messages, both for parsing and building grammar

## Examples
#### Projects
 - [Simple Calculator](examples/calculator_u8/src/parser.rs)
 - [lua 5.4 syntax parser](https://github.com/ehwan/lua_rust/blob/main/parser/src/parser.rs)
 - [Bootstrap](rusty_lr_parser/src/parser/parser.rs): rusty_lr syntax parser is written in rusty_lr itself.

#### Quick Example
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
    //    ^^^^---------------------------- `Digit` holds `char` value

    Number(i32)                         // production rule `Number` holds `i32` value
        : space* Digit+ space*          // `Number` is one or more `Digit` surrounded by zero or more spaces
    //           ^^^^^-------------------- `Digit+` holds `Vec<char>`
        { Digit.into_iter().collect::<String>().parse().unwrap() };
    //   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    //    this will be the value of `Number` (i32) by this production rule

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
```rust
let parser = EParser::new();         // generate `EParser`, this holds the parser table
let mut context = EContext::new();   // create context, this holds current state of parser
let mut userdata: i32 = 0;           // define userdata

let input_sequence = "1 + 2 * 3 + 4"; // input sequence

// start feeding tokens
for token in input_sequence.chars() {
    match context.feed(&parser, token, &mut userdata) {
        //                              ^^^^^^^^^^^^ userdata passed here as `&mut i32`
        //                      ^^^^^--------------- feed token
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
            return;
        }
    }
}
context.feed(&parser, '\0', &mut userdata).unwrap();    // feed `eof` token

let res = context.accept();   // get the value of start symbol `E(f32)`
println!("{}", res);
println!("userdata: {}", userdata);
```

## Readable error messages (with [codespan](https://github.com/brendanzab/codespan))
#### -Reduce/Reduce conflicts
![images/error1.png](images/error1.png)
#### - Shift/Reduce conflicts
![images/error2.png](images/error2.png)

## Visualized syntax tree
![images/tree.png](images/tree.png)
 - With `tree` feature enabled.

## detailed `ParseError` message
![images/parse_error.png](images/parse_error.png)
 - With `error` feature enabled.

## Cargo Features
 - `build` : Enable buildscript tools.
 - `fxhash` : In parser table, replace `std::collections::HashMap` with `FxHashMap` from [`rustc-hash`](https://github.com/rust-lang/rustc-hash).
 - `tree` : Enable automatic syntax tree construction.
    This feature should be used on debug purpose only, since it will consume much more memory and time.
 - `error` : Enable detailed parsing error messages, for `Display` and `Debug` trait.
    This feature should be used on debug purpose only, since it will consume much more memory and time.

## Syntax
See [SYNTAX.md](SYNTAX.md) for details of grammar-definition syntax.

 - [Bootstrap](rusty_lr_parser/src/parser/parser.rs): rusty_lr syntax is written in rusty_lr itself.


## Contribution
 - Any contribution is welcome.
 - Please feel free to open an issue or pull request.

## License (Since 2.8.0)
Either of
 - MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)
 - Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)