//! # RustyLR
//! GLR, LR(1) and LALR(1) parser generator for Rust.
//!
//! RustyLR provides [procedural macros](#proc-macro) and [buildscript tools](#integrating-with-buildrs) to generate GLR, LR(1) and LALR(1) parser.
//! The generated parser will be a pure Rust code, and the calculation of building DFA will be done at compile time.
//! Reduce action can be written in Rust,
//! and the error messages are **readable and detailed**.
//! For huge and complex grammars, it is recommended to use the [buildscipt](#integrating-with-buildrs).
//!
//! #### `features` in `Cargo.toml`
//!  - `build` : Enable buildscript tools.
//!  - `fxhash` : In parser table, replace `std::collections::HashMap` with `FxHashMap` from [`rustc-hash`](https://github.com/rust-lang/rustc-hash).
//!  - `tree` : Enable automatic Tree construction.
//!     This feature should be used on debug purpose only, since it will consume much more memory and time.
//!
//! ## Features
//!  - pure Rust implementation
//!  - readable error messages, both for grammar building and parsing
//!  - compile-time DFA construction from CFGs
//!  - customizable reduce action
//!  - resolving conflicts of ambiguous grammar
//!  - regex patterns partially supported
//!  - tools for integrating with `build.rs`
//!
//! ## proc-macro
//! Below procedural macros are provided:
//!  - [`lr1!`] : LR(1) parser
//!  - [`lalr1!`] : LALR(1) parser
//!
//! These macros will generate structs:
//!  - `Parser` : contains DFA tables and production rules
//!  - `ParseError` : type alias for `Error` returned from `feed()`
//!  - `Context` : contains current state and data stack
//!  - `enum NonTerminals` : a list of non-terminal symbols
//!  - [`Rule`](`ProductionRule`) : type alias for production rules
//!  - `State` : type alias for DFA states
//!
//! All structs above are prefixed by `<StartSymbol>`.
//! In most cases, what you want is the `Parser` and `ParseError` structs, and the others are used internally.
//!
//! ## Integrating with `build.rs`
//! This buildscripting tool will provide much more detailed, pretty-printed error messages than the procedural macros.
//! If you are writing a huge, complex grammar, it is recommended to use buildscript than the procedural macros.
//! Generated code will contain the same structs and functions as the procedural macros. In your actual source code, you can `include!` the generated file.
//!
//! The program searches for `%%` in the input file, not the `lr1!`, `lalr1!` macro.
//! The contents before `%%` will be copied into the output file as it is.
//! And the context-free grammar must be followed by `%%`.
//!
//! ```rust
//! // parser.rs
//! use some_crate::some_module::SomeStruct;
//!
//! enum SomeTypeDef {
//!    A,
//!    B,
//!    C,
//! }
//!
//! %% // <-- input file splitted here
//!
//! %tokentype u8;
//! %start E;
//! %eof b'\0';
//!
//! %token a b'a';
//! %token lparen b'(';
//! %token rparen b')';
//!
//! E: lparen E rparen
//!  | P
//!  ;
//!
//! P: a;
//! ```
//!
//! You must enable the feature `build` to use in the build script.
//! ```toml
//! [build-dependencies]
//! rusty_lr = { version = "...", features = ["build"] }
//! ```
//!
//! ```rust
//! // build.rs
//! use rusty_lr::build;
//!
//! fn main() {
//!     println!("cargo::rerun-if-changed=src/parser.rs");
//!
//!     let output = format!("{}/parser.rs", std::env::var("OUT_DIR").unwrap());
//!     build::Builder::new()
//!         .file("src/parser.rs") // path to the input file
//!     //  .lalr()                // to generate LALR(1) parser
//!         .build(&output);       // path to the output file
//! }
//! ```
//!
//! In your source code, include the generated file.
//! ```rust
//! include!(concat!(env!("OUT_DIR"), "/parser.rs"));
//! ```
//!
//! ## Start Parsing
//! The `Parser` struct has the following functions:
//!  - `new()` : create new parser
//!  - `begin(&self)` : create new context
//!  - `feed(&self, &mut Context, TerminalType, &mut UserData) -> Result<(), ParseError>` : feed token to the parser
//!
//! Note that the parameter `&mut UserData` is omitted if `%userdata` is not defined.
//! All you need to do is to call `new()` to generate the parser, and `begin()` to create a context.
//! Then, you can feed the input sequence one by one with `feed()` function.
//! Once the input sequence is feeded (including `eof` token), without errors,
//! you can get the value of start symbol by calling `context.accept()`.
//!
//! ```rust
//! let parser = Parser::new();
//! let context = parser.begin();
//! for token in input_sequence {
//!     match parser.feed(&context, token) {
//!         Ok(_) => {}
//!         Err(e) => { // e: ParseError
//!             println!("{}", e);
//!             return;
//!         }
//!     }
//! }
//! let start_symbol_value = context.accept();
//! ```
//!
//!
//! ## Syntax Tree
//! With the `tree` feature, `feed()` function will automatically construct the parse tree.
//! By calling `context.to_tree_list()`,
//! you can get current syntax tree. Simply print the tree list with `Display` or `Debug` will give you the pretty-printed tree.
//!
//! ```rust
//! let parser = Parser::new();
//! let mut context = parser.begin();
//! /// feed tokens...
//! println!( "{:?}", context.to_tree_list() ); // print tree list with `Debug` trait
//! println!( "{}", context.to_tree_list() );   // print tree list with `Display` trait
//! ```
//!
//! ```text
//! TreeList
//! ├─A
//! │ └─M
//! │   └─P
//! │     └─Number
//! │       ├─WS0
//! │       │ └─space*
//! │       │   └─space+
//! │       │     ├─space+
//! │       │     │ └─' '
//! │       │     └─' '
//! │       ├─Digit+
//! │       │ └─Digit
//! │       │   └─[zero-nine]
//! │       │     └─'1'
//! │       └─WS0
//! │         └─space*
//! │           └─space+
//! │             └─' '
//! ├─'+'
//! ├─M
//! │ └─P
//! │   └─Number
//! │     ├─WS0
//! │     │ └─space*
//! │     │   └─space+
//! │     │     ├─space+
//! │     │     │ └─' '
//! │     │     └─' '
//! ... continue
//! ```
//!
//! Note that default `Display` and `Debug` trait will print the whole tree recursively.
//! If you want to limit the depth of the printed tree, you can use [`Tree::pretty_print()`] or [`TreeList::pretty_print()`] function with `max_level` parameter.
//!
//!
//! ## GLR Parser
//! The GLR (Generalized LR parser) can be generated by `%glr;` directive in the grammar.
//! ```
//! // generate GLR parser;
//! // from now on, shift/reduce, reduce/reduce conflicts will not be treated as errors
//! %glr;
//! ...
//! ```
//! GLR parser can handle ambiguous grammars that LR(1) or LALR(1) parser cannot.
//! When it encounters any kind of conflict during parsing,
//! the parser will diverge into multiple states, and will try every paths until it fails.
//! Of course, there must be single unique path left at the end of parsing (the point where you feed `eof` token).
//!
//! ### Resolving Ambiguities
//! You can resolve the ambiguties through the reduce action.
//! Simply, returning `Result::Err(Error)` from the reduce action will revoke current path.
//! The `Error` variant type can be defined by `%err` directive.
//! And, setting predefined variable `shift: &mut bool` to `false` will revoke the shift action with lookahead token.
//!
//! Consider the following example:
//! ```text
//! E : E plus E
//!   | E star E
//!   | digit
//!   ;
//! ```
//! And you are trying to feed `1 + 2 * 3 + 4 eof` to the parser.
//! There are 5 ways to represent the input sequence:
//!  - `((1 + 2) * 3) + 4`
//!  - `(1 + (2 * 3)) + 4`
//!  - `1 + ((2 * 3) + 4)`
//!  - `1 + (2 * (3 + 4))`
//!  - `(1 + 2) * (3 + 4)`
//!
//! However, we know the 2nd path is the only correct one,
//! since the `star` has higher precedence than `plus`, and both are left-associative.
//!
//! To resolve the ambiguity, you can write the reduce action as follows:
//!
//! ```rust
//! E : E plus E {
//!       match *lookahead {
//!           '*' => {
//!               // no reduce if the next token is '*'
//!               // this prevent
//!               // E + E   /   *
//!               //             ^ lookahead
//!               // to be  E *  ...
//!               //        ^ (E + E)
//!               return Err("".to_string());
//!           }
//!           _ => {
//!               // revoke the shift action
//!               // this prevent
//!               // E + E   /  +
//!               //            ^ lookahead
//!               // to be E + E +  ...
//!               // and enforce the reduced token takes place
//!               // E + ...
//!               // ^ (E + E)
//!               *shift = false;
//!           }
//!
//!       }
//!   }
//!   | E star E {
//!       *shift = false;
//!   }
//!   | Number
//!   ;
//! ```
//!
//!
//! ### Note on GLR Parser
//!  - Still in development, not have been tested enough (patches are welcome!).
//!  - Since there are multiple paths, the reduce action can be called multiple times, even if the result will be thrown away in the future.
//!     - Every `RuleType` and `Term` must implement `Clone` trait.
//!     - `clone()` will be called carefully, only when there are multiple paths.
//!  - User must be aware of the point where shift/reduce or reduce/reduce conflicts occur.
//!  Every time the parser diverges, the calculation cost will increase.
//!
//!
//!
//! ## Syntax
//! To start writing down a context-free grammar, you need to define necessary directives first.
//! This is the syntax of the procedural macros.
//!
//! ```rust
//! lr1! {
//! // %directives
//! // %directives
//! // ...
//! // %directives
//!
//! // NonTerminalSymbol(RuleType): ProductionRules
//! // NonTerminalSymbol(RuleType): ProductionRules
//! // ...
//! }
//! ```
//!
//! [`lr1!`] macro will generate a parser struct with LR(1) DFA tables.
//! If you want to generate LALR(1) parser, use [`lalr1!`] macro.
//! Every line in the macro must follow the syntax below.
//!
//! Syntax can be found in [repository](https://github.com/ehwan/RustyLR/tree/main?tab=readme-ov-file#syntax).
//!
//!

// re-exports

pub use rusty_lr_core::*;
pub use rusty_lr_derive::*;

/// tools for build.rs
#[cfg(feature = "build")]
pub mod build {
    pub use rusty_lr_buildscript::*;
}
