//! # RustyLR
//! yacc-like LR(1) and LALR(1) Deterministic Finite Automata (DFA) generator from Context Free Grammar (CFGs).
//!
//! RustyLR provides both [executable](#executable-rustylr) and [procedural macros](#proc-macro) to generate LR(1) and LALR(1) parser.
//! The generated parser will be a pure Rust code, and the calculation of building DFA will be done at compile time.
//! Reduce action can be written in Rust code,
//! and the error messages are readable and detailed with [executable](#executable-rustylr).
//! For huge and complex grammars, it is recommended to use the [executable](#executable-rustylr) version.
//!
//! By default, RustyLR uses [`std::collections::HashMap`] for the parser tables.
//! If you want to use `FxHashMap` from [`rustc-hash`](https://github.com/rust-lang/rustc-hash), add `features=["fxhash"]` to your `Cargo.toml`.
//! ```toml
//! [dependencies]
//! rusty_lr = { version = "...", features = ["fxhash"] }
//! ```
//!
//! ## Features
//!  - pure Rust implementation
//!  - readable error messages, both for grammar building and parsing
//!  - compile-time DFA construction from CFGs
//!  - customizable reduce action
//!  - resolving conflicts of ambiguous grammar
//!  - regex patterns partially supported
//!  - executable for generating parser tables
//!
//! ## proc-macro
//! Below procedural macros are provided:
//!  - [`lr1!`] : LR(1) parser
//!  - [`lalr1!`] : LALR(1) parser
//!
//! These macros will generate structs:
//!  - `Parser` : contains DFA tables and production rules
//!  - [`ParseError`] : type alias for `Error` returned from `feed()`
//!  - `Context` : contains current state and data stack
//!  - `enum NonTerminals` : a list of non-terminal symbols
//!  - [`Rule`](`ProductionRule`) : type alias for production rules
//!  - [`State`] : type alias for DFA states
//!
//! All structs above are prefixed by `<StartSymbol>`.
//! In most cases, what you want is the `Parser` and `ParseError` structs, and the others are used internally.
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
//! ## Error Handling
//! There are two error variants returned from `feed()` function:
//!  - `InvalidTerminal(InvalidTerminalError)` : when invalid terminal symbol is fed
//!  - `ReduceAction(ReduceActionError)` : when the reduce action returns `Err(Error)`
//!
//! For `ReduceActionError`, the error type can be defined by [`%err`](#error-type-optional) directive. If not defined, `String` will be used.
//!
//! When printing the error message, there are two ways to get the error message:
//!  - `e.long_message( &parser, &context )` : get the error message as `String`, in a detailed format
//!  - `e as Display` : briefly print the short message through `Display` trait.
//!
//! The `long_message` function requires the reference to the parser and the context.
//! It will make a detailed error message of what current state was trying to parse, and what the expected terminal symbols were.
//! ### Example of long_message
//! ```text
//! Invalid Terminal: *. Expected one of:  , (, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
//! >>> In:
//! 	M -> M * • M
//! >>> Backtrace:
//! 	M -> M • * M
//! >>> Backtrace:
//! 	A -> A + • A
//! >>> Backtrace:
//! 	A -> A • + A
//! ```
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
//! `lr1!` macro will generate a parser struct with LR(1) DFA tables.
//! If you want to generate LALR(1) parser, use `lalr1!` macro.
//! Every line in the macro must follow the syntax below.
//!
//! Syntax can be found in [repository](https://github.com/ehwan/RustyLR/tree/main?tab=readme-ov-file#syntax).
//!
//!
//! ## executable `rustylr`
//! An executable version of `lr1!` and `lalr1!` macro.
//! Converts a context-free grammar into a deterministic finite automaton (DFA) tables,
//! and generates a Rust code that can be used as a parser for that grammar.
//!
//! ```
//! cargo install rustylr
//! ```
//!
//! This executable will provide much more detailed, pretty-printed error messages than the procedural macros.
//! If you are writing a huge, complex grammar, it is recommended to use this executable than the procedural macros.
//! `--verbose` option is useful for debugging the grammar.
//! It will print where the auto-generated rules are originated from and the resolving process of shift/reduce conflicts.
//! [like](https://github.com/ehwan/RustyLR/blob/main/images/example1.png) [this](https://github.com/ehwan/RustyLR/blob/main/images/example2.png)
//!
//! Although it is convenient to use the proc-macros for small grammars,
//! since modern IDEs feature (rust-analyzer's auto completion, inline error messages) could be enabled.
//!
//! This program searches for `%%` in the input file. ( Not the `lr1!`, `lalr1!` macro )
//!
//! The contents before `%%` will be copied into the output file as it is.
//! Context-free grammar must be followed by `%%`.
//! Each line must follow the syntax of [rusty_lr#syntax](#syntax)
//!
//! ```rust
//! // my_grammar.rs
//! use some_crate::some_module::SomeStruct;
//!
//! enum SomeTypeDef {
//!     A,
//!     B,
//!     C,
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
//! Calling the command will generate a Rust code `my_parser.rs`.
//! ```
//! $ rustylr my_grammar.rs my_parser.rs --verbose
//! ```
//!
//!
//! Possible options can be found by `--help`.
//! ```
//! $ rustylr --help
//! Usage: rustylr [OPTIONS] <INPUT_FILE> [OUTPUT_FILE]
//!
//! Arguments:
//!   <INPUT_FILE>
//!           input_file to read
//!
//!   [OUTPUT_FILE]
//!           output_file to write
//!
//!           [default: out.tab.rs]
//!
//! Options:
//!       --no-format
//!           do not rustfmt the output
//!
//!   -l, --lalr
//!           build LALR(1) parser
//!
//!   -v, --verbose
//!           print debug information.
//!     
//!           print the auto-generated rules, and where they are originated from.
//!           print the shift/reduce conflicts, and the resolving process.
//! ```

// re-exports

pub use rusty_lr_core::*;
pub use rusty_lr_derive::*;
