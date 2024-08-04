//! # RustyLR
//! LR(1) and LALR(1) code generator in Rust
//!
//! For more information, please refer to the [GitHub repository](https://github.com/ehwan/RustyLR)
//!
//! ## Features
//!  - pure Rust implementation
//!  - compile-time DFA construction from CFGs ( with proc-macro )
//!  - customizable reducing action
//!  - resolving conflicts of ambiguous grammar
//!  - tracing parser action with callback, also error handling
//!  - readable error messages, both for grammar building and parsing
//!
//! #### Why proc-macro, not external executable?
//!  - Decent built-in lexer, with consideration of unicode and comments.
//!  - Can generate *pretty* error messages, by just passing `Span` data.
//!  - With modern IDE, can see errors in real-time with specific location.

pub(crate) mod nontermdata;
pub(crate) mod termdata;

// re-exports
pub use rusty_lr_core::*;
pub use rusty_lr_derive::*;

/// type for NonTerminal data in reduce action
pub use nontermdata::NonTermData;
/// type for Terminal data in reduce action
pub use termdata::TermData;
