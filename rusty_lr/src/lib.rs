//! # RustyLR
//! LR(1) and LALR(1) code generator in Rust
//!
//! For sample and examples, please refer to the [GitHub repository](https://github.com/ehwan/RustyLR)
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

// re-exports

pub use rusty_lr_core::*;
pub use rusty_lr_derive::*;