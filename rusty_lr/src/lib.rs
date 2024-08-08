//! # RustyLR
//! LR(1) and LALR(1) code generator in Rust
//!
//! For sample and examples, please refer to the [GitHub repository](https://github.com/ehwan/RustyLR)
//!
//! ## Features
//! - pure Rust implementation
//! - readable error messages, both for grammar building and parsing
//! - compile-time DFA construction from CFGs ( with proc-macro )
//! - customizable reduce action
//! - resolving conflicts of ambiguous grammar
//! - tracing parser action with callback
//!
//! #### Why proc-macro, not external executable?
//! - Decent built-in lexer, with consideration of unicode and comments.
//! - Can generate *pretty* error messages, by just passing `Span` data.
//! - With modern IDE, auto-completion and error highlighting can be done in real-time.

// re-exports

pub use rusty_lr_core::*;
pub use rusty_lr_derive::*;
