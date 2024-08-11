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
//! - regex patterns partially supported
//! - executable for generating parser tables from CFGs
//! - features=["fxhash"] to replace `std::collections::HashMap` with `FxHashMap`

// re-exports

pub use rusty_lr_core::*;
pub use rusty_lr_derive::*;
