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

// re-exports
pub use rusty_lr_core::*;
pub use rusty_lr_derive::*;
