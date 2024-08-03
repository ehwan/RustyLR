//! # RustyLR
//! LR(1) Parser generator in Rust
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
