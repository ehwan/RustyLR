//! # rusty_lr
//! ***A Yacc-like, procedural macro-based parser generator for Rust supporting LR(1), LALR(1), and GLR parsing strategies.***
//!
//! RustyLR enables you to define context-free grammars (CFGs) directly in Rust using macros or build scripts.
//! It constructs deterministic finite automata (DFA) at compile time, ensuring efficient and reliable parsing.​
//!
// re-exports

pub use rusty_lr_core::*;
pub use rusty_lr_derive::*;

/// tools for build.rs
#[cfg(feature = "build")]
pub mod build {
    pub use rusty_lr_buildscript::*;
}
