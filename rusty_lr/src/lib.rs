//! # rusty_lr
//! ***A Bison-like, parser generator for Rust supporting IELR(1), LALR(1) parser tables, with deterministic LR and
//! non-deterministic LR (GLR) parsing strategies.***
//!
//! RustyLR is a parser generator that converts context-free grammars into IELR(1)/LALR(1) tables with deterministic LR and non-deterministic GLR parsing strategies. It supports custom reduce action in Rust, with beautiful diagnostics.
//! Highly inspired by tools like *bison*, it uses a similar syntax while integrating seamlessly with Rust's ecosystem.
//! It constructs optimized state machine, ensuring efficient and reliable parsing.
//!
//! ## Features
//!  - **Custom Reduce Actions:** Define custom actions in Rust, allowing you to build into custom data structures easily.
//!  - **Automatic Optimization:**: Reduces parser table size and improves performance by grouping terminals with identical behavior across parser states.
//!  - **Multiple Parsing Strategies:** Supports minimal-LR(1), LALR(1) parser table, and GLR parsing strategy.
//!  - **Detailed Diagnostics:** Detect grammar conflicts, verbose conflicts resolving stages, and optimization stages.
//!  - **Location Tracking:** Track the location of every tokens in the parse tree, useful for error reporting and debugging.
//!
// re-exports

pub use rusty_lr_core::*;
pub use rusty_lr_derive::*;

/// tools for build.rs
#[cfg(feature = "build")]
pub mod build {
    pub use rusty_lr_buildscript::*;
}
