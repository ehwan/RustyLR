//! Macro line parser for Rusty LR.
//!
//! This crate is private and not intended to be used directly.
//! Please use the [`rusty_lr`](https://crates.io/crates/rusty_lr) crate instead.

pub mod emit;
pub mod error;
pub mod grammar;
pub(crate) mod nonterminal_info;
pub(crate) mod parser;
pub mod partition;
pub(crate) mod pattern;
pub mod rangeresolver;
pub mod terminal_info;
pub(crate) mod terminalset;
pub(crate) mod token;
pub mod utils;

/// This, `rusty_lr_parser` is designed to generate a code, that will be relied on `rusty_lr`.
///
/// Gets the version of the rusty_lr_core crate that current crate is targeting.
/// If the version is not matched, there will be a compile-time error.
pub fn target_rusty_lr_version() -> (usize, usize, usize) {
    (3, 31, 0)
}
