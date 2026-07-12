//! Macro line parser for Rusty LR.
//!
//! This crate is private and not intended to be used directly.
//! Please use the [`rusty_lr`](https://crates.io/crates/rusty_lr) crate instead.

pub mod error;
pub mod grammar;
pub(crate) mod nonterminal_info;
pub(crate) mod parser;
pub mod partition;
pub(crate) mod pattern;
pub mod rangeresolver;
pub(crate) mod symbol;
pub mod terminal_info;
pub(crate) mod terminalset;
pub mod utils;

pub use parser::args::{
    GrammarArgs, IdentOrLiteral, PatternArgs, PrecDPrecArgs, RuleDefArgs, RuleLineArgs, TableLayout,
};
pub use parser::location::{Located, Location};
pub use terminalset::{TerminalSet, TerminalSetItem};

fn parse_package_version(version: &str) -> (usize, usize, usize) {
    let mut parts = version
        .split(['.', '-', '+'])
        .take(3)
        .map(|part| part.parse::<usize>().unwrap());
    (
        parts.next().unwrap(),
        parts.next().unwrap(),
        parts.next().unwrap(),
    )
}

/// Gets the version of the current `rusty_lr_parser` crate.
pub fn current_rusty_lr_parser_version() -> (usize, usize, usize) {
    parse_package_version(env!("CARGO_PKG_VERSION"))
}

/// Gets the `rustylr` version compatible with generated output from this crate.
pub fn compatible_rustylr_version() -> (usize, usize, usize) {
    rusty_lr_core::versions::COMPATIBLE_RUSTYLR_VERSION
}

/// This, `rusty_lr_parser` is designed to generate a code, that will be relied on `rusty_lr`.
///
/// Gets the version of the rusty_lr_core crate that current crate is targeting.
/// If the version is not matched, there will be a compile-time error.
pub fn target_rusty_lr_version() -> (usize, usize, usize) {
    rusty_lr_core::versions::TARGET_RUSTY_LR_VERSION
}
