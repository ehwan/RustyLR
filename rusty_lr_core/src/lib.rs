//! Core module for the Rusty LR parser.
//!
//! This crate is private and not intended to be used directly.
//! Please use the [`rusty_lr`](https://crates.io/crates/rusty_lr) crate instead.

/// FxHash for fast and non-cryptographic hashing
pub mod hash;

/// stack allocated vector for small data
pub mod stackvec;

/// module for tree representation of parse results (feature `tree`).
#[cfg(feature = "tree")]
pub mod tree;

pub(crate) mod location;
pub use location::DefaultLocation;
pub use location::Location;

/// module for build parser tables from CFG
pub mod builder;

/// module for core parser functionality
pub mod parser;

/// module for auto-generated types of non-terminals representation
pub mod nonterminal;

/// module for production rules representation
pub mod rule;

pub(crate) mod token;
pub use token::Token;

pub(crate) mod backtrace;
pub use backtrace::Backtrace;

/// Default error type for reduce action
#[derive(Debug, Default)]
pub struct DefaultReduceActionError;
impl std::fmt::Display for DefaultReduceActionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Default reduce action error")
    }
}
impl std::error::Error for DefaultReduceActionError {
    fn cause(&self) -> Option<&dyn std::error::Error> {
        None
    }
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
    fn description(&self) -> &str {
        "Default reduce action error"
    }
}
