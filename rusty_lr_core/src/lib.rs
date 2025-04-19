//! Core module for the Rusty LR parser.
//!
//! This crate is private and not intended to be used directly.
//! Please use the [`rusty_lr`](https://crates.io/crates/rusty_lr) crate instead.

pub(crate) mod hashmap;
pub(crate) mod rule;
pub(crate) mod token;

#[cfg(feature = "tree")]
pub(crate) mod tree;
#[cfg(feature = "tree")]
pub use tree::Tree;
#[cfg(feature = "tree")]
pub use tree::TreeList;
#[cfg(feature = "tree")]
pub use tree::TreeNonTerminal;

/// module for building parser tables from CFG
#[cfg(feature = "builder")]
pub mod builder;

/// module for single-path, deterministic LR(1), LALR(1) parser
pub mod lr;

/// module for multi-path, GLR parser
pub mod glr;

pub use hashmap::HashMap;
pub use hashmap::HashSet;

pub use rule::ProductionRule;

pub use rule::LookaheadRule;
pub use rule::LookaheadRuleRefSet;
pub use rule::ShiftedRule;
pub use rule::ShiftedRuleRef;

pub use rule::ReduceType;
pub use token::Token;

#[cfg(feature = "error")]
pub(crate) mod backtrace;
#[cfg(feature = "error")]
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
