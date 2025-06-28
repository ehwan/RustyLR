//! Core module for the Rusty LR parser.
//!
//! This crate is private and not intended to be used directly.
//! Please use the [`rusty_lr`](https://crates.io/crates/rusty_lr) crate instead.

pub(crate) mod hashmap;
pub(crate) mod rule;
pub mod stackvec;
pub(crate) mod token;

#[cfg(feature = "tree")]
pub(crate) mod tree;
#[cfg(feature = "tree")]
pub use tree::Tree;
#[cfg(feature = "tree")]
pub use tree::TreeList;
#[cfg(feature = "tree")]
pub use tree::TreeNonTerminal;

pub(crate) mod location;
pub use location::Location;

/// module for build DFA tables from CFG
pub mod builder;

/// module for deterministic LR(1), LALR(1) parser
pub mod lr;

/// module for non-deterministic GLR parser
pub mod glr;

mod nonterminal;
pub use nonterminal::NonTerminal;
pub use nonterminal::NonTerminalType;
pub use nonterminal::TokenData;

pub use hashmap::HashMap;
pub use hashmap::HashSet;

pub use rule::ProductionRule;
pub use rule::ShiftedRule;
pub use rule::ShiftedRuleRef;

pub use rule::ReduceType;
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
