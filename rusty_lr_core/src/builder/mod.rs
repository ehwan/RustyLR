mod diags;
mod error;
mod grammar;
mod reduce_type;
mod state;

pub use diags::DiagnosticCollector;
pub use error::BuildError;
pub use grammar::Grammar;
pub use reduce_type::ReduceType;
pub use state::State;

/// struct for output of parser building.
pub struct States<Term, NonTerm> {
    pub states: Vec<State<Term, NonTerm, usize, usize>>,
}
