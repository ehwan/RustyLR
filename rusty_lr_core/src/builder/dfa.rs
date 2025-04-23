use std::vec::Vec;

use super::state::State;

/// struct for output of parser building.
pub struct DFA<Term, NonTerm> {
    pub states: Vec<State<Term, NonTerm>>,
}
