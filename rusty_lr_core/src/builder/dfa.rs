use std::vec::Vec;

use crate::state::State;

/// struct for Deterministic Finite Automaton (DFA).
///
/// It contains Vec of production rules and states.
pub struct DFA<Term, NonTerm> {
    pub states: Vec<State<Term, NonTerm>>,
}
