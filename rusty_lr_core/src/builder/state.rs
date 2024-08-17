use crate::hashmap::HashMap;
use crate::rule::LookaheadRuleRefSet;

/// state in DFA
#[derive(Debug, Clone)]
pub struct State<Term, NonTerm> {
    pub shift_goto_map_term: HashMap<Term, usize>,
    pub shift_goto_map_nonterm: HashMap<NonTerm, usize>,
    pub reduce_map: HashMap<Term, usize>,
    pub ruleset: LookaheadRuleRefSet<Term>,
}
impl<Term, NonTerm> State<Term, NonTerm> {
    pub fn new() -> Self {
        State {
            shift_goto_map_term: Default::default(),
            shift_goto_map_nonterm: Default::default(),
            reduce_map: Default::default(),
            ruleset: LookaheadRuleRefSet::new(),
        }
    }
    /// We have two different `State` types. One in the `crate::builder` module and one in the `crate`.
    /// This state in `crate::builder` is used to build the DFA, which contains the lookaheads of the rules.
    /// This method converts the `State` in `crate::builder` to the `State` in `crate`.
    pub fn to_export(self) -> crate::State<Term, NonTerm> {
        crate::State {
            shift_goto_map_term: self.shift_goto_map_term,
            shift_goto_map_nonterm: self.shift_goto_map_nonterm,
            reduce_map: self.reduce_map,
            ruleset: self.ruleset.rules.into_keys().collect(),
        }
    }
}

impl<Term, NonTerm> Default for State<Term, NonTerm> {
    fn default() -> Self {
        Self::new()
    }
}
