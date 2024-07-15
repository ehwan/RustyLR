use crate::rule::ProductionRule;
use crate::term::NonTermTraitBound;
use crate::term::TermTraitBound;

pub trait Reducer<Term: TermTraitBound, NonTerm: NonTermTraitBound> {
    /// the actual state-transition of DFA is managed by parser
    /// if you are tyring to track the state transition with this method,
    /// you must also consider `reduce` method, which pop the state stack
    fn shift_and_goto(&mut self, term: &Term, state_goto: usize);

    /// the actual state-transition of DFA is managed by parser
    /// if you are tyring to track the state transition with this method,
    /// you must also consider `reduce` method, which pop the state stack
    fn shift_and_goto_nonterm(&mut self, nonterm: &NonTerm, state_goto: usize);

    /// production rule matched and reduce
    /// this is called before the reduced non-terminal is feeded
    fn reduce(&mut self, rule: &ProductionRule<Term, NonTerm>);
}

/// Default reducer that does nothing
pub struct DefaultReducer<Term: TermTraitBound, NonTerm: NonTermTraitBound> {
    _phantom: std::marker::PhantomData<(Term, NonTerm)>,
}
impl<Term: TermTraitBound, NonTerm: NonTermTraitBound> DefaultReducer<Term, NonTerm> {
    pub fn new() -> Self {
        DefaultReducer {
            _phantom: std::marker::PhantomData,
        }
    }
}
impl<Term: TermTraitBound, NonTerm: NonTermTraitBound> Default for DefaultReducer<Term, NonTerm> {
    fn default() -> Self {
        DefaultReducer::new()
    }
}

impl<Term: TermTraitBound, NonTerm: NonTermTraitBound> Reducer<Term, NonTerm>
    for DefaultReducer<Term, NonTerm>
{
    fn shift_and_goto(&mut self, _term: &Term, _goto: usize) {}
    fn shift_and_goto_nonterm(&mut self, _nonterm: &NonTerm, _goto: usize) {}
    fn reduce(&mut self, _rule: &ProductionRule<Term, NonTerm>) {}
}

/// Debug reducer that print out the state transition and reduce
pub struct DebugReducer<Term: TermTraitBound, NonTerm: NonTermTraitBound> {
    _phantom: std::marker::PhantomData<(Term, NonTerm)>,
}
impl<Term: TermTraitBound, NonTerm: NonTermTraitBound> DebugReducer<Term, NonTerm> {
    pub fn new() -> Self {
        DebugReducer {
            _phantom: std::marker::PhantomData,
        }
    }
}
impl<Term: TermTraitBound, NonTerm: NonTermTraitBound> Default for DebugReducer<Term, NonTerm> {
    fn default() -> Self {
        DebugReducer::new()
    }
}
impl<Term: TermTraitBound + std::fmt::Debug, NonTerm: NonTermTraitBound + std::fmt::Debug>
    Reducer<Term, NonTerm> for DebugReducer<Term, NonTerm>
{
    fn shift_and_goto(&mut self, term: &Term, goto: usize) {
        println!("Shift: {:?} -> {}", term, goto);
    }
    fn shift_and_goto_nonterm(&mut self, nonterm: &NonTerm, state_goto: usize) {
        println!("Shift: {:?} -> {}", nonterm, state_goto);
    }
    fn reduce(&mut self, rule: &ProductionRule<Term, NonTerm>) {
        println!("Reduce: {:?}", rule);
    }
}
