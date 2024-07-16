use crate::parser::Parser;
use crate::term::NonTermTraitBound;
use crate::term::TermTraitBound;

pub trait Callback<Term: TermTraitBound, NonTerm: NonTermTraitBound> {
    /// the actual state-transition of DFA is managed by parser
    /// if you are tyring to track the state transition with this method,
    /// you must also consider `reduce` method, which pop the state stack
    fn shift_and_goto(
        &mut self,
        parser: &Parser<Term, NonTerm>,
        state_stack: &[usize],
        term: &Term,
        state_goto: usize,
    );

    /// the actual state-transition of DFA is managed by parser
    /// if you are tyring to track the state transition with this method,
    /// you must also consider `reduce` method, which pop the state stack
    fn shift_and_goto_nonterm(
        &mut self,
        parser: &Parser<Term, NonTerm>,
        state_stack: &[usize],
        nonterm: &NonTerm,
        state_goto: usize,
    );

    /// production rule matched and reduce
    /// this is called before the reduced non-terminal is feeded
    fn reduce(&mut self, parser: &Parser<Term, NonTerm>, rule: usize);
}

/// Default reducer that does nothing
pub struct DefaultCallback<Term: TermTraitBound, NonTerm: NonTermTraitBound> {
    _phantom: std::marker::PhantomData<(Term, NonTerm)>,
}
impl<Term: TermTraitBound, NonTerm: NonTermTraitBound> DefaultCallback<Term, NonTerm> {
    pub fn new() -> Self {
        DefaultCallback {
            _phantom: std::marker::PhantomData,
        }
    }
}
impl<Term: TermTraitBound, NonTerm: NonTermTraitBound> Default for DefaultCallback<Term, NonTerm> {
    fn default() -> Self {
        DefaultCallback::new()
    }
}

impl<Term: TermTraitBound, NonTerm: NonTermTraitBound> Callback<Term, NonTerm>
    for DefaultCallback<Term, NonTerm>
{
    fn shift_and_goto(
        &mut self,
        _parser: &Parser<Term, NonTerm>,
        _state_stack: &[usize],
        _term: &Term,
        _state_goto: usize,
    ) {
    }
    fn shift_and_goto_nonterm(
        &mut self,
        _parser: &Parser<Term, NonTerm>,
        _state_stack: &[usize],
        _nonterm: &NonTerm,
        _state_goto: usize,
    ) {
    }
    fn reduce(&mut self, _parser: &Parser<Term, NonTerm>, _rule: usize) {}
}

/// Debug reducer that print out the state transition and reduce
pub struct DebugCallback<Term: TermTraitBound, NonTerm: NonTermTraitBound> {
    _phantom: std::marker::PhantomData<(Term, NonTerm)>,
}
impl<Term: TermTraitBound, NonTerm: NonTermTraitBound> DebugCallback<Term, NonTerm> {
    pub fn new() -> Self {
        DebugCallback {
            _phantom: std::marker::PhantomData,
        }
    }
}
impl<Term: TermTraitBound, NonTerm: NonTermTraitBound> Default for DebugCallback<Term, NonTerm> {
    fn default() -> Self {
        DebugCallback::new()
    }
}
impl<Term: TermTraitBound + std::fmt::Debug, NonTerm: NonTermTraitBound + std::fmt::Debug>
    Callback<Term, NonTerm> for DebugCallback<Term, NonTerm>
{
    fn shift_and_goto(
        &mut self,
        _parser: &Parser<Term, NonTerm>,
        _state_stack: &[usize],
        term: &Term,
        state_goto: usize,
    ) {
        println!("Shift: {:?} -> {}", term, state_goto);
    }
    fn shift_and_goto_nonterm(
        &mut self,
        _parser: &Parser<Term, NonTerm>,
        _state_stack: &[usize],
        nonterm: &NonTerm,
        state_goto: usize,
    ) {
        println!("Shift: {:?} -> {}", nonterm, state_goto);
    }
    fn reduce(&mut self, parser: &Parser<Term, NonTerm>, rule: usize) {
        println!("Reduce: {:?}", parser.rules[rule]);
    }
}
