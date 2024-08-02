use crate::{ProductionRule, State};

pub trait Callback<Term, NonTerm> {
    /// Error type returned by callback
    type Error;

    /// this is called after the shift of terminal symbol and state transition
    /// the actual state-transition of DFA is managed by parser
    /// if you are tyring to track the state transition with this method,
    /// you must also consider `reduce` method, which pop N states from the state stack,
    /// where N is the number of tokens of the reduced rule
    fn shift_and_goto(
        &mut self,
        rules: &[ProductionRule<Term, NonTerm>],
        states: &[State<Term, NonTerm>],
        state_stack: &[usize],
        term: &Term,
    ) -> Result<(), Self::Error>;

    /// this is called after the shift of non-terminal symbol and state transition
    /// the actual state-transition of DFA is managed by parser
    /// if you are tyring to track the state transition with this method,
    /// you must also consider `reduce` method, which pop N states from the state stack,
    /// where N is the number of tokens of the reduced rule
    fn shift_and_goto_nonterm(
        &mut self,
        rules: &[ProductionRule<Term, NonTerm>],
        states: &[State<Term, NonTerm>],
        state_stack: &[usize],
        nonterm: &NonTerm,
    ) -> Result<(), Self::Error>;

    /// this is called after poping N states from the state stack, where N is the number of tokens of the reduced rule
    /// production rule matched and reduce
    fn reduce(
        &mut self,
        rules: &[ProductionRule<Term, NonTerm>],
        states: &[State<Term, NonTerm>],
        state_stack: &[usize],
        rule: usize,
    ) -> Result<(), Self::Error>;
}

/// default callback that does nothing
pub struct DefaultCallback {}
#[allow(unused_variables)]
impl<Term, NonTerm> Callback<Term, NonTerm> for DefaultCallback {
    type Error = ();
    fn reduce(
        &mut self,
        rules: &[ProductionRule<Term, NonTerm>],
        states: &[State<Term, NonTerm>],
        state_stack: &[usize],
        rule: usize,
    ) -> Result<(), ()> {
        Ok(())
    }
    fn shift_and_goto(
        &mut self,
        rules: &[ProductionRule<Term, NonTerm>],
        states: &[State<Term, NonTerm>],
        state_stack: &[usize],
        term: &Term,
    ) -> Result<(), ()> {
        Ok(())
    }
    fn shift_and_goto_nonterm(
        &mut self,
        rules: &[ProductionRule<Term, NonTerm>],
        states: &[State<Term, NonTerm>],
        state_stack: &[usize],
        nonterm: &NonTerm,
    ) -> Result<(), ()> {
        Ok(())
    }
}
