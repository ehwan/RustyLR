use crate::context::Context;
use crate::parser::Parser;
use crate::term::NonTermTraitBound;
use crate::term::TermTraitBound;

pub trait Callback<Term: TermTraitBound, NonTerm: NonTermTraitBound> {
    /// this is called after the shift of terminal symbol and state transition
    /// the actual state-transition of DFA is managed by parser
    /// if you are tyring to track the state transition with this method,
    /// you must also consider `reduce` method, which pop N states from the state stack,
    /// where N is the number of tokens of the reduced rule
    fn shift_and_goto(
        &mut self,
        parser: &Parser<Term, NonTerm>,
        context: &Context<'_, Term, NonTerm>,
    );

    /// this is called after the shift of non-terminal symbol and state transition
    /// the actual state-transition of DFA is managed by parser
    /// if you are tyring to track the state transition with this method,
    /// you must also consider `reduce` method, which pop N states from the state stack,
    /// where N is the number of tokens of the reduced rule
    fn shift_and_goto_nonterm(
        &mut self,
        parser: &Parser<Term, NonTerm>,
        context: &Context<'_, Term, NonTerm>,
        nonterm: &NonTerm,
    );

    /// this is called after poping N states from the state stack, where N is the number of tokens of the reduced rule
    /// production rule matched and reduce
    fn reduce(
        &mut self,
        parser: &Parser<Term, NonTerm>,
        context: &Context<'_, Term, NonTerm>,
        rule: usize,
    );

    /// called when terminal symbol is given, and there is no action for it
    fn invalid_term(
        &mut self,
        parser: &Parser<Term, NonTerm>,
        context: &Context<'_, Term, NonTerm>,
    );

    /// called when non-terminal symbol is given, and there is no action for it
    fn invalid_nonterm(
        &mut self,
        parser: &Parser<Term, NonTerm>,
        context: &Context<'_, Term, NonTerm>,
        nonterm: &NonTerm,
    );
}

/// default callback that does nothing
pub struct DefaultCallback {}
#[allow(unused_variables)]
impl<Term: TermTraitBound, NonTerm: NonTermTraitBound> Callback<Term, NonTerm> for DefaultCallback {
    fn invalid_nonterm(
        &mut self,
        parser: &Parser<Term, NonTerm>,
        context: &Context<'_, Term, NonTerm>,
        nonterm: &NonTerm,
    ) {
    }
    fn invalid_term(
        &mut self,
        parser: &Parser<Term, NonTerm>,
        context: &Context<'_, Term, NonTerm>,
    ) {
    }
    fn reduce(
        &mut self,
        parser: &Parser<Term, NonTerm>,
        context: &Context<'_, Term, NonTerm>,
        rule: usize,
    ) {
    }
    fn shift_and_goto(
        &mut self,
        parser: &Parser<Term, NonTerm>,
        context: &Context<'_, Term, NonTerm>,
    ) {
    }
    fn shift_and_goto_nonterm(
        &mut self,
        parser: &Parser<Term, NonTerm>,
        context: &Context<'_, Term, NonTerm>,
        nonterm: &NonTerm,
    ) {
    }
}
