use std::hash::Hash;
use std::vec::Vec;

use super::callback::Callback;
use super::callback::DefaultCallback;
use super::context::Context;
use super::error::ParseError;
use crate::rule::ProductionRule;
use crate::state::State;

/// struct for Deterministic Finite Automaton (DFA).
///
/// It contains Vec of production rules and states.
pub struct Parser<Term, NonTerm> {
    pub rules: Vec<ProductionRule<Term, NonTerm>>,
    pub states: Vec<State<Term, NonTerm>>,
}

impl<Term, NonTerm> Parser<Term, NonTerm> {
    /// give lookahead token to parser, and check if there is any reduce action
    fn lookahead<'a, C: Callback<Term, NonTerm>>(
        &'a self,
        context: &mut Context,
        callback: &mut C,
        term: &Term,
    ) -> Result<(), ParseError<'a, Term, NonTerm, C::Error, u8>>
    where
        Term: Hash + Eq,
        NonTerm: Hash + Eq,
    {
        // fetch state from state stack
        let state = &self.states[*context.state_stack.last().unwrap()];

        // feed token to current state and get action
        // there must be no reduce/shift conflict ( that is, both shift and reduce are possible with this term ),
        // since it is resolved in state generation ( Grammar::build() )
        if let Some(reduce_rule) = state.reduce(term) {
            // reduce items in stack
            let rule = &self.rules[reduce_rule];

            if context.state_stack.len() < rule.rule.len() {
                // this should not be happened if DFA is generated correctly
                panic!(
                    "State stack not enough for reduce: {:?}",
                    context.state_stack
                );
            }
            context
                .state_stack
                .truncate(context.state_stack.len() - rule.rule.len());
            callback
                .reduce(&self.rules, &self.states, &context.state_stack, reduce_rule)
                .map_err(|e| ParseError::Callback(e))?;

            // feed reduced token
            self.feed_nonterm_callback(context, callback, &rule.name)?;

            // original lookahead token is not shifted, so feed it again
            self.lookahead(context, callback, term)?;
        }
        Ok(())
    }
    /// feed one terminal to parser, and update state stack
    pub fn feed<'a>(
        &'a self,
        context: &mut Context,
        term: Term,
    ) -> Result<(), ParseError<'a, Term, NonTerm, u8, u8>>
    where
        Term: Hash + Eq,
        NonTerm: Hash + Eq,
    {
        self.feed_callback(context, &mut DefaultCallback {}, term)
    }
    /// feed one terminal to parser, and update state stack
    pub fn feed_callback<'a, C: Callback<Term, NonTerm>>(
        &'a self,
        context: &mut Context,
        callback: &mut C,
        term: Term,
    ) -> Result<(), ParseError<'a, Term, NonTerm, C::Error, u8>>
    where
        Term: Hash + Eq,
        NonTerm: Hash + Eq,
    {
        // reduce if possible
        self.lookahead(context, callback, &term)?;

        // fetch state from state stack
        let state = &self.states[*context.state_stack.last().unwrap()];

        // feed token to current state and get action
        // there must be no reduce/shift conflict ( that is, both shift and reduce are possible with this term ),
        // since it is resolved in state generation ( Grammar::build() )
        if let Some(next_state_id) = state.shift_goto_term(&term) {
            context.state_stack.push(next_state_id);
            callback
                .shift_and_goto(&self.rules, &self.states, &context.state_stack, &term)
                .map_err(|e| ParseError::Callback(e))?;
            return Ok(());
        }
        Err(ParseError::InvalidTerminal(
            term,
            &self.rules,
            &self.states,
            context.state_stack.clone(),
        ))
    }

    /// feed one non-terminal to parser, and update state stack
    fn feed_nonterm_callback<'a, C: Callback<Term, NonTerm>>(
        &'a self,
        context: &mut Context,
        callback: &mut C,
        nonterm: &'a NonTerm,
    ) -> Result<(), ParseError<'a, Term, NonTerm, C::Error, u8>>
    where
        NonTerm: Hash + Eq,
    {
        // fetch state from state stack
        let state = &self.states[*context.state_stack.last().unwrap()];

        // feed token to current state and get action
        // for shift/reduce confict, shift has higher priority
        if let Some(next_state_id) = state.shift_goto_nonterm(nonterm) {
            context.state_stack.push(next_state_id);
            callback
                .shift_and_goto_nonterm(&self.rules, &self.states, &context.state_stack, nonterm)
                .map_err(|e| ParseError::Callback(e))?;
            return Ok(());
        }
        Err(ParseError::InvalidNonTerminal(
            nonterm,
            &self.rules,
            &self.states,
            context.state_stack.clone(),
        ))
    }

    pub fn begin(&self) -> Context {
        Context::new()
    }
}
