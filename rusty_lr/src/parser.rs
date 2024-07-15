use thiserror::Error;

use std::vec::Vec;

use crate::state::State;
use crate::term::TermTraitBound;
use crate::token::Token;

#[derive(Error, Debug)]
pub enum ParseError<Term: TermTraitBound, NonTerm: TermTraitBound> {
    #[error("Invalid Non-Terminal: {0}")]
    InvalidNonTerminal(NonTerm),

    #[error("Invalid Terminal: {0}")]
    InvalidTerminal(Term),

    #[error("State Stack is empty")]
    StateStackEmpty,

    #[error("State Stack is not enough for reduce")]
    StateStackNotEnough,

    #[error("Invalid State: Goto {0}")]
    InvalidState(usize),
}
pub struct Parser<Term: TermTraitBound, NonTerm: TermTraitBound> {
    pub states: Vec<State<Term, NonTerm>>,
    pub main_state: usize,
}

impl<Term: TermTraitBound, NonTerm: TermTraitBound> Parser<Term, NonTerm> {
    /// feed one terminal to parser, and update state stack
    fn feed(
        &self,
        state_stack: &mut Vec<usize>,
        term: &Term,
    ) -> Result<(), ParseError<Term, NonTerm>> {
        // fetch state from state stack
        let state = if let Some(state_id) = state_stack.last() {
            if let Some(state) = self.states.get(*state_id) {
                state
            } else {
                return Err(ParseError::InvalidState(*state_id));
            }
        } else {
            return Err(ParseError::StateStackEmpty);
        };

        // feed token to current state and get action
        // for shift/reduce confict, shift has higher priority
        if let Some(next_state_id) = state.shift_goto_term(term) {
            state_stack.push(next_state_id);
            return Ok(());
        }
        if let Some(reduce_rule) = state.reduce(term) {
            // reduce items in stack
            if state_stack.len() < reduce_rule.rule.len() {
                return Err(ParseError::StateStackNotEnough);
            }
            state_stack.truncate(state_stack.len() - reduce_rule.rule.len());

            // feed reduced token
            self.feed_nonterm(state_stack, &reduce_rule.name)?;

            // original feed token is not shifted, so feed it again
            self.feed(state_stack, term)?;
            return Ok(());
        }
        // TODO add curret context or production rule to error
        Err(ParseError::InvalidTerminal(term.clone()))
    }

    /// feed one non-terminal to parser, and update state stack
    fn feed_nonterm(
        &self,
        state_stack: &mut Vec<usize>,
        nonterm: &NonTerm,
    ) -> Result<(), ParseError<Term, NonTerm>> {
        // fetch state from state stack
        let state = if let Some(state_id) = state_stack.last() {
            if let Some(state) = self.states.get(*state_id) {
                state
            } else {
                return Err(ParseError::InvalidState(*state_id));
            }
        } else {
            return Err(ParseError::StateStackEmpty);
        };

        // feed token to current state and get action
        // for shift/reduce confict, shift has higher priority
        if let Some(next_state_id) = state.shift_goto_nonterm(nonterm) {
            state_stack.push(next_state_id);
            return Ok(());
        }
        // TODO add curret context or production rule to error
        Err(ParseError::InvalidNonTerminal(nonterm.clone()))
    }

    /// parse given terminals and return result
    /// if end_term is Some, it will be feeded after all tokens are feeded
    /// if end_term is None, terminals should end with End token
    pub fn parse(
        &self,
        terminals: &[Term],
        end_term: Option<Term>,
    ) -> Result<(), ParseError<Term, NonTerm>> {
        // create state stack and set default state to main_state
        let mut state_stack = Vec::new();
        state_stack.push(self.main_state);

        // feed all tokens
        for term in terminals.iter() {
            self.feed(&mut state_stack, term)?;
        }

        // feed End token
        if let Some(end_term) = end_term {
            self.feed(&mut state_stack, &end_term)?;
        }

        Ok(())
    }
}
