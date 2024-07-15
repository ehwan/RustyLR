use thiserror::Error;

use std::vec::Vec;

use crate::callback::Callback;
use crate::callback::DebugCallback;
use crate::callback::DefaultCallback;
use crate::callback::ShiftReduceConflict;
use crate::rule::ProductionRule;
use crate::state::State;
use crate::term::NonTermTraitBound;
use crate::term::TermTraitBound;

#[derive(Error, Debug)]
pub enum ParseError<Term: TermTraitBound, NonTerm: NonTermTraitBound> {
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
pub struct Parser<Term: TermTraitBound, NonTerm: NonTermTraitBound> {
    pub rules: Vec<ProductionRule<Term, NonTerm>>,
    pub states: Vec<State<Term, NonTerm>>,
    pub main_state: usize,
}

impl<Term: TermTraitBound, NonTerm: NonTermTraitBound> Parser<Term, NonTerm> {
    /// feed one terminal to parser, and update state stack
    fn feed<C: Callback<Term, NonTerm>>(
        &self,
        state_stack: &mut Vec<usize>,
        term: &Term,
        callback: &mut C,
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
        // for shift/reduce confict, resolve by callback
        let mut shift_and_goto = state.shift_goto_term(term);
        let mut reduce_rule = state.reduce(term);
        if shift_and_goto.is_some() && reduce_rule.is_some() {
            // shift/reduct conflict occured
            match callback.resolve_shift_reduce_conflict(
                self,
                state_stack,
                term,
                shift_and_goto.unwrap(),
                reduce_rule.unwrap(),
            ) {
                ShiftReduceConflict::Shift => {
                    reduce_rule = None;
                }
                ShiftReduceConflict::Reduce => {
                    shift_and_goto = None;
                }
            }
        }

        if let Some(next_state_id) = shift_and_goto {
            callback.shift_and_goto(self, state_stack, term, next_state_id);
            state_stack.push(next_state_id);
            return Ok(());
        }
        if let Some(reduce_rule) = reduce_rule {
            // reduce items in stack
            let rule = &self.rules[reduce_rule];
            if state_stack.len() < rule.rule.len() {
                return Err(ParseError::StateStackNotEnough);
            }
            state_stack.truncate(state_stack.len() - rule.rule.len());
            callback.reduce(self, reduce_rule);

            // feed reduced token
            self.feed_nonterm(state_stack, &rule.name, callback)?;

            // original feed token is not shifted, so feed it again
            self.feed(state_stack, term, callback)?;
            return Ok(());
        }
        // TODO add curret context or production rule to error
        Err(ParseError::InvalidTerminal(term.clone()))
    }

    /// feed one non-terminal to parser, and update state stack
    fn feed_nonterm<C: Callback<Term, NonTerm>>(
        &self,
        state_stack: &mut Vec<usize>,
        nonterm: &NonTerm,
        callback: &mut C,
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
            callback.shift_and_goto_nonterm(self, state_stack, nonterm, next_state_id);
            state_stack.push(next_state_id);
            return Ok(());
        }
        // TODO add curret context or production rule to error
        Err(ParseError::InvalidNonTerminal(nonterm.clone()))
    }

    /// call `parse()` with default reducer.
    pub fn parse_with_default_callback(
        &self,
        terminals: &[Term],
        end_term: Option<Term>,
    ) -> Result<(), ParseError<Term, NonTerm>> {
        let mut callback = DefaultCallback::new();

        self.parse(terminals, end_term, &mut callback)
    }

    /// call `parse()` with debug reducer
    pub fn parse_with_debug_callback(
        &self,
        terminals: &[Term],
        end_term: Option<Term>,
    ) -> Result<(), ParseError<Term, NonTerm>>
    where
        Term: std::fmt::Debug,
        NonTerm: std::fmt::Debug,
    {
        let mut callback = DebugCallback::new();

        self.parse(terminals, end_term, &mut callback)
    }

    /// parse given terminals and return result
    /// if end_term is Some, it will be feeded after all tokens are feeded
    /// if end_term is None, terminals should end with End token
    pub fn parse<C: Callback<Term, NonTerm>>(
        &self,
        terminals: &[Term],
        end_term: Option<Term>,
        callback: &mut C,
    ) -> Result<(), ParseError<Term, NonTerm>> {
        // create state stack and set default state to main_state
        let mut state_stack = Vec::new();
        state_stack.push(self.main_state);

        // feed all tokens
        for term in terminals.iter() {
            self.feed(&mut state_stack, term, callback)?;
        }

        // feed End token
        if let Some(end_term) = end_term {
            self.feed(&mut state_stack, &end_term, callback)?;
        }

        Ok(())
    }
}
