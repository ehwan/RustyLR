use thiserror::Error;

use std::vec::Vec;

use crate::reducer::DebugReducer;
use crate::reducer::DefaultReducer;
use crate::reducer::Reducer;
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
    fn feed<R: Reducer<Term, NonTerm>>(
        &self,
        state_stack: &mut Vec<usize>,
        term: &Term,
        reducer: &mut R,
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
            reducer.shift_and_goto(term, next_state_id);
            return Ok(());
        }
        if let Some(reduce_rule) = state.reduce(term) {
            // reduce items in stack
            let rule = &self.rules[reduce_rule];
            if state_stack.len() < rule.rule.len() {
                return Err(ParseError::StateStackNotEnough);
            }
            state_stack.truncate(state_stack.len() - rule.rule.len());
            reducer.reduce(rule);

            // feed reduced token
            self.feed_nonterm(state_stack, &rule.name, reducer)?;

            // original feed token is not shifted, so feed it again
            self.feed(state_stack, term, reducer)?;
            return Ok(());
        }
        // TODO add curret context or production rule to error
        Err(ParseError::InvalidTerminal(term.clone()))
    }

    /// feed one non-terminal to parser, and update state stack
    fn feed_nonterm<R: Reducer<Term, NonTerm>>(
        &self,
        state_stack: &mut Vec<usize>,
        nonterm: &NonTerm,
        reducer: &mut R,
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
            reducer.shift_and_goto_nonterm(nonterm, next_state_id);
            return Ok(());
        }
        // TODO add curret context or production rule to error
        Err(ParseError::InvalidNonTerminal(nonterm.clone()))
    }

    /// call `parse()` with default reducer.
    pub fn parse_with_default_reducer(
        &self,
        terminals: &[Term],
        end_term: Option<Term>,
    ) -> Result<(), ParseError<Term, NonTerm>> {
        let mut reducer = DefaultReducer::new();

        self.parse(terminals, end_term, &mut reducer)
    }

    /// call `parse()` with debug reducer
    pub fn parse_with_debug_reducer(
        &self,
        terminals: &[Term],
        end_term: Option<Term>,
    ) -> Result<(), ParseError<Term, NonTerm>>
    where
        Term: std::fmt::Debug,
        NonTerm: std::fmt::Debug,
    {
        let mut reducer = DebugReducer::new();

        self.parse(terminals, end_term, &mut reducer)
    }

    /// parse given terminals and return result
    /// if end_term is Some, it will be feeded after all tokens are feeded
    /// if end_term is None, terminals should end with End token
    pub fn parse<R: Reducer<Term, NonTerm>>(
        &self,
        terminals: &[Term],
        end_term: Option<Term>,
        reducer: &mut R,
    ) -> Result<(), ParseError<Term, NonTerm>> {
        // create state stack and set default state to main_state
        let mut state_stack = Vec::new();
        state_stack.push(self.main_state);

        // feed all tokens
        for term in terminals.iter() {
            self.feed(&mut state_stack, term, reducer)?;
        }

        // feed End token
        if let Some(end_term) = end_term {
            self.feed(&mut state_stack, &end_term, reducer)?;
        }

        Ok(())
    }
}
