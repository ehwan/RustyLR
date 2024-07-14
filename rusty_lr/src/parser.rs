use thiserror::Error;

use std::error;
use std::vec::Vec;

use crate::action::Action;
use crate::state::State;
use crate::term::TermTraitBound;
use crate::token::Token;

#[derive(Error, Debug)]
pub enum ParseError<Term: TermTraitBound, NonTerm: TermTraitBound> {
    #[error("Invalid Token: {0}")]
    InvalidToken(Token<Term, NonTerm>),

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
    /// feed one token to parser, and update state stack and stack
    fn feed(
        &self,
        state_stack: &mut Vec<usize>,
        token: &Token<Term, NonTerm>,
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
        let action = if let Some(action) = state.feed(token) {
            action
        } else {
            // TODO add curret context or production rule to error
            return Err(ParseError::InvalidToken(token.clone()));
        };

        if action.should_shift() {
            // TODO send shift signal to external handler
        }
        match action {
            Action::Reduce(rule) => {
                // reduce items in stack
                if state_stack.len() < rule.rule.len() {
                    return Err(ParseError::StateStackNotEnough);
                }
                state_stack.truncate(state_stack.len() - rule.rule.len());

                // feed reduced token
                let reduced_token = Token::NonTerm(rule.name.clone());
                self.feed(state_stack, &reduced_token)?;

                // original feed token is not shifted, so feed it again
                self.feed(state_stack, token)?;
            }
            Action::Goto(next_state_id) => {
                state_stack.push(*next_state_id);
            }
        }

        Ok(())
    }

    /// parse given tokens and return result
    pub fn parse(&self, tokens: &[Term]) -> Result<(), ParseError<Term, NonTerm>> {
        // create state stack and set default state to main_state
        let mut state_stack = Vec::new();
        state_stack.push(self.main_state);

        // feed all tokens
        for token in tokens.iter().cloned() {
            self.feed(&mut state_stack, &Token::Term(token))?;
        }

        // feed End token
        self.feed(&mut state_stack, &Token::End)?;

        Ok(())
    }
}
