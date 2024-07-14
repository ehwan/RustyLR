use thiserror::Error;

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

    #[error("Stack is empty")]
    StackEmpty,

    #[error("Stack is not enough to reduce")]
    StackNotEnough,

    #[error("Invalid State: {0}")]
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
        stack: &mut Vec<Token<Term, NonTerm>>,
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
            stack.push(token.clone());
        }
        match action {
            Action::Reduce(rule) => {
                // reduce items in stack
                if stack.len() < rule.rule.len() {
                    return Err(ParseError::StackNotEnough);
                }
                stack.truncate(stack.len() - rule.rule.len());

                // pop state from stack, number of tokens reduced
                state_stack.truncate(state_stack.len() - rule.rule.len());

                // feed reduced token
                let reduced_token = Token::NonTerm(rule.name.clone());
                self.feed(state_stack, stack, &reduced_token)?;
                self.feed(state_stack, stack, token)?;
            }
            Action::Goto(next_state_id) => {
                state_stack.push(*next_state_id);
            }
        }

        Ok(())
    }

    /// parse given tokens and return result
    pub fn parse(
        &self,
        tokens: &[Term],
    ) -> Result<Token<Term, NonTerm>, ParseError<Term, NonTerm>> {
        // create state stack and set default state to main_state
        let mut state_stack = Vec::new();
        state_stack.push(self.main_state);

        // create stack for tokens
        let mut stack = Vec::new();

        // feed all tokens
        for token in tokens.iter().cloned() {
            self.feed(&mut state_stack, &mut stack, &Token::Term(token))?;
        }

        // feed End token
        self.feed(&mut state_stack, &mut stack, &Token::End)?;

        // if parse succeeded, stack should have [ MainEntry, End ]
        let result = if let Some(result) = stack.get(0) {
            result
        } else {
            return Err(ParseError::StackEmpty);
        };
        Ok(result.clone())
    }
}
