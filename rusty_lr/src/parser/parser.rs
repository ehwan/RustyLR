use thiserror::Error;

use std::hash::Hash;
use std::vec::Vec;

use super::callback::Callback;
use super::callback::CallbackStr;
use super::callback::DefaultCallback;
use super::callback::DefaultCallbackStr;
use super::context::Context;
use super::context::ContextStr;
use super::ischar::IsChar;
use super::tree::Tree;
use super::tree::TreeStr;
use crate::rule::ProductionRule;
use crate::state::State;

#[derive(Error, Debug)]
pub enum ParseError<Term, NonTerm> {
    #[error("Invalid Non-Terminal: {0}")]
    InvalidNonTerminal(NonTerm),

    #[error("Invalid Terminal: {0}")]
    InvalidTerminal(Term),

    #[error("State Stack is empty; This should not be happened if DFA is generated correctly")]
    StateStackEmpty,

    #[error("State Stack is not enough for reduce; This should not be happened if DFA is generated correctly")]
    StateStackNotEnough,

    #[error("Invalid State: Goto {0}; This should not be happened if DFA is generated correctly")]
    InvalidState(usize),
}
pub struct Parser<Term, NonTerm> {
    pub rules: Vec<ProductionRule<Term, NonTerm>>,
    pub states: Vec<State<Term, NonTerm>>,
    pub main_state: usize,
}

impl<Term: Clone + Hash + Eq, NonTerm: Clone + Hash + Eq> Parser<Term, NonTerm> {
    /// give lookahead token to parser, and check if there is any reduce action
    fn lookahead<'a, C: Callback<Term, NonTerm>>(
        &self,
        context: &mut Context<'a, Term, NonTerm>,
        callback: &mut C,
        term: &Term,
    ) -> Result<(), ParseError<Term, NonTerm>> {
        // fetch state from state stack
        let state = if let Some(state_id) = context.state_safe() {
            if let Some(state) = self.states.get(*state_id) {
                state
            } else {
                // this should not be happened if DFA is generated correctly
                return Err(ParseError::InvalidState(*state_id));
            }
        } else {
            // this should not be happened if DFA is generated correctly
            return Err(ParseError::StateStackEmpty);
        };

        // feed token to current state and get action
        // there must be no reduce/shift conflict ( that is, both shift and reduce are possible with this term ),
        // since it is resolved in state generation ( Grammar::build() )
        if let Some(reduce_rule) = state.reduce(term) {
            // reduce items in stack
            let rule = &self.rules[reduce_rule];
            if context.state_stack.len() < rule.rule.len() {
                // this should not be happened if DFA is generated correctly
                return Err(ParseError::StateStackNotEnough);
            }
            context.reduce(reduce_rule, rule.rule.len());
            callback.reduce(self, context, reduce_rule);

            // feed reduced token
            self.feed_nonterm(context, callback, &rule.name)?;

            // original lookahead token is not shifted, so feed it again
            self.lookahead(context, callback, term)?;
        }
        Ok(())
    }
    /// feed one terminal to parser, and update state stack
    fn feed<'a, C: Callback<Term, NonTerm>>(
        &self,
        context: &mut Context<'a, Term, NonTerm>,
        callback: &mut C,
    ) -> Result<(), ParseError<Term, NonTerm>> {
        // fetch state from state stack
        let state = if let Some(state_id) = context.state_safe() {
            if let Some(state) = self.states.get(*state_id) {
                state
            } else {
                // this should not be happened if DFA is generated correctly
                return Err(ParseError::InvalidState(*state_id));
            }
        } else {
            // this should not be happened if DFA is generated correctly
            return Err(ParseError::StateStackEmpty);
        };

        let term = context.term();

        // feed token to current state and get action
        // there must be no reduce/shift conflict ( that is, both shift and reduce are possible with this term ),
        // since it is resolved in state generation ( Grammar::build() )
        if let Some(next_state_id) = state.shift_goto_term(term) {
            context.shift();
            context.push_state(next_state_id);
            callback.shift_and_goto(self, context);
            return Ok(());
        }
        callback.invalid_term(self, context);
        Err(ParseError::InvalidTerminal(term.clone()))
    }

    /// feed one non-terminal to parser, and update state stack
    fn feed_nonterm<'a, C: Callback<Term, NonTerm>>(
        &self,
        context: &mut Context<'a, Term, NonTerm>,
        callback: &mut C,
        nonterm: &NonTerm,
    ) -> Result<(), ParseError<Term, NonTerm>> {
        // fetch state from state stack
        let state = if let Some(state_id) = context.state_safe() {
            if let Some(state) = self.states.get(*state_id) {
                state
            } else {
                // this should not be happened if DFA is generated correctly
                return Err(ParseError::InvalidState(*state_id));
            }
        } else {
            // this should not be happened if DFA is generated correctly
            return Err(ParseError::StateStackEmpty);
        };

        // feed token to current state and get action
        // for shift/reduce confict, shift has higher priority
        if let Some(next_state_id) = state.shift_goto_nonterm(nonterm) {
            context.push_state(next_state_id);
            callback.shift_and_goto_nonterm(self, context, nonterm);
            return Ok(());
        }
        callback.invalid_nonterm(self, context, nonterm);
        Err(ParseError::InvalidNonTerminal(nonterm.clone()))
    }

    /// parse given terminals and return result
    /// terminals must not contains eof
    /// eof is explicitly given as argument
    pub fn parse_with_callback<C: Callback<Term, NonTerm>>(
        &self,
        terminals: &[Term],
        callback: &mut C,
        eof: Term,
    ) -> Result<Tree, ParseError<Term, NonTerm>> {
        let mut context = Context::new(terminals, self.main_state);

        // feed all tokens
        for term in terminals.iter() {
            self.lookahead(&mut context, callback, term)?;
            self.feed(&mut context, callback)?;
            context.inc();
        }

        // feed eof (as lookahead for final reduce)
        self.lookahead(&mut context, callback, &eof)?;

        assert_eq!(context.stack.len(), 1);

        Ok(context.stack.pop().unwrap())
    }
    /// parse given terminals and return result
    /// terminals must not contains eof
    /// eof is explicitly given as argument
    pub fn parse<C: Callback<Term, NonTerm>>(
        &self,
        terminals: &[Term],
        eof: Term,
    ) -> Result<Tree, ParseError<Term, NonTerm>> {
        let mut callback = DefaultCallback {};
        self.parse_with_callback(terminals, &mut callback, eof)
    }

    // ========================================================================
    // below is the same as above, but for &str
    // ========================================================================

    /// give lookahead token to parser, and check if there is any reduce action
    fn lookahead_str<'a, C: CallbackStr<Term, NonTerm>>(
        &self,
        context: &mut ContextStr<'a, Term, NonTerm>,
        callback: &mut C,
        term: Term,
    ) -> Result<(), ParseError<Term, NonTerm>>
    where
        char: IsChar<Term>,
    {
        // fetch state from state stack
        let state = if let Some(state_id) = context.state_safe() {
            if let Some(state) = self.states.get(*state_id) {
                state
            } else {
                // this should not be happened if DFA is generated correctly
                return Err(ParseError::InvalidState(*state_id));
            }
        } else {
            // this should not be happened if DFA is generated correctly
            return Err(ParseError::StateStackEmpty);
        };

        // feed token to current state and get action
        // there must be no reduce/shift conflict ( that is, both shift and reduce are possible with this term ),
        // since it is resolved in state generation ( Grammar::build() )
        if let Some(reduce_rule) = state.reduce(&term) {
            // reduce items in stack
            let rule = &self.rules[reduce_rule];
            if context.state_stack.len() < rule.rule.len() {
                // this should not be happened if DFA is generated correctly
                return Err(ParseError::StateStackNotEnough);
            }
            context.reduce(reduce_rule, rule.rule.len());
            callback.reduce(self, context, reduce_rule);

            // feed reduced token
            self.feed_nonterm_str(context, callback, &rule.name)?;

            // original lookahead token is not shifted, so feed it again
            self.lookahead_str(context, callback, term)?;
        }
        Ok(())
    }
    /// feed one terminal to parser, and update state stack
    fn feed_str<'a, C: CallbackStr<Term, NonTerm>>(
        &self,
        context: &mut ContextStr<'a, Term, NonTerm>,
        callback: &mut C,
    ) -> Result<(), ParseError<Term, NonTerm>>
    where
        char: IsChar<Term>,
    {
        // fetch state from state stack
        let state = if let Some(state_id) = context.state_safe() {
            if let Some(state) = self.states.get(*state_id) {
                state
            } else {
                // this should not be happened if DFA is generated correctly
                return Err(ParseError::InvalidState(*state_id));
            }
        } else {
            // this should not be happened if DFA is generated correctly
            return Err(ParseError::StateStackEmpty);
        };

        let term = context.term();

        // feed token to current state and get action
        // there must be no reduce/shift conflict ( that is, both shift and reduce are possible with this term ),
        // since it is resolved in state generation ( Grammar::build() )
        if let Some(next_state_id) = state.shift_goto_term(&term) {
            context.shift();
            context.push_state(next_state_id);
            callback.shift_and_goto(self, context);
            return Ok(());
        }
        callback.invalid_term(self, context);
        Err(ParseError::InvalidTerminal(term))
    }

    /// feed one non-terminal to parser, and update state stack
    fn feed_nonterm_str<'a, C: CallbackStr<Term, NonTerm>>(
        &self,
        context: &mut ContextStr<'a, Term, NonTerm>,
        callback: &mut C,
        nonterm: &NonTerm,
    ) -> Result<(), ParseError<Term, NonTerm>>
    where
        char: IsChar<Term>,
    {
        // fetch state from state stack
        let state = if let Some(state_id) = context.state_safe() {
            if let Some(state) = self.states.get(*state_id) {
                state
            } else {
                // this should not be happened if DFA is generated correctly
                return Err(ParseError::InvalidState(*state_id));
            }
        } else {
            // this should not be happened if DFA is generated correctly
            return Err(ParseError::StateStackEmpty);
        };

        // feed token to current state and get action
        // for shift/reduce confict, shift has higher priority
        if let Some(next_state_id) = state.shift_goto_nonterm(nonterm) {
            context.push_state(next_state_id);
            callback.shift_and_goto_nonterm(self, context, nonterm);
            return Ok(());
        }
        callback.invalid_nonterm(self, context, nonterm);
        Err(ParseError::InvalidNonTerminal(nonterm.clone()))
    }

    /// parse given str and return result
    /// terminals must not contains eof
    /// eof is explicitly given as argument
    pub fn parse_str_with_callback<C: CallbackStr<Term, NonTerm>>(
        &self,
        terminals: &str,
        callback: &mut C,
        eof: Term,
    ) -> Result<TreeStr, ParseError<Term, NonTerm>>
    where
        char: IsChar<Term>,
    {
        let mut context = ContextStr::new(terminals, self.main_state);

        // feed all tokens
        for term in terminals.chars() {
            self.lookahead_str(&mut context, callback, term.as_term())?;
            self.feed_str(&mut context, callback)?;
            context.inc();
        }

        // feed eof (as lookahead for final reduce)
        self.lookahead_str(&mut context, callback, eof)?;

        assert_eq!(context.stack.len(), 1);

        Ok(context.stack.pop().unwrap())
    }

    /// parse given str and return result
    /// terminals must not contains eof
    /// eof is explicitly given as argument
    pub fn parse_str<C: Callback<Term, NonTerm>>(
        &self,
        terminals: &str,
        eof: Term,
    ) -> Result<TreeStr, ParseError<Term, NonTerm>>
    where
        char: IsChar<Term>,
    {
        let mut callback = DefaultCallbackStr {};
        self.parse_str_with_callback(terminals, &mut callback, eof)
    }
}
