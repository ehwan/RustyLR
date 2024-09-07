pub(crate) mod context;
pub(crate) mod error;
pub(crate) mod parser;
pub(crate) mod stack;
pub(crate) mod state;

use std::hash::Hash;

pub use context::Context;
pub use error::InvalidTerminalError;
pub use error::ParseError;
pub use parser::Parser;
pub use stack::Stack;
pub use state::State;

#[cfg(feature = "tree")]
use crate::Tree;

/// feed one terminal to parser, and update state & data stack
pub fn feed<P: Parser, S: Stack<Term = P::Term, NonTerm = P::NonTerm>>(
    parser: &P,
    context: &mut Context<S>,
    term: P::Term,
    data: &mut S::UserData,
) -> Result<(), ParseError<P::Term, S::NonTerm, S::ReduceActionError>>
where
    P::Term: Hash + Eq + Clone,
    P::NonTerm: Hash + Eq + Clone,
{
    // check if there is any reduce action with given terminal
    while let Some(reduce_rule) =
        parser.get_states()[*context.state_stack.last().unwrap()].reduce(&term)
    {
        let rule = &parser.get_rules()[reduce_rule];
        {
            // pop state stack
            let new_len = context.state_stack.len() - rule.rule.len();
            context.state_stack.truncate(new_len);
        }
        // call reduce action
        context
            .data_stack
            .reduce(reduce_rule, data, &term)
            .map_err(ParseError::ReduceAction)?;

        // construct tree
        #[cfg(feature = "tree")]
        {
            let mut children = Vec::with_capacity(rule.rule.len());
            for _ in 0..rule.rule.len() {
                let tree = context.tree_stack.pop().unwrap();
                children.push(tree);
            }
            children.reverse();

            context
                .tree_stack
                .push(Tree::new_nonterminal(rule.name.clone(), children));
        }

        // shift with reduced nonterminal
        if let Some(next_state_id) =
            parser.get_states()[*context.state_stack.last().unwrap()].shift_goto_nonterm(&rule.name)
        {
            context.state_stack.push(next_state_id);
        } else {
            #[cfg(feature = "error")]
            let expected: Vec<_> = context.expected(parser).cloned().collect();
            #[cfg(feature = "error")]
            let expected_nonterm: Vec<_> = context.expected_nonterm(parser).cloned().collect();
            #[cfg(feature = "error")]
            let backtrace = context.backtrace(parser);

            // this should not happen, if the DFA is built correctly
            let error = InvalidTerminalError {
                term,
                #[cfg(feature = "error")]
                expected,
                #[cfg(feature = "error")]
                expected_nonterm,
                #[cfg(feature = "error")]
                backtrace,
                #[cfg(not(feature = "error"))]
                _phantom: std::marker::PhantomData,
            };
            return Err(ParseError::InvalidTerminal(error));
        }
    }

    let state = &parser.get_states()[*context.state_stack.last().unwrap()];

    // shift with terminal
    if let Some(next_state_id) = state.shift_goto_term(&term) {
        context.state_stack.push(next_state_id);

        #[cfg(feature = "tree")]
        context.tree_stack.push(Tree::new_terminal(term.clone()));

        context.data_stack.push(term);

        Ok(())
    } else {
        #[cfg(feature = "error")]
        let expected: Vec<_> = context.expected(parser).cloned().collect();
        #[cfg(feature = "error")]
        let expected_nonterm: Vec<_> = context.expected_nonterm(parser).cloned().collect();
        #[cfg(feature = "error")]
        let backtrace = context.backtrace(parser);
        let error = InvalidTerminalError {
            term,
            #[cfg(feature = "error")]
            expected,
            #[cfg(feature = "error")]
            expected_nonterm,
            #[cfg(feature = "error")]
            backtrace,
            #[cfg(not(feature = "error"))]
            _phantom: std::marker::PhantomData,
        };
        Err(ParseError::InvalidTerminal(error))
    }
}
