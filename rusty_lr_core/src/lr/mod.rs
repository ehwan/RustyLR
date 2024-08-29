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
    mut term: P::Term,
    data: &mut S::UserData,
) -> Result<(), ParseError<P::Term, S::ReduceActionError>>
where
    P::Term: Hash + Eq + Clone,
    P::NonTerm: Hash + Eq + Clone,
{
    term = lookahead(parser, context, term, data)?;
    let state = &parser.get_states()[*context.state_stack.last().unwrap()];
    if let Some(next_state_id) = state.shift_goto_term(&term) {
        context.state_stack.push(next_state_id);

        #[cfg(feature = "tree")]
        context.tree_stack.push(Tree::new_terminal(term.clone()));

        context.data_stack.push(term);

        Ok(())
    } else {
        let error = InvalidTerminalError {
            term,
            expected: state.expected().into_iter().cloned().collect(),
        };
        Err(ParseError::InvalidTerminal(error))
    }
}
/// give lookahead token to parser, and check if there is any reduce action
fn lookahead<P: Parser, S: Stack<Term = P::Term, NonTerm = P::NonTerm>>(
    parser: &P,
    context: &mut Context<S>,
    term: P::Term,
    data: &mut S::UserData,
) -> Result<P::Term, ParseError<P::Term, S::ReduceActionError>>
where
    P::Term: Hash + Eq + Clone,
    P::NonTerm: Hash + Eq + Clone,
{
    if let Some(reduce_rule) =
        parser.get_states()[*context.state_stack.last().unwrap()].reduce(&term)
    {
        let rule = &parser.get_rules()[reduce_rule];
        {
            let new_len = context.state_stack.len() - rule.rule.len();
            context.state_stack.truncate(new_len);
        }
        context
            .data_stack
            .reduce(reduce_rule, data, &term)
            .map_err(ParseError::ReduceAction)?;

        #[cfg(feature = "tree")]
        {
            let mut children = Vec::new();
            for _ in 0..rule.rule.len() {
                let tree = context.tree_stack.pop().unwrap();
                children.push(tree);
            }
            children.reverse();

            context
                .tree_stack
                .push(Tree::new_nonterminal(rule.name.clone(), children));
        }

        if let Some(next_state_id) =
            parser.get_states()[*context.state_stack.last().unwrap()].shift_goto_nonterm(&rule.name)
        {
            context.state_stack.push(next_state_id);
            lookahead(parser, context, term, data)
        } else {
            Err(ParseError::InvalidTerminal(InvalidTerminalError {
                term,
                expected: parser.get_states()[*context.state_stack.last().unwrap()]
                    .expected()
                    .into_iter()
                    .cloned()
                    .collect(),
            }))
        }
    } else {
        Ok(term)
    }
}
