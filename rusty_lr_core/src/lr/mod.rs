pub(crate) mod context;
pub(crate) mod error;
pub(crate) mod parser;
pub(crate) mod state;

use std::hash::Hash;

pub use context::Context;
pub use error::InvalidTerminalError;
pub use error::ParseError;
pub use parser::Parser;
pub use state::State;

/// feed one terminal to parser, and update state & data stack
pub fn feed<P: Parser, C: Context<Term = P::Term>>(
    parser: &P,
    context: &mut C,
    mut term: P::Term,
    data: &mut C::UserData,
) -> Result<(), ParseError<P::Term, C::ReduceActionError>>
where
    P::Term: Hash + Eq + Clone,
    P::NonTerm: Hash + Eq,
{
    term = lookahead(parser, context, term, data)?;
    let state = &parser.get_states()[*context.get_state_stack().last().unwrap()];
    if let Some(next_state_id) = state.shift_goto_term(&term) {
        context.get_state_stack_mut().push(next_state_id);
        context.push(term);
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
fn lookahead<P: Parser, C: Context<Term = P::Term>>(
    parser: &P,
    context: &mut C,
    term: P::Term,
    data: &mut C::UserData,
) -> Result<P::Term, ParseError<P::Term, C::ReduceActionError>>
where
    P::Term: Hash + Eq + Clone,
    P::NonTerm: Hash + Eq,
{
    if let Some(reduce_rule) =
        parser.get_states()[*context.get_state_stack().last().unwrap()].reduce(&term)
    {
        let rule = &parser.get_rules()[reduce_rule];
        {
            let new_len = context.get_state_stack().len() - rule.rule.len();
            context.get_state_stack_mut().truncate(new_len);
        }
        context
            .reduce(reduce_rule, data, &term)
            .map_err(ParseError::ReduceAction)?;
        if let Some(next_state_id) = parser.get_states()[*context.get_state_stack().last().unwrap()]
            .shift_goto_nonterm(&rule.name)
        {
            context.get_state_stack_mut().push(next_state_id);
            lookahead(parser, context, term, data)
        } else {
            Err(ParseError::InvalidTerminal(InvalidTerminalError {
                term,
                expected: parser.get_states()[*context.get_state_stack().last().unwrap()]
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
