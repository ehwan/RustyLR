pub(crate) mod context;
pub(crate) mod error;
pub(crate) mod parser;
pub(crate) mod state;
pub(crate) mod tree;

pub mod node;

pub use context::Context;
pub use error::InvalidTerminalError;
pub use error::MultiplePathError;
pub use parser::Parser;
pub use state::State;
pub use tree::Tree0;
pub use tree::Tree1;
pub use tree::TreeNonTerminal0;
pub use tree::TreeNonTerminal1;

use std::hash::Hash;
use std::rc::Rc;

use node::Node;

/// feed one terminal to parser, and update state stack.
/// For GLR parsing, this function will create multiple path if needed.
pub fn feed<P: Parser, N: Node<Term = P::Term, NonTerm = P::NonTerm>, C: Context<Node = N>>(
    parser: &P,
    context: &mut C,
    term: P::Term,
    userdata: &mut N::UserData,
) -> Result<(), InvalidTerminalError<P::Term, N::ReduceActionError>>
where
    P::Term: Hash + Eq + Clone,
    P::NonTerm: Hash + Eq + Clone,
{
    let current_nodes = context.take_current_nodes();
    let mut reduce_errors = Vec::new();
    let mut states_list = Vec::with_capacity(current_nodes.nodes.len());
    for node in current_nodes.nodes.into_iter() {
        states_list.push(node.state());
        feed_impl(parser, node, context, &term, userdata, &mut reduce_errors);
    }
    if context.is_empty() {
        let mut expected = parser.get_states()[states_list[0]].expected();
        for state in states_list.into_iter().skip(1) {
            expected = expected
                .union(&parser.get_states()[state].expected())
                .cloned()
                .collect();
        }
        Err(InvalidTerminalError {
            term,
            expected: expected.into_iter().cloned().collect(),
            reduce_errors,
        })
    } else {
        Ok(())
    }
}
/// feed one terminal to parser, and update state stack
fn feed_impl<P: Parser, N: Node<Term = P::Term, NonTerm = P::NonTerm>, C: Context<Node = N>>(
    parser: &P,
    node: Rc<N>,
    context: &mut C,
    term: &P::Term,
    userdata: &mut N::UserData,
    reduce_errors: &mut Vec<N::ReduceActionError>,
) where
    P::Term: Hash + Eq + Clone,
    P::NonTerm: Hash + Eq + Clone,
{
    if let Some(next_state_id) = parser.get_states()[node.state()].shift_goto_term(term) {
        let new_node = N::make_term_children(Rc::clone(&node), next_state_id, term.clone());
        context
            .get_current_nodes_mut()
            .nodes
            .push(Rc::new(new_node));
    }

    lookahead_impl(parser, node, context, term, userdata, reduce_errors);
}
/// give lookahead token to parser, and check if there is any reduce action
fn lookahead_impl<P: Parser, N: Node<Term = P::Term, NonTerm = P::NonTerm>, C: Context<Node = N>>(
    parser: &P,
    node: Rc<N>,
    context: &mut C,
    term: &P::Term,
    userdata: &mut N::UserData,
    reduce_errors: &mut Vec<N::ReduceActionError>,
) where
    P::Term: Hash + Eq + Clone,
    P::NonTerm: Hash + Eq + Clone,
{
    if let Some(reduce_rules) = parser.get_states()[node.state()].reduce(term) {
        for reduce_rule in reduce_rules.iter().skip(1).copied() {
            match N::reduce(
                Rc::clone(&node),
                reduce_rule,
                parser.get_rules()[reduce_rule].id,
                term,
                userdata,
            ) {
                Ok(mut nonterm_shifted_node) => {
                    if let Some(nonterm_shift_state) = parser.get_states()
                        [nonterm_shifted_node.parent().unwrap().state()]
                    .shift_goto_nonterm(&parser.get_rules()[reduce_rule].name)
                    {
                        nonterm_shifted_node.set_state(nonterm_shift_state);
                        feed_impl(
                            parser,
                            Rc::new(nonterm_shifted_node),
                            context,
                            term,
                            userdata,
                            reduce_errors,
                        );
                    }
                }
                Err(err) => {
                    reduce_errors.push(err);
                }
            }
        }
        // Do not clone for the first reduce rule
        match N::reduce(
            node,
            reduce_rules[0],
            parser.get_rules()[reduce_rules[0]].id,
            term,
            userdata,
        ) {
            Ok(mut nonterm_shifted_node) => {
                if let Some(nonterm_shift_state) = parser.get_states()
                    [nonterm_shifted_node.parent().unwrap().state()]
                .shift_goto_nonterm(&parser.get_rules()[reduce_rules[0]].name)
                {
                    nonterm_shifted_node.set_state(nonterm_shift_state);
                    feed_impl(
                        parser,
                        Rc::new(nonterm_shifted_node),
                        context,
                        term,
                        userdata,
                        reduce_errors,
                    );
                }
            }
            Err(err) => {
                reduce_errors.push(err);
            }
        }
    }
}
