pub(crate) mod context;
pub(crate) mod error;
pub(crate) mod parser;
pub(crate) mod state;

pub mod node;

pub use context::Context;
pub use error::InvalidTerminalError;
pub use error::MultiplePathError;
pub use error::ParseError;
pub use parser::Parser;
pub use state::State;

use std::hash::Hash;
use std::rc::Rc;

use node::Node;

/// feed one terminal to parser, and update state stack.
/// For GLR parsing, this function will create multiple path if needed.
pub fn feed<P: Parser, N: Node<Term = P::Term> + Hash + Eq, C: Context<Node = N>>(
    parser: &P,
    context: &mut C,
    term: P::Term,
    userdata: &mut N::UserData,
) -> Result<(), ParseError<P::Term, P::NonTerm, N::ReduceActionError>>
where
    P::Term: Hash + Eq + Clone,
    P::NonTerm: Hash + Eq + Clone,
{
    let current_nodes = context.take_current_nodes();
    let mut reduce_errors = Vec::new();
    let mut states_list = Vec::with_capacity(current_nodes.nodes.len());
    for node in current_nodes.nodes.into_iter() {
        states_list.push(node.state());
        feed_impl(parser, node, context, &term, userdata, &mut reduce_errors)?;
    }
    if context.is_empty() {
        let mut expected = parser.get_states()[states_list[0]].expected();
        for state in states_list.into_iter().skip(1) {
            expected = expected
                .union(&parser.get_states()[state].expected())
                .cloned()
                .collect();
        }
        Err(ParseError::InvalidTerminal(InvalidTerminalError {
            term,
            expected: expected.into_iter().cloned().collect(),
            reduce_errors,
        }))
    } else {
        Ok(())
    }
}
/// feed one terminal to parser, and update state stack
fn feed_impl<P: Parser, N: Node<Term = P::Term> + Hash + Eq, C: Context<Node = N>>(
    parser: &P,
    mut node: Rc<N>,
    context: &mut C,
    term: &P::Term,
    userdata: &mut N::UserData,
    reduce_errors: &mut Vec<N::ReduceActionError>,
) -> Result<(), ParseError<P::Term, P::NonTerm, N::ReduceActionError>>
where
    P::Term: Hash + Eq + Clone,
    P::NonTerm: Hash + Eq + Clone,
{
    node = lookahead_impl(
        parser,
        Rc::clone(&node),
        context,
        term,
        userdata,
        reduce_errors,
    )?;

    if let Some(next_state_id) = parser.get_states()[node.state()].shift_goto_term(term) {
        let node_rule = node.rule();
        let new_node = N::make_term_children(node, next_state_id, term.clone());
        // check conflict
        if let Some(old) = context.get_current_nodes_mut().nodes.take(&new_node) {
            let rule1 = old.parent().unwrap().rule().unwrap();
            let rule2 = node_rule.unwrap();

            return Err(ParseError::MultiplePath(MultiplePathError {
                rule1: parser.get_rules()[rule1].clone(),
                rule2: parser.get_rules()[rule2].clone(),
            }));
        } else {
            context
                .get_current_nodes_mut()
                .nodes
                .insert(Rc::new(new_node));
        }
    }
    Ok(())
}
/// give lookahead token to parser, and check if there is any reduce action
fn lookahead_impl<P: Parser, N: Node<Term = P::Term> + Hash + Eq, C: Context<Node = N>>(
    parser: &P,
    node: Rc<N>,
    context: &mut C,
    term: &P::Term,
    userdata: &mut N::UserData,
    reduce_errors: &mut Vec<N::ReduceActionError>,
) -> Result<Rc<N>, ParseError<P::Term, P::NonTerm, N::ReduceActionError>>
where
    P::Term: Hash + Eq + Clone,
    P::NonTerm: Hash + Eq + Clone,
{
    if let Some(reduce_rules) = parser.get_states()[node.state()].reduce(term) {
        for reduce_rule in reduce_rules.iter().copied() {
            let mut children_reversed = Vec::new();
            let mut n = Rc::clone(&node);
            let rule = &parser.get_rules()[reduce_rule];
            for _ in 0..rule.rule.len() {
                let parent = Rc::clone(n.parent().unwrap());
                children_reversed.push(n);
                n = parent;
            }

            if let Some(next_shift_nonterm) =
                parser.get_states()[n.state()].shift_goto_nonterm(&rule.name)
            {
                match N::make_nonterm_children(
                    n,
                    next_shift_nonterm,
                    reduce_rule,
                    children_reversed,
                    term,
                    userdata,
                ) {
                    Ok(new_node) => {
                        feed_impl(
                            parser,
                            Rc::new(new_node),
                            context,
                            term,
                            userdata,
                            reduce_errors,
                        )?;
                    }
                    Err(err) => {
                        reduce_errors.push(err);
                    }
                }
            }
        }
    }
    Ok(node)
}
