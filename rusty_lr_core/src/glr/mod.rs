pub(crate) mod completion;
pub(crate) mod context;
pub(crate) mod error;
pub(crate) mod parser;
pub(crate) mod state;

pub(crate) mod node;

pub use context::Context;
pub use error::InvalidTerminalError;
pub use error::MultiplePathError;
pub use node::Node;
pub use node::NodeData;
pub use node::NodeRefIterator;
pub use parser::Parser;
pub use state::State;

#[cfg(feature = "tree")]
use crate::Tree;

use std::hash::Hash;
use std::rc::Rc;

/// feed one terminal to parser, and update state stack.
/// For GLR parsing, this function will create multiple path if needed.
pub fn feed<P: Parser, Data: NodeData<Term = P::Term, NonTerm = P::NonTerm> + Clone>(
    parser: &P,
    context: &mut Context<Data>,
    term: P::Term,
    userdata: &mut Data::UserData,
) -> Result<(), InvalidTerminalError<P::Term, Data::ReduceActionError>>
where
    P::Term: Hash + Eq + Clone,
    P::NonTerm: Hash + Eq + Clone,
{
    let mut reduce_nodes = std::mem::take(&mut context.current_nodes);
    context.nodes_pong.clear();

    context.state_list.clear();
    context.reduce_errors.clear();

    // BFS reduce
    while !reduce_nodes.is_empty() {
        for (state, nodes) in reduce_nodes.drain() {
            let next_term_shift_state = parser.get_states()[state].shift_goto_term(&term);
            context.state_list.push(state);
            if let Some(reduce_rules) = parser.get_states()[state].reduce(&term) {
                for node in nodes.into_iter() {
                    let mut shift_for_this_node = false;

                    // In reduce action, we call `Rc::try_unwrap` to avoid `clone()` data if possible.
                    // So we need to avoid `Rc::clone()` if possible.
                    for reduce_rule in reduce_rules.iter().skip(1).copied() {
                        shift_for_this_node |= reduce(
                            parser,
                            reduce_rule,
                            Rc::clone(&node),
                            context,
                            &term,
                            userdata,
                        );
                    }
                    if let Some(next_term_shift_state) = next_term_shift_state {
                        shift_for_this_node |= reduce(
                            parser,
                            reduce_rules[0],
                            Rc::clone(&node),
                            context,
                            &term,
                            userdata,
                        );
                        if shift_for_this_node {
                            let next_node = Node {
                                parent: Some(node),
                                state: next_term_shift_state,
                                data: Some(Data::new_term(term.clone())),
                                #[cfg(feature = "tree")]
                                tree: Some(Tree::new_terminal(term.clone())),
                            };

                            context
                                .current_nodes
                                .entry(next_term_shift_state)
                                .or_default()
                                .push(Rc::new(next_node));
                        }
                    } else {
                        reduce(parser, reduce_rules[0], node, context, &term, userdata);
                    }
                }
            } else if let Some(next_term_shift_state) = next_term_shift_state {
                for node in nodes.into_iter() {
                    let next_node = Node {
                        parent: Some(node),
                        state: next_term_shift_state,
                        data: Some(Data::new_term(term.clone())),
                        #[cfg(feature = "tree")]
                        tree: Some(Tree::new_terminal(term.clone())),
                    };

                    context
                        .current_nodes
                        .entry(next_term_shift_state)
                        .or_default()
                        .push(Rc::new(next_node));
                }
            }
        }
        std::mem::swap(&mut reduce_nodes, &mut context.nodes_pong);
    }

    if context.current_nodes.is_empty() {
        Err(InvalidTerminalError {
            term,
            reduce_errors: std::mem::take(&mut context.reduce_errors),
        })
    } else {
        Ok(())
    }
}

#[cfg(feature = "tree")]
type ReduceArgs<Data> = (
    Rc<Node<Data>>,
    Tree<<Data as NodeData>::Term, <Data as NodeData>::NonTerm>,
);

#[cfg(not(feature = "tree"))]
type ReduceArgs<Data> = Rc<Node<Data>>;

/// from current node, get the last n nodes and create new non-terminal node
/// use Rc::try_unwrap to avoid clone if possible
fn clone_pop_nodes<Data: NodeData + Clone, P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
    node: Rc<Node<Data>>,
    rule_index: usize,
    parser: &P,
    context: &mut Context<Data>,
) -> ReduceArgs<Data>
where
    P::Term: Clone,
    P::NonTerm: Clone,
{
    let rule = &parser.get_rules()[rule_index];
    let count = rule.rule.len();

    #[cfg(feature = "tree")]
    let mut trees = Vec::with_capacity(count);

    let mut current_node = node;
    for _ in 0..count {
        let data = match Rc::try_unwrap(current_node) {
            Ok(node) => {
                let data = node.data.unwrap();

                #[cfg(feature = "tree")]
                {
                    let tree = node.tree.unwrap();
                    trees.push(tree);
                }

                current_node = node.parent.unwrap();
                data
            }
            Err(rc_node) => {
                let data = rc_node.data.as_ref().unwrap().clone();
                #[cfg(feature = "tree")]
                {
                    let tree = rc_node.tree.as_ref().unwrap().clone();
                    trees.push(tree);
                }
                current_node = Rc::clone(rc_node.parent.as_ref().unwrap());
                data
            }
        };
        context.reduce_args.push(data);
    }

    #[cfg(feature = "tree")]
    {
        trees.reverse();
        (
            current_node,
            Tree::new_nonterminal(rule.name.clone(), trees),
        )
    }

    #[cfg(not(feature = "tree"))]
    {
        current_node
    }
}
/// give lookahead token to parser, and check if there is any reduce action.
/// returns false if shift action is revoked
fn reduce<P: Parser, Data: NodeData<Term = P::Term, NonTerm = P::NonTerm> + Clone>(
    parser: &P,
    reduce_rule: usize,
    node: Rc<Node<Data>>,
    context: &mut Context<Data>,
    term: &P::Term,
    userdata: &mut Data::UserData,
) -> bool
where
    P::Term: Hash + Eq + Clone,
    P::NonTerm: Hash + Eq + Clone,
{
    context.reduce_args.clear();
    let data_extracted = clone_pop_nodes(Rc::clone(&node), reduce_rule, parser, context);

    #[cfg(feature = "tree")]
    let (parent, tree) = data_extracted;
    #[cfg(not(feature = "tree"))]
    let parent = data_extracted;

    let mut do_shift = true;
    match Data::new_nonterm(
        reduce_rule,
        &mut context.reduce_args,
        &mut do_shift,
        term,
        userdata,
    ) {
        Ok(new_data) => {
            if let Some(nonterm_shift_state) = parser.get_states()[parent.state]
                .shift_goto_nonterm(&parser.get_rules()[reduce_rule].name)
            {
                let new_node = Node {
                    parent: Some(parent),
                    data: Some(new_data),
                    state: nonterm_shift_state,
                    #[cfg(feature = "tree")]
                    tree: Some(tree),
                };

                context
                    .nodes_pong
                    .entry(nonterm_shift_state)
                    .or_default()
                    .push(Rc::new(new_node));
            }
        }
        Err(err) => {
            context.reduce_errors.push(err);
        }
    }
    do_shift
}
