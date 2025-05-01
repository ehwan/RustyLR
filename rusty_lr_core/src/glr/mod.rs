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
pub use state::DenseState;
pub use state::SparseState;
pub use state::State;

use crate::HashMap;
#[cfg(feature = "tree")]
use crate::Tree;

use std::hash::Hash;
use std::rc::Rc;

/// feed one terminal to parser, and update state stack.
/// For GLR parsing, this function will create multiple path if needed.
pub(crate) fn feed<P: Parser, Data: NodeData<Term = P::Term, NonTerm = P::NonTerm> + Clone>(
    parser: &P,
    context: &mut Context<Data>,
    term: P::Term,
    userdata: &mut Data::UserData,
) -> Result<(), InvalidTerminalError<P::Term, P::NonTerm, Data::ReduceActionError>>
where
    P::Term: Hash + Eq + Clone,
    P::NonTerm: Hash + Eq + Clone,
{
    // current_nodes <-> nodes_pong <-> nodes_pong2
    // cycle for no unnecessary heap allocation
    let mut reduce_nodes = std::mem::take(&mut context.current_nodes);
    std::mem::swap(&mut context.current_nodes, &mut context.nodes_pong2);
    context.current_nodes.clear();
    // here, nodes_pong2 is newlly created by `Default`, and we will assign it from `reduce_nodes` later
    context.nodes_pong.clear();
    context.reduce_errors.clear();
    context.fallback_nodes.clear();

    let class = parser.to_terminal_class(&term);

    // BFS reduce
    while !reduce_nodes.is_empty() {
        for (state, nodes) in reduce_nodes.drain() {
            let next_term_shift_state = parser.get_states()[state].shift_goto_class(class);
            if let Some(reduce_rules) = parser.get_states()[state].reduce(class) {
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
                            next_term_shift_state.is_some(),
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
                            true,
                            userdata,
                        );
                        if shift_for_this_node {
                            // some shift action was performed; remove fallback_nodes immediately
                            // to avoid cloned Rc nodes
                            context.fallback_nodes.clear();

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
                        reduce(
                            parser,
                            reduce_rules[0],
                            node,
                            context,
                            &term,
                            false,
                            userdata,
                        );
                    }
                }
            } else if let Some(next_term_shift_state) = next_term_shift_state {
                for node in nodes.into_iter() {
                    // some shift action was performed; remove fallback_nodes immediately
                    // to avoid cloned Rc nodes
                    context.fallback_nodes.clear();

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
                // no reduce, no shift
                // add to fallback_nodes to restore if any shift action was performed
                if context.current_nodes.is_empty() {
                    context.fallback_nodes.insert(state, nodes);
                }
            }
        }
        std::mem::swap(&mut reduce_nodes, &mut context.nodes_pong);
    }
    context.nodes_pong2 = reduce_nodes;

    // no shift possible; invalid terminal was given
    // restore nodes to original state from fallback_nodes
    if context.current_nodes.is_empty() {
        std::mem::swap(&mut context.current_nodes, &mut context.fallback_nodes);
        context.fallback_nodes.clear();

        #[cfg(feature = "error")]
        let backtraces = context.backtraces(parser).collect::<Vec<_>>();

        Err(InvalidTerminalError {
            term,
            reduce_errors: std::mem::take(&mut context.reduce_errors),
            #[cfg(feature = "error")]
            backtraces,

            #[cfg(not(feature = "error"))]
            _phantom: std::marker::PhantomData,
        })
    } else {
        context.fallback_nodes.clear();
        Ok(())
    }
}

pub(crate) fn feed_multiple<
    P: Parser,
    Data: NodeData<Term = P::Term, NonTerm = P::NonTerm> + Clone,
>(
    parser: &P,
    context: &mut Context<Data>,
    terms: impl Iterator<Item = P::Term>,
    userdata: &mut Data::UserData,
) -> Result<(), Vec<InvalidTerminalError<P::Term, P::NonTerm, Data::ReduceActionError>>>
where
    P::Term: Hash + Eq + Clone,
    P::NonTerm: Hash + Eq + Clone,
{
    // current_nodes <-> nodes_pong <-> nodes_pong2
    // cycle for no unnecessary heap allocation
    let mut reduce_nodes = std::mem::take(&mut context.current_nodes);
    std::mem::swap(&mut context.current_nodes, &mut context.nodes_pong2);
    context.current_nodes.clear();
    // here, nodes_pong2 is newlly created by `Default`, and we will assign it from `reduce_nodes` later
    context.nodes_pong.clear();
    context.fallback_nodes.clear();

    let mut reduce_errors: HashMap<P::Term, _> = HashMap::default();

    for term in terms {
        let class = parser.to_terminal_class(&term);
        let mut reduce_nodes = reduce_nodes.clone();

        context.reduce_errors.clear();

        // BFS reduce
        while !reduce_nodes.is_empty() {
            for (state, nodes) in reduce_nodes.drain() {
                let next_term_shift_state = parser.get_states()[state].shift_goto_class(class);
                if let Some(reduce_rules) = parser.get_states()[state].reduce(class) {
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
                                next_term_shift_state.is_some(),
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
                                true,
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
                            reduce(
                                parser,
                                reduce_rules[0],
                                node,
                                context,
                                &term,
                                false,
                                userdata,
                            );
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
                } else {
                }
            }
            std::mem::swap(&mut reduce_nodes, &mut context.nodes_pong);
        }

        if context.current_nodes.is_empty() {
            reduce_errors.insert(term, std::mem::take(&mut context.reduce_errors));
        }
    }

    // no shift possible; invalid terminal was given
    // restore nodes to original state
    if context.current_nodes.is_empty() {
        std::mem::swap(&mut context.current_nodes, &mut reduce_nodes);
        // restore nodes_pong2 to avoid unnecessary heap allocation
        reduce_nodes.clear();
        context.nodes_pong2 = reduce_nodes;

        #[cfg(feature = "error")]
        let backtraces = context.backtraces(parser).collect::<Vec<_>>();

        let errors = reduce_errors
            .into_iter()
            .map(|(term, errors)| {
                let error = InvalidTerminalError {
                    term,
                    reduce_errors: errors,
                    #[cfg(feature = "error")]
                    backtraces: backtraces.clone(),

                    #[cfg(not(feature = "error"))]
                    _phantom: std::marker::PhantomData,
                };
                error
            })
            .collect();

        Err(errors)
    } else {
        // restore nodes_pong2 to avoid unnecessary heap allocation
        reduce_nodes.clear();
        context.nodes_pong2 = reduce_nodes;
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
    has_shift: bool,
    userdata: &mut Data::UserData,
) -> bool
where
    P::Term: Hash + Eq + Clone,
    P::NonTerm: Hash + Eq + Clone,
{
    context.reduce_args.clear();
    let data_extracted = clone_pop_nodes(node, reduce_rule, parser, context);

    #[cfg(feature = "tree")]
    let (parent, tree) = data_extracted;
    #[cfg(not(feature = "tree"))]
    let parent = data_extracted;

    let mut do_shift = has_shift;
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
            if context.current_nodes.is_empty() {
                context.reduce_errors.push(err);
            }
        }
    }
    do_shift
}
