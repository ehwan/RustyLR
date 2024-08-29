pub(crate) mod context;
pub(crate) mod error;
pub(crate) mod parser;
pub(crate) mod state;

pub mod node;

pub use context::Context;
pub use error::InvalidTerminalError;
pub use error::MultiplePathError;
pub use node::Node;
pub use node::NodeData;
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
    let current_nodes = std::mem::take(&mut context.current_nodes);
    context.current_nodes.reserve(current_nodes.len());
    context.reduce_errors.clear();
    context.state_list.clear();
    context.state_list.reserve(current_nodes.len());
    for node in current_nodes.into_iter() {
        context.state_list.push(node.state);
        feed_impl(parser, node, context, &term, userdata);
    }
    if context.current_nodes.is_empty() {
        let mut expected = parser.get_states()[context.state_list[0]].expected();
        for state in context.state_list.iter().skip(1) {
            expected = expected
                .union(&parser.get_states()[*state].expected())
                .cloned()
                .collect();
        }
        Err(InvalidTerminalError {
            term,
            expected: expected.into_iter().cloned().collect(),
            reduce_errors: std::mem::take(&mut context.reduce_errors),
        })
    } else {
        Ok(())
    }
}
/// feed one terminal to parser, and update state stack
fn feed_impl<P: Parser, Data: NodeData<Term = P::Term, NonTerm = P::NonTerm> + Clone>(
    parser: &P,
    node: Rc<Node<Data>>,
    context: &mut Context<Data>,
    term: &P::Term,
    userdata: &mut Data::UserData,
) where
    P::Term: Hash + Eq + Clone,
    P::NonTerm: Hash + Eq + Clone,
{
    if let Some(next_state_id) = parser.get_states()[node.state].shift_goto_term(term) {
        let new_node = Node {
            parent: Some(Rc::clone(&node)),
            data: Some(Data::new_term(term.clone())),
            state: next_state_id,
            #[cfg(feature = "tree")]
            tree: Some(Tree::Terminal(term.clone())),
        };
        context.current_nodes.push(Rc::new(new_node));
    }

    lookahead_impl(parser, node, context, term, userdata);
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

/// give lookahead token to parser, and check if there is any reduce action
fn lookahead_impl<P: Parser, Data: NodeData<Term = P::Term, NonTerm = P::NonTerm> + Clone>(
    parser: &P,
    node: Rc<Node<Data>>,
    context: &mut Context<Data>,
    term: &P::Term,
    userdata: &mut Data::UserData,
) where
    P::Term: Hash + Eq + Clone,
    P::NonTerm: Hash + Eq + Clone,
{
    if let Some(reduce_rules) = parser.get_states()[node.state].reduce(term) {
        for reduce_rule in reduce_rules.iter().skip(1).copied() {
            let reduce_args = clone_pop_nodes(Rc::clone(&node), reduce_rule, parser, context);

            #[cfg(feature = "tree")]
            let (parent, tree) = reduce_args;
            #[cfg(not(feature = "tree"))]
            let parent = reduce_args;

            match Data::new_nonterm(reduce_rule, context, term, userdata) {
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

                        feed_impl(parser, Rc::new(new_node), context, term, userdata);
                    }
                }
                Err(err) => {
                    context.reduce_errors.push(err);
                }
            }
        }
        // Do not clone for the first reduce rule
        {
            let reduce_args = clone_pop_nodes(node, reduce_rules[0], parser, context);

            #[cfg(feature = "tree")]
            let (parent, tree) = reduce_args;
            #[cfg(not(feature = "tree"))]
            let parent = reduce_args;

            match Data::new_nonterm(reduce_rules[0], context, term, userdata) {
                Ok(new_data) => {
                    if let Some(nonterm_shift_state) = parser.get_states()[parent.state]
                        .shift_goto_nonterm(&parser.get_rules()[reduce_rules[0]].name)
                    {
                        let new_node = Node {
                            parent: Some(parent),
                            data: Some(new_data),
                            state: nonterm_shift_state,
                            #[cfg(feature = "tree")]
                            tree: Some(tree),
                        };

                        feed_impl(parser, Rc::new(new_node), context, term, userdata);
                    }
                }
                Err(err) => {
                    context.reduce_errors.push(err);
                }
            }
        }
    }
}

/// For debugging.
/// Print last n tokens for node.
#[cfg(feature = "tree")]
pub fn backtrace<Data: NodeData>(num_tokens: usize, mut node: Rc<Node<Data>>)
where
    Data::Term: Clone + std::fmt::Display,
    Data::NonTerm: Clone + std::fmt::Display,
{
    let mut nodes = Vec::new();
    while let Some(par) = node.parent.clone() {
        nodes.push(node);
        node = par;
        if nodes.len() == num_tokens {
            break;
        }
    }
    for n in nodes.into_iter().rev() {
        let tree = n.tree.clone().unwrap();
        print!("{}", tree);
    }
}

/// For debugging.
/// Print last n tokens for node.
/// This uses `Debug` trait to print tokens.
#[cfg(feature = "tree")]
pub fn backtrace_debug<Data: NodeData>(num_tokens: usize, mut node: Rc<Node<Data>>)
where
    Data::Term: Clone + std::fmt::Debug,
    Data::NonTerm: Clone + std::fmt::Debug,
{
    let mut nodes = Vec::new();
    while let Some(par) = node.parent.clone() {
        nodes.push(node);
        node = par;
        if nodes.len() == num_tokens {
            break;
        }
    }
    for n in nodes.into_iter().rev() {
        let tree = n.tree.clone().unwrap();
        print!("{:?}", tree);
    }
}
