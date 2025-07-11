mod context;
mod error;
mod node;
pub mod state;

pub use context::Context;
pub use error::ParseError;
pub use node::Node;
pub use node::NodeRefIterator;

use crate::nonterminal::TokenData;
use crate::parser::Parser;
use std::rc::Rc;

#[cfg(feature = "tree")]
type ReduceArgs<Data> = (
    Rc<Node<Data>>,
    crate::tree::Tree<<Data as TokenData>::Term, <Data as TokenData>::NonTerm>,
);

#[cfg(not(feature = "tree"))]
type ReduceArgs<Data> = Rc<Node<Data>>;

/// from current node, get the last n nodes and create new non-terminal node
/// use Rc::try_unwrap to avoid clone if possible
fn clone_pop_nodes<Data: TokenData + Clone, P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
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
    context.reduce_args.reserve(count);
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
            crate::tree::Tree::new_nonterminal(rule.name.clone(), trees),
        )
    }

    #[cfg(not(feature = "tree"))]
    {
        current_node
    }
}

/// give lookahead token to parser, and check if there is any reduce action.
/// returns false if shift action is revoked
pub(crate) fn reduce<P: Parser, Data: TokenData<Term = P::Term, NonTerm = P::NonTerm> + Clone>(
    parser: &P,
    reduce_rule: usize,
    precedence: Option<usize>,
    node: Rc<Node<Data>>,
    context: &mut Context<Data>,
    term: &P::Term,
    shift: &mut bool,
    userdata: &mut Data::UserData,
) -> Result<Rc<Node<Data>>, Data::ReduceActionError>
where
    P::Term: Clone,
    P::NonTerm: std::hash::Hash + Eq + Clone + std::fmt::Debug,
{
    use crate::Location;
    let mut new_location = Data::Location::new(
        node.iter().map(|node| &node.data.as_ref().unwrap().1),
        parser.get_rules()[reduce_rule].rule.len(),
    );

    context.reduce_args.clear();
    let data_extracted = clone_pop_nodes(node, reduce_rule, parser, context);

    #[cfg(feature = "tree")]
    let (parent, tree) = data_extracted;
    #[cfg(not(feature = "tree"))]
    let parent = data_extracted;

    use crate::parser::State;

    match Data::reduce_action(
        reduce_rule,
        &mut context.reduce_args,
        shift,
        term,
        userdata,
        &mut new_location,
    ) {
        Ok(new_data) => {
            if let Some(nonterm_shift_state) = parser.get_states()[parent.state]
                .shift_goto_nonterm(&parser.get_rules()[reduce_rule].name)
            {
                Ok(Rc::new(Node {
                    parent: Some(parent),
                    data: Some((new_data, new_location)),
                    precedence_level: precedence,
                    state: nonterm_shift_state,
                    #[cfg(feature = "tree")]
                    tree: Some(tree),
                }))
            } else {
                unreachable!(
                    "no shift state for non-terminal: {:?}",
                    parser.get_rules()[reduce_rule].name
                );
            }
        }
        Err(err) => Err(err),
    }
}
