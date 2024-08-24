use std::{hash::Hash, rc::Rc};

use crate::HashSet;

pub trait Node
where
    Self: Sized,
{
    type Term;
    type ReduceActionError;
    type UserData;

    /// Get state index of this node
    fn state(&self) -> usize;
    /// Get parent node
    fn parent(&self) -> Option<&Rc<Self>>;

    /// Make new node with terminal-shift action
    fn make_term_children(parent: Rc<Self>, state: usize, term: Self::Term) -> Self;
    /// Make new node with non-terminal shift action
    fn make_nonterm_children(
        parent: Rc<Self>,
        state: usize,
        rule: usize,
        children_reversed: Vec<Rc<Self>>,
        lookahead: &Self::Term,
        userdata: &mut Self::UserData,
    ) -> Result<Self, Self::ReduceActionError>;

    /// Get rule index of this node.
    /// This is only valid for non-terminal shift node.
    fn rule(&self) -> Option<usize>;
}

pub fn hash_node<H: std::hash::Hasher, N: Node>(node: &N, state: &mut H) {
    node.state().hash(state);
    if let Some(parent) = node.parent() {
        hash_node(parent.as_ref(), state);
    }
}
pub fn eq_node<N: Node>(node1: &N, node2: &N) -> bool {
    if node1.state() != node2.state() {
        return false;
    }
    match (node1.parent(), node2.parent()) {
        (Some(parent1), Some(parent2)) => eq_node(parent1.as_ref(), parent2.as_ref()),
        (None, None) => true,
        _ => false,
    }
}

pub struct NodeSet<N> {
    /// Set of nodes.
    /// Node is equal if their state-stack from the root is equal.
    pub nodes: HashSet<Rc<N>>,
}
impl<N> Default for NodeSet<N> {
    fn default() -> Self {
        NodeSet::new()
    }
}

impl<N> NodeSet<N> {
    pub fn new() -> Self {
        NodeSet {
            nodes: Default::default(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }
}
