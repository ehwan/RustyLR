use std::rc::Rc;

use super::{MultiplePathError, Parser, Tree1};

/// Node represents single shift action in GLR parser.
pub trait Node
where
    Self: Sized,
{
    type Term;
    type NonTerm;
    type ReduceActionError;
    type UserData;

    /// Get state index of this node
    fn state(&self) -> usize;

    fn set_state(&mut self, state: usize);

    /// Get parent node
    fn parent(&self) -> Option<&Rc<Self>>;

    /// Make new node with terminal-shift action from this node
    fn make_term_children(parent: Rc<Self>, state: usize, term: Self::Term) -> Self;

    /// Take N nodes from this node to its parent.
    /// Reduce action will be called with taken nodes.
    fn reduce(
        node: Rc<Self>,
        rule: usize,
        rule_id: usize,
        lookahead: &Self::Term,
        userdata: &mut Self::UserData,
    ) -> Result<Self, Self::ReduceActionError>;

    /// Get tree representation of this node.
    /// This tree contains only 2-height nodes.
    fn tree(&self) -> Option<&Tree1>;
}

/*
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
*/

pub struct NodeSet<N> {
    /// Set of nodes.
    /// Node is equal if their state-stack from the root is equal.
    pub nodes: Vec<Rc<N>>,
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

    pub fn accept<P: Parser<Term = N::Term, NonTerm = N::NonTerm>>(
        self,
        parser: &P,
    ) -> Result<Rc<N>, MultiplePathError<N::Term, N::NonTerm>>
    where
        N: Node,
        N::Term: Clone,
        N::NonTerm: Clone,
    {
        if self.nodes.len() == 1 {
            let mut it = self.nodes.into_iter();
            Ok(Rc::clone(it.next().unwrap().parent().unwrap()))
        } else {
            Err(MultiplePathError::from_tree1(
                self.nodes
                    .iter()
                    .map(|node| node.parent().unwrap().tree().unwrap()),
                parser,
            ))
        }
    }
}
