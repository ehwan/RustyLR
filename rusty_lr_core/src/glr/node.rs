use std::{fmt::Display, rc::Rc};

use super::{MultiplePathError, Parser, Tree1};

/// Trait for user-defined data in node.
pub trait NodeData: Sized {
    type Term;
    type NonTerm;
    type UserData;
    type ReduceActionError;

    fn new_term(term: Self::Term) -> Self;
    fn new_nonterm(
        rule_index: usize,
        data_args: Vec<Self>,
        lookahead: &Self::Term,
        userdata: &mut Self::UserData,
    ) -> Result<Self, Self::ReduceActionError>;
}

/// Node represents single shift action in GLR parser.
#[derive(Debug, Clone)]
pub struct Node<Data> {
    /// parent node
    pub parent: Option<Rc<Node<Data>>>,
    /// tree representation of this node
    pub tree: Option<Tree1>,
    /// actual data(RuleType) of this node
    pub data: Option<Data>,
    /// index of state in parser
    pub state: usize,
}

impl<Data> Node<Data> {
    /// generate new root node
    pub fn new_root() -> Self {
        Self {
            parent: None,
            tree: None,
            data: None,
            state: 0,
        }
    }
}

pub struct NodeSet<Data> {
    /// Set of nodes.
    /// Node is equal if their state-stack from the root is equal.
    pub nodes: Vec<Rc<Node<Data>>>,
}
impl<Data> Default for NodeSet<Data> {
    fn default() -> Self {
        NodeSet::new()
    }
}

impl<Data> NodeSet<Data> {
    /// crate new empty NodeSet
    pub fn new() -> Self {
        NodeSet {
            nodes: Default::default(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    pub fn accept<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        self,
        parser: &P,
    ) -> Result<Rc<Node<Data>>, MultiplePathError<Data::Term, Data::NonTerm>>
    where
        Data: NodeData,
        Data::Term: Clone,
        Data::NonTerm: Clone,
    {
        if self.nodes.len() == 1 {
            let mut it = self.nodes.into_iter();
            let eof_node = it.next().unwrap();
            let eof_node = Rc::into_inner(eof_node).unwrap();
            let node = eof_node.parent.unwrap();
            Ok(node)
        } else {
            Err(MultiplePathError::from_tree1(
                self.nodes
                    .iter()
                    .map(|node| node.parent.as_ref().unwrap().tree.as_ref().unwrap()),
                parser,
            ))
        }
    }

    /// For debugging.
    /// Print last n tokens for every node in this set.
    pub fn backtrace<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        token_count: usize,
        parser: &P,
    ) where
        Data: NodeData,
        P::Term: Clone + Display,
        P::NonTerm: Clone + Display,
    {
        for node in self.nodes.iter() {
            super::backtrace(token_count, Rc::clone(node), parser);
            println!();
        }
    }
}
