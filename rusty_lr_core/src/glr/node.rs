use std::ops::Deref;
use std::ops::DerefMut;
use std::rc::Rc;

use super::Context;

#[cfg(feature = "tree")]
use crate::Tree;
#[cfg(feature = "tree")]
use crate::TreeList;

/// Iterator for traverse node to root.
/// Note that root node is not included in this iterator.
pub struct NodeRefIterator<'a, Data: NodeData> {
    node: Option<&'a Node<Data>>,
}

impl<'a, Data: NodeData> Iterator for NodeRefIterator<'a, Data> {
    type Item = &'a Node<Data>;

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.node?;
        let parent: Option<Self::Item> = node.parent.as_deref();
        if parent.is_some() {
            self.node = parent;
            Some(node)
        } else {
            // if parent is None, it means this node is root node.
            None
        }
    }
}

/// Trait for user-defined data in node.
pub trait NodeData: Sized {
    type Term;
    type NonTerm;
    type UserData;
    type ReduceActionError;

    type StartType;

    fn new_term(term: Self::Term) -> Self;
    fn new_nonterm(
        rule_index: usize,
        context: &mut Context<Self>,
        shift: &mut bool,
        lookahead: &Self::Term,
        userdata: &mut Self::UserData,
    ) -> Result<Self, Self::ReduceActionError>;

    /// get data of start symbol
    fn into_start(self) -> Self::StartType;
}

/// Node represents single shift action in GLR parser.
#[derive(Clone)]
pub struct Node<Data: NodeData> {
    /// parent node
    pub parent: Option<Rc<Node<Data>>>,
    /// index of state in parser
    pub state: usize,

    /// actual data(RuleType) of this node
    pub data: Option<Data>,
    /// tree representation of this node
    #[cfg(feature = "tree")]
    pub tree: Option<Tree<Data::Term, Data::NonTerm>>,
}

impl<Data: NodeData> Node<Data> {
    /// generate new root node
    pub fn new_root() -> Self {
        Self {
            parent: None,
            state: 0,

            data: None,
            #[cfg(feature = "tree")]
            tree: None,
        }
    }

    /// Get token tree for this node.
    /// This function should not be called from root node.
    #[cfg(feature = "tree")]
    pub fn to_tree(&self) -> &Tree<Data::Term, Data::NonTerm> {
        assert!(self.parent.is_some());
        self.tree.as_ref().unwrap()
    }
    /// Get token tree for this node.
    /// This function should not be called from root node.
    #[cfg(feature = "tree")]
    pub fn into_tree(self) -> Tree<Data::Term, Data::NonTerm> {
        assert!(self.parent.is_some());
        self.tree.unwrap()
    }

    /// Get list of token trees from root to this node.
    /// Unlike other [`Iterator`] functions, the order of returned trees is from root to this node.
    /// This function is not *lazy*.
    /// If you don't want expensive operation (`clone()` or `reverse()`), use `iter()` and `to_tree()` instead.
    #[cfg(feature = "tree")]
    pub fn to_tree_list(&self) -> TreeList<Data::Term, Data::NonTerm>
    where
        Data::Term: Clone,
        Data::NonTerm: Clone,
    {
        let mut trees: Vec<Tree<Data::Term, Data::NonTerm>> =
            self.iter().map(|node| node.to_tree().clone()).collect();
        trees.reverse();
        TreeList { trees }
    }

    /// Get data for this node.
    /// This function should not be called from root node.
    pub fn to_data(&self) -> &Data {
        assert!(self.parent.is_some());
        self.data.as_ref().unwrap()
    }
    /// Get data for this node.
    /// This function should not be called from root node.
    pub fn into_data(self) -> Data {
        assert!(self.parent.is_some());
        self.data.unwrap()
    }
    /// Get list of data from root to this node.
    /// Unlike other [`Iterator`] functions, the order of returned data is from root to this node.
    /// This function is not *lazy*.
    /// If you don't want expensive operation (`clone()` or `reverse()`), use `iter()` and `to_tree()` instead.
    pub fn to_data_list(&self) -> Vec<Data>
    where
        Data: Clone,
    {
        let mut data: Vec<Data> = self.iter().map(|node| node.to_data().clone()).collect();
        data.reverse();
        data
    }

    /// Get iterator for traversing from this node to root.
    /// Note that root node is not included in this iterator.
    pub fn iter(&self) -> NodeRefIterator<'_, Data> {
        NodeRefIterator { node: Some(self) }
    }
}

#[cfg(feature = "tree")]
impl<Data: NodeData> Into<Tree<Data::Term, Data::NonTerm>> for Node<Data> {
    fn into(self) -> Tree<Data::Term, Data::NonTerm> {
        self.into_tree()
    }
}

#[cfg(feature = "tree")]
impl<Data: NodeData> std::fmt::Display for Node<Data>
where
    Data::Term: std::fmt::Display,
    Data::NonTerm: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.parent.is_none() {
            write!(f, "Root")
        } else {
            write!(f, "{}", self.to_tree())
        }
    }
}
#[cfg(feature = "tree")]
impl<Data: NodeData> std::fmt::Debug for Node<Data>
where
    Data::Term: std::fmt::Debug,
    Data::NonTerm: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.parent.is_none() {
            write!(f, "Root")
        } else {
            write!(f, "{:?}", self.to_tree())
        }
    }
}

impl<Data: NodeData> Deref for Node<Data> {
    type Target = Data;

    fn deref(&self) -> &Self::Target {
        self.to_data()
    }
}
impl<Data: NodeData> DerefMut for Node<Data> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.data.as_mut().unwrap()
    }
}
