use std::rc::Rc;

use super::Context;

#[cfg(feature = "tree")]
use crate::Tree;

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
        lookahead: &Self::Term,
        userdata: &mut Self::UserData,
    ) -> Result<Self, Self::ReduceActionError>;

    /// get data of start symbol
    fn into_start(self) -> Self::StartType;
}

/// Node represents single shift action in GLR parser.
#[derive(Debug, Clone)]
pub struct Node<Data: NodeData> {
    /// parent node
    pub parent: Option<Rc<Node<Data>>>,
    /// actual data(RuleType) of this node
    pub data: Option<Data>,
    /// index of state in parser
    pub state: usize,

    /// tree representation of this node
    #[cfg(feature = "tree")]
    pub tree: Option<Tree<Data::Term, Data::NonTerm>>,
}

impl<Data: NodeData> Node<Data> {
    /// generate new root node
    pub fn new_root() -> Self {
        Self {
            parent: None,
            data: None,
            state: 0,
            #[cfg(feature = "tree")]
            tree: None,
        }
    }

    /// get token tree for this node
    #[cfg(feature = "tree")]
    pub fn to_tree(&self) -> &Tree<Data::Term, Data::NonTerm>
    where
        Data::Term: Clone,
        Data::NonTerm: Clone,
    {
        self.tree.as_ref().unwrap()
    }
}
