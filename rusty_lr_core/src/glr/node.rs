use std::rc::Rc;

use super::Tree1;

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
        data_args: Vec<Self>,
        lookahead: &Self::Term,
        userdata: &mut Self::UserData,
    ) -> Result<Self, Self::ReduceActionError>;

    /// get data of start symbol
    fn into_start(self) -> Self::StartType;
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
