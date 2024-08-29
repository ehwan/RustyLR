use super::Stack;

#[cfg(feature = "tree")]
use crate::Tree;
#[cfg(feature = "tree")]
use crate::TreeList;

/// Context for LR parser.
/// This hadles actual data stack for reduce action.
#[derive(Debug, Clone)]
pub struct Context<S: Stack> {
    /// state stack
    pub state_stack: Vec<usize>,
    pub(crate) data_stack: S,

    #[cfg(feature = "tree")]
    pub(crate) tree_stack: Vec<Tree<S::Term, S::NonTerm>>,
}

impl<S: Stack> Context<S> {
    pub fn new() -> Self
    where
        S: Stack,
    {
        Context {
            state_stack: vec![0],
            data_stack: S::new(),

            #[cfg(feature = "tree")]
            tree_stack: Vec::new(),
        }
    }
    /// pop value from start rule
    #[inline]
    pub fn accept(&mut self) -> S::StartType
    where
        S: Stack,
    {
        self.data_stack.pop_start()
    }

    /// For debugging.
    /// Get `TreeList` current context holds.
    #[cfg(feature = "tree")]
    pub fn to_tree_list(&self) -> TreeList<S::Term, S::NonTerm>
    where
        S::Term: Clone,
        S::NonTerm: Clone,
    {
        TreeList {
            trees: self.tree_stack.clone(),
        }
    }
}
