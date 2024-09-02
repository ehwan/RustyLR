use super::Parser;
use super::Stack;

#[cfg(feature = "tree")]
use crate::TreeList;

/// Context for LR parser.
/// This hadles actual data stack for reduce action.
pub struct Context<S: Stack> {
    /// state stack
    pub state_stack: Vec<usize>,

    /// The top of state stack before `feed()` called.
    /// This is used for `expected()` method.
    pub(crate) last_state: usize,

    pub(crate) data_stack: S,

    #[cfg(feature = "tree")]
    pub(crate) tree_stack: TreeList<S::Term, S::NonTerm>,
}

impl<S: Stack> Context<S> {
    /// Create a new context.
    /// `state_stack` is initialized with 0 (root state).
    pub fn new() -> Self
    where
        S: Stack,
    {
        Context {
            state_stack: vec![0],
            last_state: 0,

            data_stack: S::new(),

            #[cfg(feature = "tree")]
            tree_stack: TreeList::new(),
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
    /// Get `TreeList` that current context holds.
    #[cfg(feature = "tree")]
    pub fn to_tree_list(&self) -> TreeList<S::Term, S::NonTerm>
    where
        S::Term: Clone,
        S::NonTerm: Clone,
    {
        self.tree_stack.clone()
    }
    /// For debugging.
    /// Get `TreeList` that current context holds.
    #[cfg(feature = "tree")]
    pub fn into_tree_list(self) -> TreeList<S::Term, S::NonTerm> {
        self.tree_stack
    }

    /// This function should be called after `feed()` returns `Error`.
    /// Get expected tokens for last `feed()` call.
    pub fn expected<'a, P: Parser<Term = S::Term, NonTerm = S::NonTerm>>(
        &self,
        p: &'a P,
    ) -> impl Iterator<Item = &'a S::Term>
    where
        S::Term: 'a,
        S::NonTerm: 'a,
    {
        p.get_states()[self.last_state].expected()
    }
}

impl<S: Stack> Default for Context<S>
where
    S: Stack,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<S: Stack> Clone for Context<S>
where
    S: Clone,
    S::Term: Clone,
    S::NonTerm: Clone,
{
    fn clone(&self) -> Self {
        Context {
            state_stack: self.state_stack.clone(),
            last_state: self.last_state,
            data_stack: self.data_stack.clone(),

            #[cfg(feature = "tree")]
            tree_stack: self.tree_stack.clone(),
        }
    }
}

#[cfg(feature = "tree")]
impl<S: Stack> std::fmt::Display for Context<S>
where
    S::Term: std::fmt::Display + Clone,
    S::NonTerm: std::fmt::Display + Clone,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_tree_list())
    }
}
#[cfg(feature = "tree")]
impl<S: Stack> std::fmt::Debug for Context<S>
where
    S::Term: std::fmt::Debug + Clone,
    S::NonTerm: std::fmt::Debug + Clone,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.to_tree_list())
    }
}

#[cfg(feature = "tree")]
impl<S: Stack> std::ops::Deref for Context<S> {
    type Target = TreeList<S::Term, S::NonTerm>;
    fn deref(&self) -> &Self::Target {
        &self.tree_stack
    }
}

#[cfg(feature = "tree")]
impl<S: Stack> std::ops::DerefMut for Context<S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.tree_stack
    }
}
