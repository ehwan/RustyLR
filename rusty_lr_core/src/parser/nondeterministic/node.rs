use crate::parser::semantic_value::SemanticValue;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct NodeId(usize);

impl NodeId {
    pub(crate) fn new(index: usize) -> Self {
        NodeId(index)
    }

    pub(crate) fn index(self) -> usize {
        self.0
    }
}

/// A segment of one deterministic subpath in the GLR graph-structured stack.
///
/// A full nondeterministic parse path is represented by walking from an active
/// leaf node through its parent links. Each `Node` stores a maximal contiguous
/// LR stack segment that can still be handled deterministically. Splitting a
/// path creates new nodes only at branch points, so grammars that do not branch
/// keep memory usage and stack operations close to the deterministic parser.
#[derive(Clone)]
pub struct Node<Data: SemanticValue, StateIndex> {
    pub(super) parent: Option<NodeId>,

    pub(super) child_count: usize,

    pub(super) state_stack: Vec<StateIndex>,
    pub(super) data_stack: Vec<Data>,
    pub(super) location_stack: Vec<Data::Location>,
    #[cfg(feature = "tree")]
    pub(super) tree_stack: Vec<crate::tree::Tree<Data::Term, Data::NonTerm>>,
}

impl<Data: SemanticValue, StateIndex> Default for Node<Data, StateIndex> {
    fn default() -> Self {
        Node {
            parent: None,
            child_count: 0,
            state_stack: Vec::new(),
            data_stack: Vec::new(),
            location_stack: Vec::new(),
            #[cfg(feature = "tree")]
            tree_stack: Vec::new(),
        }
    }
}

impl<Data: SemanticValue, StateIndex> Node<Data, StateIndex> {
    /// Clear this node to `Default::default()`.
    pub fn clear(&mut self) {
        self.parent = None;
        self.child_count = 0;
        self.state_stack.clear();
        self.data_stack.clear();
        self.location_stack.clear();
        #[cfg(feature = "tree")]
        self.tree_stack.clear();
    }
    pub fn len(&self) -> usize {
        self.state_stack.len()
    }
    pub fn is_leaf(&self) -> bool {
        self.child_count == 0
    }

    pub fn parent(&self) -> Option<usize> {
        self.parent.map(NodeId::index)
    }

    pub fn child_count(&self) -> usize {
        self.child_count
    }

    pub fn state_stack(&self) -> &[StateIndex] {
        &self.state_stack
    }

    pub fn data_stack(&self) -> &[Data] {
        &self.data_stack
    }

    pub fn location_stack(&self) -> &[Data::Location] {
        &self.location_stack
    }

    #[cfg(feature = "tree")]
    pub fn tree_stack(&self) -> &[crate::tree::Tree<Data::Term, Data::NonTerm>] {
        &self.tree_stack
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Node {
            parent: None,
            child_count: 0,
            state_stack: Vec::with_capacity(capacity),
            data_stack: Vec::with_capacity(capacity),
            location_stack: Vec::with_capacity(capacity),
            #[cfg(feature = "tree")]
            tree_stack: Vec::with_capacity(capacity),
        }
    }
    pub fn reserve(&mut self, additional: usize) {
        self.state_stack.reserve(additional);
        self.data_stack.reserve(additional);
        self.location_stack.reserve(additional);
        #[cfg(feature = "tree")]
        self.tree_stack.reserve(additional);
    }
}
