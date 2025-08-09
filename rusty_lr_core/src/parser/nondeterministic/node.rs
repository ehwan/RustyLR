use crate::nonterminal::DataStack;
use crate::parser::Precedence;

/// To handle multiple paths in the non-deterministic GLR parsing,
/// this node represents a subrange in stack of the parser.
/// this constructs LinkedList tree of nodes, where parent node is the previous token in the parse tree.
#[derive(Clone)]
pub struct Node<Data: DataStack, StateIndex> {
    /// parent node
    pub parent: Option<usize>,

    pub child_count: usize,

    /// index of state in parser
    pub state_stack: Vec<StateIndex>,
    pub data_stack: Data,
    pub location_stack: Vec<Data::Location>,
    pub precedence_stack: Vec<Precedence>,
    #[cfg(feature = "tree")]
    pub(crate) tree_stack: Vec<crate::tree::Tree<Data::Term, Data::NonTerm>>,
}

impl<Data: DataStack, StateIndex> Default for Node<Data, StateIndex> {
    fn default() -> Self {
        Node {
            parent: None,
            child_count: 0,
            state_stack: Vec::new(),
            data_stack: Data::default(),
            location_stack: Vec::new(),
            precedence_stack: Vec::new(),
            #[cfg(feature = "tree")]
            tree_stack: Vec::new(),
        }
    }
}

impl<Data: DataStack, StateIndex> Node<Data, StateIndex> {
    /// Clear this node to `Default::default()`.
    pub fn clear(&mut self) {
        self.parent = None;
        self.child_count = 0;
        self.state_stack.clear();
        self.data_stack.clear();
        self.location_stack.clear();
        self.precedence_stack.clear();
        #[cfg(feature = "tree")]
        self.tree_stack.clear();
    }
    pub fn len(&self) -> usize {
        self.state_stack.len()
    }
    pub fn is_leaf(&self) -> bool {
        self.child_count == 0
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Node {
            parent: None,
            child_count: 0,
            state_stack: Vec::with_capacity(capacity),
            data_stack: Data::with_capacity(capacity),
            location_stack: Vec::with_capacity(capacity),
            precedence_stack: Vec::with_capacity(capacity),
            #[cfg(feature = "tree")]
            tree_stack: Vec::with_capacity(capacity),
        }
    }
    pub fn reserve(&mut self, additional: usize) {
        self.state_stack.reserve(additional);
        self.data_stack.reserve(additional);
        self.location_stack.reserve(additional);
        self.precedence_stack.reserve(additional);
        #[cfg(feature = "tree")]
        self.tree_stack.reserve(additional);
    }
}
