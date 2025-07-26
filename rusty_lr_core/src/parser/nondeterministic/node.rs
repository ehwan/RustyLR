use crate::nonterminal::TokenData;
use crate::parser::Precedence;

/// To handle multiple paths in the non-deterministic GLR parsing,
/// this node represents a subrange in stack of the parser.
/// this constructs LinkedList tree of nodes, where parent node is the previous token in the parse tree.
#[derive(Clone)]
pub struct Node<Data: TokenData> {
    /// parent node
    pub parent: Option<usize>,

    pub child_count: usize,

    /// index of state in parser
    pub state_stack: Vec<usize>,
    pub data_stack: Vec<Data>,
    pub location_stack: Vec<Data::Location>,
    pub precedence_stack: Vec<Precedence>,
    #[cfg(feature = "tree")]
    pub(crate) tree_stack: Vec<crate::tree::Tree<Data::Term, Data::NonTerm>>,
}

impl<Data: TokenData> Default for Node<Data> {
    fn default() -> Self {
        Node {
            parent: None,
            child_count: 0,
            state_stack: Vec::new(),
            data_stack: Vec::new(),
            location_stack: Vec::new(),
            precedence_stack: Vec::new(),
            #[cfg(feature = "tree")]
            tree_stack: Vec::new(),
        }
    }
}

impl<Data: TokenData> Node<Data> {
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
        self.data_stack.len()
    }
    pub fn is_leaf(&self) -> bool {
        self.child_count == 0
    }
}
