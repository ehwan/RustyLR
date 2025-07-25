use crate::nonterminal::TokenData;

/// Node represents single token in the parse tree.
/// To handle multiple paths in the GLR parsing,
/// this constructs Linked List of nodes, where parent node is the previous token in the parse tree.
#[derive(Clone)]
pub struct Node<Data: TokenData> {
    /// parent node
    pub parent: Option<usize>,

    pub reference_count: usize,

    /// index of state in parser
    pub state_stack: Vec<usize>,
    pub data_stack: Vec<Data>,
    pub location_stack: Vec<Data::Location>,
    pub precedence_stack: Vec<Option<usize>>,
    #[cfg(feature = "tree")]
    pub(crate) tree_stack: Vec<crate::tree::Tree<Data::Term, Data::NonTerm>>,
}

impl<Data: TokenData> Default for Node<Data> {
    fn default() -> Self {
        Node {
            parent: None,
            reference_count: 1,
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
        self.reference_count = 1;
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
    pub fn is_unique(&self) -> bool {
        self.reference_count == 1
    }
}
