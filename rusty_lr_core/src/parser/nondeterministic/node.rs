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

    /*
    /// Get set of `%trace` non-terminal symbols that current context is trying to parse.
    ///
    /// The order of the returned set does not mean anything.
    /// If the current context is attempting to recognize following grammar:
    ///
    /// Chunk -> Statement -> IfStatement -> ReturnStatement -> ...
    ///
    /// Then the returned set will be:
    /// [`Chunk`, `Statement`, `IfStatement`, `ReturnStatement`]

    /// Get backtrace information for current state.
    /// What current state is trying to parse, and where it comes from.

    /// Simulate parser and get next expected (terminals, non-terminals) for current context.
    pub fn expected_token<'a, P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        self: &Rc<Self>,
        parser: &'a P,
        terms: &mut std::collections::BTreeSet<usize>,
        nonterms: &mut std::collections::BTreeSet<Data::NonTerm>,
    ) where
        Data::NonTerm: Ord + Copy + std::hash::Hash + NonTerminal,
    {
        let s = self.state;
        let s = parser.get_states().get(s).expect("state must exist");

        terms.extend(s.expected_shift_term());
        nonterms.extend(s.expected_shift_nonterm());

        let mut reduce_nonterms = std::collections::BTreeSet::new();
        for reduce_rule in s.expected_reduce_rule() {
            let prod_rule = &parser.get_rules()[reduce_rule];
            reduce_nonterms.insert((prod_rule.name, prod_rule.rule.len()));
        }
        for &(nonterm, len) in reduce_nonterms.iter() {
            let mut node = self;
            for _ in 0..len {
                node = node.parent.as_ref().unwrap();
            }
            let last_state = node.state;
            if let Some(next_state) = parser.get_states()[last_state].shift_goto_nonterm(&nonterm) {
                let next_node = Self {
                    parent: Some(Rc::clone(node)),
                    precedence_level: None,
                    state: next_state,
                    data: None,
                    location: None,
                    #[cfg(feature = "tree")]
                    tree: None,
                };
                let next_node = Rc::new(next_node);
                Self::expected_token(&next_node, parser, terms, nonterms);
            }
        }
    }


    /// check if current context can enter panic mode
    pub fn can_panic<P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        mut node: &Rc<Node<Data>>,
        parser: &P,
        error_prec: Option<usize>,
    ) -> bool
    where
        Data::NonTerm: std::hash::Hash + Eq + NonTerminal,
    {
        loop {
            if Self::can_feed_impl(node, parser, TerminalSymbol::Error, error_prec) {
                return true;
            } else {
                node = match node.parent.as_ref() {
                    Some(parent) => parent,
                    None => {
                        // if we reached root node, then panic mode is not possible
                        return false;
                    }
                };
            }
        }
    }
    */
}
