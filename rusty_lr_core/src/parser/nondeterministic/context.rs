use std::num::NonZeroUsize;

use super::Node;
use super::NodeId;
use super::ParseError;
use super::error::FeedSuccess;

use crate::Location;
use crate::TerminalSymbol;
use crate::parser::Parser;
use crate::parser::nonterminal::NonTerminal;
use crate::parser::semantic_value::SemanticValue;
use crate::parser::semantic_value::StartExtractor;
use crate::parser::table::Index;
use crate::parser::table::ParserTables;
use crate::parser::terminalclass::TerminalClass;

/// Iterator for traverse node to root.
/// Note that root node is not included in this iterator.
pub struct NodeRefIterator<
    'a,
    P: Parser<Term = Data::Term, NonTerm = Data::NonTerm, StateIndex = StateIndex>,
    Data: SemanticValue,
    Start: StartExtractor<Data>,
    StateIndex,
    const MAX_REDUCE_RULES: usize,
> {
    context: &'a Context<P, Data, Start, StateIndex, MAX_REDUCE_RULES>,
    node: Option<NodeId>,
}
impl<
    'a,
    P: Parser<Term = Data::Term, NonTerm = Data::NonTerm, StateIndex = StateIndex>,
    Data: SemanticValue,
    Start: StartExtractor<Data>,
    StateIndex,
    const MAX_REDUCE_RULES: usize,
> Clone for NodeRefIterator<'a, P, Data, Start, StateIndex, MAX_REDUCE_RULES>
{
    fn clone(&self) -> Self {
        NodeRefIterator {
            context: self.context,
            node: self.node,
        }
    }
}
impl<
    'a,
    P: Parser<Term = Data::Term, NonTerm = Data::NonTerm, StateIndex = StateIndex>,
    Data: SemanticValue,
    Start: StartExtractor<Data>,
    StateIndex: Index + Copy,
    const MAX_REDUCE_RULES: usize,
> Iterator for NodeRefIterator<'a, P, Data, Start, StateIndex, MAX_REDUCE_RULES>
{
    type Item = &'a Node<Data, StateIndex>;

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.context.node(self.node?);
        self.node = node.parent;
        Some(node)
    }
}

struct BranchDebug<'a, Data: SemanticValue> {
    index: usize,
    state: usize,
    state_stack: Vec<usize>,
    data_stack: Vec<&'a Data>,
    userdata: &'a Data::UserData,
}

impl<Data> std::fmt::Debug for BranchDebug<'_, Data>
where
    Data: SemanticValue + std::fmt::Debug,
    Data::UserData: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Branch")
            .field("index", &self.index)
            .field("state", &self.state)
            .field("state_stack", &self.state_stack)
            .field("data_stack", &self.data_stack)
            .field("userdata", self.userdata)
            .finish()
    }
}

#[derive(Clone)]
struct Branch<UserData> {
    node: NodeId,
    userdata: UserData,
}

/// Branch state returned when a GLR feed path reaches no terminal shift.
///
/// The caller only needs the graph node and user data to keep or discard that branch; the lookahead
/// terminal and location are owned by the failed attempt and do not need to be returned.
struct UnshiftedBranch<UserData> {
    node: NodeId,
    userdata: UserData,
}

impl<UserData> Branch<UserData> {
    fn new(node: NodeId, userdata: UserData) -> Self {
        Branch { node, userdata }
    }
}

/// A struct that maintains the current state and the values associated with each symbol.
/// This handles the divergence and merging of the parser.
pub struct Context<
    P: Parser<Term = Data::Term, NonTerm = Data::NonTerm, StateIndex = StateIndex>,
    Data: SemanticValue,
    Start: StartExtractor<Data>,
    StateIndex,
    const MAX_REDUCE_RULES: usize,
> {
    pub(crate) nodes_pool: Vec<Node<Data, StateIndex>>,
    pub(crate) empty_node_indices: Vec<NodeId>,

    /// Each element represents an active GLR branch and its tail node.
    current_branches: Vec<Branch<Data::UserData>>,

    /// temporary storage
    next_branches: Vec<Branch<Data::UserData>>,

    /// For recovery from error
    fallback_branches: Vec<Branch<Data::UserData>>,

    /// For temporary use. store reduce errors returned from `reduce_action`.
    /// But we don't want to reallocate every `feed` call
    pub(crate) reduce_errors: Vec<Data::ReduceActionError>,

    // Future optimization point: GLR can also reuse simulation state stacks, but branch-local
    // recursion and cloned alternatives need a separate scratch ownership strategy.
    /// Decoded parser tables shared by every active GLR branch.
    ///
    /// Branches clone stack/userdata state, but they all read the same immutable runtime tables.
    pub(crate) tables: &'static P::Tables,
    pub(crate) _phantom: std::marker::PhantomData<(P, Start)>,
}

impl<
    P: Parser<Term = Data::Term, NonTerm = Data::NonTerm, StateIndex = StateIndex>,
    Data: SemanticValue,
    Start: StartExtractor<Data>,
    StateIndex: Index,
    const MAX_REDUCE_RULES: usize,
> Context<P, Data, Start, StateIndex, MAX_REDUCE_RULES>
{
    /// Create a new context.
    /// `current_branches` is initialized with a root node.
    pub fn new(userdata: Data::UserData) -> Self {
        P::__assert_rusty_lr_parser_version_compatible();
        let mut context = Context {
            nodes_pool: Default::default(),
            empty_node_indices: Default::default(),
            current_branches: Default::default(),
            next_branches: Default::default(),
            reduce_errors: Default::default(),
            fallback_branches: Default::default(),
            tables: P::get_tables(),
            _phantom: std::marker::PhantomData,
        };
        let root_node = context.new_node();
        context
            .current_branches
            .push(Branch::new(root_node, userdata));
        context.init_start_branch(Start::BRANCH_INDEX);
        context
    }

    pub fn with_default_userdata() -> Self
    where
        Data::UserData: Default,
    {
        Self::new(Default::default())
    }

    fn init_start_branch(&mut self, branch_idx: u32) {
        let class = P::TermClass::from_virtual_start(branch_idx);
        let shift_to = self.tables.shift_goto_class(0, class).unwrap_or_else(|| {
            panic!(
                "Failed to resolve shift for virtual start branch {}",
                branch_idx
            )
        });
        let root_node_idx = self.current_branches[0].node;
        let root_node = self.node_mut(root_node_idx);
        root_node.state_stack.push(shift_to.state);
        root_node.data_stack.push(Data::new_empty());
        root_node
            .location_stack
            .push(Data::Location::new(std::iter::empty(), 0));
        #[cfg(feature = "tree")]
        {
            root_node.tree_stack.push(crate::tree::Tree::new_terminal(
                TerminalSymbol::VirtualStart(branch_idx),
            ));
        }
    }

    /// Borrow the user data for the first active path.
    ///
    /// In GLR mode, each forked branch owns an independently cloned user data value.
    pub fn userdata(&self) -> &Data::UserData {
        self.current_branches
            .first()
            .map(|branch| &branch.userdata)
            .expect("userdata() requires at least one active GLR path")
    }

    /// Borrow the user data for every active path.
    ///
    /// In GLR mode, each forked branch owns an independently cloned user data value.
    pub fn userdata_all(&self) -> impl Iterator<Item = &Data::UserData> {
        self.current_branches.iter().map(|branch| &branch.userdata)
    }

    /// Mutably borrow the user data for the first active path.
    ///
    /// In GLR mode, each forked branch owns an independently cloned user data value.
    pub fn userdata_mut(&mut self) -> &mut Data::UserData {
        self.current_branches
            .first_mut()
            .map(|branch| &mut branch.userdata)
            .expect("userdata_mut() requires at least one active GLR path")
    }

    /// Mutably borrow the user data for every active path.
    ///
    /// In GLR mode, each forked branch owns an independently cloned user data value.
    pub fn userdata_all_mut(&mut self) -> impl Iterator<Item = &mut Data::UserData> {
        self.current_branches
            .iter_mut()
            .map(|branch| &mut branch.userdata)
    }

    pub(crate) fn node(&self, node: NodeId) -> &Node<Data, StateIndex> {
        debug_assert!(
            !self.empty_node_indices.contains(&node) && node.index() < self.nodes_pool.len()
        );

        &self.nodes_pool[node.index()]
    }
    pub(crate) fn node_mut(&mut self, node: NodeId) -> &mut Node<Data, StateIndex> {
        debug_assert!(
            !self.empty_node_indices.contains(&node) && node.index() < self.nodes_pool.len()
        );

        &mut self.nodes_pool[node.index()]
    }

    /// for debugging; checks for memory leak, not freed nodes, etc.
    pub fn debug_check(&self) {
        let mut active_nodes = std::collections::BTreeSet::new();

        for branch in self.current_branches.iter() {
            let tail_node = branch.node;
            let mut node = tail_node;
            loop {
                if !active_nodes.insert(node) {
                    // panic!("node {} is already in active nodes", node);
                }
                if let Some(parent) = self.node(node).parent {
                    node = parent;
                } else {
                    break; // reached root node
                }
            }
        }

        for (node, _) in self.nodes_pool.iter().enumerate() {
            let node = NodeId::new(node);
            if self.empty_node_indices.contains(&node) {
                if active_nodes.contains(&node) {
                    panic!("empty node {} is in active nodes", node.index());
                }
                continue; // empty node
            } else {
                if !active_nodes.contains(&node) {
                    panic!("node {} is not in active nodes", node.index());
                }
                active_nodes.remove(&node);
            }
        }
        if !active_nodes.is_empty() {
            panic!("active nodes are not empty: {:?}", active_nodes);
        }
    }

    /// Create a new node in the pool and return its index.
    pub(crate) fn new_node_with_capacity(&mut self, capacity: usize) -> NodeId {
        if let Some(idx) = self.empty_node_indices.pop() {
            self.node_mut(idx).reserve(capacity);
            idx
        } else {
            let idx = NodeId::new(self.nodes_pool.len());
            self.nodes_pool.push(Node::with_capacity(capacity));
            idx
        }
    }
    pub(crate) fn new_node(&mut self) -> NodeId {
        if let Some(idx) = self.empty_node_indices.pop() {
            idx
        } else {
            let idx = NodeId::new(self.nodes_pool.len());
            self.nodes_pool.push(Node::default());
            idx
        }
    }
    pub(crate) fn add_child(&mut self, node: NodeId, child: NodeId) {
        debug_assert!(self.node(child).parent.is_none());
        self.node_mut(node).child_count += 1;
        self.node_mut(child).parent = Some(node);
    }
    pub(crate) fn try_remove_node(&mut self, node: NodeId) -> Option<NodeId> {
        let node_ = self.node_mut(node);
        let parent = node_.parent;

        if node_.is_leaf() {
            if node.index() == self.nodes_pool.len() - 1 {
                self.nodes_pool.pop();
            } else {
                self.node_mut(node).clear();
                self.empty_node_indices.push(node);
            }
        } else {
            node_.parent = None;
        }

        if let Some(parent) = parent {
            let parent_node = self.node_mut(parent);
            parent_node.child_count -= 1;
        }
        parent
    }
    pub(crate) fn try_remove_node_recursive(&mut self, node: NodeId) -> Option<NodeId> {
        // remove node recursive
        let mut node = node;
        loop {
            let parent = self.try_remove_node(node);
            if let Some(parent) = parent {
                if self.node(parent).is_leaf() {
                    node = parent;
                    continue;
                }
            }
            break parent;
        }
    }

    /// Get iterator for all nodes in the current context.
    fn node_iter(
        &self,
        node: NodeId,
    ) -> NodeRefIterator<'_, P, Data, Start, StateIndex, MAX_REDUCE_RULES> {
        NodeRefIterator {
            context: self,
            node: Some(node),
        }
    }
    /// Get iterator for `node` that traverses from `node` to root on the parsing tree.
    fn location_iter(&self, node: NodeId) -> impl Iterator<Item = &Data::Location> + Clone
    where
        Data: Clone,
    {
        self.node_iter(node)
            .flat_map(|node| node.location_stack.iter().rev())
    }
    /// Get iterator for `node` that traverses from `node` to root on the parsing tree.
    fn state_iter(&self, node: NodeId) -> impl Iterator<Item = usize> + '_ {
        self.node_iter(node).flat_map(|node| {
            node.state_stack
                .iter()
                .rev()
                .copied()
                .map(Index::into_usize)
        })
    }
    fn data_iter(&self, node: NodeId) -> impl Iterator<Item = &Data> + '_ {
        self.node_iter(node)
            .flat_map(|node| node.data_stack.iter().rev())
    }
    #[cfg(feature = "tree")]
    /// Get iterator for `node` that traverses from `node` to root on the parsing tree.
    fn tree_iter(
        &self,
        node: NodeId,
    ) -> impl Iterator<Item = &crate::tree::Tree<Data::Term, Data::NonTerm>> {
        self.node_iter(node)
            .flat_map(|node| node.tree_stack.iter().rev())
    }

    /// Get state of the node.
    fn state(&self, mut node: NodeId) -> usize {
        while self.node(node).state_stack.is_empty() {
            if let Some(parent) = self.node(node).parent {
                node = parent;
            } else {
                return 0; // root node
            }
        }
        self.node(node).state_stack.last().unwrap().into_usize()
    }

    /// Get current states in every diverged paths.
    pub fn states(&self) -> impl Iterator<Item = usize> + '_ {
        self.current_branches
            .iter()
            .map(|branch| self.state(branch.node))
    }

    /// Get iterators of state stacks in all diverged paths.
    pub fn state_stacks(&self) -> impl Iterator<Item = impl Iterator<Item = usize> + '_> + '_ {
        self.current_branches
            .iter()
            .map(move |branch| self.state_iter(branch.node))
    }

    /// From `node`, collect `reduce_token_count` number of tokens for reduce_action.
    /// Returns the index of node that it's data_stack, location_stack and tree_stack have more elements than reduce_token_count,
    /// and other stack containing the (data_stack.len() - reduce_token_count) number of elements.
    fn prepare_reduce_node(
        &mut self,
        node_idx: NodeId,
        reduce_token_count: usize,
        capacity: usize,
        can_reuse_node_for_reduce: bool,
    ) -> NodeId
    where
        Data: Clone,
        Data::Term: Clone,
        Data::NonTerm: Clone,
    {
        let node = self.node(node_idx);
        if reduce_token_count <= node.len() {
            // count <= node.len
            let i = node.len() - reduce_token_count;

            if can_reuse_node_for_reduce && node.is_leaf() {
                let node = &mut self.node_mut(node_idx);

                // truncate stacks to cut off reduce_token_count elements from back
                node.state_stack.truncate(i);

                node_idx
            } else {
                // clone the values in range [i..] from current_node to reduce_args
                let parent = node.parent;

                if i == 0 {
                    // parent <- node[0..]
                    //        <- new_node

                    let node_data_stack = node.data_stack.clone();
                    let node_location_stack = node.location_stack.clone();
                    #[cfg(feature = "tree")]
                    let node_tree_stack = node.tree_stack.clone();

                    // create new empty node pointing to this node's parent node, and use it as node_to_shift
                    let new_node_idx = self.new_node_with_capacity(capacity);
                    if let Some(parent) = parent {
                        self.add_child(parent, new_node_idx);
                    }
                    let new_node = self.node_mut(new_node_idx);
                    new_node.data_stack = node_data_stack;
                    new_node.location_stack = node_location_stack;
                    #[cfg(feature = "tree")]
                    {
                        new_node.tree_stack = node_tree_stack;
                    }

                    new_node_idx
                } else if i == node.len() {
                    // create new empty node pointing to this node, and use it as node_to_shift
                    // node <- new_node
                    let new_node_idx = self.new_node_with_capacity(capacity);
                    self.add_child(node_idx, new_node_idx);
                    new_node_idx
                } else {
                    // split the node into [..i] and [i..]
                    // and make new parent node with [..i]
                    // create new empty node pointing to parent node, and use it as node_to_shift
                    // new_parent[..i] <- current_node[i..]
                    //                 <- new_node (empty)

                    let new_parent = self.new_node();
                    let node = self.node_mut(node_idx);

                    let mut parent_data_stack = node.data_stack.split_off(i);
                    let mut parent_state_stack = node.state_stack.split_off(i);
                    let mut parent_location_stack = node.location_stack.split_off(i);
                    #[cfg(feature = "tree")]
                    let mut parent_tree_stack = node.tree_stack.split_off(i);

                    std::mem::swap(&mut parent_data_stack, &mut node.data_stack);
                    std::mem::swap(&mut parent_state_stack, &mut node.state_stack);
                    std::mem::swap(&mut parent_location_stack, &mut node.location_stack);
                    #[cfg(feature = "tree")]
                    std::mem::swap(&mut parent_tree_stack, &mut node.tree_stack);

                    let node_data_stack = node.data_stack.clone();
                    let node_location_stack = node.location_stack.clone();
                    #[cfg(feature = "tree")]
                    let node_tree_stack = node.tree_stack.clone();

                    let parent_node = self.node_mut(new_parent);
                    parent_node.data_stack = parent_data_stack;
                    parent_node.state_stack = parent_state_stack;
                    parent_node.location_stack = parent_location_stack;
                    #[cfg(feature = "tree")]
                    {
                        parent_node.tree_stack = parent_tree_stack;
                    }

                    if let Some(parent) = parent {
                        self.node_mut(node_idx).parent = None;
                        self.node_mut(parent).child_count -= 1;

                        self.add_child(parent, new_parent);
                    }
                    self.add_child(new_parent, node_idx);

                    let new_node = self.new_node_with_capacity(capacity);
                    self.add_child(new_parent, new_node);
                    {
                        let new_node = self.node_mut(new_node);
                        new_node.data_stack = node_data_stack;
                        new_node.location_stack = node_location_stack;
                        #[cfg(feature = "tree")]
                        {
                            new_node.tree_stack = node_tree_stack;
                        }
                    }

                    new_node
                }
            }
        } else {
            let len = node.len();
            let parent = node.parent;
            let node_stack = if can_reuse_node_for_reduce && node.is_leaf() {
                // move the values from current_node to reduce_args

                let node = &mut self.node_mut(node_idx);
                let node_data_stack = std::mem::take(&mut node.data_stack);
                let node_location_stack = std::mem::take(&mut node.location_stack);
                #[cfg(feature = "tree")]
                let node_tree_stack = std::mem::take(&mut node.tree_stack);

                self.try_remove_node(node_idx);

                #[cfg(feature = "tree")]
                let ret = (node_data_stack, node_location_stack, node_tree_stack);

                #[cfg(not(feature = "tree"))]
                let ret = (node_data_stack, node_location_stack);

                ret
            } else {
                // clone the values from current_node to reduce_args

                let node_data_stack = node.data_stack.clone();
                let node_location_stack = node.location_stack.clone();
                #[cfg(feature = "tree")]
                let node_tree_stack = node.tree_stack.clone();

                #[cfg(feature = "tree")]
                let ret = (node_data_stack, node_location_stack, node_tree_stack);

                #[cfg(not(feature = "tree"))]
                let ret = (node_data_stack, node_location_stack);

                ret
            };

            #[cfg(feature = "tree")]
            let (mut node_data_stack, mut node_location_stack, mut node_tree_stack) = node_stack;
            #[cfg(not(feature = "tree"))]
            let (mut node_data_stack, mut node_location_stack) = node_stack;

            let reduce_node_idx = self.prepare_reduce_node(
                parent.unwrap(),
                reduce_token_count - len,
                capacity,
                can_reuse_node_for_reduce,
            );
            let reduce_node = self.node_mut(reduce_node_idx);
            reduce_node.data_stack.append(&mut node_data_stack);
            reduce_node.location_stack.append(&mut node_location_stack);
            #[cfg(feature = "tree")]
            reduce_node.tree_stack.append(&mut node_tree_stack);

            reduce_node_idx
        }
    }

    /// give lookahead token to parser, and check if there is any reduce action.
    /// returns false if shift action is revoked
    fn reduce(
        &mut self,
        reduce_rule: usize,
        node: NodeId,
        term: &crate::TerminalSymbol<P::Term>,
        shift: &mut bool,
        can_reuse_node_for_reduce: bool,
        userdata: &mut Data::UserData,
    ) -> Result<NodeId, Data::ReduceActionError>
    where
        Data: Clone,
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
    {
        use crate::Location;
        let rule = *self.tables.rule(reduce_rule);
        let count = rule.len;
        let mut new_location = Data::Location::new(self.location_iter(node), count);

        let node_to_shift = self.prepare_reduce_node(node, count, count, can_reuse_node_for_reduce);

        let state = self.state(node_to_shift);
        let Some(next_nonterm_shift) = self.tables.shift_goto_nonterm(state, rule.lhs) else {
            unreachable!(
                "Failed to shift non-terminal: {:?} in state {}",
                rule.lhs, state
            );
        };

        let node = self.node_mut(node_to_shift);
        #[cfg(feature = "tree")]
        let trees = node.tree_stack.split_off(node.tree_stack.len() - count);

        match Data::reduce_action(
            &mut node.data_stack,
            &mut node.location_stack,
            next_nonterm_shift.push,
            reduce_rule,
            shift,
            term,
            userdata,
            &mut new_location,
        ) {
            Ok(_) => {
                node.state_stack.push(next_nonterm_shift.state);
                node.location_stack.push(new_location);
                #[cfg(feature = "tree")]
                {
                    node.tree_stack
                        .push(crate::tree::Tree::new_nonterminal(rule.lhs.clone(), trees));
                }
                Ok(node_to_shift)
            }
            Err(err) => {
                self.try_remove_node_recursive(node_to_shift);
                Err(err)
            }
        }
    }

    /// Get number of diverged paths
    pub fn len_paths(&self) -> usize {
        self.current_branches.len()
    }

    /// Is there any path alive?
    pub fn is_empty(&self) -> bool {
        self.current_branches.is_empty()
    }

    /// End this context and return the first successful start symbol and user data pair.
    pub fn accept(
        self,
    ) -> Result<
        (Start::StartType, Data::UserData),
        ParseError<Data::Term, Data::Location, Data::ReduceActionError>,
    >
    where
        Data: Clone,
        Data::UserData: Clone,
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
    {
        Ok(self.accept_all()?.next().unwrap())
    }

    /// End this context and return iterator of the start value and user data from each successful path.
    pub fn accept_all(
        mut self,
    ) -> Result<
        impl Iterator<Item = (Start::StartType, Data::UserData)>,
        ParseError<Data::Term, Data::Location, Data::ReduceActionError>,
    >
    where
        Data: Clone,
        Data::UserData: Clone,
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
    {
        self.feed_eof()?;
        // since `eof` is feeded, every node graph should be like this:
        // Root <- Start <- EOF
        //                  ^^^ here, current_node
        let Context {
            mut nodes_pool,
            current_branches,
            ..
        } = self;

        let mut accepted = Vec::with_capacity(current_branches.len());
        for Branch { node, userdata } in current_branches {
            let mut data_stack = std::mem::take(&mut nodes_pool[node.index()].data_stack);
            data_stack.pop(); // eof
            let start_value = data_stack.pop().unwrap_or_else(|| {
                panic!(
                    "Accepted GLR path for virtual start branch {} has no start value",
                    Start::BRANCH_INDEX
                )
            });
            let start_value = Start::extract(start_value).unwrap_or_else(|| {
                panic!(
                    "Accepted GLR path produced a value that does not match virtual start branch {}",
                    Start::BRANCH_INDEX
                )
            });
            accepted.push((start_value, userdata));
        }

        Ok(accepted.into_iter())
    }

    /// For debugging.
    /// Get all sequence of token trees (from root to current node) for every diverged path.
    #[cfg(feature = "tree")]
    pub fn to_tree_lists(
        &self,
    ) -> impl Iterator<Item = crate::tree::TreeList<Data::Term, Data::NonTerm>> + '_
    where
        Data::Term: Clone,
        Data::NonTerm: Clone,
    {
        self.current_branches.iter().map(|branch| {
            let mut trees: Vec<_> = self.tree_iter(branch.node).cloned().collect();
            trees.reverse();
            crate::tree::TreeList { trees }
        })
    }

    fn expected_token_impl(
        &self,
        extra_state_stack: &mut Vec<StateIndex>,
        node_and_len: Option<(NodeId, NonZeroUsize)>,
        terms: &mut std::collections::BTreeSet<P::TermClass>,
        nonterms: &mut std::collections::BTreeSet<Data::NonTerm>,
    ) where
        P::TermClass: Ord,
        P::NonTerm: Ord,
    {
        let state = extra_state_stack
            .last()
            .copied()
            .map(Index::into_usize)
            .unwrap_or_else(|| {
                node_and_len
                    .map(|(node, stack_len)| {
                        self.node(node).state_stack[stack_len.get() - 1].into_usize()
                    })
                    .unwrap_or(0)
            });
        terms.extend(self.tables.expected_shift_term(state));
        nonterms.extend(self.tables.expected_shift_nonterm(state));

        let mut reduce_nonterms = std::collections::BTreeSet::new();
        for reduce_rule in self.tables.expected_reduce_rule(state) {
            let prod_rule = self.tables.rule(reduce_rule.into_usize());
            reduce_nonterms.insert((prod_rule.len, prod_rule.lhs));
        }
        for &(mut tokens_len, nonterm) in reduce_nonterms.iter() {
            let mut node_and_len = node_and_len;
            let mut extra_state_stack = if tokens_len > extra_state_stack.len() {
                tokens_len -= extra_state_stack.len();

                let (node, node_len) = node_and_len.unwrap();
                let node_len = node_len.get();

                if tokens_len < node_len {
                    node_and_len = Some((node, NonZeroUsize::new(node_len - tokens_len).unwrap()));
                } else {
                    let parent = self.node(node).parent;
                    if let Some(parent) = parent {
                        node_and_len = self
                            .skip_last_n(parent, tokens_len - node_len)
                            .map(|(node, i)| (node, NonZeroUsize::new(i + 1).unwrap()));
                    } else {
                        // reached root node
                        node_and_len = None;
                    }
                }
                Vec::new()
            } else {
                extra_state_stack[..extra_state_stack.len() - tokens_len].to_vec()
            };

            let state = extra_state_stack
                .last()
                .copied()
                .map(Index::into_usize)
                .unwrap_or_else(|| {
                    node_and_len
                        .map(|(node, stack_len)| {
                            self.node(node).state_stack[stack_len.get() - 1].into_usize()
                        })
                        .unwrap_or(0)
                });
            if let Some(next_state) = self.tables.shift_goto_nonterm(state, nonterm) {
                extra_state_stack.push(next_state.state);
                self.expected_token_impl(&mut extra_state_stack, node_and_len, terms, nonterms);
            }
        }
    }

    /// Get next expected (terminals, non-terminals) for current context.
    pub fn expected_token(
        &self,
    ) -> (
        std::collections::BTreeSet<P::TermClass>,
        std::collections::BTreeSet<P::NonTerm>,
    )
    where
        P::TermClass: Ord,
        P::NonTerm: Ord,
    {
        let mut terms = std::collections::BTreeSet::new();
        let mut nonterms = std::collections::BTreeSet::new();
        let mut extra_state_stack = Vec::new();

        for branch in self.current_branches.iter() {
            let node = branch.node;
            let node_and_len = {
                let node_ = self.node(node);
                if node_.len() == 0 {
                    // only root node can have 0 length stack
                    None
                } else {
                    Some((node, NonZeroUsize::new(node_.len()).unwrap()))
                }
            };
            extra_state_stack.clear();

            self.expected_token_impl(
                &mut extra_state_stack,
                node_and_len,
                &mut terms,
                &mut nonterms,
            );
        }

        (terms, nonterms)
    }
    /// Same as `expected_token()`, but returns as printable type.
    pub fn expected_token_str<'a>(
        &self,
    ) -> (
        impl Iterator<Item = &'static str> + use<P, Data, Start, StateIndex, MAX_REDUCE_RULES>,
        impl Iterator<Item = &'static str> + use<P, Data, Start, StateIndex, MAX_REDUCE_RULES>,
    )
    where
        P::TermClass: Ord,
        P::NonTerm: Ord,
    {
        let (terms, nonterms) = self.expected_token();
        (
            terms.into_iter().map(|term| term.as_str()),
            nonterms.into_iter().map(|nonterm| nonterm.as_str()),
        )
    }

    /// Feed one terminal to parser, and update stacks.
    /// This will use `Default::default()` for location.
    pub fn feed(
        &mut self,
        term: Data::Term,
    ) -> Result<
        FeedSuccess<Data::Term, Data::Location, Data::ReduceActionError>,
        ParseError<Data::Term, Data::Location, Data::ReduceActionError>,
    >
    where
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
        Data: Clone,
        Data::UserData: Clone,
        Data::Location: Default,
    {
        self.feed_location(term, Default::default())
    }
    fn skip_last_n(&self, mut node: NodeId, mut count: usize) -> Option<(NodeId, usize)> {
        loop {
            let node_ = self.node(node);
            if count < node_.len() {
                break Some((node, node_.len() - 1 - count));
            } else {
                if let Some(parent) = node_.parent {
                    count -= node_.len();
                    node = parent;
                } else {
                    break None; // reached root node
                }
            }
        }
    }

    fn feed_location_impl(
        &mut self,
        node: NodeId,
        term: TerminalSymbol<P::Term>,
        class: P::TermClass,
        location: Data::Location,
        userdata: Data::UserData,
    ) -> Result<(), UnshiftedBranch<Data::UserData>>
    where
        Data: Clone,
        Data::UserData: Clone,
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
    {
        debug_assert!(self.node(node).is_leaf());
        use crate::parser::table::ReduceRules;

        let last_state = self.state(node);
        let (shift_state, reduce) = match self.tables.term_action(last_state, class) {
            Some(action) => (action.shift(), action.reduce()),
            None => (None, None),
        };
        if let Some(reduce_rules) = reduce {
            let mut shift = shift_state;

            let mut shifted = false;
            let mut reduced_node = node;
            let mut reduced_userdata = None;

            // Each GLR branch receives its own user data copy before running reduce actions.
            let mut shift_ = false;
            let l = reduce_rules.to_iter().len();
            for (idx, reduce_rule) in reduce_rules.to_iter().enumerate() {
                let mut pass = shift.is_some();
                let mut branch_userdata = userdata.clone();

                // The current node may still be needed by later reduce alternatives
                // or by the shift branch, so only the last reduce without a pending
                // shift is allowed to reuse it in-place.
                let can_reuse_node_for_reduce = idx == l - 1 && shift.is_none();
                match self.reduce(
                    reduce_rule.into_usize(),
                    node,
                    &term,
                    &mut pass,
                    can_reuse_node_for_reduce,
                    &mut branch_userdata,
                ) {
                    Ok(next_node) => {
                        shift_ |= pass;
                        // reduce recursively

                        match self.feed_location_impl(
                            next_node,
                            term.clone(),
                            class,
                            location.clone(),
                            branch_userdata,
                        ) {
                            Ok(_) => {
                                shifted = true;
                            }
                            Err(unshifted) => {
                                reduced_node = unshifted.node;
                                reduced_userdata = Some(unshifted.userdata);
                            }
                        }
                    }
                    Err(err) => {
                        shift_ |= pass;
                        self.reduce_errors.push(err);
                    }
                }
            }
            // if every reduce action revoked shift,
            // then reset shift to None
            if !shift_ {
                if shift.is_some() {
                    // remove node recursive
                    self.try_remove_node_recursive(node);
                    shift = None;
                }
            }
            if let Some(shift) = shift {
                // If `node` acquired children during the reduce loop (e.g. an
                // empty rule created a child via `prepare_reduce_node`), it is
                // no longer a leaf.  Pushing it directly to `next_branches` would
                // violate the `is_leaf()` precondition of the next
                // `feed_location_impl` call.  Instead, create a fresh leaf
                // child that carries only the shift result.
                let shift_node = if self.node(node).is_leaf() {
                    node
                } else {
                    let new_node_idx = self.new_node_with_capacity(1);
                    self.add_child(node, new_node_idx);
                    new_node_idx
                };

                let node_ = self.node_mut(shift_node);
                node_.state_stack.push(shift.state);
                node_.location_stack.push(location.clone());
                #[cfg(feature = "tree")]
                node_
                    .tree_stack
                    .push(crate::tree::Tree::new_terminal(term.clone()));

                if shift.push {
                    match term {
                        TerminalSymbol::Terminal(term) => {
                            node_.data_stack.push(Data::new_terminal(term));
                        }
                        TerminalSymbol::Error
                        | TerminalSymbol::Eof
                        | TerminalSymbol::VirtualStart(_) => {
                            node_.data_stack.push(Data::new_empty());
                        }
                    }
                } else {
                    match term {
                        TerminalSymbol::Terminal(_)
                        | TerminalSymbol::Error
                        | TerminalSymbol::Eof
                        | TerminalSymbol::VirtualStart(_) => {
                            node_.data_stack.push(Data::new_empty());
                        }
                    }
                }

                self.next_branches.push(Branch::new(shift_node, userdata));
                Ok(())
            } else if shifted {
                Ok(())
            } else {
                Err(UnshiftedBranch {
                    node: reduced_node,
                    userdata: reduced_userdata.unwrap_or(userdata),
                })
            }
        } else if let Some(shift) = shift_state {
            let node_ = self.node_mut(node);
            node_.state_stack.push(shift.state);
            node_.location_stack.push(location);
            #[cfg(feature = "tree")]
            node_
                .tree_stack
                .push(crate::tree::Tree::new_terminal(term.clone()));

            if shift.push {
                match term {
                    TerminalSymbol::Terminal(term) => {
                        node_.data_stack.push(Data::new_terminal(term));
                    }
                    TerminalSymbol::Error
                    | TerminalSymbol::Eof
                    | TerminalSymbol::VirtualStart(_) => {
                        node_.data_stack.push(Data::new_empty());
                    }
                }
            } else {
                match term {
                    TerminalSymbol::Terminal(_)
                    | TerminalSymbol::Error
                    | TerminalSymbol::Eof
                    | TerminalSymbol::VirtualStart(_) => {
                        node_.data_stack.push(Data::new_empty());
                    }
                }
            }

            self.next_branches.push(Branch::new(node, userdata));
            Ok(())
        } else {
            // no reduce, no shift
            Err(UnshiftedBranch { node, userdata })
        }
    }
    fn panic_mode(
        &mut self,
        mut node: NodeId,
        userdata: Data::UserData,
        extra_state_stack: &mut Vec<StateIndex>,
    ) where
        Data: Clone,
        Data::UserData: Clone,
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
    {
        use crate::Location;
        use crate::TriState;

        let mut error_location_preserved = None;

        let pop_count = loop {
            let node_ = self.node(node);
            if !node_.is_leaf() {
                self.try_remove_node(node);
                return;
            }

            let mut pop_count = 0;
            let mut found = false;

            for &s in node_.state_stack.iter().rev() {
                match self.tables.can_accept_error(s.into_usize()) {
                    TriState::False => {}
                    TriState::Maybe => {
                        extra_state_stack.clear();

                        if self.can_feed_impl(
                            extra_state_stack,
                            Some((node, NonZeroUsize::new(node_.len() - pop_count).unwrap())),
                            P::TermClass::ERROR,
                        ) {
                            found = true;
                            break;
                        }
                    }
                    TriState::True => {
                        found = true;
                        break;
                    }
                }

                pop_count += 1;
            }

            if !found {
                error_location_preserved = Some(if let Some(prev) = error_location_preserved {
                    Data::Location::new(
                        std::iter::once(&prev).chain(self.location_iter(node)),
                        pop_count + 1,
                    )
                } else {
                    Data::Location::new(self.location_iter(node), pop_count)
                });

                if let Some(parent) = self.try_remove_node(node) {
                    node = parent;
                    continue;
                } else {
                    return;
                }
            } else {
                break pop_count;
            }
        };

        let error_location = if let Some(prev) = error_location_preserved {
            Data::Location::new(
                std::iter::once(&prev).chain(self.location_iter(node)),
                pop_count + 1,
            )
        } else {
            Data::Location::new(self.location_iter(node), pop_count)
        };
        let node_ = self.node_mut(node);
        let l = node_.len() - pop_count;
        node_.location_stack.truncate(l);
        node_.state_stack.truncate(l);
        node_.data_stack.truncate(l);
        #[cfg(feature = "tree")]
        {
            let l = node_.tree_stack.len() - pop_count;
            node_.tree_stack.truncate(l);
        }
        match self.feed_location_impl(
            node,
            TerminalSymbol::Error,
            P::TermClass::ERROR,
            error_location,
            userdata,
        ) {
            Ok(()) => {}
            Err(unshifted) => {
                self.try_remove_node(unshifted.node);
            } // other errors
        }
    }
    /// Feed one terminal with location to parser, and update state stack.
    ///
    /// Each active GLR branch is first checked with the same CFG simulation used by `can_feed`.
    /// Branches rejected by that simulation are `NoAction` branches and are the only branches
    /// eligible for panic-mode recovery. If at least one branch can grammatically shift the
    /// terminal, recovery is not entered; failed sibling branches are returned in
    /// `FeedSuccess::errors`.
    pub fn feed_location(
        &mut self,
        term: P::Term,
        location: Data::Location,
    ) -> Result<
        FeedSuccess<Data::Term, Data::Location, Data::ReduceActionError>,
        ParseError<Data::Term, Data::Location, Data::ReduceActionError>,
    >
    where
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
        Data: Clone,
        Data::UserData: Clone,
    {
        use crate::Location;

        self.reduce_errors.clear();
        self.fallback_branches.clear();
        self.next_branches.clear();

        let class = P::TermClass::from_term(&term);

        let mut current_branches = std::mem::take(&mut self.current_branches);
        let mut feedable_branch_seen = false;
        for Branch { node, userdata } in current_branches.drain(..) {
            // Classify this branch before mutating the graph-structured stack. Only branches that
            // fail this CFG simulation are considered `NoAction` branches.
            let mut extra_state_stack = Vec::new();
            let node_and_len = {
                let node_ = self.node(node);
                if node_.len() == 0 {
                    None
                } else {
                    Some((node, NonZeroUsize::new(node_.len()).unwrap()))
                }
            };

            if self.can_feed_impl(&mut extra_state_stack, node_and_len, class) {
                feedable_branch_seen = true;

                // The CFG admits this terminal on this branch. From here on, failures are caused
                // by semantic reduce actions or runtime conflict decisions, so they must not
                // trigger error recovery.
                let _ = self.feed_location_impl(
                    node,
                    TerminalSymbol::Terminal(term.clone()),
                    class,
                    location.clone(),
                    userdata,
                );
            } else {
                // This branch cannot grammatically shift the lookahead. Keep it only as a
                // recovery candidate in case every active branch is also `NoAction`.
                self.fallback_branches.push(Branch::new(node, userdata));
            }
        }
        // put back for reused allocated memory
        self.current_branches = current_branches;

        // next_branches is empty; invalid terminal was given
        // check for panic mode
        // and restore nodes to original state from fallback_branches
        if self.next_branches.is_empty() {
            if feedable_branch_seen {
                // A branch was grammatically able to shift the terminal, but failed while
                // committing semantic/runtime actions. That is not syntax recovery input.
                std::mem::swap(&mut self.current_branches, &mut self.fallback_branches);

                return Err(ParseError {
                    term: TerminalSymbol::Terminal(term),
                    location,
                    reduce_action_errors: std::mem::take(&mut self.reduce_errors),
                    states: self.states().collect(),
                });
            }

            // early return if `error` token is not used in the grammar
            if !P::ERROR_USED {
                std::mem::swap(&mut self.current_branches, &mut self.fallback_branches);

                return Err(ParseError {
                    term: TerminalSymbol::Terminal(term),
                    location,
                    reduce_action_errors: std::mem::take(&mut self.reduce_errors),
                    states: self.states().collect(),
                });
            }

            let mut fallback_branches = std::mem::take(&mut self.fallback_branches);
            let mut extra_state_stack = Vec::new();
            // try enter panic mode and store error nodes to next_branches
            for Branch { node, userdata } in fallback_branches.drain(..) {
                // GLR recovery only searches concrete branch stacks. The implicit root state 0 is
                // before the virtual start symbol and cannot accept a synthetic `error` token.
                self.panic_mode(node, userdata, &mut extra_state_stack);
            }
            // put back for reuse allocated memory
            self.fallback_branches = fallback_branches;

            if !self.reduce_errors.is_empty() {
                // Shifting the synthetic `error` token may execute reduce actions. If one fails,
                // recovery itself failed semantically and must be reported.
                return Err(ParseError {
                    term: TerminalSymbol::Terminal(term),
                    location,
                    reduce_action_errors: std::mem::take(&mut self.reduce_errors),
                    states: self.states().collect(),
                });
            }

            // if next_node is still empty, then no panic mode was entered, this is an error
            // restore current_branches to fallback_branches
            if self.next_branches.is_empty() {
                Err(ParseError {
                    term: TerminalSymbol::Terminal(term),
                    location,
                    reduce_action_errors: std::mem::take(&mut self.reduce_errors),
                    states: self.states().collect(),
                })
            } else {
                // try shift term to error state
                let mut next_branches = std::mem::take(&mut self.next_branches);
                for Branch {
                    node: error_node,
                    userdata,
                } in next_branches.drain(..)
                {
                    let last_state = self.state(error_node);
                    if let Some(next_state) = self.tables.shift_goto_class(last_state, class) {
                        // A -> a . error b
                        // and b is fed, shift error and b
                        let node = self.node_mut(error_node);
                        node.state_stack.push(next_state.state);
                        node.location_stack.push(location.clone());
                        #[cfg(feature = "tree")]
                        node.tree_stack.push(crate::tree::Tree::new_terminal(
                            TerminalSymbol::Terminal(term.clone()),
                        ));

                        if next_state.push {
                            node.data_stack.push(Data::new_terminal(term.clone()));
                        } else {
                            node.data_stack.push(Data::new_empty());
                        }

                        self.current_branches
                            .push(Branch::new(error_node, userdata));
                    } else {
                        // here, fed token is in `error` non-terminal
                        // so merge location with previous

                        let new_location = Data::Location::new(
                            std::iter::once(&location).chain(self.location_iter(error_node)),
                            2, // error node + fed token
                        );
                        let node = self.node_mut(error_node);
                        *node.location_stack.last_mut().unwrap() = new_location;

                        self.current_branches
                            .push(Branch::new(error_node, userdata));
                    }
                }
                self.next_branches = next_branches;
                // Recovery consumed or merged the lookahead. This is a successful feed, and the
                // original `NoAction` branches have been transformed into recovery branches.
                Ok(FeedSuccess { errors: None })
            }
        } else {
            // At least one original branch accepted the terminal. Report sibling branch failures
            // to callers without treating this feed as fatal.
            let errors = if self.fallback_branches.is_empty() && self.reduce_errors.is_empty() {
                None
            } else {
                let states = self
                    .fallback_branches
                    .iter()
                    .map(|branch| self.state(branch.node))
                    .collect();
                self.fallback_branches.clear();
                Some(ParseError {
                    term: TerminalSymbol::Terminal(term),
                    location,
                    reduce_action_errors: std::mem::take(&mut self.reduce_errors),
                    states,
                })
            };
            std::mem::swap(&mut self.current_branches, &mut self.next_branches);
            Ok(FeedSuccess { errors })
        }
    }
    /// Feed one terminal with location to parser, and update state stack.
    fn can_feed_impl(
        &self,
        extra_state_stack: &mut Vec<StateIndex>,
        mut node_and_len: Option<(NodeId, NonZeroUsize)>,
        class: P::TermClass,
    ) -> bool {
        let last_state = extra_state_stack
            .last()
            .copied()
            .map(Index::into_usize)
            .unwrap_or_else(|| {
                node_and_len
                    .map(|(node, stack_len)| {
                        self.node(node).state_stack[stack_len.get() - 1].into_usize()
                    })
                    .unwrap_or(0)
            });
        let (shift_state, reduce) = match self.tables.term_action(last_state, class) {
            Some(action) => (action.shift(), action.reduce()),
            None => (None, None),
        };
        if let Some(reduce_rules) = reduce {
            let shift = shift_state;

            use crate::parser::table::ReduceRules;

            if shift.is_some() {
                return true;
            }
            let mut reduces = reduce_rules.to_iter();
            match reduces.len() {
                0 => return false, // since reduce is `Some`, this should be unreachable
                // if there is only one reduce rule, we can just reduce and shift with nonterminal
                1 => {
                    let rule_index = reduces.next().unwrap().into_usize();
                    let rule_info = *self.tables.rule(rule_index);
                    let tokens_len = rule_info.len;

                    // pop state stack
                    if tokens_len <= extra_state_stack.len() {
                        extra_state_stack.truncate(extra_state_stack.len() - tokens_len);
                    } else {
                        let left = tokens_len - extra_state_stack.len();
                        extra_state_stack.clear();

                        let (node, len) = node_and_len.unwrap();
                        let len = len.get();

                        if left < len {
                            node_and_len = Some((node, NonZeroUsize::new(len - left).unwrap()));
                        } else {
                            let parent = self.node(node).parent;
                            if let Some(parent) = parent {
                                node_and_len = self
                                    .skip_last_n(parent, left - len)
                                    .map(|(node, i)| (node, NonZeroUsize::new(i + 1).unwrap()));
                            } else {
                                // reached root node
                                node_and_len = None;
                            }
                        }
                    }

                    // shift with reduced nonterminal
                    let last_state = extra_state_stack
                        .last()
                        .copied()
                        .map(Index::into_usize)
                        .unwrap_or_else(|| {
                            node_and_len
                                .map(|(node, stack_len)| {
                                    self.node(node).state_stack[stack_len.get() - 1].into_usize()
                                })
                                .unwrap_or(0)
                        });
                    if let Some(next_state_id) =
                        self.tables.shift_goto_nonterm(last_state, rule_info.lhs)
                    {
                        extra_state_stack.push(next_state_id.state);
                    } else {
                        unreachable!(
                            "unreachable: nonterminal shift should always succeed after reduce operation. \
Failed to shift nonterminal '{}' after reducing rule '{}'. This indicates a parser state machine bug.",
                            rule_info.lhs.as_str(),
                            rule_index
                        );
                    }

                    self.can_feed_impl(extra_state_stack, node_and_len, class)
                }
                // if there are multiple reduce rules, we need to check all of them, create new state stack for each reduce rule, and check if any of them can shift the terminal
                _ => {
                    for reduce_rule in reduces {
                        let reduce_rule = *self.tables.rule(reduce_rule.into_usize());
                        let tokens_len = reduce_rule.len;

                        let mut extra_state_stack = extra_state_stack.clone();

                        // pop state stack
                        let new_node_and_len = if tokens_len <= extra_state_stack.len() {
                            extra_state_stack.truncate(extra_state_stack.len() - tokens_len);
                            node_and_len
                        } else {
                            let left = tokens_len - extra_state_stack.len();
                            extra_state_stack.clear();

                            let (node, len) = node_and_len.unwrap();
                            let len = len.get();

                            if left < len {
                                Some((node, NonZeroUsize::new(len - left).unwrap()))
                            } else {
                                let parent = self.node(node).parent;
                                if let Some(parent) = parent {
                                    self.skip_last_n(parent, left - len)
                                        .map(|(node, i)| (node, NonZeroUsize::new(i + 1).unwrap()))
                                } else {
                                    // reached root node
                                    None
                                }
                            }
                        };

                        // shift with reduced nonterminal
                        let last_state = extra_state_stack
                            .last()
                            .copied()
                            .map(Index::into_usize)
                            .unwrap_or_else(|| {
                                new_node_and_len
                                    .map(|(node, stack_len)| {
                                        self.node(node).state_stack[stack_len.get() - 1]
                                            .into_usize()
                                    })
                                    .unwrap_or(0)
                            });

                        if let Some(next_state_id) =
                            self.tables.shift_goto_nonterm(last_state, reduce_rule.lhs)
                        {
                            extra_state_stack.push(next_state_id.state);
                        } else {
                            unreachable!(
                                "unreachable: nonterminal shift should always succeed after reduce operation, but failed for nonterminal '{}' from state {:?}",
                                reduce_rule.lhs.as_str(),
                                last_state
                            );
                        }

                        if self.can_feed_impl(&mut extra_state_stack, new_node_and_len, class) {
                            return true;
                        }
                    }
                    false
                }
            }
        } else {
            shift_state.is_some()
        }
    }

    /// Check if `term` can be feeded to current state.
    /// This does not simulate for reduce action error, or panic mode.
    /// So this function will return `false` even if term can be shifted as `error` token,
    /// and will return `true` if `Err` variant is returned by `reduce_action`.
    pub fn can_feed(&self, term: &P::Term) -> bool {
        let class = P::TermClass::from_term(term);
        let mut extra_state_stack = Vec::new();
        self.current_branches.iter().any(move |branch| {
            let node = branch.node;
            extra_state_stack.clear();

            let node_and_len = {
                let node_ = self.node(node);
                if node_.len() == 0 {
                    // only root node can have 0 length stack
                    None
                } else {
                    Some((node, NonZeroUsize::new(node_.len()).unwrap()))
                }
            };
            self.can_feed_impl(&mut extra_state_stack, node_and_len, class)
        })
    }

    /// Check if current context can enter panic mode.
    pub fn can_panic(&self) -> bool {
        // if `error` token was not used in the grammar, early return here
        if !P::ERROR_USED {
            return false;
        }

        let mut extra_state_stack = Vec::new();

        self.current_branches.iter().any(move |branch| {
            let mut node = branch.node;
            let mut len = self.node(node).len();

            if len > 0 {
                loop {
                    extra_state_stack.clear();

                    if self.can_feed_impl(
                        &mut extra_state_stack,
                        Some((node, NonZeroUsize::new(len).unwrap())),
                        P::TermClass::ERROR,
                    ) {
                        return true;
                    } else {
                        len -= 1;
                        if len == 0 {
                            if let Some(parent) = self.node(node).parent {
                                node = parent;
                                len = self.node(parent).len();
                            } else {
                                break;
                            }
                        }
                    }
                }
            }

            // State 0 is the implicit root before virtual start and cannot accept `error`.
            false
        })
    }

    /// Feed eof symbol with default zero-length location from the end of stream.
    fn feed_eof(
        &mut self,
    ) -> Result<(), ParseError<Data::Term, Data::Location, Data::ReduceActionError>>
    where
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
        Data: Clone,
        Data::UserData: Clone,
    {
        use crate::location::Location;

        self.reduce_errors.clear();
        self.fallback_branches.clear();
        self.next_branches.clear();

        let eof_location = if let Some(branch) = self.current_branches.first() {
            Data::Location::new(self.location_iter(branch.node), 0)
        } else {
            Data::Location::new(std::iter::empty(), 0)
        };

        let mut current_branches = std::mem::take(&mut self.current_branches);
        for Branch { node, userdata } in current_branches.drain(..) {
            let node_eof_location = Data::Location::new(self.location_iter(node), 0);
            if let Err(unshifted) = self.feed_location_impl(
                node,
                TerminalSymbol::Eof,
                P::TermClass::EOF,
                node_eof_location,
                userdata,
            ) {
                self.fallback_branches
                    .push(Branch::new(unshifted.node, unshifted.userdata));
            }
        }
        self.current_branches = current_branches;

        // next_branches is empty; invalid terminal was given
        // check for panic mode
        // and restore nodes to original state from fallback_branches
        if self.next_branches.is_empty() {
            std::mem::swap(&mut self.current_branches, &mut self.fallback_branches);

            Err(ParseError {
                term: TerminalSymbol::Eof,
                location: eof_location,
                reduce_action_errors: std::mem::take(&mut self.reduce_errors),
                states: self.states().collect(),
            })
        } else {
            std::mem::swap(&mut self.current_branches, &mut self.next_branches);
            Ok(())
        }
    }
    /// Check if current context can be terminated and get the start value.
    pub fn can_accept(&self) -> bool {
        let mut extra_state_stack = Vec::new();
        self.current_branches.iter().any(move |branch| {
            let node = branch.node;
            extra_state_stack.clear();

            let node_and_len = {
                let node_ = self.node(node);
                if node_.len() == 0 {
                    // only root node can have 0 length stack
                    None
                } else {
                    Some((node, NonZeroUsize::new(node_.len()).unwrap()))
                }
            };
            self.can_feed_impl(&mut extra_state_stack, node_and_len, P::TermClass::EOF)
        })
    }
}

impl<
    P: Parser<Term = Data::Term, NonTerm = Data::NonTerm, StateIndex = StateIndex>,
    Data: SemanticValue,
    Start: StartExtractor<Data>,
    StateIndex: Index,
    const MAX_REDUCE_RULES: usize,
> Default for Context<P, Data, Start, StateIndex, MAX_REDUCE_RULES>
where
    Data::UserData: Default,
{
    fn default() -> Self {
        Self::new(Default::default())
    }
}

impl<
    P: Parser<Term = Data::Term, NonTerm = Data::NonTerm, StateIndex = StateIndex>,
    Data: SemanticValue,
    Start: StartExtractor<Data>,
    StateIndex: Index,
    const MAX_REDUCE_RULES: usize,
> Clone for Context<P, Data, Start, StateIndex, MAX_REDUCE_RULES>
where
    Node<Data, StateIndex>: Clone,
    Data::UserData: Clone,
{
    fn clone(&self) -> Self {
        Context {
            nodes_pool: self.nodes_pool.clone(),
            empty_node_indices: self.empty_node_indices.clone(),
            current_branches: self.current_branches.clone(),
            next_branches: Default::default(),
            reduce_errors: Default::default(),
            fallback_branches: Default::default(),
            tables: self.tables,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<
    P: Parser<Term = Data::Term, NonTerm = Data::NonTerm, StateIndex = StateIndex>,
    Data: SemanticValue,
    Start: StartExtractor<Data>,
    StateIndex: Index,
    const MAX_REDUCE_RULES: usize,
> std::fmt::Debug for Context<P, Data, Start, StateIndex, MAX_REDUCE_RULES>
where
    Data: std::fmt::Debug,
    Data::UserData: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let branches: Vec<_> = self
            .current_branches
            .iter()
            .enumerate()
            .map(|(index, branch)| {
                let node = branch.node;
                let userdata = &branch.userdata;
                let mut state_stack: Vec<_> = self.state_iter(node).collect();
                state_stack.reverse();
                let mut data_stack: Vec<_> = self.data_iter(node).collect();
                data_stack.reverse();

                BranchDebug {
                    index,
                    state: self.state(node),
                    state_stack,
                    data_stack,
                    userdata,
                }
            })
            .collect();

        f.debug_struct("Context")
            .field("branch_count", &self.current_branches.len())
            .field("branches", &branches)
            .finish()
    }
}
