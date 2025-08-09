use std::hash::Hash;

use super::Node;
use super::ParseError;

use crate::nonterminal::NonTerminal;
use crate::parser::data_stack::DataStack;
use crate::parser::state::Index;
use crate::parser::Parser;
use crate::parser::Precedence;
use crate::parser::State;
use crate::TerminalSymbol;

type SmallVecNode = smallvec::SmallVec<[usize; 3]>;

/// Iterator for traverse node to root.
/// Note that root node is not included in this iterator.
#[derive(Clone)]
pub struct NodeRefIterator<'a, Data: DataStack, StateIndex> {
    context: &'a Context<Data, StateIndex>,
    node: Option<usize>,
}
impl<'a, Data: DataStack, StateIndex: Index + Copy> Iterator
    for NodeRefIterator<'a, Data, StateIndex>
{
    type Item = &'a Node<Data, StateIndex>;

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.context.node(self.node?);
        self.node = node.parent;
        Some(node)
    }
}

/// A struct that maintains the current state and the values associated with each symbol.
/// This handles the divergence and merging of the parser.
pub struct Context<Data: DataStack, StateIndex> {
    pub(crate) nodes_pool: Vec<Node<Data, StateIndex>>,
    pub(crate) empty_node_indices: std::collections::BTreeSet<usize>,

    /// each element represents an end-point of diverged paths.
    pub(crate) current_nodes: SmallVecNode,

    /// temporary storage
    pub(crate) next_nodes: SmallVecNode,

    /// For recovery from error
    pub(crate) fallback_nodes: SmallVecNode,

    /// For temporary use. store reduce errors returned from `reduce_action`.
    /// But we don't want to reallocate every `feed` call
    pub(crate) reduce_errors: Vec<Data::ReduceActionError>,

    /// For temporary use.
    /// store rule indices where shift/reduce conflicts occured with no precedence defined.
    pub(crate) no_precedences: Vec<usize>,
}

impl<Data: DataStack, StateIndex: Index + Copy> Context<Data, StateIndex> {
    /// Create a new context.
    /// `current_nodes` is initialized with a root node.
    pub fn new() -> Self {
        Default::default()
    }

    pub fn node(&self, node: usize) -> &Node<Data, StateIndex> {
        debug_assert!(!self.empty_node_indices.contains(&node) && node < self.nodes_pool.len());

        &self.nodes_pool[node]
    }
    pub fn node_mut(&mut self, node: usize) -> &mut Node<Data, StateIndex> {
        debug_assert!(!self.empty_node_indices.contains(&node) && node < self.nodes_pool.len());

        &mut self.nodes_pool[node]
    }

    /// for debugging; checks for memory leak, not freed nodes, etc.
    pub fn debug_check(&self) {
        let mut active_nodes = std::collections::BTreeSet::new();

        for &tail_node in self.current_nodes.iter() {
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
            if self.empty_node_indices.contains(&node) {
                if active_nodes.contains(&node) {
                    panic!("empty node {} is in active nodes", node);
                }
                continue; // empty node
            } else {
                if !active_nodes.contains(&node) {
                    panic!("node {} is not in active nodes", node);
                }
                active_nodes.remove(&node);
            }
        }
        if active_nodes.len() > 0 {
            panic!("active nodes are not empty: {:?}", active_nodes);
        }
    }

    /// Create a new node in the pool and return its index.
    pub(crate) fn new_node_with_capacity(&mut self, capacity: usize) -> usize {
        if let Some(idx) = self.empty_node_indices.pop_first() {
            self.node_mut(idx).reserve(capacity);
            idx
        } else {
            let idx = self.nodes_pool.len();
            self.nodes_pool.push(Node::with_capacity(capacity));
            idx
        }
    }
    pub(crate) fn new_node(&mut self) -> usize {
        if let Some(idx) = self.empty_node_indices.pop_first() {
            idx
        } else {
            let idx = self.nodes_pool.len();
            self.nodes_pool.push(Node::default());
            idx
        }
    }
    pub(crate) fn add_child(&mut self, node: usize, child: usize) {
        debug_assert!(self.node(child).parent.is_none());
        self.node_mut(node).child_count += 1;
        self.node_mut(child).parent = Some(node);
    }
    pub(crate) fn try_remove_node(&mut self, node: usize) -> Option<usize> {
        let node_ = self.node_mut(node);
        let parent = node_.parent;

        if node_.is_leaf() {
            if node == self.nodes_pool.len() - 1 {
                self.nodes_pool.pop();
            } else {
                self.node_mut(node).clear();
                self.empty_node_indices.insert(node);
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
    pub(crate) fn try_remove_node_recursive(&mut self, node: usize) -> Option<usize> {
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
    fn node_iter(&self, node: usize) -> NodeRefIterator<'_, Data, StateIndex> {
        NodeRefIterator {
            context: self,
            node: Some(node),
        }
    }
    /// Get iterator for `node` that traverses from `node` to root on the parsing tree.
    fn location_iter(&self, node: usize) -> impl Iterator<Item = &Data::Location> + Clone
    where
        Data: Clone,
    {
        self.node_iter(node)
            .flat_map(|node| node.location_stack.iter().rev())
    }
    /// Get iterator for `node` that traverses from `node` to root on the parsing tree.
    fn state_iter(&self, node: usize) -> impl Iterator<Item = usize> + '_ {
        self.node_iter(node).flat_map(|node| {
            node.state_stack
                .iter()
                .rev()
                .copied()
                .map(Index::into_usize)
        })
    }
    #[cfg(feature = "tree")]
    /// Get iterator for `node` that traverses from `node` to root on the parsing tree.
    fn tree_iter(
        &self,
        node: usize,
    ) -> impl Iterator<Item = &crate::tree::Tree<Data::Term, Data::NonTerm>> {
        self.node_iter(node)
            .flat_map(|node| node.tree_stack.iter().rev())
    }

    /// Get state of the node.
    fn state(&self, mut node: usize) -> usize {
        while self.node(node).state_stack.is_empty() {
            if let Some(parent) = self.node(node).parent {
                node = parent;
            } else {
                return 0; // root node
            }
        }
        self.node(node).state_stack.last().unwrap().into_usize()
    }

    /// pop one stack from the node.
    fn pop(&mut self, node: usize) -> Option<usize> {
        match self.node(node).len() {
            0 => unreachable!("cannot pop from empty node"),
            1 => self.try_remove_node(node),
            _ => {
                let node_ = self.node_mut(node);
                node_.data_stack.pop();
                node_.location_stack.pop();
                #[cfg(feature = "tree")]
                node_.tree_stack.pop();
                node_.state_stack.pop();
                node_.precedence_stack.pop();
                Some(node)
            }
        }
    }

    /// From `node`, collect `reduce_token_count` number of tokens for reduce_action.
    /// Returns the index of node that it's data_stack, location_stack and tree_stack have more elements than reduce_token_count,
    /// and other stack containing the (data_stack.len() - reduce_token_count) number of elements.
    fn prepare_reduce_node(
        &mut self,
        node_idx: usize,
        reduce_token_count: usize,
        capacity: usize,
    ) -> usize
    where
        Data: Clone,
        Data::Term: Clone,
        Data::NonTerm: Clone,
    {
        let node = self.node(node_idx);
        if reduce_token_count <= node.len() {
            // count <= node.len
            let i = node.len() - reduce_token_count;

            if node.is_leaf() {
                let node = &mut self.node_mut(node_idx);

                // truncate stacks to cut off reduce_token_count elements from back
                node.state_stack.truncate(i);
                node.precedence_stack.truncate(i);

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
                    let mut parent_precedence_stack = node.precedence_stack.split_off(i);
                    #[cfg(feature = "tree")]
                    let mut parent_tree_stack = node.tree_stack.split_off(i);

                    std::mem::swap(&mut parent_data_stack, &mut node.data_stack);
                    std::mem::swap(&mut parent_state_stack, &mut node.state_stack);
                    std::mem::swap(&mut parent_location_stack, &mut node.location_stack);
                    std::mem::swap(&mut parent_precedence_stack, &mut node.precedence_stack);
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
                    parent_node.precedence_stack = parent_precedence_stack;
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
            let node_stack = if node.is_leaf() {
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

            let reduce_node_idx =
                self.prepare_reduce_node(parent.unwrap(), reduce_token_count - len, capacity);
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
    fn reduce<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &mut self,
        parser: &P,
        reduce_rule: usize,
        precedence: Precedence,
        node: usize,
        term: &crate::TerminalSymbol<P::Term>,
        shift: &mut bool,
        userdata: &mut Data::UserData,
    ) -> Result<usize, Data::ReduceActionError>
    where
        Data: Clone,
        Data::Term: Clone,
        Data::NonTerm: Clone + std::fmt::Debug + NonTerminal + Hash + Eq,
    {
        use crate::Location;
        let rule = &parser.get_rules()[reduce_rule];
        let count = rule.rule.len();
        let mut new_location = Data::Location::new(self.location_iter(node), count);

        let node_to_shift = self.prepare_reduce_node(node, count, count);

        use crate::parser::State;

        let state = self.state(node_to_shift);
        let node = self.node_mut(node_to_shift);
        #[cfg(feature = "tree")]
        let trees = node.tree_stack.split_off(node.tree_stack.len() - count);

        match Data::reduce_action(
            &mut node.data_stack,
            &mut node.location_stack,
            reduce_rule,
            shift,
            term,
            userdata,
            &mut new_location,
        ) {
            Ok(_) => {
                if let Some(nonterm_shift_state) =
                    parser.get_states()[state].shift_goto_nonterm(&rule.name)
                {
                    node.state_stack
                        .push(StateIndex::from_usize_unchecked(nonterm_shift_state));
                    node.location_stack.push(new_location);
                    node.precedence_stack.push(precedence);
                    #[cfg(feature = "tree")]
                    {
                        node.tree_stack
                            .push(crate::tree::Tree::new_nonterminal(rule.name.clone(), trees));
                    }
                    Ok(node_to_shift)
                } else {
                    unreachable!(
                        "no shift state for non-terminal: {:?}",
                        parser.get_rules()[reduce_rule].name
                    );
                }
            }
            Err(err) => {
                self.try_remove_node_recursive(node_to_shift);
                Err(err)
            }
        }
    }

    /// Get number of diverged paths
    pub fn len_paths(&self) -> usize {
        self.current_nodes.len()
    }

    /// Is there any path alive?
    pub fn is_empty(&self) -> bool {
        self.current_nodes.is_empty()
    }

    /// Get current index of states in every diverged paths.
    pub fn states(&self) -> impl Iterator<Item = usize> + '_ {
        self.current_nodes.iter().map(|node| self.state(*node))
    }

    /// End this context and return iterator of the start value from the data stack.
    pub fn accept<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        mut self,
        parser: &P,
        userdata: &mut Data::UserData,
    ) -> Result<impl Iterator<Item = Data::StartType>, ParseError<Data>>
    where
        Data: Clone,
        P::Term: Clone,
        P::NonTerm: Hash + Eq + Clone + std::fmt::Debug + NonTerminal,
    {
        self.feed_eof(parser, userdata)?;
        // since `eof` is feeded, every node graph should be like this:
        // Root <- Start <- EOF
        //                  ^^^ here, current_node
        let nodes = std::mem::take(&mut self.current_nodes);
        Ok(nodes.into_iter().map(move |eof_node| {
            let node = self.pop(eof_node).unwrap();
            // Since <EOF> does not have ruletype, no need to pop
            self.nodes_pool[node].data_stack.pop_start().unwrap()
        }))
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
        self.current_nodes.iter().map(|node| {
            let mut trees: Vec<_> = self.tree_iter(*node).cloned().collect();
            trees.reverse();
            crate::tree::TreeList { trees }
        })
    }

    fn trace_impl<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
        node: usize,
    ) -> crate::hash::HashSet<Data::NonTerm>
    where
        Data::NonTerm: Copy + Eq + std::hash::Hash + crate::nonterminal::NonTerminal,
    {
        use crate::hash::HashSet;
        use crate::nonterminal::NonTerminal;
        use crate::token::Token;
        use std::collections::BTreeSet;

        let rules = parser.get_rules();
        let states = parser.get_states();

        let mut zero_shifted_rules = BTreeSet::new();
        let mut non_zero_shifted_rules = BTreeSet::new();
        {
            let last_state = &states[self.state(node)];
            for rule in last_state.get_rules().iter() {
                if rule.shifted == 0 {
                    zero_shifted_rules.insert(rule.rule);
                } else {
                    non_zero_shifted_rules.insert((rule.rule, rule.shifted));
                }
            }
        }

        let mut ret: HashSet<Data::NonTerm> = Default::default();

        for state in self.state_iter(node).chain(std::iter::once(0)) {
            let state = &states[state];
            let ruleset = &state.get_rules();

            // insert new shifted rule that brings zero_shifted rules in this state
            let mut new_zero_shifted_rules = Vec::new();
            loop {
                let zero_len0 = zero_shifted_rules.len();
                let nonzero_len0 = non_zero_shifted_rules.len();

                new_zero_shifted_rules.clear();

                for &zero_rule in zero_shifted_rules.iter() {
                    let nonterm0 = rules[zero_rule].name;
                    for rule in ruleset.iter() {
                        let prod_rule = &rules[rule.rule];
                        if let Some(Token::NonTerm(nonterm)) = prod_rule.rule.get(rule.shifted) {
                            if &nonterm0 == nonterm {
                                if rule.shifted == 0 {
                                    new_zero_shifted_rules.push(rule.rule);
                                } else {
                                    // insert new shifted rule
                                    non_zero_shifted_rules.insert((rule.rule, rule.shifted));
                                }
                            }
                        }
                    }
                }
                zero_shifted_rules.extend(new_zero_shifted_rules.iter().copied());

                if zero_len0 == zero_shifted_rules.len()
                    && nonzero_len0 == non_zero_shifted_rules.len()
                {
                    break;
                }
            }

            // push nonterminal of zero-shifted-rules into backtrace vector
            for &zero_rule in zero_shifted_rules.iter() {
                let nonterm0 = rules[zero_rule].name;
                // do not insert auto-generated nonterminals
                // since user don't need to know about them
                if nonterm0.is_trace() {
                    ret.insert(nonterm0);
                }
            }

            // shift to next state
            zero_shifted_rules.clear();
            let mut new_non_zero_shifted_rules = BTreeSet::new();
            for (rule, shifted) in non_zero_shifted_rules.into_iter() {
                if shifted == 1 {
                    zero_shifted_rules.insert(rule);
                } else {
                    new_non_zero_shifted_rules.insert((rule, shifted - 1));
                }
            }
            non_zero_shifted_rules = new_non_zero_shifted_rules;
        }

        ret
    }

    /// Get set of `%trace` non-terminal symbols that current context is trying to parse.
    ///
    /// The order of the returned set does not mean anything.
    /// If the current context is attempting to recognize following grammar:
    ///
    /// Chunk -> Statement -> IfStatement -> ReturnStatement -> ...
    ///
    /// Then the returned set will be:
    /// [`Chunk`, `Statement`, `IfStatement`, `ReturnStatement`]
    pub fn trace<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
    ) -> crate::hash::HashSet<Data::NonTerm>
    where
        Data::NonTerm: Copy + Eq + std::hash::Hash + crate::nonterminal::NonTerminal,
    {
        let mut ret: crate::hash::HashSet<Data::NonTerm> = Default::default();
        for &node in self.current_nodes.iter() {
            let set = self.trace_impl(parser, node);
            ret.extend(set.into_iter());
        }
        ret
    }

    fn backtrace_impl<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
        node: usize,
    ) -> crate::Backtrace<&'static str, Data::NonTerm>
    where
        Data::Term: Clone,
        Data::NonTerm: std::hash::Hash + Eq + Clone,
    {
        use crate::hash::HashSet;
        use crate::rule::ShiftedRule;
        use crate::rule::ShiftedRuleRef;
        use crate::Backtrace;
        use crate::Token;
        use std::collections::BTreeSet;

        // root
        if self.node(node).len() == 0 && self.node(node).parent.is_none() {
            let state0 = &parser.get_states()[0];
            let mut rules = Vec::with_capacity(state0.get_rules().len());
            for rule in state0.get_rules().iter() {
                rules.push(ShiftedRule {
                    rule: parser.get_rules()[rule.rule].clone(),
                    shifted: rule.shifted,
                });
            }

            return Backtrace {
                traces: vec![rules],
            };
        }

        let mut traces = Vec::new();
        let mut current_rules: BTreeSet<_> = parser.get_states()[self.state(node)]
            .get_rules()
            .iter()
            // .filter(|rule| rule.shifted > 0)
            .copied()
            .collect();
        let mut next_rules = BTreeSet::new();
        traces.push(current_rules.clone());
        let mut zero_shifted_rules: HashSet<Data::NonTerm> = Default::default();

        for state_idx in self.state_iter(node).skip(1) {
            zero_shifted_rules.clear();
            next_rules.clear();
            for rule in current_rules.iter() {
                if rule.shifted > 0 {
                    next_rules.insert(ShiftedRuleRef {
                        rule: rule.rule,
                        shifted: rule.shifted - 1,
                    });
                    if rule.shifted == 1 {
                        zero_shifted_rules.insert(parser.get_rules()[rule.rule].name.clone());
                    }
                }
            }
            std::mem::swap(&mut current_rules, &mut next_rules);
            if zero_shifted_rules.is_empty() {
                continue;
            }

            loop {
                let len0 = current_rules.len();
                for rule in parser.get_states()[state_idx].get_rules().iter() {
                    let prod_rule = &parser.get_rules()[rule.rule];
                    if let Some(Token::NonTerm(nonterm)) = prod_rule.rule.get(rule.shifted) {
                        if zero_shifted_rules.contains(nonterm) {
                            current_rules.insert(*rule);
                            if rule.shifted == 0 {
                                zero_shifted_rules.insert(prod_rule.name.clone());
                            }
                        }
                    }
                }
                if len0 == current_rules.len() {
                    break;
                }
            }
            traces.push(current_rules.clone());
        }

        // node.iter() does not include root node.
        // so explicitly add root node.
        {
            let state_idx = 0;
            zero_shifted_rules.clear();
            next_rules.clear();
            for rule in current_rules.iter() {
                if rule.shifted > 0 {
                    next_rules.insert(ShiftedRuleRef {
                        rule: rule.rule,
                        shifted: rule.shifted - 1,
                    });
                    if rule.shifted == 1 {
                        zero_shifted_rules.insert(parser.get_rules()[rule.rule].name.clone());
                    }
                }
            }
            std::mem::swap(&mut current_rules, &mut next_rules);
            if !zero_shifted_rules.is_empty() {
                loop {
                    let len0 = current_rules.len();
                    for rule in parser.get_states()[state_idx].get_rules().iter() {
                        let prod_rule = &parser.get_rules()[rule.rule];
                        if let Some(Token::NonTerm(nonterm)) = prod_rule.rule.get(rule.shifted) {
                            if zero_shifted_rules.contains(nonterm) {
                                current_rules.insert(*rule);
                                if rule.shifted == 0 {
                                    zero_shifted_rules.insert(prod_rule.name.clone());
                                }
                            }
                        }
                    }
                    if len0 == current_rules.len() {
                        break;
                    }
                }
                traces.push(current_rules.clone());
            }
        }

        Backtrace {
            traces: traces
                .into_iter()
                .map(|rules| {
                    rules
                        .into_iter()
                        .map(|rule| ShiftedRule {
                            rule: parser.get_rules()[rule.rule].clone(),
                            shifted: rule.shifted,
                        })
                        .collect()
                })
                .collect(),
        }
    }

    /// Get backtrace infos for all paths.
    pub fn backtraces<'a, P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &'a self,
        parser: &'a P,
    ) -> impl Iterator<Item = crate::Backtrace<&'static str, P::NonTerm>> + 'a
    where
        Data::Term: Clone,
        Data::NonTerm: Clone + Hash + Eq,
    {
        self.current_nodes
            .iter()
            .map(|&node| self.backtrace_impl(parser, node))
    }

    fn expected_token_impl<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
        node: usize,
        terms: &mut std::collections::BTreeSet<usize>,
        nonterms: &mut std::collections::BTreeSet<Data::NonTerm>,
    ) where
        Data::NonTerm: Ord + Copy + std::hash::Hash + NonTerminal,
    {
        let s = self.state(node);
        let state = parser.get_states().get(s).expect("state must exist");

        terms.extend(state.expected_shift_term());
        nonterms.extend(state.expected_shift_nonterm());
    }

    /// Get next expected (terminals, non-terminals) for current context.
    pub fn expected_token<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
    ) -> (
        std::collections::BTreeSet<usize>,
        std::collections::BTreeSet<Data::NonTerm>,
    )
    where
        Data::NonTerm: Ord + Copy + Hash + NonTerminal,
    {
        let mut terms = std::collections::BTreeSet::new();
        let mut nonterms = std::collections::BTreeSet::new();
        for &node in self.current_nodes.iter() {
            self.expected_token_impl(parser, node, &mut terms, &mut nonterms);
        }

        (terms, nonterms)
    }
    /// Same as `expected_token()`, but returns as printable type.
    pub fn expected_token_str<'a, P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &'a P,
    ) -> (
        impl Iterator<Item = P::TerminalClassElement> + 'a,
        impl Iterator<Item = &'static str> + 'a,
    )
    where
        Data::NonTerm: Ord + Copy + Hash + crate::nonterminal::NonTerminal + 'a,
    {
        use crate::nonterminal::NonTerminal;
        let (terms, nonterms) = self.expected_token(parser);
        (
            terms
                .into_iter()
                .flat_map(|term| parser.get_terminals(term).unwrap()),
            nonterms.into_iter().map(|nonterm| nonterm.as_str()),
        )
    }

    /// Feed one terminal to parser, and update stacks.
    /// This will use `Default::default()` for location.
    pub fn feed<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &mut self,
        parser: &P,
        term: Data::Term,
        userdata: &mut Data::UserData,
    ) -> Result<(), ParseError<Data>>
    where
        P::Term: Clone,
        P::NonTerm: Hash + Eq + Clone + std::fmt::Debug + NonTerminal,
        Data: Clone,
        Data::Location: Default,
    {
        self.feed_location(parser, term, userdata, Default::default())
    }

    fn skip_last_n(&self, mut node: usize, mut count: usize) -> Option<(usize, usize)> {
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

    fn feed_location_impl<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &mut self,
        parser: &P,
        node: usize,
        term: TerminalSymbol<P::Term>,
        class: TerminalSymbol<usize>,
        shift_prec: Precedence,
        location: Option<Data::Location>,
        userdata: &mut Data::UserData,
    ) -> Result<(), (usize, TerminalSymbol<P::Term>, Option<Data::Location>)>
    where
        Data: Clone,
        Data::Term: Clone,
        Data::NonTerm: Clone + std::fmt::Debug + NonTerminal + Hash + Eq,
    {
        debug_assert!(
            (term.is_eof() && location.is_none()) || (!term.is_eof() && location.is_some())
        );
        debug_assert!(self.node(node).is_leaf());
        use crate::parser::State;

        let last_state = self.state(node);
        let shift_state = parser.get_states()[last_state].shift_goto_class(class);
        if let Some(reduce_rules) = parser.get_states()[last_state].reduce(class) {
            let mut shift = None;
            let mut reduces: smallvec::SmallVec<[_; 2]> = Default::default();

            for reduce_rule in reduce_rules {
                let rule = &parser.get_rules()[reduce_rule];
                let reduce_prec = match rule.precedence {
                    Some(crate::rule::Precedence::Fixed(level)) => Precedence::new(level as u8),
                    Some(crate::rule::Precedence::Dynamic(token_index)) => {
                        // fix the value to the offset from current node
                        let ith = rule.rule.len() - token_index - 1;
                        let (node, ith) = self.skip_last_n(node, ith).unwrap();
                        self.node(node).precedence_stack[ith]
                    }
                    None => Precedence::none(),
                };

                // if there is shift/reduce conflict, check for reduce rule's precedence and shift terminal's precedence
                match (shift_state.is_some(), shift_prec, reduce_prec) {
                    (true, Precedence(shift_prec_), Precedence(reduce_prec_))
                        if shift_prec.is_some() && reduce_prec.is_some() =>
                    {
                        match reduce_prec_.cmp(&shift_prec_) {
                            std::cmp::Ordering::Less => {
                                // no reduce
                                shift = shift_state;
                            }
                            std::cmp::Ordering::Equal => {
                                // check for reduce_type
                                use crate::builder::ReduceType;
                                match parser.precedence_types(reduce_prec_) {
                                    Some(ReduceType::Left) => {
                                        // no shift
                                        reduces.push((reduce_rule, reduce_prec));
                                    }
                                    Some(ReduceType::Right) => {
                                        // no reduce
                                        shift = shift_state;
                                    }
                                    None => {
                                        // cannot determine precedence, error
                                        self.no_precedences.push(reduce_rule);
                                    }
                                }
                            }
                            std::cmp::Ordering::Greater => {
                                // no shift
                                reduces.push((reduce_rule, reduce_prec));
                            }
                        }
                    }
                    _ => {
                        // nothing; go for both reduce and shift
                        shift = shift_state;
                        reduces.push((reduce_rule, reduce_prec));
                    }
                }
            }

            let mut shifted = false;
            let mut reduced_node = node;
            if !reduces.is_empty() {
                // call every reduce action
                // and check if every reduce action revoked shift
                let mut shift_ = false;
                let l = reduces.len();
                for (idx, (reduce_rule, precedence)) in reduces.into_iter().enumerate() {
                    let mut pass = shift.is_some();

                    // in `self.reduce()`, it will delete the node if it is leaf node.
                    // but there are multiple reduce actions for this node and also shift action,
                    // so we need to prevent this node from being cleared.
                    // force this node as non-leaf if it is not in the last reduce action, or there is a shift action left.
                    let prevent_leaf = idx < l - 1 || shift.is_some();
                    if prevent_leaf {
                        self.node_mut(node).child_count += 1;
                    }
                    match self.reduce(
                        parser,
                        reduce_rule,
                        precedence,
                        node,
                        &term,
                        &mut pass,
                        userdata,
                    ) {
                        Ok(next_node) => {
                            shift_ |= pass;
                            // reduce recursively

                            match self.feed_location_impl(
                                parser,
                                next_node,
                                term.clone(),
                                class,
                                shift_prec,
                                location.clone(),
                                userdata,
                            ) {
                                Ok(_) => {
                                    shifted = true;
                                }
                                Err((reduced_node_, _, _)) => {
                                    reduced_node = reduced_node_;
                                }
                            }
                        }
                        Err(err) => {
                            shift_ |= pass;
                            self.reduce_errors.push(err);
                        }
                    }

                    if prevent_leaf {
                        // if there are more reduce_rule left, or there is shift action,
                        // add child to this node to prevent it from being cleared (as leaf)
                        self.node_mut(node).child_count -= 1;
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
            }
            if let Some(shift) = shift {
                let node_ = self.node_mut(node);
                node_
                    .state_stack
                    .push(StateIndex::from_usize_unchecked(shift));
                node_.precedence_stack.push(shift_prec);
                if let Some(location) = &location {
                    node_.location_stack.push(location.clone());
                }
                #[cfg(feature = "tree")]
                node_
                    .tree_stack
                    .push(crate::tree::Tree::new_terminal(term.clone()));

                match term {
                    TerminalSymbol::Term(term) => {
                        node_.data_stack.push_terminal(term);
                    }
                    TerminalSymbol::Eof | TerminalSymbol::Error => {
                        node_.data_stack.push_empty();
                    }
                }

                self.next_nodes.push(node);
                Ok(())
            } else {
                if shifted {
                    Ok(())
                } else {
                    Err((reduced_node, term, location))
                }
            }
        } else if let Some(shift) = shift_state {
            let node_ = self.node_mut(node);
            node_
                .state_stack
                .push(StateIndex::from_usize_unchecked(shift));
            node_.precedence_stack.push(shift_prec);
            if let Some(location) = location {
                node_.location_stack.push(location);
            }
            #[cfg(feature = "tree")]
            node_
                .tree_stack
                .push(crate::tree::Tree::new_terminal(term.clone()));

            match term {
                TerminalSymbol::Term(term) => {
                    node_.data_stack.push_terminal(term);
                }
                TerminalSymbol::Eof | TerminalSymbol::Error => {
                    node_.data_stack.push_empty();
                }
            }

            self.next_nodes.push(node);
            Ok(())
        } else {
            // no reduce, no shift
            Err((node, term, location))
        }
    }

    fn panic_mode<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &mut self,
        parser: &P,
        mut node: usize,
        error_prec: Precedence,
        userdata: &mut Data::UserData,
    ) -> bool
    where
        Data: Clone,
        Data::Term: Clone,
        Data::NonTerm: std::hash::Hash + Eq + Clone + std::fmt::Debug + NonTerminal,
    {
        use crate::Location;

        let mut error_location = Data::Location::new(self.location_iter(node), 0);

        loop {
            match self.feed_location_impl(
                parser,
                node,
                TerminalSymbol::Error,
                TerminalSymbol::Error,
                error_prec,
                Some(error_location),
                userdata,
            ) {
                Ok(_) => {
                    return true;
                }
                Err((err_node, _, err_loc)) => {
                    if self.node(err_node).len() == 0 {
                        return false; // root node; no more nodes to process
                    }
                    error_location = Data::Location::new(
                        std::iter::once(&err_loc.unwrap()).chain(self.location_iter(err_node)),
                        2,
                    );
                    if let Some(next_node) = self.pop(err_node) {
                        node = next_node;
                    } else {
                        return false;
                    }
                }
            }
        }
    }

    /// Feed one terminal with location to parser, and update state stack.
    pub fn feed_location<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &mut self,
        parser: &P,
        term: P::Term,
        userdata: &mut Data::UserData,
        location: Data::Location,
    ) -> Result<(), ParseError<Data>>
    where
        P::Term: Clone,
        P::NonTerm: Hash + Eq + Clone + std::fmt::Debug + NonTerminal,
        Data: Clone,
    {
        use crate::parser::State;
        use crate::Location;

        self.reduce_errors.clear();
        self.no_precedences.clear();
        self.fallback_nodes.clear();
        self.next_nodes.clear();

        let class = parser.to_terminal_class(&term);
        let shift_prec = parser.class_precedence(TerminalSymbol::Term(class));

        let mut current_nodes = std::mem::take(&mut self.current_nodes);
        for node in current_nodes.drain(..) {
            if let Err((node, _, _)) = self.feed_location_impl(
                parser,
                node,
                TerminalSymbol::Term(term.clone()),
                TerminalSymbol::Term(class),
                shift_prec,
                Some(location.clone()),
                userdata,
            ) {
                // store to fallback nodes in case of all nodes failed to shift
                self.fallback_nodes.push(node);
            }
        }
        self.current_nodes = current_nodes;

        // next_nodes is empty; invalid terminal was given
        // check for panic mode
        // and restore nodes to original state from fallback_nodes
        if self.next_nodes.is_empty() {
            // early return if `error` token is not used in the grammar
            if !P::ERROR_USED {
                std::mem::swap(&mut self.current_nodes, &mut self.fallback_nodes);

                return Err(ParseError {
                    term: TerminalSymbol::Term(term),
                    location: Some(location),
                    reduce_action_errors: std::mem::take(&mut self.reduce_errors),
                    no_precedences: std::mem::take(&mut self.no_precedences),
                });
            }

            let error_prec = parser.class_precedence(TerminalSymbol::Error);

            let mut fallback_nodes = std::mem::take(&mut self.fallback_nodes);
            // try enter panic mode and store error nodes to next_nodes
            for node in fallback_nodes.drain(..) {
                self.panic_mode(parser, node, error_prec, userdata);
            }
            self.fallback_nodes = fallback_nodes;
            // if next_node is still empty, then no panic mode was entered, this is an error
            // restore current_nodes to fallback_nodes
            if self.next_nodes.is_empty() {
                Err(ParseError {
                    term: TerminalSymbol::Term(term),
                    location: Some(location),
                    reduce_action_errors: std::mem::take(&mut self.reduce_errors),
                    no_precedences: std::mem::take(&mut self.no_precedences),
                })
            } else {
                // try shift term to error state
                let mut next_nodes = std::mem::take(&mut self.next_nodes);
                for error_node in next_nodes.drain(..) {
                    let last_state = self.state(error_node);
                    if let Some(next_state) = parser.get_states()[last_state]
                        .shift_goto_class(TerminalSymbol::Term(class))
                    {
                        // A -> a . error b
                        // and b is fed, shift error and b
                        let node = self.node_mut(error_node);
                        node.state_stack
                            .push(StateIndex::from_usize_unchecked(next_state));
                        node.precedence_stack.push(shift_prec);
                        node.location_stack.push(location.clone());
                        #[cfg(feature = "tree")]
                        node.tree_stack.push(crate::tree::Tree::new_terminal(
                            TerminalSymbol::Term(term.clone()),
                        ));
                        node.data_stack.push_terminal(term.clone());

                        self.current_nodes.push(error_node);
                    } else {
                        // here, fed token is in `error` non-terminal
                        // so merge location with previous

                        let new_location = Data::Location::new(
                            std::iter::once(&location).chain(self.location_iter(error_node)),
                            2, // error node + fed token
                        );
                        let node = self.node_mut(error_node);
                        *node.location_stack.last_mut().unwrap() = new_location;

                        self.current_nodes.push(error_node);
                    }
                }
                self.next_nodes = next_nodes;
                Ok(())
            }
        } else {
            std::mem::swap(&mut self.current_nodes, &mut self.next_nodes);
            Ok(())
        }
    }

    /// Feed one terminal with location to parser, and update state stack.
    fn can_feed_impl<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
        node: usize,
        class: TerminalSymbol<usize>,
        shift_prec: Precedence,
    ) -> bool
    where
        P::NonTerm: std::hash::Hash + Eq + NonTerminal,
    {
        use crate::parser::State;

        let last_state = self.state(node);
        let shift_state = parser.get_states()[last_state].shift_goto_class(class);
        if let Some(reduce_rules) = parser.get_states()[last_state].reduce(class) {
            let mut shift = None;
            let mut reduces: smallvec::SmallVec<[_; 2]> = Default::default();

            for reduce_rule in reduce_rules {
                let rule = &parser.get_rules()[reduce_rule];
                let reduce_prec = match rule.precedence {
                    Some(crate::rule::Precedence::Fixed(level)) => Precedence::new(level as u8),
                    Some(crate::rule::Precedence::Dynamic(token_index)) => {
                        // fix the value to the offset from current node
                        let ith = rule.rule.len() - token_index - 1;
                        let (node, ith) = self.skip_last_n(node, ith).unwrap();
                        self.node(node).precedence_stack[ith]
                    }
                    None => Precedence::none(),
                };

                // if there is shift/reduce conflict, check for reduce rule's precedence and shift terminal's precedence
                match (shift_state.is_some(), shift_prec, reduce_prec) {
                    (true, Precedence(shift_prec_), Precedence(reduce_prec_))
                        if shift_prec.is_some() && reduce_prec.is_some() =>
                    {
                        match reduce_prec_.cmp(&shift_prec_) {
                            std::cmp::Ordering::Less => {
                                // no reduce
                                shift = shift_state;
                            }
                            std::cmp::Ordering::Equal => {
                                // check for reduce_type
                                use crate::builder::ReduceType;
                                match parser.precedence_types(reduce_prec_) {
                                    Some(ReduceType::Left) => {
                                        // no shift
                                        reduces.push((reduce_rule, reduce_prec));
                                    }
                                    Some(ReduceType::Right) => {
                                        // no reduce
                                        shift = shift_state;
                                    }
                                    None => {
                                        // cannot determine precedence
                                    }
                                }
                            }
                            std::cmp::Ordering::Greater => {
                                // no shift
                                reduces.push((reduce_rule, reduce_prec));
                            }
                        }
                    }
                    _ => {
                        // nothing; go for both reduce and shift
                        shift = shift_state;
                        reduces.push((reduce_rule, reduce_prec));
                    }
                }
            }

            shift.is_some() || !reduces.is_empty()
        } else if shift_state.is_some() {
            true
        } else {
            false
        }
    }

    /// Check if `term` can be feeded to current state.
    /// This does not simulate for reduce action error, or panic mode.
    /// So this function will return `false` even if term can be shifted as `error` token,
    /// and will return `true` if `Err` variant is returned by `reduce_action`.
    pub fn can_feed<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
        term: &P::Term,
    ) -> bool
    where
        P::NonTerm: Hash + Eq + NonTerminal,
    {
        let class = parser.to_terminal_class(term);
        let shift_prec = parser.class_precedence(TerminalSymbol::Term(class));
        self.current_nodes
            .iter()
            .any(|node| self.can_feed_impl(parser, *node, TerminalSymbol::Term(class), shift_prec))
    }

    /// Check if current context can enter panic mode.
    pub fn can_panic<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
    ) -> bool
    where
        Data::NonTerm: Hash + Eq + NonTerminal,
    {
        // if `error` token was not used in the grammar, early return here
        if !P::ERROR_USED {
            return false;
        }

        self.current_nodes.iter().any(|&node| {
            self.state_iter(node).any(|state| {
                let state = &parser.get_states()[state];
                state.shift_goto_class(TerminalSymbol::Error).is_some()
                    || state.reduce(TerminalSymbol::Error).is_some()
            })
        })
    }

    /// Feed eof symbol with default zero-length location from the end of stream.
    fn feed_eof<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &mut self,
        parser: &P,
        userdata: &mut Data::UserData,
    ) -> Result<(), ParseError<Data>>
    where
        P::Term: Clone,
        P::NonTerm: Hash + Eq + Clone + std::fmt::Debug + NonTerminal,
        Data: Clone,
    {
        self.reduce_errors.clear();
        self.no_precedences.clear();
        self.fallback_nodes.clear();
        self.next_nodes.clear();

        let mut current_nodes = std::mem::take(&mut self.current_nodes);
        for node in current_nodes.drain(..) {
            if let Err((node, _, _)) = self.feed_location_impl(
                parser,
                node,
                TerminalSymbol::Eof,
                TerminalSymbol::Eof,
                Precedence::none(),
                None,
                userdata,
            ) {
                self.fallback_nodes.push(node);
            }
        }
        self.current_nodes = current_nodes;

        // next_nodes is empty; invalid terminal was given
        // check for panic mode
        // and restore nodes to original state from fallback_nodes
        if self.next_nodes.is_empty() {
            std::mem::swap(&mut self.current_nodes, &mut self.fallback_nodes);

            return Err(ParseError {
                term: TerminalSymbol::Eof,
                location: None,
                reduce_action_errors: std::mem::take(&mut self.reduce_errors),
                no_precedences: std::mem::take(&mut self.no_precedences),
            });
        } else {
            std::mem::swap(&mut self.current_nodes, &mut self.next_nodes);
            Ok(())
        }
    }
    /// Check if current context can be terminated and get the start value.
    pub fn can_accept<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
    ) -> bool
    where
        P::NonTerm: Hash + Eq + NonTerminal,
    {
        self.current_nodes
            .iter()
            .any(|node| self.can_feed_impl(parser, *node, TerminalSymbol::Eof, Precedence::none()))
    }
}

impl<Data: DataStack, StateIndex: Index + Copy> Default for Context<Data, StateIndex> {
    fn default() -> Self {
        let mut context = Context {
            nodes_pool: Default::default(),
            empty_node_indices: Default::default(),
            current_nodes: Default::default(),
            next_nodes: Default::default(),
            reduce_errors: Default::default(),
            fallback_nodes: Default::default(),
            no_precedences: Default::default(),
        };
        let root_node = context.new_node();
        context.current_nodes.push(root_node);
        context
    }
}

impl<Data: DataStack, StateIndex: Index + Copy> Clone for Context<Data, StateIndex>
where
    Node<Data, StateIndex>: Clone,
{
    fn clone(&self) -> Self {
        Context {
            nodes_pool: self.nodes_pool.clone(),
            empty_node_indices: self.empty_node_indices.clone(),
            current_nodes: self.current_nodes.clone(),
            ..Default::default()
        }
    }
}

#[cfg(feature = "tree")]
impl<Data: DataStack, StateIndex: Index + Copy> std::fmt::Display for Context<Data, StateIndex>
where
    Data::Term: std::fmt::Display + Clone,
    Data::NonTerm: std::fmt::Display + Clone + crate::nonterminal::NonTerminal,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, path) in self.to_tree_lists().enumerate() {
            writeln!(f, "Path {}:", i)?;
            writeln!(f, "{}", path)?;
        }
        Ok(())
    }
}
#[cfg(feature = "tree")]
impl<Data: DataStack, StateIndex: Index + Copy> std::fmt::Debug for Context<Data, StateIndex>
where
    Data::Term: std::fmt::Debug + Clone,
    Data::NonTerm: std::fmt::Debug + Clone + crate::nonterminal::NonTerminal,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, path) in self.to_tree_lists().enumerate() {
            writeln!(f, "Path {}:", i)?;
            writeln!(f, "{:?}", path)?;
        }
        Ok(())
    }
}
