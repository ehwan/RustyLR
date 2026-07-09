use std::cell::RefCell;
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

/// Branch state returned when a GLR feed path reaches no terminal shift, but its
/// graph node is still live and must be removed by the caller.
///
/// The caller only needs the graph node to discard that branch; the lookahead terminal, location,
/// and user data are owned by the failed attempt and do not need to be returned.
struct FailedBranchToRemove {
    node: NodeId,
}

enum FailedFeedPlan {
    FailedAndNeedsRemoval(FailedBranchToRemove),
    FailedAndAlreadyRemoved,
}

#[derive(Clone, Copy)]
struct GlrFeedPlan<StateIndex> {
    shift: Option<crate::parser::table::ShiftTarget<StateIndex>>,
    /// Head of the reduction alternatives for this plan.
    ///
    /// Reductions are stored in `FeedPlanContainer::reductions` as a sibling-linked list instead
    /// of a nested `Vec` so that feed planning can reuse one flat allocation and roll back failed
    /// speculative branches with `truncate`.
    first_reduction: Option<usize>,
}

#[derive(Clone, Copy)]
struct PlannedGlrReduction<NonTerm, StateIndex> {
    rule_index: usize,
    tokens_len: usize,
    #[allow(dead_code)]
    lhs: NonTerm,
    next_nonterm_shift: crate::parser::table::ShiftTarget<StateIndex>,
    /// Continuation plan after applying this reduction.
    ///
    /// This is an index into `FeedPlanContainer::plans`, not an owned child, to keep the recursive
    /// plan tree in a flat arena. That avoids per-plan heap allocations and keeps speculative
    /// planning compatible with cheap append/truncate rollback.
    next_plan: usize,
    /// Next reduction alternative belonging to the same `GlrFeedPlan`.
    ///
    /// The sibling link represents a small adjacency list inside the flat `reductions` arena,
    /// avoiding a separate `Vec<PlannedGlrReduction>` allocation for every plan node.
    next_sibling: Option<usize>,
}

/// Non-empty prefix of a graph-structured-stack node used by CFG-only simulation.
///
/// Feed planning does not mutate parser nodes while it simulates reductions. When a simulated
/// reduction pops only part of a node segment, the remaining simulated stack is represented as
/// `node.state_stack[..prefix_len]`: a subrange taken from the front of that node's contiguous
/// stack segment. The suffix after `prefix_len` is ignored by the simulation.
#[derive(Clone, Copy)]
struct NodeSubRange {
    node: NodeId,
    prefix_len: NonZeroUsize,
}

impl NodeSubRange {
    fn new(node: NodeId, prefix_len: NonZeroUsize) -> Self {
        NodeSubRange { node, prefix_len }
    }

    fn node(self) -> NodeId {
        self.node
    }

    fn len(self) -> usize {
        self.prefix_len.get()
    }

    fn last_index(self) -> usize {
        self.len() - 1
    }
}

struct FeedPlanContainer<NonTerm, StateIndex> {
    /// Feed plans generated by the CFG-only GLR pre-feed simulation.
    ///
    /// Plans are stored in DFS pre-order, so replay usually visits child plans close to the
    /// parent plan in memory.
    plans: Vec<GlrFeedPlan<StateIndex>>,

    /// Flat sibling-linked reduction records used by `plans`.
    ///
    /// Keeping reductions in one reusable buffer avoids allocating a nested `Vec` per plan node.
    reductions: Vec<PlannedGlrReduction<NonTerm, StateIndex>>,

    /// Reusable simulated state stacks for GLR feed planning alternatives.
    extra_state_stacks: Vec<Vec<StateIndex>>,
}

impl<NonTerm, StateIndex> Default for FeedPlanContainer<NonTerm, StateIndex> {
    fn default() -> Self {
        FeedPlanContainer {
            plans: Default::default(),
            reductions: Default::default(),
            extra_state_stacks: Default::default(),
        }
    }
}

impl<NonTerm, StateIndex> FeedPlanContainer<NonTerm, StateIndex> {
    fn clear_plans(&mut self) {
        self.plans.clear();
        self.reductions.clear();
    }

    fn take_extra_state_stack(&mut self) -> Vec<StateIndex> {
        let mut stack = self.extra_state_stacks.pop().unwrap_or_default();
        stack.clear();
        stack
    }

    fn put_extra_state_stack(&mut self, mut stack: Vec<StateIndex>) {
        stack.clear();
        self.extra_state_stacks.push(stack);
    }

    fn node_subrange<P, Data, Start, const MAX_REDUCE_RULES: usize>(
        &self,
        parser: &Context<P, Data, Start, StateIndex, MAX_REDUCE_RULES>,
        node: NodeId,
    ) -> Option<NodeSubRange>
    where
        P: Parser<Term = Data::Term, NonTerm = NonTerm, StateIndex = StateIndex>,
        Data: SemanticValue<NonTerm = NonTerm>,
        NonTerm: NonTerminal,
        Start: StartExtractor<Data>,
        StateIndex: Index,
    {
        let node_ = parser.node(node);
        if node_.len() == 0 {
            None
        } else {
            Some(NodeSubRange::new(
                node,
                NonZeroUsize::new(node_.len()).unwrap(),
            ))
        }
    }

    fn simulated_state<P, Data, Start, const MAX_REDUCE_RULES: usize>(
        parser: &Context<P, Data, Start, StateIndex, MAX_REDUCE_RULES>,
        extra_state_stack: &[StateIndex],
        node_range: Option<NodeSubRange>,
    ) -> usize
    where
        P: Parser<Term = Data::Term, NonTerm = NonTerm, StateIndex = StateIndex>,
        Data: SemanticValue<NonTerm = NonTerm>,
        NonTerm: NonTerminal,
        Start: StartExtractor<Data>,
        StateIndex: Index,
    {
        extra_state_stack
            .last()
            .copied()
            .map(Index::into_usize)
            .unwrap_or_else(|| {
                node_range
                    .map(|range| {
                        parser.node(range.node()).state_stack[range.last_index()].into_usize()
                    })
                    .unwrap_or(0)
            })
    }

    fn simulate_planned_reduction<P, Data, Start, const MAX_REDUCE_RULES: usize>(
        parser: &Context<P, Data, Start, StateIndex, MAX_REDUCE_RULES>,
        extra_state_stack: &mut Vec<StateIndex>,
        mut node_range: Option<NodeSubRange>,
        rule_index: usize,
    ) -> (
        Option<NodeSubRange>,
        NonTerm,
        usize,
        crate::parser::table::ShiftTarget<StateIndex>,
    )
    where
        P: Parser<Term = Data::Term, NonTerm = NonTerm, StateIndex = StateIndex>,
        Data: SemanticValue<NonTerm = NonTerm>,
        NonTerm: NonTerminal,
        Start: StartExtractor<Data>,
        StateIndex: Index,
    {
        let rule_info = *parser.tables.rule(rule_index);
        let tokens_len = rule_info.len;

        if tokens_len <= extra_state_stack.len() {
            extra_state_stack.truncate(extra_state_stack.len() - tokens_len);
        } else {
            let left = tokens_len - extra_state_stack.len();
            extra_state_stack.clear();

            let range = node_range.unwrap();
            let node = range.node();
            let len = range.len();

            if left < len {
                node_range = Some(NodeSubRange::new(
                    node,
                    NonZeroUsize::new(len - left).unwrap(),
                ));
            } else {
                let parent = parser.node(node).parent;
                if let Some(parent) = parent {
                    node_range = parser.skip_last_n(parent, left - len).map(|(node, i)| {
                        NodeSubRange::new(node, NonZeroUsize::new(i + 1).unwrap())
                    });
                } else {
                    node_range = None;
                }
            }
        }

        let last_state = Self::simulated_state(parser, extra_state_stack, node_range);
        let Some(next_nonterm_shift) = parser.tables.shift_goto_nonterm(last_state, rule_info.lhs)
        else {
            unreachable!(
                "unreachable: nonterminal shift should always succeed after reduce operation, but failed for nonterminal '{}' from state {:?}",
                rule_info.lhs.as_str(),
                last_state
            );
        };
        extra_state_stack.push(next_nonterm_shift.state);

        (node_range, rule_info.lhs, tokens_len, next_nonterm_shift)
    }

    fn plan_feed<P, Data, Start, const MAX_REDUCE_RULES: usize>(
        &mut self,
        parser: &Context<P, Data, Start, StateIndex, MAX_REDUCE_RULES>,
        extra_state_stack: &mut Vec<StateIndex>,
        node_range: Option<NodeSubRange>,
        class: P::TermClass,
    ) -> Option<usize>
    where
        P: Parser<Term = Data::Term, NonTerm = NonTerm, StateIndex = StateIndex>,
        Data: SemanticValue<NonTerm = NonTerm>,
        NonTerm: NonTerminal,
        Start: StartExtractor<Data>,
        StateIndex: Index,
    {
        self.clear_plans();
        self.plan_feed_impl(parser, extra_state_stack, node_range, class)
    }

    fn plan_feed_impl<P, Data, Start, const MAX_REDUCE_RULES: usize>(
        &mut self,
        parser: &Context<P, Data, Start, StateIndex, MAX_REDUCE_RULES>,
        extra_state_stack: &mut Vec<StateIndex>,
        node_range: Option<NodeSubRange>,
        class: P::TermClass,
    ) -> Option<usize>
    where
        P: Parser<Term = Data::Term, NonTerm = NonTerm, StateIndex = StateIndex>,
        Data: SemanticValue<NonTerm = NonTerm>,
        NonTerm: NonTerminal,
        Start: StartExtractor<Data>,
        StateIndex: Index,
    {
        use crate::parser::table::ReduceRules;

        let last_state = Self::simulated_state(parser, extra_state_stack, node_range);
        let (shift, reduce) = match parser.tables.term_action(last_state, class) {
            Some(action) => (action.shift(), action.reduce()),
            None => (None, None),
        };

        let plan_id = self.plans.len();
        self.plans.push(GlrFeedPlan {
            shift,
            first_reduction: None,
        });

        let mut first_reduction: Option<usize> = None;
        let mut previous_reduction: Option<usize> = None;

        if let Some(reduce_rules) = reduce {
            for reduce_rule in reduce_rules.to_iter() {
                let rule_index = reduce_rule.into_usize();
                let reduction_id = self.reductions.len();
                self.reductions.push(PlannedGlrReduction {
                    rule_index,
                    tokens_len: 0,
                    lhs: parser.tables.rule(rule_index).lhs,
                    next_nonterm_shift: crate::parser::table::ShiftTarget {
                        state: StateIndex::from_usize_unchecked(0),
                        push: false,
                    },
                    next_plan: usize::MAX,
                    next_sibling: None,
                });

                let plan_checkpoint = self.plans.len();
                let mut branch_extra_state_stack = self.take_extra_state_stack();
                branch_extra_state_stack.extend_from_slice(extra_state_stack);

                let (next_node_range, lhs, tokens_len, next_nonterm_shift) =
                    Self::simulate_planned_reduction(
                        parser,
                        &mut branch_extra_state_stack,
                        node_range,
                        rule_index,
                    );

                let next_plan = self.plan_feed_impl(
                    parser,
                    &mut branch_extra_state_stack,
                    next_node_range,
                    class,
                );
                self.put_extra_state_stack(branch_extra_state_stack);

                if let Some(next_plan) = next_plan {
                    self.reductions[reduction_id] = PlannedGlrReduction {
                        rule_index,
                        tokens_len,
                        lhs,
                        next_nonterm_shift,
                        next_plan,
                        next_sibling: None,
                    };

                    if let Some(previous_reduction) = previous_reduction {
                        self.reductions[previous_reduction].next_sibling = Some(reduction_id);
                    } else {
                        first_reduction = Some(reduction_id);
                    }
                    previous_reduction = Some(reduction_id);
                } else {
                    self.plans.truncate(plan_checkpoint);
                    self.reductions.truncate(reduction_id);
                }
            }
        }

        if shift.is_none() && first_reduction.is_none() {
            debug_assert_eq!(self.plans.len(), plan_id + 1);
            self.plans.pop();
            None
        } else {
            self.plans[plan_id] = GlrFeedPlan {
                shift,
                first_reduction,
            };
            Some(plan_id)
        }
    }
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

    /// Reusable container for CFG-only GLR feed plans.
    feed_plan_container: FeedPlanContainer<P::NonTerm, StateIndex>,

    /// Reusable plan container for `can_feed`/`can_accept`/`can_panic`.
    ///
    /// These queries share the same CFG-only planner as real feeds, but keep their buffers
    /// separate because they only borrow the context immutably.
    can_feed_plan_container: RefCell<FeedPlanContainer<P::NonTerm, StateIndex>>,

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
            feed_plan_container: Default::default(),
            can_feed_plan_container: Default::default(),
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
    fn node_is_live(&self, node: NodeId) -> bool {
        node.index() < self.nodes_pool.len() && !self.empty_node_indices.contains(&node)
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
    pub(crate) fn try_remove_node_subtree_recursive(&mut self, node: NodeId) -> Option<NodeId> {
        let children = self
            .nodes_pool
            .iter()
            .enumerate()
            .filter_map(|(idx, child)| {
                let child_id = NodeId::new(idx);
                if !self.empty_node_indices.contains(&child_id) && child.parent == Some(node) {
                    Some(child_id)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        for child in children {
            self.try_remove_node_subtree_recursive(child);
        }

        if self.node_is_live(node) {
            self.try_remove_node_recursive(node)
        } else {
            None
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

    fn reduce_planned(
        &mut self,
        reduction: PlannedGlrReduction<P::NonTerm, StateIndex>,
        node: NodeId,
        term: &crate::TerminalSymbol<P::Term>,
        shift: &mut bool,
        can_reuse_node_for_reduce: bool,
        userdata: &mut Data::UserData,
    ) -> Result<NodeId, Data::ReduceActionError>
    where
        Data: Clone,
        P::Term: Clone,
    {
        use crate::Location;
        let count = reduction.tokens_len;
        let mut new_location = Data::Location::new(self.location_iter(node), count);

        let node_to_shift = self.prepare_reduce_node(node, count, count, can_reuse_node_for_reduce);

        let node = self.node_mut(node_to_shift);
        #[cfg(feature = "tree")]
        let trees = node.tree_stack.split_off(node.tree_stack.len() - count);

        match Data::reduce_action(
            &mut node.data_stack,
            &mut node.location_stack,
            reduction.next_nonterm_shift.push,
            reduction.rule_index,
            shift,
            term,
            userdata,
            &mut new_location,
        ) {
            Ok(_) => {
                node.state_stack.push(reduction.next_nonterm_shift.state);
                node.location_stack.push(new_location);
                #[cfg(feature = "tree")]
                {
                    node.tree_stack
                        .push(crate::tree::Tree::new_nonterminal(reduction.lhs, trees));
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
        node_range: Option<NodeSubRange>,
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
                node_range
                    .map(|range| {
                        self.node(range.node()).state_stack[range.last_index()].into_usize()
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
            let mut node_range = node_range;
            let mut extra_state_stack = if tokens_len > extra_state_stack.len() {
                tokens_len -= extra_state_stack.len();

                let range = node_range.unwrap();
                let node = range.node();
                let node_len = range.len();

                if tokens_len < node_len {
                    node_range = Some(NodeSubRange::new(
                        node,
                        NonZeroUsize::new(node_len - tokens_len).unwrap(),
                    ));
                } else {
                    let parent = self.node(node).parent;
                    if let Some(parent) = parent {
                        node_range =
                            self.skip_last_n(parent, tokens_len - node_len)
                                .map(|(node, i)| {
                                    NodeSubRange::new(node, NonZeroUsize::new(i + 1).unwrap())
                                });
                    } else {
                        // reached root node
                        node_range = None;
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
                    node_range
                        .map(|range| {
                            self.node(range.node()).state_stack[range.last_index()].into_usize()
                        })
                        .unwrap_or(0)
                });
            if let Some(next_state) = self.tables.shift_goto_nonterm(state, nonterm) {
                extra_state_stack.push(next_state.state);
                self.expected_token_impl(&mut extra_state_stack, node_range, terms, nonterms);
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
            let node_range = {
                let node_ = self.node(node);
                if node_.len() == 0 {
                    // only root node can have 0 length stack
                    None
                } else {
                    Some(NodeSubRange::new(
                        node,
                        NonZeroUsize::new(node_.len()).unwrap(),
                    ))
                }
            };
            extra_state_stack.clear();

            self.expected_token_impl(
                &mut extra_state_stack,
                node_range,
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

    fn shift_terminal_for_plan(
        &mut self,
        node: NodeId,
        shift: crate::parser::table::ShiftTarget<StateIndex>,
        term: TerminalSymbol<P::Term>,
        location: Data::Location,
        userdata: Data::UserData,
    ) where
        P::Term: Clone,
    {
        let shift_node = if self.node(node).is_leaf() {
            node
        } else {
            let new_node_idx = self.new_node_with_capacity(1);
            self.add_child(node, new_node_idx);
            new_node_idx
        };

        let node_ = self.node_mut(shift_node);
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
                TerminalSymbol::Error | TerminalSymbol::Eof | TerminalSymbol::VirtualStart(_) => {
                    node_.data_stack.push(Data::new_empty());
                }
            }
        } else {
            node_.data_stack.push(Data::new_empty());
        }

        self.next_branches.push(Branch::new(shift_node, userdata));
    }

    fn apply_feed_plan(
        &mut self,
        container: &FeedPlanContainer<P::NonTerm, StateIndex>,
        plan_id: usize,
        node: NodeId,
        term: TerminalSymbol<P::Term>,
        location: Data::Location,
        userdata: Data::UserData,
    ) -> Result<(), FailedFeedPlan>
    where
        Data: Clone,
        Data::UserData: Clone,
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
    {
        let plan = container.plans[plan_id];

        if let Some(mut reduction_id) = plan.first_reduction {
            let mut shifted = false;
            let mut failed_branch = None;
            let mut shift_allowed = false;
            let mut node_removed = false;
            let mut reused_node_kept = false;

            loop {
                let reduction = container.reductions[reduction_id];
                let mut pass = plan.shift.is_some();
                let mut branch_userdata = userdata.clone();
                let can_reuse_node_for_reduce =
                    reduction.next_sibling.is_none() && plan.shift.is_none();

                match self.reduce_planned(
                    reduction,
                    node,
                    &term,
                    &mut pass,
                    can_reuse_node_for_reduce,
                    &mut branch_userdata,
                ) {
                    Ok(next_node) => {
                        shift_allowed |= pass;
                        let mut branch_accepted = false;
                        match self.apply_feed_plan(
                            container,
                            reduction.next_plan,
                            next_node,
                            term.clone(),
                            location.clone(),
                            branch_userdata,
                        ) {
                            Ok(()) => {
                                shifted = true;
                                branch_accepted = true;
                            }
                            Err(FailedFeedPlan::FailedAndNeedsRemoval(branch_to_remove)) => {
                                if let Some(previous) = failed_branch.replace(branch_to_remove) {
                                    self.try_remove_node_subtree_recursive(previous.node);
                                }
                            }
                            Err(FailedFeedPlan::FailedAndAlreadyRemoved) => {}
                        }
                        if can_reuse_node_for_reduce {
                            reused_node_kept = branch_accepted;
                        }
                    }
                    Err(err) => {
                        shift_allowed |= pass;
                        if can_reuse_node_for_reduce {
                            node_removed = true;
                        }
                        self.reduce_errors.push(err);
                    }
                }

                if let Some(next_sibling) = reduction.next_sibling {
                    reduction_id = next_sibling;
                } else {
                    break;
                }
            }

            let mut shift = plan.shift;
            if !shift_allowed {
                if shift.is_some() {
                    self.try_remove_node_recursive(node);
                    node_removed = true;
                    shift = None;
                }
            }

            if let Some(shift) = shift {
                if let Some(branch_to_remove) = failed_branch {
                    self.try_remove_node_subtree_recursive(branch_to_remove.node);
                }
                self.shift_terminal_for_plan(node, shift, term, location, userdata);
                Ok(())
            } else if shifted {
                if let Some(branch_to_remove) = failed_branch {
                    self.try_remove_node_subtree_recursive(branch_to_remove.node);
                }
                if !node_removed
                    && !reused_node_kept
                    && self.node_is_live(node)
                    && self.node(node).is_leaf()
                {
                    self.try_remove_node_recursive(node);
                }
                Ok(())
            } else {
                match failed_branch {
                    Some(failed_branch) => {
                        Err(FailedFeedPlan::FailedAndNeedsRemoval(failed_branch))
                    }
                    None if node_removed => Err(FailedFeedPlan::FailedAndAlreadyRemoved),
                    None => Err(FailedFeedPlan::FailedAndNeedsRemoval(
                        FailedBranchToRemove { node },
                    )),
                }
            }
        } else if let Some(shift) = plan.shift {
            self.shift_terminal_for_plan(node, shift, term, location, userdata);
            Ok(())
        } else {
            unreachable!("GLR feed plan must contain at least one shift or reduction")
        }
    }

    fn discard_branch_node(&mut self, node: NodeId) {
        self.try_remove_node_subtree_recursive(node);
    }

    fn discard_fallback_branches(&mut self) {
        while let Some(branch) = self.fallback_branches.pop() {
            self.discard_branch_node(branch.node);
        }
    }

    fn panic_mode(
        &mut self,
        container: &mut FeedPlanContainer<P::NonTerm, StateIndex>,
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
        let mut error_location_preserved = None;

        let (pop_count, plan_id) = loop {
            if !self.node(node).is_leaf() {
                self.try_remove_node(node);
                return;
            }

            let node_len = self.node(node).len();
            let mut pop_count = 0;
            let mut found_plan = None;

            for retained_len in (1..=node_len).rev() {
                extra_state_stack.clear();

                if let Some(plan_id) = container.plan_feed(
                    self,
                    extra_state_stack,
                    Some(NodeSubRange::new(
                        node,
                        NonZeroUsize::new(retained_len).unwrap(),
                    )),
                    P::TermClass::ERROR,
                ) {
                    found_plan = Some(plan_id);
                    break;
                }

                pop_count += 1;
            }

            if let Some(plan_id) = found_plan {
                break (pop_count, plan_id);
            } else {
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
        match self.apply_feed_plan(
            container,
            plan_id,
            node,
            TerminalSymbol::Error,
            error_location,
            userdata,
        ) {
            Ok(()) => {}
            Err(FailedFeedPlan::FailedAndNeedsRemoval(branch_to_remove)) => {
                self.try_remove_node_subtree_recursive(branch_to_remove.node);
            }
            Err(FailedFeedPlan::FailedAndAlreadyRemoved) => {}
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

        let mut feedable_branch_found = false;
        let mut accepted_branch_found = false;
        let mut container = std::mem::take(&mut self.feed_plan_container);
        let mut extra_state_stack = container.take_extra_state_stack();
        while let Some(Branch { node, userdata }) = self.current_branches.pop() {
            // Plan before mutating the graph-structured stack so syntax failures stay cleanly
            // separated from semantic reduce-action failures.
            extra_state_stack.clear();
            let node_range = container.node_subrange(self, node);

            if let Some(plan_id) =
                container.plan_feed(self, &mut extra_state_stack, node_range, class)
            {
                feedable_branch_found = true;

                // Once the CFG admits this terminal, later failures are semantic/runtime branch
                // pruning and must not be reclassified as syntax recovery input.
                match self.apply_feed_plan(
                    &container,
                    plan_id,
                    node,
                    TerminalSymbol::Terminal(term.clone()),
                    location.clone(),
                    userdata,
                ) {
                    Ok(()) => {
                        accepted_branch_found = true;
                    }
                    Err(FailedFeedPlan::FailedAndNeedsRemoval(branch_to_remove)) => {
                        self.try_remove_node_subtree_recursive(branch_to_remove.node);
                    }
                    Err(FailedFeedPlan::FailedAndAlreadyRemoved) => {}
                }
            } else {
                // Keep grammatical `NoAction` branches only as recovery candidates; a surviving
                // sibling branch makes recovery inappropriate for this lookahead.
                self.fallback_branches.push(Branch::new(node, userdata));
            }
        }
        container.put_extra_state_stack(extra_state_stack);

        if !accepted_branch_found {
            debug_assert!(self.next_branches.is_empty());

            if !P::ERROR_USED || feedable_branch_found {
                // Recovery is reserved for pure syntax failure. If an admitted branch died while
                // replaying its plan, report that semantic/runtime failure instead.

                std::mem::swap(&mut self.current_branches, &mut self.fallback_branches);
                self.feed_plan_container = container;

                return Err(ParseError {
                    term: TerminalSymbol::Terminal(term),
                    location,
                    reduce_action_errors: std::mem::take(&mut self.reduce_errors),
                    states: self.states().collect(),
                });
            }

            let mut extra_state_stack = container.take_extra_state_stack();
            while let Some(Branch { node, userdata }) = self.fallback_branches.pop() {
                // Recovery searches only concrete branch stacks; the implicit root state precedes
                // the virtual start symbol and cannot accept a synthetic `error`.
                self.panic_mode(&mut container, node, userdata, &mut extra_state_stack);
            }
            container.put_extra_state_stack(extra_state_stack);

            if self.next_branches.is_empty() {
                self.feed_plan_container = container;
                Err(ParseError {
                    term: TerminalSymbol::Terminal(term),
                    location,
                    reduce_action_errors: std::mem::take(&mut self.reduce_errors),
                    states: self.states().collect(),
                })
            } else {
                debug_assert!(self.current_branches.is_empty());
                std::mem::swap(&mut self.current_branches, &mut self.next_branches);
                let mut extra_state_stack = container.take_extra_state_stack();
                // `error` was feed, now check with the original terminal symbol again.
                // If the original terminal is still not accepted, it is part of the `error`.
                while let Some(Branch {
                    node: error_node,
                    userdata,
                }) = self.current_branches.pop()
                {
                    extra_state_stack.clear();
                    let node_range = container.node_subrange(self, error_node);

                    if let Some(plan_id) =
                        container.plan_feed(self, &mut extra_state_stack, node_range, class)
                    {
                        // After shifting `error`, the original lookahead is a normal feed again:
                        // it may need nullable reductions before the terminal shift is visible.
                        match self.apply_feed_plan(
                            &container,
                            plan_id,
                            error_node,
                            TerminalSymbol::Terminal(term.clone()),
                            location.clone(),
                            userdata,
                        ) {
                            Ok(()) => {}
                            Err(FailedFeedPlan::FailedAndNeedsRemoval(branch_to_remove)) => {
                                self.try_remove_node_subtree_recursive(branch_to_remove.node);
                            }
                            Err(FailedFeedPlan::FailedAndAlreadyRemoved) => {}
                        }
                    } else {
                        // Otherwise the lookahead is part of the `error`; merge it so
                        // diagnostics report the full discarded range.
                        let new_location = Data::Location::new(
                            std::iter::once(&location).chain(self.location_iter(error_node)),
                            2, // error node + fed token
                        );
                        let node = self.node_mut(error_node);
                        *node.location_stack.last_mut().unwrap() = new_location;

                        self.next_branches.push(Branch::new(error_node, userdata));
                    }
                }
                container.put_extra_state_stack(extra_state_stack);

                if self.next_branches.is_empty() {
                    self.feed_plan_container = container;
                    Err(ParseError {
                        term: TerminalSymbol::Terminal(term),
                        location,
                        reduce_action_errors: std::mem::take(&mut self.reduce_errors),
                        states: self.states().collect(),
                    })
                } else {
                    // A recovered branch keeps the feed successful. Semantic failures from sibling
                    // recovery branches are still surfaced, matching ordinary GLR branch pruning.
                    let errors = if self.reduce_errors.is_empty() {
                        None
                    } else {
                        Some(ParseError {
                            term: TerminalSymbol::Terminal(term),
                            location: location.clone(),
                            reduce_action_errors: std::mem::take(&mut self.reduce_errors),
                            states: Vec::new(),
                        })
                    };

                    std::mem::swap(&mut self.current_branches, &mut self.next_branches);
                    // Recovery consumed or absorbed the lookahead, so the original syntax failure
                    // has been transformed into live recovery branches.
                    self.feed_plan_container = container;
                    Ok(FeedSuccess { errors })
                }
            }
        } else {
            debug_assert!(!self.next_branches.is_empty());
            // Surviving original branches define feed success; sibling failures are diagnostics,
            // not a fatal parse result.
            let errors = if self.fallback_branches.is_empty() && self.reduce_errors.is_empty() {
                None
            } else {
                let states = self
                    .fallback_branches
                    .iter()
                    .map(|branch| self.state(branch.node))
                    .collect();
                self.discard_fallback_branches();
                Some(ParseError {
                    term: TerminalSymbol::Terminal(term),
                    location,
                    reduce_action_errors: std::mem::take(&mut self.reduce_errors),
                    states,
                })
            };
            std::mem::swap(&mut self.current_branches, &mut self.next_branches);
            self.feed_plan_container = container;
            Ok(FeedSuccess { errors })
        }
    }
    /// Check if `term` can be feeded to current state.
    /// This does not simulate for reduce action error, or panic mode.
    /// So this function will return `false` even if term can be shifted as `error` token,
    /// and will return `true` if `Err` variant is returned by `reduce_action`.
    pub fn can_feed(&self, term: &P::Term) -> bool {
        let class = P::TermClass::from_term(term);
        let mut container = self.can_feed_plan_container.borrow_mut();
        let mut extra_state_stack = container.take_extra_state_stack();
        let can_feed = self.current_branches.iter().any(|branch| {
            let node = branch.node;
            extra_state_stack.clear();

            let node_range = container.node_subrange(self, node);
            container
                .plan_feed(self, &mut extra_state_stack, node_range, class)
                .is_some()
        });
        container.put_extra_state_stack(extra_state_stack);
        can_feed
    }

    /// Check if current context can enter panic mode.
    pub fn can_panic(&self) -> bool {
        // if `error` token was not used in the grammar, early return here
        if !P::ERROR_USED {
            return false;
        }

        let mut container = self.can_feed_plan_container.borrow_mut();
        let mut extra_state_stack = container.take_extra_state_stack();

        let can_panic = self.current_branches.iter().any(|branch| {
            let mut node = branch.node;
            let mut len = self.node(node).len();

            if len > 0 {
                loop {
                    extra_state_stack.clear();

                    if container
                        .plan_feed(
                            self,
                            &mut extra_state_stack,
                            Some(NodeSubRange::new(node, NonZeroUsize::new(len).unwrap())),
                            P::TermClass::ERROR,
                        )
                        .is_some()
                    {
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
        });
        container.put_extra_state_stack(extra_state_stack);
        can_panic
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

        let mut container = std::mem::take(&mut self.feed_plan_container);
        let mut extra_state_stack = container.take_extra_state_stack();
        while let Some(Branch { node, userdata }) = self.current_branches.pop() {
            let node_eof_location = Data::Location::new(self.location_iter(node), 0);

            extra_state_stack.clear();
            let node_range = container.node_subrange(self, node);

            if let Some(plan_id) =
                container.plan_feed(self, &mut extra_state_stack, node_range, P::TermClass::EOF)
            {
                match self.apply_feed_plan(
                    &container,
                    plan_id,
                    node,
                    TerminalSymbol::Eof,
                    node_eof_location,
                    userdata,
                ) {
                    Ok(()) => {}
                    Err(FailedFeedPlan::FailedAndNeedsRemoval(branch_to_remove)) => {
                        self.try_remove_node_subtree_recursive(branch_to_remove.node);
                    }
                    Err(FailedFeedPlan::FailedAndAlreadyRemoved) => {}
                }
            } else {
                self.fallback_branches.push(Branch::new(node, userdata));
            }
        }
        container.put_extra_state_stack(extra_state_stack);

        // next_branches is empty; invalid terminal was given
        // check for panic mode
        // and restore nodes to original state from fallback_branches
        if self.next_branches.is_empty() {
            std::mem::swap(&mut self.current_branches, &mut self.fallback_branches);
            self.feed_plan_container = container;

            Err(ParseError {
                term: TerminalSymbol::Eof,
                location: eof_location,
                reduce_action_errors: std::mem::take(&mut self.reduce_errors),
                states: self.states().collect(),
            })
        } else {
            self.discard_fallback_branches();
            std::mem::swap(&mut self.current_branches, &mut self.next_branches);
            self.feed_plan_container = container;
            Ok(())
        }
    }
    /// Check if current context can be terminated and get the start value.
    pub fn can_accept(&self) -> bool {
        let mut container = self.can_feed_plan_container.borrow_mut();
        let mut extra_state_stack = container.take_extra_state_stack();
        let can_accept = self.current_branches.iter().any(|branch| {
            let node = branch.node;
            extra_state_stack.clear();

            let node_range = container.node_subrange(self, node);
            container
                .plan_feed(self, &mut extra_state_stack, node_range, P::TermClass::EOF)
                .is_some()
        });
        container.put_extra_state_stack(extra_state_stack);
        can_accept
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
            feed_plan_container: Default::default(),
            can_feed_plan_container: Default::default(),
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
