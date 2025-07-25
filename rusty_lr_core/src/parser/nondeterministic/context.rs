use std::hash::Hash;

use super::Node;
use super::ParseError;

use crate::nonterminal::NonTerminal;
use crate::nonterminal::TokenData;
use crate::parser::Parser;
use crate::TerminalSymbol;

type SmallVecNode = smallvec::SmallVec<[usize; 3]>;

/// Iterator for traverse node to root.
/// Note that root node is not included in this iterator.
#[derive(Clone)]
pub struct NodeRefIterator<'a, Data: TokenData> {
    context: &'a Context<Data>,
    node: Option<usize>,
}

/// A struct that maintains the current state and the values associated with each symbol.
/// This handles the divergence and merging of the parser.
pub struct Context<Data: TokenData> {
    pub(crate) nodes_pool: Vec<Node<Data>>,
    pub(crate) empty_node_indices: std::collections::BTreeSet<usize>,

    /// each element represents an end-point of diverged paths.
    pub(crate) current_nodes: SmallVecNode,

    /// temporary storage
    pub(crate) next_nodes: SmallVecNode,

    /// For recovery from error
    pub(crate) fallback_nodes: SmallVecNode,

    /// For temporary use. store arguments for calling `reduce_action`.
    /// But we don't want to reallocate every `feed` call
    pub(crate) reduce_args: crate::nonterminal::ReduceArgsStack<Data>,

    /// For temporary use. store reduce errors returned from `reduce_action`.
    /// But we don't want to reallocate every `feed` call
    pub(crate) reduce_errors: Vec<Data::ReduceActionError>,

    /// For temporary use.
    /// store rule indices where shift/reduce conflicts occured with no precedence defined.
    pub(crate) no_precedences: Vec<usize>,
}

impl<'a, Data: TokenData> Iterator for NodeRefIterator<'a, Data> {
    type Item = &'a Node<Data>;

    fn next(&mut self) -> Option<Self::Item> {
        let node = &self.context.nodes_pool[self.node?];
        self.node = node.parent;
        Some(node)
    }
}

impl<Data: TokenData> Context<Data> {
    /// Create a new context.
    /// `current_nodes` is initialized with a root node.
    pub fn new() -> Self {
        Default::default()
    }

    /// Create a new node in the pool and return its index.
    pub(crate) fn new_node(&mut self) -> usize {
        if let Some(idx) = self.empty_node_indices.pop_first() {
            idx
        } else {
            let idx = self.nodes_pool.len();
            self.nodes_pool.push(Node::default());
            idx
        }
    }
    /// increase reference count of the node.
    pub(crate) fn inc(&mut self, node: usize) {
        self.nodes_pool[node].reference_count += 1;
    }
    /// decrease reference count of the node.
    /// If the reference count reaches zero, the node is cleared and returned to the pool.
    /// This decrease is recursive, so it will clear all parent nodes until the reference count is greater than zero.
    pub(crate) fn dec(&mut self, mut node: usize) {
        loop {
            self.nodes_pool[node].reference_count -= 1;
            if self.nodes_pool[node].reference_count == 0 {
                self.empty_node_indices.insert(node);
                let parent = self.nodes_pool[node].parent;
                self.nodes_pool[node].clear();
                if let Some(parent) = parent {
                    node = parent;
                }
            } else {
                break;
            }
        }
    }
    /// Get iterator for all nodes in the current context.
    fn node_iter(&self, node: usize) -> NodeRefIterator<Data> {
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
    fn tree_iter(
        &self,
        node: usize,
    ) -> impl Iterator<Item = &crate::tree::Tree<Data::Term, Data::NonTerm>> {
        self.node_iter(node)
            .flat_map(|node| node.tree_stack.iter().rev())
    }

    /// Get state of the node.
    fn state(&self, mut node: usize) -> usize {
        while self.nodes_pool[node].state_stack.is_empty() {
            if let Some(parent) = self.nodes_pool[node].parent {
                node = parent;
            } else {
                return 0; // root node
            }
        }
        *self.nodes_pool[node].state_stack.last().unwrap()
    }

    /// pop one stack from the node.
    fn pop(&mut self, node: usize) -> Option<usize> {
        match self.nodes_pool[node].len() {
            0 => unreachable!("cannot pop from empty node"),
            1 => {
                let parent = self.nodes_pool[node].parent;
                self.dec(node);
                if let Some(parent) = parent {
                    Some(parent)
                } else {
                    None // root node
                }
            }
            _ => {
                let node_ = &mut self.nodes_pool[node];
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

    /// give lookahead token to parser, and check if there is any reduce action.
    /// returns false if shift action is revoked
    fn reduce<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &mut self,
        parser: &P,
        reduce_rule: usize,
        precedence: Option<usize>,
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
        let mut count = rule.rule.len();
        let mut new_location = Data::Location::new(self.location_iter(node), count);

        self.reduce_args.clear();
        self.reduce_args.reserve(count);

        #[cfg(feature = "tree")]
        let mut trees = Vec::with_capacity(count);

        let mut current_node = node;
        self.reduce_args.reserve(count);
        let node_to_shift = loop {
            if count > self.nodes_pool[current_node].len() {
                count -= self.nodes_pool[current_node].len();
                if self.nodes_pool[current_node].is_unique() {
                    // move the values from current_node to reduce_args

                    let node = &mut self.nodes_pool[current_node];
                    let data_stack = std::mem::take(&mut node.data_stack);
                    let location_stack = std::mem::take(&mut node.location_stack);
                    #[cfg(feature = "tree")]
                    let tree_stack = std::mem::take(&mut node.tree_stack);

                    self.reduce_args.extend(
                        data_stack
                            .into_iter()
                            .rev()
                            .zip(location_stack.into_iter().rev()),
                    );
                    #[cfg(feature = "tree")]
                    trees.extend(tree_stack.into_iter().rev());
                } else {
                    // clone the values from current_node to reduce_args

                    let node = &self.nodes_pool[current_node];
                    let data_stack = &node.data_stack;
                    let location_stack = &node.location_stack;
                    #[cfg(feature = "tree")]
                    let tree_stack = &node.tree_stack;

                    self.reduce_args.extend(
                        data_stack
                            .iter()
                            .rev()
                            .cloned()
                            .zip(location_stack.iter().rev().cloned()),
                    );
                    #[cfg(feature = "tree")]
                    trees.extend(tree_stack.iter().rev().cloned());
                }
                let parent = self.nodes_pool[current_node].parent;
                self.dec(current_node);
                current_node = parent.unwrap(); // since count > 0, there must be parent node
            } else {
                // count <= node.len
                let i = self.nodes_pool[current_node].len() - count;

                if self.nodes_pool[current_node].is_unique() {
                    // move the values in range [i..] from current_node to reduce_args
                    // use this node as node_to_shift

                    let node = &mut self.nodes_pool[current_node];

                    self.reduce_args.extend(
                        node.data_stack
                            .drain(i..)
                            .rev()
                            .zip(node.location_stack.drain(i..).rev()),
                    );
                    #[cfg(feature = "tree")]
                    trees.extend(node.tree_stack.drain(i..).rev());

                    node.state_stack.truncate(i);
                    node.precedence_stack.truncate(i);

                    break current_node;
                } else {
                    // clone the values in range [i..] from current_node to reduce_args
                    let node = &self.nodes_pool[current_node];
                    let parent = node.parent;
                    self.reduce_args.extend(
                        node.data_stack[i..]
                            .iter()
                            .rev()
                            .cloned()
                            .zip(node.location_stack[i..].iter().rev().cloned()),
                    );
                    #[cfg(feature = "tree")]
                    trees.extend(node.tree_stack[i..].iter().rev().cloned());

                    if i == 0 {
                        // create new empty node pointing to this node's parent node, and use it as node_to_shift
                        let new_node = self.new_node();
                        self.nodes_pool[new_node].parent = parent;
                        break new_node;
                    } else {
                        // split the node into [..i] and [i..]
                        // and make new parent node with [..i]
                        // create new empty node pointing to parent node, and use it as node_to_shift
                        // new_parent[..i] <- current_node[..i]
                        //                 <- new_node (empty)

                        let new_parent = self.new_node();
                        let parent_data_stack = self.nodes_pool[current_node]
                            .data_stack
                            .drain(..i)
                            .collect();
                        let parent_state_stack = self.nodes_pool[current_node]
                            .state_stack
                            .drain(..i)
                            .collect();
                        let parent_location_stack = self.nodes_pool[current_node]
                            .location_stack
                            .drain(..i)
                            .collect();
                        let parent_precedence_stack = self.nodes_pool[current_node]
                            .precedence_stack
                            .drain(..i)
                            .collect();
                        #[cfg(feature = "tree")]
                        let parent_tree_stack = self.nodes_pool[current_node]
                            .tree_stack
                            .drain(..i)
                            .collect();
                        self.nodes_pool[new_parent].data_stack = parent_data_stack;
                        self.nodes_pool[new_parent].state_stack = parent_state_stack;
                        self.nodes_pool[new_parent].location_stack = parent_location_stack;
                        self.nodes_pool[new_parent].precedence_stack = parent_precedence_stack;
                        #[cfg(feature = "tree")]
                        {
                            self.nodes_pool[new_parent].tree_stack = parent_tree_stack;
                        }

                        self.nodes_pool[new_parent].parent = parent;
                        self.nodes_pool[current_node].parent = Some(new_parent);

                        let new_node = self.new_node();
                        self.nodes_pool[new_node].parent = Some(new_parent);
                        self.inc(new_parent);

                        break new_node;
                    }
                }
            }
        };

        #[cfg(feature = "tree")]
        trees.reverse();

        use crate::parser::State;

        let state = self.state(node_to_shift);

        match Data::reduce_action(
            reduce_rule,
            &mut self.reduce_args,
            shift,
            term,
            userdata,
            &mut new_location,
        ) {
            Ok(new_data) => {
                if let Some(nonterm_shift_state) =
                    parser.get_states()[state].shift_goto_nonterm(&rule.name)
                {
                    let node = &mut self.nodes_pool[node_to_shift];
                    node.state_stack.push(nonterm_shift_state);
                    node.data_stack.push(new_data);
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
            Err(err) => Err(err),
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
        Data: Clone + TryInto<Data::StartType>,
        P::Term: Clone,
        P::NonTerm: Hash + Eq + Clone + std::fmt::Debug + NonTerminal,
    {
        self.feed_eof(parser, userdata)?;
        // since `eof` is feeded, every node graph should be like this:
        // Root <- Start <- EOF
        //                  ^^^ here, current_node
        let nodes = self.current_nodes;
        let mut pool = self.nodes_pool;
        Ok(nodes.into_iter().map(move |eof_node| {
            debug_assert!(pool[eof_node].data_stack.len() > 0);
            let data = if pool[eof_node].data_stack.len() > 1 {
                pool[eof_node].data_stack.pop();
                pool[eof_node].data_stack.pop().unwrap()
            } else {
                let parent = pool[eof_node].parent.unwrap();
                debug_assert!(pool[parent].reference_count == 1);
                pool[parent].data_stack.pop().unwrap()
            };

            match data.try_into() {
                Ok(start) => start,
                Err(_) => unreachable!("data stack must have start symbol at this point"),
            }
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

    /// Simulate parser and get next expected (terminals, non-terminals) for current context.
    /*
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
        for node in self.nodes() {
            Node::expected_token(node, parser, &mut terms, &mut nonterms);
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

    /// Get set of `%trace` non-terminal symbols that current context is trying to parse.
    ///
    /// The order of the returned set does not mean anything.
    /// If the current context is attempting to recognize following grammar:
    ///
    /// Chunk -> Statement -> IfStatement -> ReturnStatement -> ...
    ///
    /// Then the returned set will be:
    /// [`Chunk`, `Statement`, `IfStatement`, `ReturnStatement`]
    pub fn trace<P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
    ) -> crate::hash::HashSet<Data::NonTerm>
    where
        Data::NonTerm: Copy + Eq + std::hash::Hash + crate::nonterminal::NonTerminal,
    {
        let mut ret: crate::hash::HashSet<Data::NonTerm> = Default::default();
        for node in self.nodes() {
            let set = node.trace(parser);
            ret.extend(set.into_iter());
        }
        ret
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
        self.nodes().map(|node| node.backtrace(parser))
    }

    /// move all nodes in `other` to `self`.
    pub fn append(&mut self, other: &mut Self) {
        self.current_nodes.append(&mut other.current_nodes);
    }
    */

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
        while count >= self.nodes_pool[node].len() {
            count -= self.nodes_pool[node].len();
            if let Some(parent) = self.nodes_pool[node].parent {
                node = parent;
            } else {
                return None;
            }
        }
        Some((node, self.nodes_pool[node].len() - 1 - count))
    }

    fn feed_location_impl<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &mut self,
        parser: &P,
        node: usize,
        term: TerminalSymbol<P::Term>,
        class: TerminalSymbol<usize>,
        shift_prec: Option<usize>,
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
        debug_assert!(self.nodes_pool[node].reference_count == 1);
        use crate::parser::State;
        use crate::rule::Precedence;

        let last_state = self.state(node);
        let shift_state = parser.get_states()[last_state].shift_goto_class(class);
        if let Some(reduce_rules) = parser.get_states()[last_state].reduce(class) {
            let mut shift = None;
            let mut reduces: smallvec::SmallVec<[_; 2]> = Default::default();

            for reduce_rule in reduce_rules {
                let rule = &parser.get_rules()[reduce_rule];
                let reduce_prec = match rule.precedence {
                    Some(Precedence::Fixed(level)) => Some(level),
                    Some(Precedence::Dynamic(token_index)) => {
                        // fix the value to the offset from current node
                        let ith = rule.rule.len() - token_index - 1;
                        let (node, ith) = self.skip_last_n(node, ith).unwrap();
                        self.nodes_pool[node].precedence_stack[ith]
                    }
                    None => None,
                };

                // if there is shift/reduce conflict, check for reduce rule's precedence and shift terminal's precedence
                match (shift_state.is_some(), shift_prec, reduce_prec) {
                    (true, Some(shift_prec_), Some(reduce_prec_)) => {
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

                    if idx < l - 1 || shift.is_some() {
                        self.inc(node);
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
                }
                // if every reduce action revoked shift,
                // then reset shift to None
                if !shift_ {
                    shift = None;
                }
            }
            if let Some(shift) = shift {
                let node_ = &mut self.nodes_pool[node];
                node_.state_stack.push(shift);
                node_.precedence_stack.push(shift_prec);
                if let Some(location) = &location {
                    node_.location_stack.push(location.clone());
                }
                #[cfg(feature = "tree")]
                node_
                    .tree_stack
                    .push(crate::tree::Tree::new_terminal(term.clone()));
                node_.data_stack.push(match &term {
                    TerminalSymbol::Term(term) => Data::new_terminal(term.clone()),
                    TerminalSymbol::Eof => Data::new_empty(),
                    TerminalSymbol::Error => Data::new_error(),
                });

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
            let node_ = &mut self.nodes_pool[node];
            node_.state_stack.push(shift);
            node_.precedence_stack.push(shift_prec);
            if let Some(location) = location {
                node_.location_stack.push(location);
            }
            #[cfg(feature = "tree")]
            node_
                .tree_stack
                .push(crate::tree::Tree::new_terminal(term.clone()));
            node_.data_stack.push(match term {
                TerminalSymbol::Term(term) => Data::new_terminal(term),
                TerminalSymbol::Eof => Data::new_empty(),
                TerminalSymbol::Error => Data::new_error(),
            });

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
        error_prec: Option<usize>,
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
                    if self.nodes_pool[err_node].len() == 0 {
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
                self.fallback_nodes.push(node);
            }
        }
        self.current_nodes = current_nodes;

        // next_nodes is empty; invalid terminal was given
        // check for panic mode
        // and restore nodes to original state from fallback_nodes
        if self.next_nodes.is_empty() {
            // early return if `error` token is not used in the grammar
            if !P::error_used() {
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
                        let node = &mut self.nodes_pool[error_node];
                        node.state_stack.push(next_state);
                        node.precedence_stack.push(shift_prec);
                        node.location_stack.push(location.clone());
                        #[cfg(feature = "tree")]
                        node.tree_stack.push(crate::tree::Tree::new_terminal(
                            TerminalSymbol::Term(term.clone()),
                        ));
                        node.data_stack.push(Data::new_terminal(term.clone()));

                        self.current_nodes.push(error_node);
                    } else {
                        // here, fed token is in `error` non-terminal
                        // so merge location with previous

                        let new_location = Data::Location::new(
                            std::iter::once(&location).chain(self.location_iter(error_node)),
                            2, // error node + fed token
                        );
                        let node = &mut self.nodes_pool[error_node];
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
        shift_prec: Option<usize>,
    ) -> bool
    where
        P::NonTerm: std::hash::Hash + Eq + NonTerminal,
    {
        use crate::parser::State;
        use crate::rule::Precedence;

        let last_state = self.state(node);
        let shift_state = parser.get_states()[last_state].shift_goto_class(class);
        if let Some(reduce_rules) = parser.get_states()[last_state].reduce(class) {
            let mut shift = None;
            let mut reduces: smallvec::SmallVec<[_; 2]> = Default::default();

            for reduce_rule in reduce_rules {
                let rule = &parser.get_rules()[reduce_rule];
                let reduce_prec = match rule.precedence {
                    Some(Precedence::Fixed(level)) => Some(level),
                    Some(Precedence::Dynamic(token_index)) => {
                        // fix the value to the offset from current node
                        let ith = rule.rule.len() - token_index - 1;
                        let (node, ith) = self.skip_last_n(node, ith).unwrap();
                        self.nodes_pool[node].precedence_stack[ith]
                    }
                    None => None,
                };

                // if there is shift/reduce conflict, check for reduce rule's precedence and shift terminal's precedence
                match (shift_state.is_some(), shift_prec, reduce_prec) {
                    (true, Some(shift_prec_), Some(reduce_prec_)) => {
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
    /*
    pub fn can_panic<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
    ) -> bool
    where
        Data::NonTerm: Hash + Eq + NonTerminal,
    {
        // if `error` token was not used in the grammar, early return here
        if !P::error_used() {
            return false;
        }
        let error_prec = parser.class_precedence(TerminalSymbol::Error);

        self.current_nodes
            .iter()
            .any(|node| Node::can_panic(node, parser, error_prec))
    }
    */

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
                None,
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
            .any(|node| self.can_feed_impl(parser, *node, TerminalSymbol::Eof, None))
    }
}

impl<Data: TokenData> Default for Context<Data> {
    fn default() -> Self {
        let mut context = Context {
            nodes_pool: Default::default(),
            empty_node_indices: Default::default(),
            current_nodes: Default::default(),
            next_nodes: Default::default(),
            reduce_errors: Default::default(),
            reduce_args: Default::default(),
            fallback_nodes: Default::default(),
            no_precedences: Default::default(),
        };
        let root_node = context.new_node();
        context.current_nodes.push(root_node);
        context
    }
}

impl<Data: TokenData> Clone for Context<Data> {
    fn clone(&self) -> Self {
        Context {
            current_nodes: self.current_nodes.clone(),
            ..Default::default()
        }
    }
}

#[cfg(feature = "tree")]
impl<Data: TokenData> std::fmt::Display for Context<Data>
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
impl<Data: TokenData> std::fmt::Debug for Context<Data>
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
