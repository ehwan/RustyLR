use std::hash::Hash;
use std::rc::Rc;

use super::Node;
use super::ParseError;
use super::Parser;
use super::State;

use crate::HashMap;
use crate::TokenData;

#[cfg(feature = "tree")]
use crate::TreeList;

/// A struct that maintains the current state and the values associated with each symbol.
/// This handles the divergence and merging of the parser.
pub struct Context<Data: TokenData> {
    /// each element represents an end-point of diverged paths.
    pub(crate) current_nodes: HashMap<usize, Vec<Rc<Node<Data>>>>,

    /// For temporary use. store reduce errors returned from `reduce_action`.
    /// But we don't want to reallocate every `feed` call
    pub(crate) reduce_errors: Vec<Data::ReduceActionError>,

    /// For temporary use. store arguments for calling `reduce_action`.
    /// But we don't want to reallocate every `feed` call
    pub(crate) reduce_args: Vec<Data>,

    /// For temporary use. store nodes for next reduce.
    pub(crate) nodes_pong: HashMap<usize, Vec<Rc<Node<Data>>>>,
    pub(crate) nodes_pong2: HashMap<usize, Vec<Rc<Node<Data>>>>,

    /// For recovery from error
    pub(crate) fallback_nodes: HashMap<usize, Vec<Rc<Node<Data>>>>,
}

impl<Data: TokenData> Context<Data> {
    /// Create a new context.
    /// `current_nodes` is initialized with a root node.
    pub fn new() -> Self {
        Default::default()
    }

    /// Get number of diverged paths
    pub fn len_paths(&self) -> usize {
        self.current_nodes.values().map(|nodes| nodes.len()).sum()
    }

    /// Is there any path alive?
    pub fn is_empty(&self) -> bool {
        self.current_nodes.is_empty()
    }

    /// Get index of states in parser for every diverged paths.
    pub fn states(&self) -> impl Iterator<Item = usize> + '_ {
        self.current_nodes.keys().copied()
    }

    /// Get every nodes in current diverged paths.
    /// Note that node is tail of the path.
    pub fn nodes(&self) -> impl Iterator<Item = &Rc<Node<Data>>> {
        self.current_nodes.values().flat_map(|nodes| nodes.iter())
    }

    /// Get every nodes in current diverged paths.
    /// Note that node is tail of the path.
    pub fn into_nodes(self) -> impl Iterator<Item = Rc<Node<Data>>> {
        self.current_nodes
            .into_values()
            .flat_map(|nodes| nodes.into_iter())
    }

    /// Returns an iterator of `%start` symbols from all diverged paths.
    /// This function should be called after feeding all tokens (including EOF).
    pub fn accept(self) -> impl Iterator<Item = Data::StartType>
    where
        Data: Clone + TryInto<Data::StartType>,
    {
        // since `eof` is feeded, the node graph should be like this:
        // Root <- Start <- EOF
        //                  ^^^ here, current_node
        self.into_nodes().filter_map(|rc_eof_node| {
            let rc_data_node = Rc::clone(rc_eof_node.parent.as_ref()?);
            drop(rc_eof_node);

            let data_node = match Rc::try_unwrap(rc_data_node) {
                Ok(data_node) => data_node.data?,
                Err(rc_data_node) => rc_data_node.data.as_ref()?.clone(),
            };
            data_node.try_into().ok()
        })
    }

    /// For debugging.
    /// Get all sequence of token trees (from root to current node) for every diverged path.
    #[cfg(feature = "tree")]
    pub fn to_tree_lists(&self) -> impl Iterator<Item = TreeList<Data::Term, Data::NonTerm>> + '_
    where
        Data::Term: Clone,
        Data::NonTerm: Clone,
    {
        self.nodes().map(|node| node.to_tree_list())
    }
    /// For debugging.
    /// Get all sequence of token trees (from root to current node) for every diverged path.
    #[cfg(feature = "tree")]
    pub fn into_tree_lists(self) -> impl Iterator<Item = TreeList<Data::Term, Data::NonTerm>>
    where
        Data::Term: Clone,
        Data::NonTerm: Clone,
    {
        self.into_nodes().map(|node| node.to_tree_list())
    }

    /// Get all sequence of data (from root to current node) for every diverged path.
    pub fn to_data_lists(&self) -> impl Iterator<Item = Vec<Data>> + '_
    where
        Data: Clone,
    {
        self.nodes().map(|node| {
            node.iter()
                .map(|node| node.data.as_ref().unwrap().clone())
                .collect()
        })
    }
    /// Get all sequence of data (from root to current node) for every diverged path.
    pub fn into_data_lists(self) -> impl Iterator<Item = Vec<Data>>
    where
        Data: Clone,
    {
        self.into_nodes().map(|node| {
            node.iter()
                .map(|node| node.data.as_ref().unwrap().clone())
                .collect()
        })
    }

    /// Get expected terminal classes for next `feed()` call.
    /// This could contain duplicate tokens.
    pub fn expected_class<'a, P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &'a self,
        parser: &'a P,
    ) -> impl Iterator<Item = usize> + 'a {
        self.nodes().flat_map(|node| node.expected_class(parser))
    }
    /// Get expected tokens for next `feed()` call.
    /// This could contain duplicate tokens.
    pub fn expected<'a, P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &'a self,
        parser: &'a P,
    ) -> impl Iterator<Item = P::TerminalClassElement> + 'a {
        self.nodes().flat_map(|node| node.expected(parser))
    }
    /// Get expected non-terminal tokens for next `feed()` call.
    /// This could contain duplicate tokens.
    pub fn expected_nonterm<'a, P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &'a self,
        parser: &'a P,
    ) -> impl Iterator<Item = &'a Data::NonTerm> {
        self.nodes().flat_map(|node| node.expected_nonterm(parser))
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
    ) -> crate::HashSet<Data::NonTerm>
    where
        Data::NonTerm: Copy + Eq + std::hash::Hash + crate::NonTerminal,
    {
        let mut ret: crate::HashSet<Data::NonTerm> = Default::default();
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
        for (state, mut nodes) in other.current_nodes.drain() {
            self.current_nodes
                .entry(state)
                .or_default()
                .append(&mut nodes);
        }
    }

    /// Feed one terminal to parser, and update state stack.
    /// For GLR parsing, this function will create multiple path if needed.
    pub fn feed<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &mut self,
        parser: &P,
        term: P::Term,
        userdata: &mut Data::UserData,
    ) -> Result<(), ParseError<P::Term, Data::ReduceActionError>>
    where
        P::Term: Hash + Eq + Clone,
        P::NonTerm: Hash + Eq + Clone,
        Data: Clone + From<P::Term>,
    {
        #[cfg(feature = "tree")]
        use crate::Tree;

        // current_nodes <-> nodes_pong <-> nodes_pong2
        // cycle for no unnecessary heap allocation
        let mut reduce_nodes = std::mem::take(&mut self.current_nodes);
        std::mem::swap(&mut self.current_nodes, &mut self.nodes_pong2);
        self.current_nodes.clear();
        // here, nodes_pong2 is newlly created by `Default`, and we will assign it from `reduce_nodes` later
        self.nodes_pong.clear();
        self.reduce_errors.clear();
        self.fallback_nodes.clear();

        let class = parser.to_terminal_class(&term);

        // BFS reduce
        while !reduce_nodes.is_empty() {
            for (state, nodes) in reduce_nodes.drain() {
                let next_term_shift_state = parser.get_states()[state].shift_goto_class(class);
                if let Some(reduce_rules) = parser.get_states()[state].reduce(class) {
                    for node in nodes.into_iter() {
                        let mut shift_for_this_node = false;

                        let mut reduce_rules = reduce_rules.clone();
                        let reduce0 = reduce_rules.next().unwrap();

                        // In reduce action, we call `Rc::try_unwrap` to avoid `clone()` data if possible.
                        // So we need to avoid `Rc::clone()` if possible.
                        for reduce_rule in reduce_rules {
                            shift_for_this_node |= super::reduce(
                                parser,
                                reduce_rule,
                                Rc::clone(&node),
                                self,
                                &term,
                                next_term_shift_state.is_some(),
                                userdata,
                            );
                        }
                        if let Some(next_term_shift_state) = next_term_shift_state {
                            shift_for_this_node |= super::reduce(
                                parser,
                                reduce0,
                                Rc::clone(&node),
                                self,
                                &term,
                                true,
                                userdata,
                            );
                            if shift_for_this_node {
                                // some shift action was performed; remove fallback_nodes immediately
                                // to avoid cloned Rc nodes
                                self.fallback_nodes.clear();

                                let next_node = Node {
                                    parent: Some(node),
                                    state: next_term_shift_state,
                                    data: Some(term.clone().into()),
                                    #[cfg(feature = "tree")]
                                    tree: Some(Tree::new_terminal(term.clone())),
                                };

                                self.current_nodes
                                    .entry(next_term_shift_state)
                                    .or_default()
                                    .push(Rc::new(next_node));
                            }
                        } else {
                            super::reduce(parser, reduce0, node, self, &term, false, userdata);
                        }
                    }
                } else if let Some(next_term_shift_state) = next_term_shift_state {
                    for node in nodes.into_iter() {
                        // some shift action was performed; remove fallback_nodes immediately
                        // to avoid cloned Rc nodes
                        self.fallback_nodes.clear();

                        let next_node = Node {
                            parent: Some(node),
                            state: next_term_shift_state,
                            data: Some(term.clone().into()),
                            #[cfg(feature = "tree")]
                            tree: Some(Tree::new_terminal(term.clone())),
                        };

                        self.current_nodes
                            .entry(next_term_shift_state)
                            .or_default()
                            .push(Rc::new(next_node));
                    }
                } else {
                    // no reduce, no shift
                    // add to fallback_nodes to restore if any shift action was performed
                    if self.current_nodes.is_empty() {
                        self.fallback_nodes.insert(state, nodes);
                    }
                }
            }
            std::mem::swap(&mut reduce_nodes, &mut self.nodes_pong);
        }
        self.nodes_pong2 = reduce_nodes;

        // no shift possible; invalid terminal was given
        // restore nodes to original state from fallback_nodes
        if self.current_nodes.is_empty() {
            let mut error_nodes = Vec::new();
            for (_, nodes) in self.fallback_nodes.iter() {
                for node in nodes.iter() {
                    if let Some(error_node) = Node::panic_mode(node, parser) {
                        error_nodes.push(error_node);
                    }
                }
            }
            if error_nodes.is_empty() {
                std::mem::swap(&mut self.current_nodes, &mut self.fallback_nodes);
                self.fallback_nodes.clear();

                if self.reduce_errors.is_empty() {
                    Err(ParseError::NoAction(term))
                } else {
                    Err(ParseError::ReduceAction(std::mem::take(
                        &mut self.reduce_errors,
                    )))
                }
            } else {
                // try shift term to error state
                for error_node in error_nodes {
                    let next_state = parser.get_states()[error_node.state].shift_goto_class(class);
                    if let Some(next_state) = next_state {
                        let next_node = Node {
                            parent: Some(Rc::new(error_node)),
                            state: next_state,
                            data: Some(term.clone().into()),
                            #[cfg(feature = "tree")]
                            tree: Some(Tree::new_terminal(term.clone())),
                        };
                        self.current_nodes
                            .entry(next_state)
                            .or_default()
                            .push(Rc::new(next_node));
                    } else {
                        self.current_nodes
                            .entry(error_node.state)
                            .or_default()
                            .push(Rc::new(error_node));
                    }
                }
                Ok(())
            }
        } else {
            self.fallback_nodes.clear();
            Ok(())
        }
    }

    /*
    /// feed multiple terminal symbols to the context.
    /// This tries to feed all symbols at the same time, to the same state.
    pub fn feed_multiple<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &mut self,
        parser: &P,
        terms: impl Iterator<Item = P::Term>,
        userdata: &mut Data::UserData,
    ) -> Result<(), Vec<InvalidTerminalError<Data::Term, Data::NonTerm, Data::ReduceActionError>>>
    where
        Data: Clone,
        P::Term: Hash + Eq + Clone,
        P::NonTerm: Hash + Eq + Clone,
    {
        super::feed_multiple(parser, self, terms, userdata)
    }
    */

    /// Check if `term` can be feeded to current state.
    /// This does not check for reduce action error, nor the panic mode.
    /// You should call `can_panic_mode()` after this fails to check if panic mode can accept this term.
    ///
    /// This does not change the state of the context.
    pub fn can_feed<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
        term: &P::Term,
    ) -> bool
    where
        P::Term: Hash + Eq,
        P::NonTerm: Hash + Eq,
    {
        let mut nodes = self.current_nodes.clone();
        let mut nodes_pong: HashMap<usize, Vec<Rc<Node<Data>>>> = HashMap::default();
        let class = parser.to_terminal_class(term);

        loop {
            if nodes.is_empty() {
                break;
            }

            nodes_pong.clear();
            for (state, nodes) in nodes.drain() {
                let state = &parser.get_states()[state];
                if state.shift_goto_class(class).is_some() {
                    return true;
                }

                if let Some(reduce_rules) = state.reduce(class) {
                    for reduce_rule in reduce_rules {
                        let reduce_rule = &parser.get_rules()[reduce_rule];
                        let reduce_len = reduce_rule.rule.len();

                        for p in nodes.iter() {
                            let mut parent = Rc::clone(p);
                            for _ in 0..reduce_len {
                                parent = Rc::clone(parent.parent.as_ref().unwrap());
                            }
                            if let Some(nonterm_shift_state) = parser.get_states()[parent.state]
                                .shift_goto_nonterm(&reduce_rule.name)
                            {
                                if parser.get_states()[nonterm_shift_state]
                                    .shift_goto_class(class)
                                    .is_some()
                                {
                                    return true;
                                }

                                let nonterm_node = Rc::new(Node {
                                    parent: Some(parent),
                                    state: nonterm_shift_state,
                                    data: None,
                                    #[cfg(feature = "tree")]
                                    tree: None,
                                });
                                nodes_pong
                                    .entry(nonterm_shift_state)
                                    .or_default()
                                    .push(nonterm_node);
                            }
                        }
                    }
                }
            }
            std::mem::swap(&mut nodes, &mut nodes_pong);
        }

        false
    }

    /// Check if current context can enter panic mode.
    pub fn can_panic_mode<P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &mut self,
        parser: &P,
    ) -> bool
    where
        Data::NonTerm: std::hash::Hash + Eq,
    {
        let Some(error_nonterm) = parser.get_error_nonterm() else {
            return false;
        };
        for (_, nodes) in self.current_nodes.iter() {
            for mut node in nodes.iter() {
                loop {
                    let last_state = &parser.get_states()[node.state];
                    if last_state.shift_goto_nonterm(&error_nonterm).is_some() {
                        return true;
                    }

                    if let Some(parent) = node.parent.as_ref() {
                        node = parent;
                    } else {
                        break;
                    }
                }
            }
        }
        false
    }

    /*
    /// Search for the shortest path that can be accepted and represented as CurrentState -> Terms^N -> Tails.
    /// Where Terms is set of terminals `terms`, and Tails is a sequence of terminals `tails`.
    /// Returns true if there is a alive path.
    pub fn complete<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &mut self,
        parser: &P,
        terms: impl Iterator<Item = P::Term> + Clone,
        userdata: &mut Data::UserData,
        tails: impl Iterator<Item = P::Term> + Clone,
        max_depth: usize,
    ) -> bool
    where
        Data: Clone,
        P::Term: Clone + Hash + Eq,
        P::NonTerm: Clone + Hash + Eq,
    {
        super::completion::complete(parser, self, terms, userdata, tails, max_depth)
    }
    */
}

impl<Data: TokenData> Default for Context<Data> {
    fn default() -> Self {
        Context {
            current_nodes: HashMap::from_iter([(0, vec![Rc::new(Node::new_root())])]),
            reduce_errors: Default::default(),
            reduce_args: Default::default(),
            nodes_pong: Default::default(),
            nodes_pong2: Default::default(),
            fallback_nodes: Default::default(),
        }
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
    Data::NonTerm: std::fmt::Display + Clone + crate::NonTerminal,
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
    Data::NonTerm: std::fmt::Debug + Clone + crate::NonTerminal,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, path) in self.to_tree_lists().enumerate() {
            writeln!(f, "Path {}:", i)?;
            writeln!(f, "{:?}", path)?;
        }
        Ok(())
    }
}
