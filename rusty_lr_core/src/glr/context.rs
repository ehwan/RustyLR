use std::hash::Hash;
use std::rc::Rc;

use super::InvalidTerminalError;
use super::Node;
use super::Parser;
use super::{MultiplePathError, NodeData};

use crate::HashMap;
use crate::HashSet;

#[cfg(feature = "tree")]
use crate::TreeList;

/// Context trait for GLR parser.
/// This handles the divergence and merging of the parser.
pub struct Context<Data: NodeData> {
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

impl<Data: NodeData> Context<Data> {
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
    pub fn states(&self) -> impl Iterator<Item = &usize> {
        self.current_nodes.keys()
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

    /// After feeding all tokens (include EOF), call this function to get result.
    /// Get value of start symbol, if there is only one path.
    pub fn accept(self) -> Result<Data::StartType, MultiplePathError<Data::Term, Data::NonTerm>>
    where
        Data: Clone,
        Data::Term: Clone,
        Data::NonTerm: Clone,
    {
        if self.len_paths() == 1 {
            // if there is only one path, we can get the result

            // since `eof` is feeded, the node graph should be like this:
            // Root <- Start <- EOF
            //                  ^^^ here, current_node

            let rc_eof_node = self
                .current_nodes
                .into_iter()
                .next()
                .unwrap()
                .1
                .into_iter()
                .next()
                .unwrap();
            let rc_data_node = Rc::clone(rc_eof_node.parent.as_ref().unwrap());
            let data_node = match Rc::try_unwrap(rc_data_node) {
                Ok(data_node) => data_node.data.unwrap(),
                Err(rc_data_node) => rc_data_node.data.as_ref().unwrap().clone(),
            };
            Ok(data_node.into_start())
        } else {
            Err(MultiplePathError {
                #[cfg(feature = "tree")]
                tree_lists: self.to_tree_lists().collect(),

                #[cfg(not(feature = "tree"))]
                _phantom: std::marker::PhantomData,
            })
        }
    }

    /// After feeding all tokens (include EOF), call this function to get result.
    /// Unlike `accept`, this function will return all possible results if there are multiple paths.
    pub fn accept_all(self) -> impl Iterator<Item = Data::StartType>
    where
        Data: Clone,
    {
        // since `eof` is feeded, the node graph should be like this:
        // Root <- Start <- EOF
        //                  ^^^ here, current_node
        self.current_nodes.into_iter().flat_map(|(_, nodes)| {
            nodes.into_iter().map(|rc_eof_node| {
                let rc_data_node = Rc::clone(rc_eof_node.parent.as_ref().unwrap());
                drop(rc_eof_node);

                let data_node = match Rc::try_unwrap(rc_data_node) {
                    Ok(data_node) => data_node.data.unwrap(),
                    Err(rc_data_node) => rc_data_node.data.as_ref().unwrap().clone(),
                };
                data_node.into_start()
            })
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

    /// Get expected tokens for next `feed()` call.
    pub fn expected<'a, P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &'a self,
        parser: &'a P,
    ) -> impl Iterator<Item = &'a Data::Term>
    where
        Data::Term: 'a + std::hash::Hash + Eq,
        Data::NonTerm: 'a,
    {
        let dedupped: HashSet<&'a Data::Term> = self
            .nodes()
            .flat_map(|node| node.expected(parser))
            .collect();

        dedupped.into_iter()
    }
    /// Get expected non-terminal tokens for next `feed()` call.
    pub fn expected_nonterm<'a, P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &'a self,
        parser: &'a P,
    ) -> impl Iterator<Item = &'a Data::NonTerm>
    where
        Data::Term: 'a,
        Data::NonTerm: 'a + std::hash::Hash + Eq,
    {
        let dedupped: HashSet<&'a Data::NonTerm> = self
            .nodes()
            .flat_map(|node| node.expected_nonterm(parser))
            .collect();

        dedupped.into_iter()
    }

    /// This does the same thing as `expected`, for backward compatibility.
    pub fn expected_on_error<'a, P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &'a self,
        parser: &'a P,
    ) -> impl Iterator<Item = &'a Data::Term>
    where
        Data::Term: 'a + std::hash::Hash + Eq,
        Data::NonTerm: 'a,
    {
        self.expected(parser)
    }

    /// Get set of non-terminal symbols that current context is trying to parse.
    ///
    /// The order of the returned set does not mean anything.
    /// If the current context is attempting to recognize following grammar:
    ///
    /// Chunk -> Statement -> IfStatement -> ReturnStatement -> ...
    ///
    /// Then the returned set will be:
    /// [`Chunk`, `Statement`, `IfStatement`, `ReturnStatement`]
    pub fn on_parsing<P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
    ) -> crate::HashSet<Data::NonTerm>
    where
        Data::NonTerm: Copy + Eq + std::hash::Hash + crate::NonTerminal<Data::Term>,
    {
        let mut ret: crate::HashSet<Data::NonTerm> = Default::default();
        for node in self.nodes() {
            let set = node.on_parsing(parser);
            ret.extend(set.into_iter());
        }
        ret
    }

    /// Get backtrace infos for all paths.
    pub fn backtraces<'a, P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &'a self,
        parser: &'a P,
    ) -> impl Iterator<Item = crate::Backtrace<P::Term, P::NonTerm>> + 'a
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

    /// feed a terminal symbol to the context.
    pub fn feed<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &mut self,
        parser: &P,
        term: P::Term,
        userdata: &mut Data::UserData,
    ) -> Result<(), InvalidTerminalError<Data::Term, Data::NonTerm, Data::ReduceActionError>>
    where
        Data: Clone,
        P::Term: Hash + Eq + Clone,
        P::NonTerm: Hash + Eq + Clone,
    {
        super::feed(parser, self, term, userdata)
    }

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

    /// Check if `term` can be feeded to current state.
    /// This does not check for reduce action error.
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

        loop {
            if nodes.is_empty() {
                break;
            }

            nodes_pong.clear();
            for (state, nodes) in nodes.drain() {
                let state = &parser.get_states()[state];
                if state.shift_goto_term(term).is_some() {
                    return true;
                }

                if let Some(reduce_rules) = state.reduce(term) {
                    for reduce_rule in reduce_rules {
                        let reduce_rule = &parser.get_rules()[*reduce_rule];
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
                                    .shift_goto_term(term)
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
}

impl<Data: NodeData> Default for Context<Data> {
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

impl<Data: NodeData> Clone for Context<Data> {
    fn clone(&self) -> Self {
        Context {
            current_nodes: self.current_nodes.clone(),
            ..Default::default()
        }
    }
}

#[cfg(feature = "tree")]
impl<Data: NodeData> std::fmt::Display for Context<Data>
where
    Data::Term: std::fmt::Display + Clone,
    Data::NonTerm: std::fmt::Display + Clone,
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
impl<Data: NodeData> std::fmt::Debug for Context<Data>
where
    Data::Term: std::fmt::Debug + Clone,
    Data::NonTerm: std::fmt::Debug + Clone,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, path) in self.to_tree_lists().enumerate() {
            writeln!(f, "Path {}:", i)?;
            writeln!(f, "{:?}", path)?;
        }
        Ok(())
    }
}
