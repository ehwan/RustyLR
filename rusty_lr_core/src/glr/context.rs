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

    /// For temporary use. store state of each node in `current_nodes`.
    /// But we don't want to reallocate every `feed` call
    pub(crate) state_list: Vec<usize>,

    /// For temporary use. store reduce errors returned from `reduce_action`.
    /// But we don't want to reallocate every `feed` call
    pub(crate) reduce_errors: Vec<Data::ReduceActionError>,

    /// For temporary use. store arguments for calling `reduce_action`.
    /// But we don't want to reallocate every `feed` call
    pub(crate) reduce_args: Vec<Data>,

    /// For temporary use. store nodes for next reduce.
    pub(crate) nodes_pong: HashMap<usize, Vec<Rc<Node<Data>>>>,
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
    pub fn accept(self) -> Result<Data::StartType, MultiplePathError>
    where
        Data: Clone,
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
            Err(MultiplePathError)
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

    /// This function should be called after `feed()` returns `Error`.
    /// Get expected tokens for last `feed()` call.
    /// The iterator can contain duplicate tokens.
    pub fn expected<'a, P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &'a self,
        p: &'a P,
    ) -> impl Iterator<Item = &'a Data::Term>
    where
        Data::Term: 'a,
        Data::NonTerm: 'a,
    {
        self.state_list
            .iter()
            .flat_map(|state| p.get_states()[*state].expected())
    }

    /// This function should be called after `feed()` returns `Error`.
    /// Get expected tokens for last `feed()` call.
    /// The iterator does not contain duplicate tokens.
    pub fn expected_dedup<'a, P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &'a self,
        p: &'a P,
    ) -> impl Iterator<Item = &'a Data::Term>
    where
        Data::Term: 'a + std::hash::Hash + Eq,
        Data::NonTerm: 'a,
    {
        let dedupped: HashSet<&'a Data::Term> = self.expected(p).collect();
        dedupped.into_iter()
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
    ) -> Result<(), InvalidTerminalError<Data::Term, Data::ReduceActionError>>
    where
        Data: Clone,
        P::Term: Hash + Eq + Clone,
        P::NonTerm: Hash + Eq + Clone,
    {
        super::feed(parser, self, term, userdata)
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
            state_list: Default::default(),
            reduce_errors: Default::default(),
            reduce_args: Default::default(),
            nodes_pong: Default::default(),
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
