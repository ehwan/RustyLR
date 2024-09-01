use std::rc::Rc;

use super::Node;
use super::{MultiplePathError, NodeData};

use crate::HashMap;

#[cfg(feature = "tree")]
use super::node::to_tree_list;
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
    pub reduce_args: Vec<Data>,
}

impl<Data: NodeData> Context<Data> {
    /// Create a new context.
    /// `current_nodes` is initialized with a root node.
    pub fn new() -> Self {
        Context {
            current_nodes: HashMap::from_iter([(0, vec![Rc::new(Node::new_root())])]),
            state_list: Vec::new(),
            reduce_errors: Vec::new(),
            reduce_args: Vec::new(),
        }
    }

    /// get number of diverged paths
    pub fn len_paths(&self) -> usize {
        self.current_nodes
            .iter()
            .map(|(_, nodes)| nodes.len())
            .sum()
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
            return Err(MultiplePathError);
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
    pub fn to_tree_lists(&self) -> impl Iterator<Item = TreeList<Data::Term, Data::NonTerm>>
    where
        Data: NodeData,
        Data::Term: Clone,
        Data::NonTerm: Clone,
    {
        self.current_nodes
            .clone()
            .into_iter()
            .flat_map(|(_, nodes)| nodes.into_iter().map(|node| to_tree_list(node)))
    }
}

impl<Data: NodeData> Default for Context<Data> {
    fn default() -> Self {
        Self::new()
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
