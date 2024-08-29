use std::rc::Rc;

use super::Node;
use super::{MultiplePathError, NodeData};

#[cfg(feature = "tree")]
use crate::TreeList;

/// Context trait for GLR parser.
/// This handles the divergence and merging of the parser.
pub struct Context<Data: NodeData> {
    /// each element represents an end-point of diverged paths.
    pub current_nodes: Vec<Rc<Node<Data>>>,

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
    pub fn new() -> Self {
        Context {
            current_nodes: vec![Rc::new(Node::new_root())],
            state_list: Vec::new(),
            reduce_errors: Vec::new(),
            reduce_args: Vec::new(),
        }
    }

    /// after feeding all tokens (include EOF), call this function to get result.
    pub fn accept(self) -> Result<Data::StartType, MultiplePathError>
    where
        Data: NodeData,
        Data::Term: Clone,
        Data::NonTerm: Clone,
    {
        if self.current_nodes.len() == 1 {
            // since `eof` is feeded, there must be only one node in the set.
            // and it must be
            // Augmented -> Start EOF
            //                    ^^^ here, current_node

            // we have to extract data from `Start` node
            let mut it = self.current_nodes.into_iter();
            let eof_node = Rc::into_inner(it.next().unwrap()).unwrap();
            let data = Rc::into_inner(eof_node.parent.unwrap())
                .unwrap()
                .data
                .unwrap();
            Ok(data.into_start())
        } else {
            Err(MultiplePathError)
        }
    }

    /// For debugging.
    /// Get all token trees (from the root) for every diverged path.
    #[cfg(feature = "tree")]
    pub fn to_tree_lists(&self) -> Vec<TreeList<Data::Term, Data::NonTerm>>
    where
        Data: NodeData,
        Data::Term: Clone,
        Data::NonTerm: Clone,
    {
        self.current_nodes
            .iter()
            .map(|node| {
                let mut trees = Vec::new();
                let mut current_node = Rc::clone(node);
                while let Some(parent) = &current_node.parent {
                    trees.push(current_node.to_tree().clone());
                    current_node = Rc::clone(parent);
                }
                trees.reverse();
                TreeList { trees }
            })
            .collect()
    }
}
