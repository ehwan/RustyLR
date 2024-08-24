use super::node::NodeSet;

/// Context trait for GLR parser.
/// This handles the divergence and merging of the parser.
pub trait Context {
    type Node;

    /// take list of current nodes
    fn take_current_nodes(&mut self) -> NodeSet<Self::Node>;
    /// check if current nodes is empty
    fn is_empty(&self) -> bool;

    /// get mutable reference of current nodes
    fn get_current_nodes_mut(&mut self) -> &mut NodeSet<Self::Node>;
}
