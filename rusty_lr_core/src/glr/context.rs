use std::fmt::Display;
use std::rc::Rc;

use super::Node;
use super::{MultiplePathError, NodeData, Parser};

/// Context trait for GLR parser.
/// This handles the divergence and merging of the parser.
pub struct Context<Data> {
    pub current_nodes: Vec<Rc<Node<Data>>>,
}

impl<Data> Context<Data> {
    pub fn new() -> Self {
        Context {
            current_nodes: vec![Rc::new(Node::new_root())],
        }
    }

    /// after feeding all tokens (include EOF), call this function to get result.
    pub fn accept<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        self,
        parser: &P,
    ) -> Result<Data::StartType, MultiplePathError<Data::Term, Data::NonTerm>>
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
            Err(MultiplePathError::from_tree1(
                self.current_nodes
                    .iter()
                    .map(|node| node.parent.as_ref().unwrap().tree.as_ref().unwrap()),
                parser,
            ))
        }
    }

    /// For debugging.
    /// Print last n tokens for every node in this set.
    pub fn backtrace<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        token_count: usize,
        parser: &P,
    ) where
        Data: NodeData,
        P::Term: Clone + Display,
        P::NonTerm: Clone + Display,
    {
        for node in self.current_nodes.iter() {
            super::backtrace(token_count, Rc::clone(node), parser);
            println!();
        }
    }
}
