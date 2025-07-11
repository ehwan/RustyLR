use std::hash::Hash;
use std::rc::Rc;

use super::Node;
use super::ParseError;

use crate::nonterminal::TokenData;
use crate::parser::Parser;

type SmallVecNode<Data> = smallvec::SmallVec<[Rc<Node<Data>>; 3]>;

/// A struct that maintains the current state and the values associated with each symbol.
/// This handles the divergence and merging of the parser.
pub struct Context<Data: TokenData> {
    /// each element represents an end-point of diverged paths.
    pub(crate) current_nodes: SmallVecNode<Data>,

    /// temporary storage
    pub(crate) next_nodes: SmallVecNode<Data>,

    /// For recovery from error
    pub(crate) fallback_nodes: SmallVecNode<Data>,

    /// For temporary use. store arguments for calling `reduce_action`.
    /// But we don't want to reallocate every `feed` call
    pub(crate) reduce_args: Vec<(Data, Data::Location)>,

    /// For temporary use. store reduce errors returned from `reduce_action`.
    /// But we don't want to reallocate every `feed` call
    pub(crate) reduce_errors: Vec<Data::ReduceActionError>,

    /// For temporary use.
    /// store rule indices where shift/reduce conflicts occured with no precedence defined.
    pub(crate) no_precedences: Vec<usize>,
}

impl<Data: TokenData> Context<Data> {
    /// Create a new context.
    /// `current_nodes` is initialized with a root node.
    pub fn new() -> Self {
        Default::default()
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
        self.current_nodes.iter().map(|node| node.state)
    }

    /// Get every nodes in current diverged paths.
    /// Note that node is tail of the path.
    pub fn nodes(&self) -> impl Iterator<Item = &Rc<Node<Data>>> {
        self.current_nodes.iter()
    }

    /// Get every nodes in current diverged paths.
    /// Note that node is tail of the path.
    pub fn into_nodes(self) -> impl Iterator<Item = Rc<Node<Data>>> {
        self.current_nodes.into_iter()
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
            data_node.0.try_into().ok()
        })
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
        self.nodes().map(|node| node.to_tree_list())
    }
    /// For debugging.
    /// Get all sequence of token trees (from root to current node) for every diverged path.
    #[cfg(feature = "tree")]
    pub fn into_tree_lists(
        self,
    ) -> impl Iterator<Item = crate::tree::TreeList<Data::Term, Data::NonTerm>>
    where
        Data::Term: Clone,
        Data::NonTerm: Clone,
    {
        self.into_nodes().map(|node| node.to_tree_list())
    }

    /// Simulate parser and get next expected (terminals, non-terminals) for current context.
    pub fn expected_token<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
    ) -> (
        std::collections::BTreeSet<usize>,
        std::collections::BTreeSet<Data::NonTerm>,
    )
    where
        Data::NonTerm: Ord + Copy + Hash,
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
        P::NonTerm: Hash + Eq + Clone + std::fmt::Debug,
        Data: Clone,
        Data::Location: Default,
    {
        self.feed_location(parser, term, userdata, Default::default())
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
        P::NonTerm: Hash + Eq + Clone + std::fmt::Debug,
        Data: Clone,
    {
        use crate::parser::State;
        use crate::Location;

        let class = parser.to_terminal_class(&term);
        let shift_prec = parser.class_precedence(class);

        let mut current_nodes = std::mem::take(&mut self.current_nodes);
        for node in current_nodes.drain(..) {
            if let Err(node) = Node::feed_location(
                node,
                self,
                parser,
                term.clone(),
                class,
                shift_prec,
                userdata,
                location.clone(),
            ) {
                if self.next_nodes.is_empty() {
                    self.fallback_nodes.push(node);
                }
            }
        }
        self.current_nodes = current_nodes;

        // next_nodes is empty; invalid terminal was given
        // check for panic mode
        // and restore nodes to original state from fallback_nodes
        if self.next_nodes.is_empty() {
            let mut error_nodes: smallvec::SmallVec<[Node<Data>; 3]> = smallvec::SmallVec::new();
            // try enter panic mode and store error nodes to next_nodes
            for node in self.fallback_nodes.iter() {
                if let Some(error_node) = Node::panic_mode(node, parser) {
                    error_nodes.push(error_node);
                }
            }
            // if error_nodes is still empty, then no panic mode was entered, this is an error
            // restore current_nodes to fallback_nodes
            if error_nodes.is_empty() {
                std::mem::swap(&mut self.current_nodes, &mut self.fallback_nodes);

                Err(ParseError {
                    term,
                    location,
                    reduce_action_errors: std::mem::take(&mut self.reduce_errors),
                    no_precedences: std::mem::take(&mut self.no_precedences),
                })
            } else {
                self.fallback_nodes.clear();

                // try shift term to error state
                for mut error_node in error_nodes {
                    if let Some(next_state) =
                        parser.get_states()[error_node.state].shift_goto_class(class)
                    {
                        // A -> a . error b
                        // and b is fed, shift error and b

                        let next_node = Node {
                            parent: Some(Rc::new(error_node)),
                            state: next_state,
                            data: Some((Data::new_terminal(term.clone()), location.clone())),
                            precedence_level: shift_prec,
                            #[cfg(feature = "tree")]
                            tree: Some(crate::tree::Tree::new_terminal(term.clone())),
                        };
                        self.current_nodes.push(Rc::new(next_node));
                    } else {
                        // here, fed token is in `error` non-terminal
                        // so merge location with previous

                        let new_location = Data::Location::new(
                            std::iter::once(&location).chain(
                                error_node.iter().map(|node| &node.data.as_ref().unwrap().1),
                            ),
                            2, // error node + fed token
                        );
                        error_node.data.as_mut().unwrap().1 = new_location;

                        self.current_nodes.push(Rc::new(error_node));
                    }
                }
                Ok(())
            }
        } else {
            std::mem::swap(&mut self.current_nodes, &mut self.next_nodes);
            Ok(())
        }
    }

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
        P::NonTerm: Hash + Eq,
    {
        let class = parser.to_terminal_class(term);
        let shift_prec = parser.class_precedence(class);
        self.current_nodes
            .iter()
            .any(|node| Node::can_feed(node, parser, term, class, shift_prec))
    }

    /// Check if current context can enter panic mode.
    pub fn can_panic_mode<P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &mut self,
        parser: &P,
    ) -> bool
    where
        Data::NonTerm: std::hash::Hash + Eq,
    {
        use crate::parser::State;

        let Some(error_nonterm) = parser.get_error_nonterm() else {
            return false;
        };
        for mut node in self.current_nodes.iter() {
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
        false
    }
}

impl<Data: TokenData> Default for Context<Data> {
    fn default() -> Self {
        Context {
            current_nodes: FromIterator::from_iter([Rc::new(Node::new_root())]),
            next_nodes: Default::default(),
            reduce_errors: Default::default(),
            reduce_args: Default::default(),
            fallback_nodes: Default::default(),
            no_precedences: Default::default(),
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
