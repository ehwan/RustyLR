use std::fmt::Debug;
use std::fmt::Display;
use std::ops::Deref;
use std::ops::DerefMut;

use termtree::Tree as TermTree;

use crate::nonterminal::NonTerminal;
use crate::nonterminal::NonTerminalType;

/// Tree represention of single non-terminal token.
/// User must enable feature `tree` to use this.
#[derive(Clone)]
pub struct TreeNonTerminal<Term, NonTerm> {
    /// non terminal symbol that this tree reduced to
    pub nonterm: NonTerm,

    /// children of this token consumed by reduction
    pub tokens: Vec<Tree<Term, NonTerm>>,
}

impl<Term, NonTerm> TreeNonTerminal<Term, NonTerm> {
    pub fn new(nonterm: NonTerm, tokens: Vec<Tree<Term, NonTerm>>) -> Self {
        Self { nonterm, tokens }
    }

    /// convert this tree to termtree::Tree using Display trait
    pub(crate) fn to_term_tree<D: Display>(
        &self,
        term_to_display: &impl Fn(&Term) -> D,
        nonterm_to_display: &impl Fn(&NonTerm) -> D,
    ) -> Vec<TermTree<D>>
    where
        NonTerm: NonTerminal,
    {
        // Manually configure the format for the auto-generated non-teminals.
        // for example, one or more repetitions A+ will be implemented as left recursion,
        // but we want to display the tree as flat array.
        let nonterm_name = nonterm_to_display(&self.nonterm);
        match self.nonterm.nonterm_type() {
            // normal tree
            None
            | Some(NonTerminalType::Augmented)
            | Some(NonTerminalType::Error)
            | Some(NonTerminalType::Group) => {
                let tree = TermTree::new(nonterm_name);
                vec![tree.with_leaves(
                    self.tokens
                        .iter()
                        .flat_map(|token| token.to_term_tree(term_to_display, nonterm_to_display)),
                )]
            }

            // remove parent, directly add children
            Some(NonTerminalType::Lookahead) | Some(NonTerminalType::TerminalSet) => self
                .tokens
                .iter()
                .flat_map(|token| token.to_term_tree(term_to_display, nonterm_to_display))
                .collect(),

            // remove left/right recursion, make it to flat array
            Some(NonTerminalType::Star) => {
                let tree = TermTree::new(nonterm_name);
                let tree = if self.tokens.is_empty() {
                    tree
                } else {
                    let plus = self.tokens[0]
                        .to_term_tree(term_to_display, nonterm_to_display)
                        .into_iter()
                        .next()
                        .unwrap();
                    tree.with_leaves(plus.leaves)
                };
                vec![tree]
            }
            // remove left/right recursion, make it to flat array
            Some(NonTerminalType::PlusLeft) => {
                let tree = TermTree::new(nonterm_name);
                let tree = match self.tokens.len() {
                    1 => {
                        let child = self.tokens[0]
                            .to_term_tree(term_to_display, nonterm_to_display)
                            .into_iter()
                            .next()
                            .unwrap();
                        tree.with_leaves([child])
                    }
                    2 => {
                        let mut child_list = self.tokens[0]
                            .to_term_tree(term_to_display, nonterm_to_display)
                            .into_iter()
                            .next()
                            .unwrap()
                            .leaves;
                        let child = self.tokens[1]
                            .to_term_tree(term_to_display, nonterm_to_display)
                            .into_iter()
                            .next()
                            .unwrap();
                        child_list.push(child);
                        tree.with_leaves(child_list)
                    }
                    3 => {
                        let mut child_list = self.tokens[0]
                            .to_term_tree(term_to_display, nonterm_to_display)
                            .into_iter()
                            .next()
                            .unwrap()
                            .leaves;
                        let separator = self.tokens[1]
                            .to_term_tree(term_to_display, nonterm_to_display)
                            .into_iter()
                            .next()
                            .unwrap();
                        let child = self.tokens[2]
                            .to_term_tree(term_to_display, nonterm_to_display)
                            .into_iter()
                            .next()
                            .unwrap();
                        child_list.push(separator);
                        child_list.push(child);
                        tree.with_leaves(child_list)
                    }
                    _ => {
                        unreachable!("PlusLeft length of child: {}", self.tokens.len())
                    }
                };
                vec![tree]
            }
            // remove left/right recursion, make it to flat array
            Some(NonTerminalType::PlusRight) => {
                let tree = TermTree::new(nonterm_name);
                let tree = match self.tokens.len() {
                    1 => {
                        let child = self.tokens[0]
                            .to_term_tree(term_to_display, nonterm_to_display)
                            .into_iter()
                            .next()
                            .unwrap();
                        tree.with_leaves([child])
                    }
                    2 => {
                        let child = self.tokens[0]
                            .to_term_tree(term_to_display, nonterm_to_display)
                            .into_iter()
                            .next()
                            .unwrap();
                        let mut child_list = self.tokens[1]
                            .to_term_tree(term_to_display, nonterm_to_display)
                            .into_iter()
                            .next()
                            .unwrap()
                            .leaves;
                        let mut children = vec![child];
                        children.append(&mut child_list);

                        tree.with_leaves(children)
                    }
                    3 => {
                        // with separator
                        let child = self.tokens[0]
                            .to_term_tree(term_to_display, nonterm_to_display)
                            .into_iter()
                            .next()
                            .unwrap();
                        let separator = self.tokens[1]
                            .to_term_tree(term_to_display, nonterm_to_display)
                            .into_iter()
                            .next()
                            .unwrap();
                        let mut child_list = self.tokens[2]
                            .to_term_tree(term_to_display, nonterm_to_display)
                            .into_iter()
                            .next()
                            .unwrap()
                            .leaves;
                        let mut children = vec![child];
                        children.push(separator);
                        children.append(&mut child_list);

                        tree.with_leaves(children)
                    }
                    _ => {
                        unreachable!("PlusRight length of child: {}", self.tokens.len())
                    }
                };
                vec![tree]
            }
            // remove left/right recursion, make it to flat array
            Some(NonTerminalType::Optional) => {
                let tree = TermTree::new(nonterm_name);
                let tree =
                    if self.tokens.is_empty() {
                        tree
                    } else {
                        tree.with_leaves(self.tokens.iter().flat_map(|token| {
                            token.to_term_tree(term_to_display, nonterm_to_display)
                        }))
                    };
                vec![tree]
            }

            // show the literal directly
            Some(NonTerminalType::LiteralString) => {
                let tree = TermTree::new(nonterm_name);
                vec![tree]
            }
        }
    }
}

/// Tree representation of single token.
/// User must enable feature `tree` to use this.
#[derive(Clone)]
pub enum Tree<Term, NonTerm> {
    Terminal(Term),
    NonTerminal(TreeNonTerminal<Term, NonTerm>),
}

impl<Term, NonTerm> Tree<Term, NonTerm> {
    pub fn new_terminal(term: Term) -> Self {
        Tree::Terminal(term)
    }
    pub fn new_nonterminal(nonterm: NonTerm, tokens: Vec<Tree<Term, NonTerm>>) -> Self {
        Tree::NonTerminal(TreeNonTerminal::new(nonterm, tokens))
    }

    /// convert this tree to termtree::Tree using Display trait
    pub(crate) fn to_term_tree<D: Display>(
        &self,
        term_to_display: &impl Fn(&Term) -> D,
        nonterm_to_display: &impl Fn(&NonTerm) -> D,
    ) -> Vec<TermTree<D>>
    where
        NonTerm: NonTerminal,
    {
        match self {
            Tree::Terminal(term) => vec![TermTree::new(term_to_display(term))],
            Tree::NonTerminal(nonterm) => nonterm.to_term_tree(term_to_display, nonterm_to_display),
        }
    }
}

impl<Term: Display, NonTerm: NonTerminal> Display for Tree<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let child = self.to_term_tree(&|term| term.to_string(), &|nonterm| {
            nonterm.as_str().to_string()
        });
        write!(
            f,
            "{}",
            TermTree::new("Tree".to_string()).with_leaves(child)
        )
    }
}
impl<Term: Debug, NonTerm: NonTerminal> Debug for Tree<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let child = self.to_term_tree(&|term| format!("{:?}", term), &|nonterm| {
            nonterm.as_str().to_string()
        });
        write!(
            f,
            "{}",
            TermTree::new("Tree".to_string()).with_leaves(child)
        )
    }
}

/// List of [`Tree`]
#[derive(Clone)]
pub struct TreeList<Term, NonTerm> {
    pub trees: Vec<Tree<Term, NonTerm>>,
}
impl<Term, NonTerm> Deref for TreeList<Term, NonTerm> {
    type Target = Vec<Tree<Term, NonTerm>>;
    fn deref(&self) -> &Self::Target {
        &self.trees
    }
}
impl<Term, NonTerm> DerefMut for TreeList<Term, NonTerm> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.trees
    }
}
impl<Term, NonTerm> TreeList<Term, NonTerm> {
    /// create new empty tree list
    pub fn new() -> Self {
        Self { trees: Vec::new() }
    }
}
impl<Term: Display, NonTerm: NonTerminal> Display for TreeList<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tree =
            TermTree::new("TreeList".to_string()).with_leaves(self.trees.iter().flat_map(|tree| {
                tree.to_term_tree(&|term| term.to_string(), &|nonterm| {
                    nonterm.as_str().to_string()
                })
            }));
        write!(f, "{}", tree)
    }
}
impl<Term: Debug, NonTerm: NonTerminal> Debug for TreeList<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tree =
            TermTree::new("TreeList".to_string()).with_leaves(self.trees.iter().flat_map(|tree| {
                tree.to_term_tree(&|term| format!("{:?}", term), &|nonterm| {
                    nonterm.as_str().to_string()
                })
            }));
        write!(f, "{}", tree)
    }
}
impl<Term, NonTerm> Default for TreeList<Term, NonTerm> {
    /// create new empty tree list
    fn default() -> Self {
        Self::new()
    }
}
