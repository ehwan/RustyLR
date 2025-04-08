use std::fmt::Debug;
use std::fmt::Display;
use std::ops::Deref;
use std::ops::DerefMut;

use termtree::Tree as TermTree;

/// Default depth of tree when pretty printing through Display
const DEFAULT_MAX_LEVEL: usize = usize::MAX;

/// Tree represention of single non-terminal token.
/// User must enable feature `tree` to use this.
#[derive(Debug, Clone)]
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
    pub(crate) fn to_term_tree(&self, max_level: usize) -> TermTree<String>
    where
        Term: Display,
        NonTerm: Display,
    {
        let tree = TermTree::new(format!("{}", self.nonterm));
        if max_level == 0 {
            tree
        } else {
            tree.with_leaves(
                self.tokens
                    .iter()
                    .map(|token| token.to_term_tree(max_level - 1)),
            )
        }
    }

    /// convert this tree to termtree::Tree using Debug trait
    pub(crate) fn to_term_tree_debug(&self, max_level: usize) -> TermTree<String>
    where
        Term: Debug,
        NonTerm: Debug,
    {
        let tree = TermTree::new(format!("{:?}", self.nonterm));
        if max_level == 0 {
            tree
        } else {
            tree.with_leaves(
                self.tokens
                    .iter()
                    .map(|token| token.to_term_tree_debug(max_level - 1)),
            )
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
    pub(crate) fn to_term_tree(&self, max_level: usize) -> TermTree<String>
    where
        Term: Display,
        NonTerm: Display,
    {
        match self {
            Tree::Terminal(term) => TermTree::new(format!("{}", term)),
            Tree::NonTerminal(nonterm) => nonterm.to_term_tree(max_level),
        }
    }

    /// convert this tree to termtree::Tree using Debug trait
    pub(crate) fn to_term_tree_debug(&self, max_level: usize) -> TermTree<String>
    where
        Term: Debug,
        NonTerm: Debug,
    {
        match self {
            Tree::Terminal(term) => TermTree::new(format!("{:?}", term)),
            Tree::NonTerminal(nonterm) => nonterm.to_term_tree_debug(max_level),
        }
    }

    /// pretty print this tree using Display trait
    pub fn pretty_print_format(
        &self,
        max_level: usize,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result
    where
        Term: Display,
        NonTerm: Display,
    {
        write!(f, "{}", self.to_term_tree(max_level))
    }

    /// pretty print this tree using Debug trait
    pub fn pretty_print_format_debug(
        &self,
        max_level: usize,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result
    where
        Term: Debug,
        NonTerm: Debug,
    {
        write!(f, "{}", self.to_term_tree_debug(max_level))
    }

    /// pretty print this tree with max level using Display trait
    pub fn pretty_print(&self, max_level: usize)
    where
        Term: Display,
        NonTerm: Display,
    {
        print!("{}", self.to_term_tree(max_level));
    }
    /// pretty print this tree with max level using Debug trait
    pub fn pretty_print_debug(&self, max_level: usize)
    where
        Term: Debug,
        NonTerm: Debug,
    {
        print!("{}", self.to_term_tree_debug(max_level));
    }
}

impl<Term: Display, NonTerm: Display> Display for Tree<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_print_format(DEFAULT_MAX_LEVEL, f)
    }
}
impl<Term: Debug, NonTerm: Debug> Debug for Tree<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_print_format_debug(DEFAULT_MAX_LEVEL, f)
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
        Default::default()
    }

    /// pretty print this tree list using `Display` trait
    pub fn pretty_print_format(
        &self,
        max_level: usize,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result
    where
        Term: Display,
        NonTerm: Display,
    {
        let tree = TermTree::new("TreeList".to_string());
        let mut tree = if max_level == 0 {
            tree
        } else {
            tree.with_leaves(
                self.trees
                    .iter()
                    .map(|tree| tree.to_term_tree(max_level - 1)),
            )
        };
        tree.set_multiline(false);
        writeln!(f, "{}", tree)
    }

    /// pretty print this tree using `Debug` trait
    pub fn pretty_print_format_debug(
        &self,
        max_level: usize,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result
    where
        Term: Debug,
        NonTerm: Debug,
    {
        let tree = TermTree::new("TreeList".to_string());
        let mut tree = if max_level == 0 {
            tree
        } else {
            tree.with_leaves(
                self.trees
                    .iter()
                    .map(|tree| tree.to_term_tree_debug(max_level - 1)),
            )
        };
        tree.set_multiline(true);
        writeln!(f, "{}", tree)
    }

    /// pretty print this tree with max level using `Display` trait
    pub fn pretty_print(&self, max_level: usize)
    where
        Term: Display,
        NonTerm: Display,
    {
        let tree = TermTree::new("TreeList".to_string());
        let mut tree = if max_level == 0 {
            tree
        } else {
            tree.with_leaves(
                self.trees
                    .iter()
                    .map(|tree| tree.to_term_tree(max_level - 1)),
            )
        };
        tree.set_multiline(false);
        println!("{}", tree)
    }
    /// pretty print this tree with max level
    pub fn pretty_print_debug(&self, max_level: usize)
    where
        Term: Debug,
        NonTerm: Debug,
    {
        let tree = TermTree::new("TreeList".to_string());
        let mut tree = if max_level == 0 {
            tree
        } else {
            tree.with_leaves(
                self.trees
                    .iter()
                    .map(|tree| tree.to_term_tree_debug(max_level - 1)),
            )
        };
        tree.set_multiline(false);
        println!("{}", tree)
    }
}
impl<Term: Display, NonTerm: Display> Display for TreeList<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_print_format(DEFAULT_MAX_LEVEL, f)
    }
}
impl<Term: Debug, NonTerm: Debug> Debug for TreeList<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_print_format_debug(DEFAULT_MAX_LEVEL, f)
    }
}
impl<Term, NonTerm> Default for TreeList<Term, NonTerm> {
    /// create new empty tree list
    fn default() -> Self {
        Self { trees: Vec::new() }
    }
}
