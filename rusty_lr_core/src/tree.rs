use std::fmt::Debug;
use std::fmt::Display;
use std::ops::Deref;
use std::ops::DerefMut;

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

    /// pretty print this tree
    /// This function is called by `Display` trait.
    pub fn pretty_print(
        &self,
        current_level: usize,
        max_level: usize,
        prefix: &str,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result
    where
        Term: Display,
        NonTerm: Display,
    {
        writeln!(f, "{}", self.nonterm)?;
        if current_level < max_level {
            for (idx, token) in self.tokens.iter().enumerate() {
                if idx == self.tokens.len() - 1 {
                    write!(f, "{}└─", prefix)?;
                    token.pretty_print_format_impl(
                        current_level + 1,
                        max_level,
                        format!("{}  ", prefix).as_str(),
                        f,
                    )?;
                } else {
                    write!(f, "{}├─", prefix)?;
                    token.pretty_print_format_impl(
                        current_level + 1,
                        max_level,
                        format!("{}│ ", prefix).as_str(),
                        f,
                    )?;
                }
            }
        }
        Ok(())
    }

    /// pretty print this tree
    /// This function is called by `Debug` trait.
    pub fn pretty_print_debug(
        &self,
        current_level: usize,
        max_level: usize,
        prefix: &str,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result
    where
        Term: Debug,
        NonTerm: Debug,
    {
        writeln!(f, "{:?}", self.nonterm)?;
        if current_level < max_level {
            for (idx, token) in self.tokens.iter().enumerate() {
                if idx == self.tokens.len() - 1 {
                    write!(f, "{}└─", prefix)?;
                    token.pretty_print_format_debug_impl(
                        current_level + 1,
                        max_level,
                        format!("{}  ", prefix).as_str(),
                        f,
                    )?;
                } else {
                    write!(f, "{}├─", prefix)?;
                    token.pretty_print_format_debug_impl(
                        current_level + 1,
                        max_level,
                        format!("{}│ ", prefix).as_str(),
                        f,
                    )?;
                }
            }
        }
        Ok(())
    }
}

impl<Term, NonTerm> Display for TreeNonTerminal<Term, NonTerm>
where
    Term: Display,
    NonTerm: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_print(0, DEFAULT_MAX_LEVEL, "", f)
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

    /// pretty print this tree
    /// This function is called by `Display` trait.
    pub(crate) fn pretty_print_format_impl(
        &self,
        current_level: usize,
        max_level: usize,
        prefix: &str,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result
    where
        Term: Display,
        NonTerm: Display,
    {
        match self {
            Tree::Terminal(term) => writeln!(f, "{}", term),
            Tree::NonTerminal(nonterm) => nonterm.pretty_print(current_level, max_level, prefix, f),
        }
    }

    /// pretty print this tree
    /// This function is called by `Debug` trait.
    pub(crate) fn pretty_print_format_debug_impl(
        &self,
        current_level: usize,
        max_level: usize,
        prefix: &str,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result
    where
        Term: Debug,
        NonTerm: Debug,
    {
        match self {
            Tree::Terminal(term) => writeln!(f, "{:?}", term),
            Tree::NonTerminal(nonterm) => {
                nonterm.pretty_print_debug(current_level, max_level, prefix, f)
            }
        }
    }

    /// pretty print this tree
    /// This function is called by `Display` trait.
    pub fn pretty_print_format(
        &self,
        max_level: usize,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result
    where
        Term: Display,
        NonTerm: Display,
    {
        self.pretty_print_format_impl(0, max_level, "", f)
    }
    /// pretty print this tree
    /// This function is called by `Debug` trait.
    pub fn pretty_print_format_debug(
        &self,
        max_level: usize,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result
    where
        Term: Debug,
        NonTerm: Debug,
    {
        self.pretty_print_format_debug_impl(0, max_level, "", f)
    }

    /// pretty print this tree with max level
    pub fn pretty_print(&self, max_level: usize)
    where
        Term: Display,
        NonTerm: Display,
    {
        print!(
            "{}",
            TreeWithMaxLevel {
                tree: self,
                max_level,
            }
        );
    }
    /// pretty print this tree with max level
    pub fn pretty_print_debug(&self, max_level: usize)
    where
        Term: Debug,
        NonTerm: Debug,
    {
        print!(
            "{:?}",
            TreeWithMaxLevel {
                tree: self,
                max_level,
            }
        );
    }
}

impl<Term: Display, NonTerm: Display> Display for Tree<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_print_format_impl(0, DEFAULT_MAX_LEVEL, "", f)
    }
}
impl<Term: Debug, NonTerm: Debug> Debug for Tree<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_print_format_debug_impl(0, DEFAULT_MAX_LEVEL, "", f)
    }
}

/// List of [`Tree`]
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

    /// pretty print this tree list
    /// This function is called by `Display` trait.
    pub fn pretty_print_format(
        &self,
        max_level: usize,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result
    where
        Term: Display,
        NonTerm: Display,
    {
        writeln!(f, "TreeList")?;
        for (idx, tree) in self.trees.iter().enumerate() {
            if idx == self.trees.len() - 1 {
                write!(f, "└─")?;
                tree.pretty_print_format_impl(0, max_level, "  ", f)?;
            } else {
                write!(f, "├─")?;
                tree.pretty_print_format_impl(0, max_level, "│ ", f)?;
            }
        }
        Ok(())
    }

    /// pretty print this tree
    /// This function is called by `Debug` trait.
    pub fn pretty_print_format_debug(
        &self,
        max_level: usize,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result
    where
        Term: Debug,
        NonTerm: Debug,
    {
        writeln!(f, "TreeList")?;
        for (idx, tree) in self.trees.iter().enumerate() {
            if idx == self.trees.len() - 1 {
                write!(f, "└─")?;
                tree.pretty_print_format_debug_impl(0, max_level, "  ", f)?;
            } else {
                write!(f, "├─")?;
                tree.pretty_print_format_debug_impl(0, max_level, "│ ", f)?;
            }
        }
        Ok(())
    }

    /// pretty print this tree with max level
    pub fn pretty_print(&self, max_level: usize)
    where
        Term: Display,
        NonTerm: Display,
    {
        print!(
            "{}",
            TreeListWithMaxLevel {
                trees: self,
                max_level,
            }
        );
    }
    /// pretty print this tree with max level
    pub fn pretty_print_debug(&self, max_level: usize)
    where
        Term: Debug,
        NonTerm: Debug,
    {
        print!(
            "{:?}",
            TreeListWithMaxLevel {
                trees: self,
                max_level,
            }
        );
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

/// Private struct to `Display` or `Debug` with max level
struct TreeWithMaxLevel<'a, Term, NonTerm> {
    tree: &'a Tree<Term, NonTerm>,
    max_level: usize,
}
impl<'a, Term: Display, NonTerm: Display> Display for TreeWithMaxLevel<'a, Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.tree.pretty_print_format(self.max_level, f)
    }
}
impl<'a, Term: Debug, NonTerm: Debug> Debug for TreeWithMaxLevel<'a, Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.tree.pretty_print_format_debug(self.max_level, f)
    }
}

/// Private struct to `Display` or `Debug` with max level
struct TreeListWithMaxLevel<'a, Term, NonTerm> {
    trees: &'a TreeList<Term, NonTerm>,
    max_level: usize,
}

impl<'a, Term: Display, NonTerm: Display> Display for TreeListWithMaxLevel<'a, Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.trees.pretty_print_format(self.max_level, f)
    }
}
impl<'a, Term: Debug, NonTerm: Debug> Debug for TreeListWithMaxLevel<'a, Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.trees.pretty_print_format_debug(self.max_level, f)
    }
}
