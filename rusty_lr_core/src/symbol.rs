use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;

/// A type for terminal symbols in the grammar.
/// just because we have to take care of the `error` token specially,
/// and future support for other special tokens.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TerminalSymbol<Term> {
    Terminal(Term),    // index in the terminals vector
    Error,             // error token
    Eof,               // end of file token
    VirtualStart(u32), // virtual start branch token for multiple start symbols
}
impl<Term> TerminalSymbol<Term> {
    pub fn is_error(&self) -> bool {
        matches!(self, TerminalSymbol::Error)
    }
    pub fn is_term(&self) -> bool {
        matches!(self, TerminalSymbol::Terminal(_))
    }
    pub fn is_eof(&self) -> bool {
        matches!(self, TerminalSymbol::Eof)
    }
    pub fn is_virtual_start(&self) -> bool {
        matches!(self, TerminalSymbol::VirtualStart(_))
    }
    /// converts self to a terminal if it is a `Terminal` variant, otherwise returns `None`.
    pub fn to_term(&self) -> Option<&Term> {
        match self {
            TerminalSymbol::Terminal(term) => Some(term),
            _ => None,
        }
    }
    /// converts self to a terminal if it is a `Terminal` variant, otherwise returns `None`.
    pub fn into_term(self) -> Option<Term> {
        match self {
            TerminalSymbol::Terminal(term) => Some(term),
            _ => None,
        }
    }
}

impl<Term: Display> std::fmt::Display for TerminalSymbol<Term> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TerminalSymbol::Terminal(term) => write!(f, "{}", term),
            TerminalSymbol::Error => write!(f, "error"),
            TerminalSymbol::Eof => write!(f, "eof"),
            TerminalSymbol::VirtualStart(i) => write!(f, "start_branch_{}", i),
        }
    }
}

/// Symbol represents a terminal or non-terminal symbol in the grammar.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Symbol<Term, NonTerm> {
    Terminal(Term),
    NonTerminal(NonTerm),
}
impl<Term: Display, NonTerm: Display> Display for Symbol<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::Terminal(term) => write!(f, "{}", term),
            Symbol::NonTerminal(nonterm) => write!(f, "{}", nonterm),
        }
    }
}
impl<Term: Debug, NonTerm: Debug> Debug for Symbol<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::Terminal(term) => write!(f, "{:?}", term),
            Symbol::NonTerminal(nonterm) => write!(f, "{:?}", nonterm),
        }
    }
}

impl<Term, NonTerm> Symbol<Term, NonTerm> {
    pub fn map<NewTerm, NewNonTerm>(
        self,
        term_map: impl Fn(Term) -> NewTerm,
        nonterm_map: impl Fn(NonTerm) -> NewNonTerm,
    ) -> Symbol<NewTerm, NewNonTerm> {
        match self {
            Symbol::Terminal(term) => Symbol::Terminal(term_map(term)),
            Symbol::NonTerminal(nonterm) => Symbol::NonTerminal(nonterm_map(nonterm)),
        }
    }
}
