use std::collections::BTreeMap;
use std::collections::BTreeSet;

use std::fmt::Debug;
use std::fmt::Display;

use crate::symbol::Symbol;

/// For resolving shift/reduce conflict
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Associativity {
    /// reduce to the left, i.e. reduce first
    Left,
    /// reduce to the right, i.e. shift first
    Right,
}
impl std::fmt::Display for Associativity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Associativity::Left => write!(f, "Left"),
            Associativity::Right => write!(f, "Right"),
        }
    }
}

/// Operator precedence for production rules
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    /// no operator precedence
    #[default]
    None,

    /// fixed precedence level
    Fixed(usize), // precedence level
}
impl Precedence {
    pub fn is_some(self) -> bool {
        !matches!(self, Precedence::None)
    }

    pub fn is_none(self) -> bool {
        matches!(self, Precedence::None)
    }
}

// Production rule.
//
// lhs -> Symbol0 Symbol1 Symbol2 ...
#[derive(Clone, Default)]
pub struct Production<Term, NonTerm> {
    pub lhs: NonTerm,
    pub rhs: Vec<Symbol<Term, NonTerm>>,
    pub precedence: Precedence,
}
impl<Term: Display, NonTerm: Display> Display for Production<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> ", self.lhs)?;
        for (id, symbol) in self.rhs.iter().enumerate() {
            write!(f, "{}", symbol)?;
            if id < self.rhs.len() - 1 {
                write!(f, " ")?;
            }
        }
        Ok(())
    }
}
impl<Term: Debug, NonTerm: Debug> Debug for Production<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} -> ", self.lhs)?;
        for (id, symbol) in self.rhs.iter().enumerate() {
            write!(f, "{:?}", symbol)?;
            if id < self.rhs.len() - 1 {
                write!(f, " ")?;
            }
        }

        if self.precedence.is_some() {
            write!(f, " [prec: {:?}]", self.precedence)?;
        }
        Ok(())
    }
}

impl<Term, NonTerm> Production<Term, NonTerm> {
    /// Map terminal and non-terminal symbols to another type.
    /// This is useful when exporting & importing rules.
    pub fn map<NewTerm, NewNonTerm>(
        self,
        term_map: impl Fn(Term) -> NewTerm,
        nonterm_map: impl Fn(NonTerm) -> NewNonTerm,
    ) -> Production<NewTerm, NewNonTerm> {
        Production {
            lhs: nonterm_map(self.lhs),
            rhs: self
                .rhs
                .into_iter()
                .map(move |symbol| match symbol {
                    Symbol::Terminal(term) => Symbol::Terminal(term_map(term)),
                    Symbol::NonTerminal(nonterm) => Symbol::NonTerminal(nonterm_map(nonterm)),
                })
                .collect(),
            precedence: self.precedence,
        }
    }

    /// shift this rule (create an LR(0) item)
    pub fn into_shifted(self, dot: usize) -> LR0Item<Term, NonTerm> {
        LR0Item {
            production: self,
            dot,
        }
    }
}

/// A struct for single shifted named production rule (LR(0) Item).
///
/// ```text
/// lhs -> Symbol1 Symbol2 . Symbol3
///
///        ^^^^^^^^^^^^^^^ dot = 2
/// ```
///
/// This struct has index of the Rule in Grammar::productions
/// and it will be used for Eq, Ord, Hash
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Copy, Default)]
pub struct LR0ItemRef {
    /// index of the production in `productions`
    pub production_idx: usize,
    /// dot index
    pub dot: usize,
}

#[derive(Clone, Default)]
pub struct LR0Item<Term, NonTerm> {
    pub production: Production<Term, NonTerm>,
    pub dot: usize,
}
impl<Term: Display, NonTerm: Display> Display for LR0Item<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> ", self.production.lhs)?;
        for (id, symbol) in self.production.rhs.iter().enumerate() {
            if id == self.dot {
                write!(f, "• ")?;
            }
            write!(f, "{}", symbol)?;
            if id < self.production.rhs.len() - 1 {
                write!(f, " ")?;
            }
        }
        if self.dot == self.production.rhs.len() {
            write!(f, " •")?;
        }

        if self.production.precedence.is_some() {
            write!(f, " [prec: {:?}]", self.production.precedence)?;
        }
        Ok(())
    }
}
impl<Term: Debug, NonTerm: Debug> Debug for LR0Item<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} -> ", self.production.lhs)?;
        for (id, symbol) in self.production.rhs.iter().enumerate() {
            if id == self.dot {
                write!(f, "• ")?;
            }
            write!(f, "{:?}", symbol)?;
            if id < self.production.rhs.len() - 1 {
                write!(f, " ")?;
            }
        }
        if self.dot == self.production.rhs.len() {
            write!(f, " •")?;
        }
        Ok(())
    }
}

/// shifted rule with lookahead tokens (LR(1) Item Ref)
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct LR1ItemRef<Term> {
    pub item: LR0ItemRef,
    pub lookaheads: BTreeSet<Term>,
}

/// shifted rule with lookahead tokens (LR(1) Item)
#[derive(Clone)]
pub struct LR1Item<Term, NonTerm> {
    pub item: LR0Item<Term, NonTerm>,
    pub lookaheads: BTreeSet<Term>,
}
impl<Term: Display, NonTerm: Display> Display for LR1Item<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} / ", self.item)?;
        for (id, lookahead) in self.lookaheads.iter().enumerate() {
            write!(f, "{}", lookahead)?;
            if id < self.lookaheads.len() - 1 {
                write!(f, ", ")?;
            }
        }
        Ok(())
    }
}
impl<Term: Debug, NonTerm: Debug> Debug for LR1Item<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} / ", self.item)?;
        for (id, lookahead) in self.lookaheads.iter().enumerate() {
            write!(f, "{:?}", lookahead)?;
            if id < self.lookaheads.len() - 1 {
                write!(f, ", ")?;
            }
        }
        Ok(())
    }
}

/// set of lookahead rules (Closure / Item Set in a State)
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct ItemSet<Term> {
    pub items: BTreeMap<LR0ItemRef, BTreeSet<Term>>,
}
impl<Term> ItemSet<Term> {
    pub fn new() -> Self {
        ItemSet {
            items: BTreeMap::new(),
        }
    }
    pub fn add(&mut self, item: LR0ItemRef, mut lookaheads: BTreeSet<Term>) -> bool
    where
        Term: Ord,
    {
        let mut changed = false;
        let set = self.items.entry(item).or_insert_with(|| {
            changed = true;
            BTreeSet::new()
        });
        let old = set.len();
        set.append(&mut lookaheads);
        changed || old != set.len()
    }
}
