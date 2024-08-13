use std::collections::BTreeMap;
use std::collections::BTreeSet;

use std::fmt::Debug;
use std::fmt::Display;

use crate::token::Token;

/// for resolving shift/reduce conflict
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ReduceType {
    /// reduce to the left, i.e. reduce first
    Left,
    /// reduce to the right, i.e. shift first
    Right,
}
impl Display for ReduceType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReduceType::Left => write!(f, "Left"),
            ReduceType::Right => write!(f, "Right"),
        }
    }
}

/// Production rule.
///
/// name -> Token0 Token1 Token2 ...
#[derive(Clone, Default)]
pub struct ProductionRule<Term, NonTerm> {
    pub name: NonTerm,
    pub rule: Vec<Token<Term, NonTerm>>,
}
impl<Term: Display, NonTerm: Display> Display for ProductionRule<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> ", self.name)?;
        for (id, token) in self.rule.iter().enumerate() {
            write!(f, "{}", token)?;
            if id < self.rule.len() - 1 {
                write!(f, " ")?;
            }
        }
        Ok(())
    }
}
impl<Term: Debug, NonTerm: Debug> Debug for ProductionRule<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} -> ", self.name)?;
        for (id, token) in self.rule.iter().enumerate() {
            write!(f, "{:?}", token)?;
            if id < self.rule.len() - 1 {
                write!(f, " ")?;
            }
        }
        Ok(())
    }
}

/// A struct for single shifted named production rule.
///
/// name -> Token1 Token2 . Token3
///
///         ^^^^^^^^^^^^^ shifted = 2
///
/// This struct has index of the Rule in Grammar::rules
/// and it will be used for Eq, Ord, Hash
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Copy, Default)]
pub struct ShiftedRuleRef {
    pub rule: usize,
    pub shifted: usize,
}

/// A struct for single shifted named production rule
///
/// name -> Token1 Token2 . Token3
///
///         ^^^^^^^^^^^^^ shifted = 2
/// This struct is only for Display purpose.
#[derive(Clone)]
pub(crate) struct ShiftedRuleRef2<'a, Term, NonTerm> {
    pub rule: &'a ProductionRule<Term, NonTerm>,
    pub shifted: usize,
}
impl<'a, Term: Display, NonTerm: Display> Display for ShiftedRuleRef2<'a, Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> ", self.rule.name)?;
        for (id, token) in self.rule.rule.iter().enumerate() {
            if id == self.shifted {
                write!(f, "• ")?;
            }
            write!(f, "{}", token)?;
            if id < self.rule.rule.len() - 1 {
                write!(f, " ")?;
            }
        }
        if self.shifted == self.rule.rule.len() {
            write!(f, " •")?;
        }
        Ok(())
    }
}
impl<'a, Term: Debug, NonTerm: Debug> Debug for ShiftedRuleRef2<'a, Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} -> ", self.rule.name)?;
        for (id, token) in self.rule.rule.iter().enumerate() {
            if id == self.shifted {
                write!(f, "• ")?;
            }
            write!(f, "{:?}", token)?;
            if id < self.rule.rule.len() - 1 {
                write!(f, " ")?;
            }
        }
        if self.shifted == self.rule.rule.len() {
            write!(f, " •")?;
        }
        Ok(())
    }
}

#[derive(Clone, Default)]
pub struct ShiftedRule<Term, NonTerm> {
    pub rule: ProductionRule<Term, NonTerm>,
    pub shifted: usize,
}
impl<Term: Display, NonTerm: Display> Display for ShiftedRule<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> ", self.rule.name)?;
        for (id, token) in self.rule.rule.iter().enumerate() {
            if id == self.shifted {
                write!(f, "• ")?;
            }
            write!(f, "{}", token)?;
            if id < self.rule.rule.len() - 1 {
                write!(f, " ")?;
            }
        }
        if self.shifted == self.rule.rule.len() {
            write!(f, " •")?;
        }
        Ok(())
    }
}
impl<Term: Debug, NonTerm: Debug> Debug for ShiftedRule<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} -> ", self.rule.name)?;
        for (id, token) in self.rule.rule.iter().enumerate() {
            if id == self.shifted {
                write!(f, "• ")?;
            }
            write!(f, "{:?}", token)?;
            if id < self.rule.rule.len() - 1 {
                write!(f, " ")?;
            }
        }
        if self.shifted == self.rule.rule.len() {
            write!(f, " •")?;
        }
        Ok(())
    }
}

/// shifted rule with lookahead tokens
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct LookaheadRuleRef<Term> {
    pub rule: ShiftedRuleRef,
    pub lookaheads: BTreeSet<Term>,
}

/// shifted rule with lookahead tokens
#[derive(Clone)]
pub struct LookaheadRule<Term, NonTerm> {
    pub rule: ShiftedRule<Term, NonTerm>,
    pub lookaheads: BTreeSet<Term>,
}
impl<Term: Display, NonTerm: Display> Display for LookaheadRule<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} / ", self.rule)?;
        for (id, lookahead) in self.lookaheads.iter().enumerate() {
            write!(f, "{}", lookahead)?;
            if id < self.lookaheads.len() - 1 {
                write!(f, ", ")?;
            }
        }
        Ok(())
    }
}
impl<Term: Debug, NonTerm: Debug> Debug for LookaheadRule<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} / ", self.rule)?;
        for (id, lookahead) in self.lookaheads.iter().enumerate() {
            write!(f, "{:?}", lookahead)?;
            if id < self.lookaheads.len() - 1 {
                write!(f, ", ")?;
            }
        }
        Ok(())
    }
}

/// set of lookahead rules
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct LookaheadRuleRefSet<Term> {
    pub rules: BTreeMap<ShiftedRuleRef, BTreeSet<Term>>,
}
impl<Term> LookaheadRuleRefSet<Term> {
    pub fn new() -> Self {
        LookaheadRuleRefSet {
            rules: BTreeMap::new(),
        }
    }
    pub fn add(&mut self, rule: ShiftedRuleRef, mut lookaheads: BTreeSet<Term>) -> bool
    where
        Term: Ord,
    {
        let mut changed = false;
        let set = self.rules.entry(rule).or_insert_with(|| {
            changed = true;
            BTreeSet::new()
        });
        let old = set.len();
        set.append(&mut lookaheads);
        changed || old != set.len()
    }
}
// impl<'a, Term: TermTraitBound + Display, NonTerm: NonTermTraitBound + Display> Display
//     for LookaheadRuleRefSet<'a, Term, NonTerm>
// {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         for (id, rule) in self.rules.iter().enumerate() {
//             rule.fmt(f)?;
//             if id < self.rules.len() - 1 {
//                 writeln!(f)?;
//             }
//         }
//         Ok(())
//     }
// }
