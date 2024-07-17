use std::collections::BTreeSet;

use std::fmt::Debug;
use std::fmt::Display;

use crate::token::Token;

/// wheather to reduce to the left or right, for reduce/shift conflict resolving
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ReduceType {
    Left,
    Right,
    /// error when conflict occurs
    Error,
}
impl Display for ReduceType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReduceType::Left => write!(f, "Left"),
            ReduceType::Right => write!(f, "Right"),
            ReduceType::Error => write!(f, "Error"),
        }
    }
}

/// Production rule.
/// name -> Token0 Token1 Token2 ...
#[derive(Clone)]
pub struct ProductionRule<Term, NonTerm> {
    pub name: NonTerm,
    pub rule: Vec<Token<Term, NonTerm>>,
    pub reduce_type: ReduceType,
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
        match self.reduce_type {
            ReduceType::Left => write!(f, " (Left)")?,
            ReduceType::Right => write!(f, " (Right)")?,
            ReduceType::Error => {}
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
        match self.reduce_type {
            ReduceType::Left => write!(f, " (Left)")?,
            ReduceType::Right => write!(f, " (Right)")?,
            ReduceType::Error => {}
        }
        Ok(())
    }
}

/// A struct for single shifted named production rule
/// name -> Token1 Token2 . Token3
///         ^^^^^^^^^^^^^ shifted = 2
/// This struct has index of the Rule in Grammar::rules
/// and it will be used for Eq, Ord, Hash
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ShiftedRuleRef {
    pub rule: usize,
    pub shifted: usize,
}

/// A struct for single shifted named production rule
/// name -> Token1 Token2 . Token3
///         ^^^^^^^^^^^^^ shifted = 2
#[derive(Clone)]
pub struct ShiftedRule<Term, NonTerm> {
    pub rule: ProductionRule<Term, NonTerm>,
    pub shifted: usize,
}
impl<Term: Display, NonTerm: Display> Display for ShiftedRule<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> ", self.rule.name)?;
        for (id, token) in self.rule.rule.iter().enumerate() {
            if id == self.shifted {
                write!(f, ". ")?;
            }
            write!(f, "{}", token)?;
            if id < self.rule.rule.len() - 1 {
                write!(f, " ")?;
            }
        }
        if self.shifted == self.rule.rule.len() {
            write!(f, " .")?;
        }
        match self.rule.reduce_type {
            ReduceType::Left => write!(f, " (Left)")?,
            ReduceType::Right => write!(f, " (Right)")?,
            ReduceType::Error => {}
        }
        Ok(())
    }
}
impl<Term: Debug, NonTerm: Debug> Debug for ShiftedRule<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} -> ", self.rule.name)?;
        for (id, token) in self.rule.rule.iter().enumerate() {
            if id == self.shifted {
                write!(f, ". ")?;
            }
            write!(f, "{:?}", token)?;
            if id < self.rule.rule.len() - 1 {
                write!(f, " ")?;
            }
        }
        if self.shifted == self.rule.rule.len() {
            write!(f, " .")?;
        }
        match self.rule.reduce_type {
            ReduceType::Left => write!(f, " (Left)")?,
            ReduceType::Right => write!(f, " (Right)")?,
            ReduceType::Error => {}
        }
        Ok(())
    }
}

/// shifted rule with lookahead tokens
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LookaheadRuleRef<Term> {
    pub rule: ShiftedRuleRef,
    pub lookaheads: BTreeSet<Term>,
}

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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LookaheadRuleRefSet<Term> {
    pub rules: BTreeSet<LookaheadRuleRef<Term>>,
}
impl<Term: Ord> LookaheadRuleRefSet<Term> {
    pub fn new() -> Self {
        LookaheadRuleRefSet {
            rules: BTreeSet::new(),
        }
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
