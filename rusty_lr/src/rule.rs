use std::collections::BTreeSet;

use std::fmt::Display;

use crate::term::NonTermTraitBound;
use crate::term::TermTraitBound;
use crate::token::Token;

/// Production rule.
/// name -> Token0 Token1 Token2 ...
#[derive(Debug, Clone)]
pub struct ProductionRule<Term: TermTraitBound, NonTerm: NonTermTraitBound> {
    pub name: NonTerm,
    pub rule: Vec<Token<Term, NonTerm>>,
    pub uid: usize,
}
impl<Term: TermTraitBound, NonTerm: NonTermTraitBound> PartialEq for ProductionRule<Term, NonTerm> {
    fn eq(&self, other: &Self) -> bool {
        self.uid == other.uid
    }
}
impl<Term: TermTraitBound, NonTerm: NonTermTraitBound> Eq for ProductionRule<Term, NonTerm> {}
impl<Term: TermTraitBound, NonTerm: NonTermTraitBound> PartialOrd
    for ProductionRule<Term, NonTerm>
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.uid.partial_cmp(&other.uid)
    }
}
impl<Term: TermTraitBound, NonTerm: NonTermTraitBound> Ord for ProductionRule<Term, NonTerm> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.uid.cmp(&other.uid)
    }
}
impl<Term: TermTraitBound + Display, NonTerm: NonTermTraitBound + Display> Display
    for ProductionRule<Term, NonTerm>
{
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

/// A struct for single shifted named production rule
/// name -> Token1 Token2 . Token3
///         ^^^^^^^^^^^^^ shifted = 2
#[derive(Debug, Clone)]
pub struct ShiftedRuleRef<'a, Term: TermTraitBound, NonTerm: NonTermTraitBound> {
    pub rule: &'a ProductionRule<Term, NonTerm>,
    pub shifted: usize,
}
impl<'a, Term: TermTraitBound, NonTerm: NonTermTraitBound> PartialEq
    for ShiftedRuleRef<'a, Term, NonTerm>
{
    fn eq(&self, other: &Self) -> bool {
        self.rule.uid == other.rule.uid && self.shifted == other.shifted
    }
}
impl<'a, Term: TermTraitBound, NonTerm: NonTermTraitBound> ShiftedRuleRef<'a, Term, NonTerm> {
    pub fn first(&self) -> Option<&Token<Term, NonTerm>> {
        self.rule.rule.get(self.shifted)
    }
    pub fn rest(&self) -> &[Token<Term, NonTerm>] {
        &self.rule.rule[self.shifted + 1..]
    }
}
impl<'a, Term: TermTraitBound, NonTerm: NonTermTraitBound> Eq
    for ShiftedRuleRef<'a, Term, NonTerm>
{
}
impl<'a, Term: TermTraitBound, NonTerm: NonTermTraitBound> PartialOrd
    for ShiftedRuleRef<'a, Term, NonTerm>
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.rule.uid == other.rule.uid {
            self.shifted.partial_cmp(&other.shifted)
        } else {
            self.rule.uid.partial_cmp(&other.rule.uid)
        }
    }
}
impl<'a, Term: TermTraitBound, NonTerm: NonTermTraitBound> Ord
    for ShiftedRuleRef<'a, Term, NonTerm>
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.rule.uid == other.rule.uid {
            self.shifted.cmp(&other.shifted)
        } else {
            self.rule.uid.cmp(&other.rule.uid)
        }
    }
}
impl<'a, Term: TermTraitBound + Display, NonTerm: NonTermTraitBound + Display> Display
    for ShiftedRuleRef<'a, Term, NonTerm>
{
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
        Ok(())
    }
}

/// shifted rule with lookahead tokens
#[derive(Debug, Clone)]
pub struct LookaheadRuleRef<'a, Term: TermTraitBound, NonTerm: NonTermTraitBound> {
    pub rule: ShiftedRuleRef<'a, Term, NonTerm>,
    pub lookaheads: BTreeSet<Term>,
}
impl<'a, Term: TermTraitBound, NonTerm: NonTermTraitBound> PartialEq
    for LookaheadRuleRef<'a, Term, NonTerm>
{
    fn eq(&self, other: &Self) -> bool {
        self.rule == other.rule && self.lookaheads == other.lookaheads
    }
}
impl<'a, Term: TermTraitBound, NonTerm: NonTermTraitBound> Eq
    for LookaheadRuleRef<'a, Term, NonTerm>
{
}
impl<'a, Term: TermTraitBound, NonTerm: NonTermTraitBound> PartialOrd
    for LookaheadRuleRef<'a, Term, NonTerm>
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.rule == other.rule {
            self.lookaheads.partial_cmp(&other.lookaheads)
        } else {
            self.rule.partial_cmp(&other.rule)
        }
    }
}
impl<'a, Term: TermTraitBound, NonTerm: NonTermTraitBound> Ord
    for LookaheadRuleRef<'a, Term, NonTerm>
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.rule == other.rule {
            self.lookaheads.cmp(&other.lookaheads)
        } else {
            self.rule.cmp(&other.rule)
        }
    }
}
impl<'a, Term: TermTraitBound + Display, NonTerm: NonTermTraitBound + Display> Display
    for LookaheadRuleRef<'a, Term, NonTerm>
{
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

/// set of lookahead rules
#[derive(Debug, Clone)]
pub struct LookaheadRuleRefSet<'a, Term: TermTraitBound, NonTerm: NonTermTraitBound> {
    pub rules: BTreeSet<LookaheadRuleRef<'a, Term, NonTerm>>,
}
impl<'a, Term: TermTraitBound, NonTerm: NonTermTraitBound> LookaheadRuleRefSet<'a, Term, NonTerm> {
    pub fn new() -> Self {
        LookaheadRuleRefSet {
            rules: BTreeSet::new(),
        }
    }
}
impl<'a, Term: TermTraitBound, NonTerm: NonTermTraitBound> PartialOrd
    for LookaheadRuleRefSet<'a, Term, NonTerm>
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.rules.partial_cmp(&other.rules)
    }
}
impl<'a, Term: TermTraitBound, NonTerm: NonTermTraitBound> Ord
    for LookaheadRuleRefSet<'a, Term, NonTerm>
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.rules.cmp(&other.rules)
    }
}
impl<'a, Term: TermTraitBound, NonTerm: NonTermTraitBound> PartialEq
    for LookaheadRuleRefSet<'a, Term, NonTerm>
{
    fn eq(&self, other: &Self) -> bool {
        self.rules == other.rules
    }
}
impl<'a, Term: TermTraitBound, NonTerm: NonTermTraitBound> Eq
    for LookaheadRuleRefSet<'a, Term, NonTerm>
{
}
impl<'a, Term: TermTraitBound + Display, NonTerm: NonTermTraitBound + Display> Display
    for LookaheadRuleRefSet<'a, Term, NonTerm>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (id, rule) in self.rules.iter().enumerate() {
            rule.fmt(f)?;
            if id < self.rules.len() - 1 {
                writeln!(f)?;
            }
        }
        Ok(())
    }
}
