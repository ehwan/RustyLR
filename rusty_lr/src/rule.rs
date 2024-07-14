use std::collections::BTreeSet;

use std::fmt::Display;

use crate::term::TermTraitBound;
use crate::token::Token;

/// A struct for single shifted named production rule
/// name -> Token1 Token2 . Token3
///         ^^^^^^^^^^^^^ shifted = 2
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct NamedShiftedRule<Term: TermTraitBound, NonTerm: TermTraitBound> {
    /// unique identifier of this rule
    pub name: NonTerm,

    /// production rule
    pub rule: Vec<Token<Term, NonTerm>>,

    /// index of shifted token
    pub shifted: usize,
}
impl<Term: TermTraitBound, NonTerm: TermTraitBound> NamedShiftedRule<Term, NonTerm> {
    pub fn new(name: NonTerm, rule: Vec<Token<Term, NonTerm>>, shifted: usize) -> Self {
        NamedShiftedRule {
            name,
            rule,
            shifted,
        }
    }

    /// get first token of shifted rule
    pub fn first(&self) -> Option<&Token<Term, NonTerm>> {
        self.rule.get(self.shifted)
    }
    /// get rest of the shifted rule (excluding first token)
    pub fn rest(&self) -> &[Token<Term, NonTerm>] {
        &self.rule[self.shifted + 1..]
    }
}
impl<Term: TermTraitBound + Display, NonTerm: TermTraitBound + Display> Display
    for NamedShiftedRule<Term, NonTerm>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> ", self.name)?;
        for (id, token) in self.rule.iter().enumerate() {
            if id == self.shifted {
                write!(f, ". ")?;
            }
            write!(f, "{}", token)?;
            if id < self.rule.len() - 1 {
                write!(f, " ")?;
            }
        }
        if self.shifted == self.rule.len() {
            write!(f, " .")?;
        }
        Ok(())
    }
}

/// A struct for single shifted named production rule with lookahead tokens
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LookaheadRule<Term: TermTraitBound, NonTerm: TermTraitBound> {
    pub rule: NamedShiftedRule<Term, NonTerm>,
    pub lookaheads: BTreeSet<Token<Term, NonTerm>>,
}
impl<Term: TermTraitBound + Display, NonTerm: TermTraitBound + Display> Display
    for LookaheadRule<Term, NonTerm>
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

/// A struct for set of lookahead rules
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LookaheadRuleSet<Term: TermTraitBound, NonTerm: TermTraitBound> {
    pub rules: BTreeSet<LookaheadRule<Term, NonTerm>>,
}
impl<Term: TermTraitBound, NonTerm: TermTraitBound> LookaheadRuleSet<Term, NonTerm> {
    pub fn new() -> Self {
        LookaheadRuleSet {
            rules: BTreeSet::new(),
        }
    }
    pub fn add_rule(&mut self, rule: LookaheadRule<Term, NonTerm>) {
        self.rules.insert(rule);
    }
}
impl<Term: TermTraitBound + Display, NonTerm: TermTraitBound + Display> Display
    for LookaheadRuleSet<Term, NonTerm>
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
