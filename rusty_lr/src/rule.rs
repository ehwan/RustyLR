use std::collections::BTreeSet;

use std::fmt::Display;

use crate::term::TermTraitBound;
use crate::token::Token;

/// A struct for single shifted named production rule
/// name -> Token1 Token2 . Token3
///         ^^^^^^^^^^^^^ shifted = 2
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct NamedShiftedRule<Term: TermTraitBound> {
    pub name: String,
    pub rule: Vec<Token<Term>>,
    pub shifted: usize,
}
impl<Term: TermTraitBound> NamedShiftedRule<Term> {
    pub fn new(name: String, rule: Vec<Token<Term>>, shifted: usize) -> Self {
        NamedShiftedRule {
            name,
            rule,
            shifted,
        }
    }

    /// get first token of shifted rule
    pub fn first(&self) -> Option<&Token<Term>> {
        self.rule.get(self.shifted)
    }
    /// get rest of the shifted rule (excluding first token)
    pub fn rest(&self) -> &[Token<Term>] {
        &self.rule[self.shifted + 1..]
    }
}
impl<Term: TermTraitBound + Display> Display for NamedShiftedRule<Term> {
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
pub struct LookaheadRule<Term: TermTraitBound> {
    pub rule: NamedShiftedRule<Term>,
    pub lookaheads: BTreeSet<Token<Term>>,
}
impl<Term: TermTraitBound + Display> Display for LookaheadRule<Term> {
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
pub struct LookaheadRuleSet<Term: TermTraitBound> {
    pub rules: BTreeSet<LookaheadRule<Term>>,
}
impl<Term: TermTraitBound> LookaheadRuleSet<Term> {
    pub fn new() -> Self {
        LookaheadRuleSet {
            rules: BTreeSet::new(),
        }
    }
    pub fn add_rule(&mut self, rule: LookaheadRule<Term>) {
        self.rules.insert(rule);
    }
}
impl<Term: TermTraitBound + Display> Display for LookaheadRuleSet<Term> {
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
