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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ShiftedRuleRef {
    pub rule: usize,
    pub shifted: usize,
}
// impl<'a, Term: TermTraitBound + Display, NonTerm: NonTermTraitBound + Display> Display
//     for ShiftedRuleRef<'a, Term, NonTerm>
// {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{} -> ", self.rule.name)?;
//         for (id, token) in self.rule.rule.iter().enumerate() {
//             if id == self.shifted {
//                 write!(f, ". ")?;
//             }
//             write!(f, "{}", token)?;
//             if id < self.rule.rule.len() - 1 {
//                 write!(f, " ")?;
//             }
//         }
//         if self.shifted == self.rule.rule.len() {
//             write!(f, " .")?;
//         }
//         Ok(())
//     }
// }

/// shifted rule with lookahead tokens
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LookaheadRuleRef<Term> {
    pub rule: ShiftedRuleRef,
    pub lookaheads: BTreeSet<Term>,
}
// impl<'a, Term: TermTraitBound + Display, NonTerm: NonTermTraitBound + Display> Display
//     for LookaheadRuleRef<'a, Term, NonTerm>
// {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{} / ", self.rule)?;
//         for (id, lookahead) in self.lookaheads.iter().enumerate() {
//             write!(f, "{}", lookahead)?;
//             if id < self.lookaheads.len() - 1 {
//                 write!(f, ", ")?;
//             }
//         }
//         Ok(())
//     }
// }

/// set of lookahead rules
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LookaheadRuleRefSet<Term: TermTraitBound> {
    pub rules: BTreeSet<LookaheadRuleRef<Term>>,
}
impl<Term: TermTraitBound> LookaheadRuleRefSet<Term> {
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
