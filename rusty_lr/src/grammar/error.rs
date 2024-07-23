use std::fmt::Debug;
use std::fmt::Display;

use super::grammar::Grammar;
use crate::rule::LookaheadRule;
use crate::rule::LookaheadRuleRef;
use crate::rule::ShiftedRule;

pub enum BuildError<'a, Term, NonTerm> {
    RuleNotFound(NonTerm),

    ReduceReduceConflict {
        lookahead: Term,
        rule1: usize,
        rule2: usize,
        grammar: &'a Grammar<Term, NonTerm>,
    },

    /// shift/reduce conflict, one of the rule have ReduceType::Error
    ShiftReduceConflictError {
        reduce: usize,
        shift: LookaheadRuleRef<Term>,
        grammar: &'a Grammar<Term, NonTerm>,
    },

    /// shift/reduce conflict, one has ReduceType::Left and other has ReduceType::Right
    ShiftReduceConflict {
        reduce: usize,
        left: LookaheadRuleRef<Term>,
        right: LookaheadRuleRef<Term>,
        grammar: &'a Grammar<Term, NonTerm>,
    },

    NoAugmented,
}

impl<'a, Term: Display + Clone, NonTerm: Display + Clone> Display
    for BuildError<'a, Term, NonTerm>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RuleNotFound(nonterm) => write!(f, "Production Rule not found: {}", nonterm)?,
            Self::ReduceReduceConflict {
                lookahead,
                rule1,
                rule2,
                grammar,
            } => write!(
                f,
                r#"Reduce/Reduce Conflict with lookahead: {}
{}
and
{}"#,
                lookahead, grammar.rules[*rule1], grammar.rules[*rule2]
            )?,
            Self::ShiftReduceConflictError {
                reduce,
                shift,
                grammar,
            } => write!(
                f,
                r#"Shift/Reduce Conflict
This rule has ReduceType::Error:
{}
and the reduce rule is:
{}
Try rearanging the rules or change ReduceType to Left or Right."#,
                LookaheadRule {
                    rule: ShiftedRule {
                        rule: grammar.rules[shift.rule.rule].clone(),
                        shifted: shift.rule.shifted,
                    },
                    lookaheads: shift.lookaheads.clone(),
                },
                grammar.rules[*reduce],
            )?,
            Self::ShiftReduceConflict {
                reduce,
                left,
                right,
                grammar,
            } => write!(
                f,
                r#"Shift/Reduce Conflict
This rule has ReduceType::Left:
{}
this rule has ReduceType::Right:
{}
and the reduce rule is:
{}
Try rearanging the rules or change ReduceType to Left or Right."#,
                LookaheadRule {
                    rule: ShiftedRule {
                        rule: grammar.rules[left.rule.rule].clone(),
                        shifted: left.rule.shifted,
                    },
                    lookaheads: left.lookaheads.clone(),
                },
                LookaheadRule {
                    rule: ShiftedRule {
                        rule: grammar.rules[right.rule.rule].clone(),
                        shifted: right.rule.shifted,
                    },
                    lookaheads: right.lookaheads.clone(),
                },
                grammar.rules[*reduce],
            )?,
            Self::NoAugmented => {
                write!(f, "No Augmented Rule found.")?;
            }
        }
        Ok(())
    }
}

impl<'a, Term: Debug + Clone, NonTerm: Debug + Clone> Debug for BuildError<'a, Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RuleNotFound(nonterm) => write!(f, "Production Rule not found: {:?}", nonterm)?,
            Self::ReduceReduceConflict {
                lookahead,
                rule1,
                rule2,
                grammar,
            } => write!(
                f,
                r#"Reduce/Reduce Conflict with lookahead: {:?}
{:?}
and
{:?}"#,
                lookahead, grammar.rules[*rule1], grammar.rules[*rule2]
            )?,
            Self::ShiftReduceConflictError {
                reduce,
                shift,
                grammar,
            } => write!(
                f,
                r#"Shift/Reduce Conflict
This rule has ReduceType::Error:
{:?}
and the reduce rule is:
{:?}
Try rearanging the rules or change ReduceType to Left or Right."#,
                LookaheadRule {
                    rule: ShiftedRule {
                        rule: grammar.rules[shift.rule.rule].clone(),
                        shifted: shift.rule.shifted,
                    },
                    lookaheads: shift.lookaheads.clone(),
                },
                grammar.rules[*reduce],
            )?,
            Self::ShiftReduceConflict {
                reduce,
                left,
                right,
                grammar,
            } => write!(
                f,
                r#"Shift/Reduce Conflict
This rule has ReduceType::Left:
{:?}
this rule has ReduceType::Right:
{:?}
and the reduce rule is:
{:?}
Try rearanging the rules or change ReduceType to Left or Right."#,
                LookaheadRule {
                    rule: ShiftedRule {
                        rule: grammar.rules[left.rule.rule].clone(),
                        shifted: left.rule.shifted,
                    },
                    lookaheads: left.lookaheads.clone(),
                },
                LookaheadRule {
                    rule: ShiftedRule {
                        rule: grammar.rules[right.rule.rule].clone(),
                        shifted: right.rule.shifted,
                    },
                    lookaheads: right.lookaheads.clone(),
                },
                grammar.rules[*reduce],
            )?,
            Self::NoAugmented => {
                write!(f, "No Augmented Rule found.")?;
            }
        }
        Ok(())
    }
}
