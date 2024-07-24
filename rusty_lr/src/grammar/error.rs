use std::fmt::Debug;
use std::fmt::Display;

use crate::rule::LookaheadRule;
use crate::rule::LookaheadRuleRef;
use crate::rule::ShiftedRule;
use crate::ProductionRule;

pub enum BuildError<'a, Term, NonTerm> {
    RuleNotFound(NonTerm),

    ReduceReduceConflict {
        lookahead: Term,
        rule1: usize,
        rule2: usize,
        rules: &'a Vec<ProductionRule<Term, NonTerm>>,
    },

    /// shift/reduce conflict, one of the rule have ReduceType::Error
    ShiftReduceConflictError {
        reduce: usize,
        shift: LookaheadRuleRef<Term>,
        rules: &'a Vec<ProductionRule<Term, NonTerm>>,
    },

    /// shift/reduce conflict, one has ReduceType::Left and other has ReduceType::Right
    ShiftReduceConflict {
        reduce: usize,
        left: LookaheadRuleRef<Term>,
        right: LookaheadRuleRef<Term>,
        rules: &'a Vec<ProductionRule<Term, NonTerm>>,
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
                rules: grammar,
            } => write!(
                f,
                r#"Reduce/Reduce Conflict with lookahead: {}
{}
and
{}"#,
                lookahead, grammar[*rule1], grammar[*rule2]
            )?,
            Self::ShiftReduceConflictError {
                reduce,
                shift,
                rules: grammar,
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
                        rule: grammar[shift.rule.rule].clone(),
                        shifted: shift.rule.shifted,
                    },
                    lookaheads: shift.lookaheads.clone(),
                },
                grammar[*reduce],
            )?,
            Self::ShiftReduceConflict {
                reduce,
                left,
                right,
                rules: grammar,
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
                        rule: grammar[left.rule.rule].clone(),
                        shifted: left.rule.shifted,
                    },
                    lookaheads: left.lookaheads.clone(),
                },
                LookaheadRule {
                    rule: ShiftedRule {
                        rule: grammar[right.rule.rule].clone(),
                        shifted: right.rule.shifted,
                    },
                    lookaheads: right.lookaheads.clone(),
                },
                grammar[*reduce],
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
                rules: grammar,
            } => write!(
                f,
                r#"Reduce/Reduce Conflict with lookahead: {:?}
{:?}
and
{:?}"#,
                lookahead, grammar[*rule1], grammar[*rule2]
            )?,
            Self::ShiftReduceConflictError {
                reduce,
                shift,
                rules: grammar,
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
                        rule: grammar[shift.rule.rule].clone(),
                        shifted: shift.rule.shifted,
                    },
                    lookaheads: shift.lookaheads.clone(),
                },
                grammar[*reduce],
            )?,
            Self::ShiftReduceConflict {
                reduce,
                left,
                right,
                rules: grammar,
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
                        rule: grammar[left.rule.rule].clone(),
                        shifted: left.rule.shifted,
                    },
                    lookaheads: left.lookaheads.clone(),
                },
                LookaheadRule {
                    rule: ShiftedRule {
                        rule: grammar[right.rule.rule].clone(),
                        shifted: right.rule.shifted,
                    },
                    lookaheads: right.lookaheads.clone(),
                },
                grammar[*reduce],
            )?,
            Self::NoAugmented => {
                write!(f, "No Augmented Rule found.")?;
            }
        }
        Ok(())
    }
}
