use std::fmt::Debug;
use std::fmt::Display;

use crate::rule::LookaheadRule;
use crate::rule::LookaheadRuleRefSet;
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

    /// shift/reduce conflict
    ShiftReduceConflict {
        reduce: usize,
        shift: LookaheadRuleRefSet<Term>,
        term: Term,
        rules: &'a Vec<ProductionRule<Term, NonTerm>>,
    },

    NoAugmented,

    /// different reduce type assigned to same terminal symbol
    MultipleReduceType(Term),
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
            Self::ShiftReduceConflict {
                reduce,
                shift,
                term,
                rules: grammar,
            } => {
                write!(
                    f,
                    r#"Shift/Reduce Conflict
NextTerm: {}
Reduce Rule:
{}
Shift Rules:
"#,
                    term,
                    grammar[*reduce],
                )?;
                for (rule, lookaheads) in shift.rules.iter() {
                    writeln!(
                        f,
                        "{}",
                        LookaheadRule {
                            rule: ShiftedRule {
                                rule: grammar[rule.rule].clone(),
                                shifted: rule.shifted,
                            },
                            lookaheads: lookaheads.clone(),
                        }
                    )?;
                }
                write!(f, 
                    "Try rearanging the rules or set ReduceType to Terminal to resolve the conflict.",
                )?;
            }
            Self::NoAugmented => {
                write!(f, "No Augmented Rule found.")?;
            }

            Self::MultipleReduceType(term) => {
                write!(f, "Multiple ReduceType for terminal symbol: {}", term)?;
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
            Self::ShiftReduceConflict {
                reduce,
                shift,
                term,
                rules: grammar,
            } => {
                write!(
                    f,
                    r#"Shift/Reduce Conflict
NextTerm: {:?}
Reduce Rule:
{:?}
Shift Rules:
"#,
                    term,
                    grammar[*reduce],
                )?;
                for (rule, lookaheads) in shift.rules.iter() {
                    writeln!(
                        f,
                        "{:?}",
                        LookaheadRule {
                            rule: ShiftedRule {
                                rule: grammar[rule.rule].clone(),
                                shifted: rule.shifted,
                            },
                            lookaheads: lookaheads.clone(),
                        }
                    )?;
                }
                write!(f, 
                    "Try rearanging the rules or set ReduceType to Terminal to resolve the conflict.",
                )?;
            }
            Self::NoAugmented => {
                write!(f, "No Augmented Rule found.")?;
            }
            Self::MultipleReduceType(term) => {
                write!(f, "Multiple ReduceType for terminal symbol: {:?}", term)?;
            }
        }
        Ok(())
    }
}
