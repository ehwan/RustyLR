use std::collections::BTreeSet;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;

use crate::rule::ShiftedRule;
use crate::state::State;
use crate::ProductionRule;
use crate::ShiftedRuleRef;
use crate::Token;

/// Error type for feed()
pub enum ParseError<'a, Term, NonTerm, CallbackError, ReduceActionError> {
    /// Invalid non-terminal feeded.
    /// This variant should be removed in the future.
    /// it will not occur if the grammar is builded correctly
    InvalidNonTerminal(
        &'a NonTerm,
        &'a [ProductionRule<Term, NonTerm>],
        &'a [State<Term, NonTerm>],
        Vec<usize>,
    ),

    /// Invalid terminal feeded
    InvalidTerminal(
        Term,
        &'a [ProductionRule<Term, NonTerm>],
        &'a [State<Term, NonTerm>],
        Vec<usize>,
    ),

    /// Error from callback trait
    Callback(CallbackError),

    /// Error from macro reduce action
    ReduceAction(ReduceActionError),
}

impl<'a, Term, NonTerm, CallbackError, ReduceActionError>
    ParseError<'a, Term, NonTerm, CallbackError, ReduceActionError>
{
    /// generate backtrace information.
    /// returned Vec holds a list of ruleset, each ruleset is what current state was trying to parse.
    /// 0'th index is the latest, that is, the last element of Vec will hold the initial state's ruleset
    pub fn backtrace(
        rules: &[ProductionRule<Term, NonTerm>],
        states: &[State<Term, NonTerm>],
        state_stack: &[usize],
    ) -> Vec<BTreeSet<ShiftedRuleRef>>
    where
        NonTerm: Eq,
    {
        let mut backtrace = Vec::with_capacity(state_stack.len());

        // if it is on first state, print all rules
        if state_stack.len() == 1 {
            backtrace.push(states[0].ruleset.rules.keys().copied().collect());
            return backtrace;
        } else {
            let state = &states[*state_stack.last().unwrap()];
            let mut cur_rules = BTreeSet::new();
            for (rule, _) in state.ruleset.rules.iter() {
                if rule.shifted > 0 {
                    cur_rules.insert(*rule);
                }
            }
            for prev_state in state_stack.iter().rev().skip(1) {
                backtrace.push(cur_rules.clone());

                // prepare for next iteration
                let mut prev_rules = BTreeSet::new();
                for mut r in cur_rules.into_iter() {
                    match r.shifted {
                        0 => {}

                        _ => {
                            r.shifted -= 1;
                            prev_rules.insert(r);
                        }
                    }
                }

                loop {
                    let mut add_rules = BTreeSet::new();
                    for r in prev_rules.iter() {
                        // this rule's shift == 0,
                        // it must be added by other rule start with this rule's non-terminal
                        if r.shifted == 0 {
                            let rule_name = &rules[r.rule].name;
                            for (rule, _) in states[*prev_state].ruleset.rules.iter() {
                                if let Some(Token::NonTerm(next_token)) =
                                    rules[rule.rule].rule.get(rule.shifted)
                                {
                                    if next_token == rule_name {
                                        add_rules.insert(*rule);
                                    }
                                }
                            }
                        }
                    }
                    let len0 = prev_rules.len();
                    prev_rules.append(&mut add_rules);
                    if prev_rules.len() == len0 {
                        break;
                    }
                }

                cur_rules = prev_rules;
            }
            backtrace.push(cur_rules);
            backtrace
        }
    }
}

impl<
        'a,
        Term: Display + Clone + Ord + Hash + Eq,
        NonTerm: Display + Clone + Eq,
        CallbackError: Display,
        ReduceActionError: Display,
    > Display for ParseError<'a, Term, NonTerm, CallbackError, ReduceActionError>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::InvalidNonTerminal(nonterm, rules, states, state_stack) => {
                // this variant should be removed in the future.
                // it will not occur if the grammar is builded correctly
                writeln!(f, "Invalid NonTerminal: {}", nonterm)?;
                let state = &states[*state_stack.last().unwrap()];

                let expected: BTreeSet<Term> = state.expected().into_iter().collect();
                if expected.is_empty() {
                    writeln!(f, "No expected token")?;
                } else {
                    write!(f, "Expected one of: ")?;
                    let len = expected.len();
                    for (id, term) in expected.into_iter().enumerate() {
                        write!(f, "{}", term)?;
                        if id < len - 1 {
                            write!(f, ", ")?;
                        } else {
                            writeln!(f)?;
                        }
                    }
                }

                let backtrace = Self::backtrace(rules, states, state_stack);
                for (id, ruleset) in backtrace.iter().enumerate() {
                    if id == 0 {
                        writeln!(f, "{:-^80}", "Backtracing state")?;
                    } else {
                        writeln!(f, "{:-^80}", "Prev state")?;
                    }
                    for rule in ruleset.iter() {
                        let shifted = ShiftedRule {
                            rule: rules[rule.rule].clone(),
                            shifted: rule.shifted,
                        };
                        writeln!(f, "{}", shifted)?;
                    }
                }
            }
            ParseError::InvalidTerminal(term, rules, states, state_stack) => {
                writeln!(f, "Invalid Terminal: {}", term,)?;
                let state = &states[*state_stack.last().unwrap()];

                let expected: BTreeSet<Term> = state.expected().into_iter().collect();
                if expected.is_empty() {
                    writeln!(f, "No expected token")?;
                } else {
                    write!(f, "Expected one of: ")?;
                    let len = expected.len();
                    for (id, term) in expected.into_iter().enumerate() {
                        write!(f, "{}", term)?;
                        if id < len - 1 {
                            write!(f, ", ")?;
                        } else {
                            writeln!(f)?;
                        }
                    }
                }

                let backtrace = Self::backtrace(rules, states, state_stack);
                for (id, ruleset) in backtrace.iter().enumerate() {
                    if id == 0 {
                        writeln!(f, "{:-^80}", "Backtracing state")?;
                    } else {
                        writeln!(f, "{:-^80}", "Prev state")?;
                    }
                    for rule in ruleset.iter() {
                        let shifted = ShiftedRule {
                            rule: rules[rule.rule].clone(),
                            shifted: rule.shifted,
                        };
                        writeln!(f, "{}", shifted)?;
                    }
                }
            }
            ParseError::Callback(message) => {
                write!(f, "Callback Error: {}", message)?;
            }
            ParseError::ReduceAction(message) => {
                write!(f, "{}", message)?;
            }
        }
        Ok(())
    }
}
impl<
        'a,
        Term: Debug + Clone + Ord + Hash + Eq,
        NonTerm: Debug + Clone + Eq,
        CallbackError: Debug,
        ReduceActionError: Debug,
    > Debug for ParseError<'a, Term, NonTerm, CallbackError, ReduceActionError>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::InvalidNonTerminal(nonterm, rules, states, state_stack) => {
                // this variant should be removed in the future.
                // it will not occur if the grammar is builded correctly
                writeln!(f, "Invalid NonTerminal: {:?}", nonterm)?;
                let state = &states[*state_stack.last().unwrap()];

                let expected: BTreeSet<Term> = state.expected().into_iter().collect();
                if expected.is_empty() {
                    writeln!(f, "No expected token")?;
                } else {
                    write!(f, "Expected one of: ")?;
                    let len = expected.len();
                    for (id, term) in expected.into_iter().enumerate() {
                        write!(f, "{:?}", term)?;
                        if id < len - 1 {
                            write!(f, ", ")?;
                        } else {
                            writeln!(f)?;
                        }
                    }
                }

                let backtrace = Self::backtrace(rules, states, state_stack);
                for (id, ruleset) in backtrace.iter().enumerate() {
                    if id == 0 {
                        writeln!(f, "{:-^80}", "Backtracing state")?;
                    } else {
                        writeln!(f, "{:-^80}", "Prev state")?;
                    }
                    for rule in ruleset.iter() {
                        let shifted = ShiftedRule {
                            rule: rules[rule.rule].clone(),
                            shifted: rule.shifted,
                        };
                        writeln!(f, "{:?}", shifted)?;
                    }
                }
            }
            ParseError::InvalidTerminal(term, rules, states, state_stack) => {
                writeln!(f, "Invalid Terminal: {:?}", term,)?;
                let state = &states[*state_stack.last().unwrap()];

                let expected: BTreeSet<Term> = state.expected().into_iter().collect();
                if expected.is_empty() {
                    writeln!(f, "No expected token")?;
                } else {
                    write!(f, "Expected one of: ")?;
                    let len = expected.len();
                    for (id, term) in expected.into_iter().enumerate() {
                        write!(f, "{:?}", term)?;
                        if id < len - 1 {
                            write!(f, ", ")?;
                        } else {
                            writeln!(f)?;
                        }
                    }
                }

                let backtrace = Self::backtrace(rules, states, state_stack);
                for (id, ruleset) in backtrace.iter().enumerate() {
                    if id == 0 {
                        writeln!(f, "{:-^80}", "Backtracing state")?;
                    } else {
                        writeln!(f, "{:-^80}", "Prev state")?;
                    }
                    for rule in ruleset.iter() {
                        let shifted = ShiftedRule {
                            rule: rules[rule.rule].clone(),
                            shifted: rule.shifted,
                        };
                        writeln!(f, "{:?}", shifted)?;
                    }
                }
            }
            ParseError::Callback(message) => {
                write!(f, "Callback Error: {:?}", message)?;
            }
            ParseError::ReduceAction(message) => {
                write!(f, "{:?}", message)?;
            }
        }
        Ok(())
    }
}
