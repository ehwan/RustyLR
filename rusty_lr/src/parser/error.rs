use std::fmt::Debug;
use std::fmt::Display;

use crate::rule::ShiftedRule;
use crate::state::State;
use crate::ProductionRule;

pub enum ParseError<'a, Term, NonTerm, CallbackError> {
    InvalidNonTerminal(
        &'a NonTerm,
        &'a [ProductionRule<Term, NonTerm>],
        &'a [State<Term, NonTerm>],
        Vec<usize>,
    ),

    InvalidTerminal(
        Term,
        &'a [ProductionRule<Term, NonTerm>],
        &'a [State<Term, NonTerm>],
        Vec<usize>,
    ),

    /// Error from callback trait
    Callback(CallbackError),

    /// Error from macro reduce action
    CustomMessage(String),
}

impl<'a, Term: Display + Clone, NonTerm: Display + Clone, CallbackError: Display> Display
    for ParseError<'a, Term, NonTerm, CallbackError>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::InvalidNonTerminal(nonterm, rules, states, state_stack) => {
                writeln!(
                    f,
                    "Invalid Non-Terminal: {} at state {}",
                    nonterm,
                    state_stack.last().unwrap()
                )?;
                let state = *state_stack.last().unwrap();
                let state = &states[state];

                writeln!(f, "Terminals for reduce:")?;
                for (reduce_token, ruleid) in state.reduce_map.iter() {
                    writeln!(f, "{}: {}", reduce_token, rules[*ruleid])?;
                }
                writeln!(f, "Terminals for shift:")?;
                for (shift_token, next_state) in state.shift_goto_map_term.iter() {
                    writeln!(f, "{}: {}", shift_token, next_state)?;
                }
                writeln!(f, "NonTerminals for shift:")?;
                for (shift_token, next_state) in state.shift_goto_map_nonterm.iter() {
                    writeln!(f, "{}: {}", shift_token, next_state)?;
                }
                writeln!(f, "Backtrace states:")?;
                for state_id in state_stack.iter().rev() {
                    let state = &states[*state_id];
                    for (rule, lookaheads) in state.ruleset.rules.iter() {
                        let shifted = ShiftedRule {
                            rule: rules[rule.rule].clone(),
                            shifted: rule.shifted,
                        };
                        write!(f, "{} / ", shifted)?;
                        for lookahead in lookaheads.iter() {
                            write!(f, "{}, ", lookahead)?;
                        }
                        writeln!(f)?;
                    }
                    writeln!(f, "{:-^40}", "Prev State")?;
                }
            }
            ParseError::InvalidTerminal(term, rules, states, state_stack) => {
                writeln!(
                    f,
                    "Invalid Terminal: {} at state {}",
                    term,
                    state_stack.last().unwrap()
                )?;
                let state = *state_stack.last().unwrap();
                let state = &states[state];

                writeln!(f, "Terminals for reduce:")?;
                for (reduce_token, ruleid) in state.reduce_map.iter() {
                    writeln!(f, "{}: {}", reduce_token, rules[*ruleid])?;
                }
                writeln!(f, "Terminals for shift:")?;
                for (shift_token, next_state) in state.shift_goto_map_term.iter() {
                    writeln!(f, "{}: {}", shift_token, next_state)?;
                }
                writeln!(f, "NonTerminals for shift:")?;
                for (shift_token, next_state) in state.shift_goto_map_nonterm.iter() {
                    writeln!(f, "{}: {}", shift_token, next_state)?;
                }
                writeln!(f, "Backtrace states:")?;
                for state_id in state_stack.iter().rev() {
                    let state = &states[*state_id];
                    for (rule, lookaheads) in state.ruleset.rules.iter() {
                        let shifted = ShiftedRule {
                            rule: rules[rule.rule].clone(),
                            shifted: rule.shifted,
                        };
                        write!(f, "{} / ", shifted)?;
                        for lookahead in lookaheads.iter() {
                            write!(f, "{}, ", lookahead)?;
                        }
                        writeln!(f)?;
                    }
                    writeln!(f, "{:-^40}", "Prev State")?;
                }
            }
            ParseError::Callback(message) => {
                write!(f, "Callback Error: {}", message)?;
            }
            ParseError::CustomMessage(message) => {
                write!(f, "{}", message)?;
            }
        }
        Ok(())
    }
}
impl<'a, Term: Debug + Clone, NonTerm: Debug + Clone, CallbackError: Debug> Debug
    for ParseError<'a, Term, NonTerm, CallbackError>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::InvalidNonTerminal(nonterm, rules, states, state_stack) => {
                writeln!(
                    f,
                    "Invalid Non-Terminal: {:?} at state {}",
                    nonterm,
                    state_stack.last().unwrap()
                )?;
                let state = *state_stack.last().unwrap();
                let state = &states[state];

                writeln!(f, "Terminals for reduce:")?;
                for (reduce_token, ruleid) in state.reduce_map.iter() {
                    writeln!(f, "{:?}: {:?}", reduce_token, rules[*ruleid])?;
                }
                writeln!(f, "Terminals for shift:")?;
                for (shift_token, next_state) in state.shift_goto_map_term.iter() {
                    writeln!(f, "{:?}: {}", shift_token, next_state)?;
                }
                writeln!(f, "NonTerminals for shift:")?;
                for (shift_token, next_state) in state.shift_goto_map_nonterm.iter() {
                    writeln!(f, "{:?}: {}", shift_token, next_state)?;
                }
                writeln!(f, "Backtrace states:")?;
                for state_id in state_stack.iter().rev() {
                    let state = &states[*state_id];
                    for (rule, lookaheads) in state.ruleset.rules.iter() {
                        let shifted = ShiftedRule {
                            rule: rules[rule.rule].clone(),
                            shifted: rule.shifted,
                        };
                        write!(f, "{:?} / ", shifted)?;
                        for lookahead in lookaheads.iter() {
                            write!(f, "{:?}, ", lookahead)?;
                        }
                        writeln!(f)?;
                    }
                    writeln!(f, "{:-^40}", "Prev State")?;
                }
            }
            ParseError::InvalidTerminal(term, rules, states, state_stack) => {
                writeln!(
                    f,
                    "Invalid Terminal: {:?} at state {}",
                    term,
                    state_stack.last().unwrap()
                )?;
                let state = *state_stack.last().unwrap();
                let state = &states[state];

                writeln!(f, "Terminals for reduce:")?;
                for (reduce_token, ruleid) in state.reduce_map.iter() {
                    writeln!(f, "{:?}: {:?}", reduce_token, rules[*ruleid])?;
                }
                writeln!(f, "Terminals for shift:")?;
                for (shift_token, next_state) in state.shift_goto_map_term.iter() {
                    writeln!(f, "{:?}: {}", shift_token, next_state)?;
                }
                writeln!(f, "NonTerminals for shift:")?;
                for (shift_token, next_state) in state.shift_goto_map_nonterm.iter() {
                    writeln!(f, "{:?}: {}", shift_token, next_state)?;
                }
                writeln!(f, "Backtrace states:")?;
                for state_id in state_stack.iter().rev() {
                    let state = &states[*state_id];
                    for (rule, lookaheads) in state.ruleset.rules.iter() {
                        let shifted = ShiftedRule {
                            rule: rules[rule.rule].clone(),
                            shifted: rule.shifted,
                        };
                        write!(f, "{:?} / ", shifted)?;
                        for lookahead in lookaheads.iter() {
                            write!(f, "{:?}, ", lookahead)?;
                        }
                        writeln!(f)?;
                    }
                    writeln!(f, "{:-^40}", "Prev State")?;
                }
            }
            ParseError::Callback(message) => {
                write!(f, "Callback Error: {:?}", message)?;
            }
            ParseError::CustomMessage(message) => {
                write!(f, "{}", message)?;
            }
        }
        Ok(())
    }
}
