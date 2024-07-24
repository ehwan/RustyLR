use std::fmt::Debug;
use std::fmt::Display;

use super::context::Context;
use super::context::ContextStr;
use super::parser::Parser;
use crate::rule::ShiftedRule;

pub enum ParseError<'a, Term, NonTerm> {
    // #[error("Invalid Non-Terminal: {0}")]
    InvalidNonTerminal(
        NonTerm,
        &'a Parser<Term, NonTerm>,
        Context<'a, Term, NonTerm>,
    ),

    // #[error("Invalid Terminal: {0}")]
    InvalidTerminal(
        &'a Term,
        &'a Parser<Term, NonTerm>,
        Context<'a, Term, NonTerm>,
    ),

    // #[error("State Stack is empty; This should not be happened if DFA is generated correctly")]
    StateStackEmpty,

    // #[error("State Stack is not enough for reduce; This should not be happened if DFA is generated correctly")]
    StateStackNotEnough,

    // #[error("Invalid State: Goto {0}; This should not be happened if DFA is generated correctly")]
    InvalidState(usize),
}

impl<'a, Term: Display + Clone, NonTerm: Display + Clone> Display
    for ParseError<'a, Term, NonTerm>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::InvalidNonTerminal(nonterm, parser, context) => {
                writeln!(
                    f,
                    "Invalid Non-Terminal: {} at state {}",
                    nonterm,
                    context.state()
                )?;
                let state = context.state();
                let state = &parser.states[state];

                writeln!(f, "Terminals for reduce:")?;
                for (reduce_token, ruleid) in state.reduce_map.iter() {
                    writeln!(f, "{}: {}", reduce_token, parser.rules[*ruleid])?;
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
                for state_id in context.state_stack.iter().rev() {
                    let state = &parser.states[*state_id];
                    for (rule, lookaheads) in state.ruleset.rules.iter() {
                        let shifted = ShiftedRule {
                            rule: parser.rules[rule.rule].clone(),
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
            ParseError::InvalidTerminal(term, parser, context) => {
                writeln!(f, "Invalid Terminal: {} at state {}", term, context.state())?;
                let state = context.state();
                let state = &parser.states[state];

                writeln!(f, "Terminals for reduce:")?;
                for (reduce_token, ruleid) in state.reduce_map.iter() {
                    writeln!(f, "{}: {}", reduce_token, parser.rules[*ruleid])?;
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
                for state_id in context.state_stack.iter().rev() {
                    let state = &parser.states[*state_id];
                    for (rule, lookaheads) in state.ruleset.rules.iter() {
                        let shifted = ShiftedRule {
                            rule: parser.rules[rule.rule].clone(),
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
            ParseError::StateStackEmpty => {
                write!(
                f,
                "State Stack is empty; This should not be happened if DFA is generated correctly"
            )?;
            }
            ParseError::StateStackNotEnough => {
                write!(
                f,
                "State Stack is not enough for reduce; This should not be happened if DFA is generated correctly"
            )?;
            }
            ParseError::InvalidState(state) => {
                write!(f, "Invalid State: Goto {}; This should not be happened if DFA is generated correctly", state)?;
            }
        }
        Ok(())
    }
}
impl<'a, Term: Debug + Clone, NonTerm: Debug + Clone> Debug for ParseError<'a, Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::InvalidNonTerminal(nonterm, parser, context) => {
                writeln!(
                    f,
                    "Invalid Non-Terminal: {:?} at state {}",
                    nonterm,
                    context.state()
                )?;
                let state = context.state();
                let state = &parser.states[state];

                writeln!(f, "Terminals for reduce:")?;
                for (reduce_token, ruleid) in state.reduce_map.iter() {
                    writeln!(f, "{:?}: {:?}", reduce_token, parser.rules[*ruleid])?;
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
                for state_id in context.state_stack.iter().rev() {
                    let state = &parser.states[*state_id];
                    for (rule, lookaheads) in state.ruleset.rules.iter() {
                        let shifted = ShiftedRule {
                            rule: parser.rules[rule.rule].clone(),
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
            ParseError::InvalidTerminal(term, parser, context) => {
                writeln!(
                    f,
                    "Invalid Terminal: {:?} at state {}",
                    term,
                    context.state()
                )?;
                let state = context.state();
                let state = &parser.states[state];

                writeln!(f, "Terminals for reduce:")?;
                for (reduce_token, ruleid) in state.reduce_map.iter() {
                    writeln!(f, "{:?}: {:?}", reduce_token, parser.rules[*ruleid])?;
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
                for state_id in context.state_stack.iter().rev() {
                    let state = &parser.states[*state_id];
                    for (rule, lookaheads) in state.ruleset.rules.iter() {
                        let shifted = ShiftedRule {
                            rule: parser.rules[rule.rule].clone(),
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
            ParseError::StateStackEmpty => {
                write!(
                f,
                "State Stack is empty; This should not be happened if DFA is generated correctly"
            )?;
            }
            ParseError::StateStackNotEnough => {
                write!(
                f,
                "State Stack is not enough for reduce; This should not be happened if DFA is generated correctly"
            )?;
            }
            ParseError::InvalidState(state) => {
                write!(f, "Invalid State: Goto {}; This should not be happened if DFA is generated correctly", state)?;
            }
        }
        Ok(())
    }
}

pub enum ParseErrorStr<'a, Term, NonTerm> {
    // #[error("Invalid Non-Terminal: {0}")]
    InvalidNonTerminal(
        NonTerm,
        &'a Parser<Term, NonTerm>,
        ContextStr<'a, Term, NonTerm>,
    ),

    // #[error("Invalid Terminal: {0}")]
    InvalidTerminal(
        char,
        &'a Parser<Term, NonTerm>,
        ContextStr<'a, Term, NonTerm>,
    ),

    // #[error("State Stack is empty; This should not be happened if DFA is generated correctly")]
    StateStackEmpty,

    // #[error("State Stack is not enough for reduce; This should not be happened if DFA is generated correctly")]
    StateStackNotEnough,

    // #[error("Invalid State: Goto {0}; This should not be happened if DFA is generated correctly")]
    InvalidState(usize),
}

impl<'a, Term: Display + Clone, NonTerm: Display + Clone> Display
    for ParseErrorStr<'a, Term, NonTerm>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseErrorStr::InvalidNonTerminal(nonterm, parser, context) => {
                writeln!(
                    f,
                    "Invalid Non-Terminal: {} at state {}",
                    nonterm,
                    context.state()
                )?;
                let state = context.state();
                let state = &parser.states[state];

                writeln!(f, "Terminals for reduce:")?;
                for (reduce_token, ruleid) in state.reduce_map.iter() {
                    writeln!(f, "{}: {}", reduce_token, parser.rules[*ruleid])?;
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
                for state_id in context.state_stack.iter().rev() {
                    let state = &parser.states[*state_id];
                    for (rule, lookaheads) in state.ruleset.rules.iter() {
                        let shifted = ShiftedRule {
                            rule: parser.rules[rule.rule].clone(),
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
            ParseErrorStr::InvalidTerminal(term, parser, context) => {
                writeln!(
                    f,
                    "Invalid Terminal: '{}' at state {}",
                    term,
                    context.state()
                )?;
                let state = context.state();
                let state = &parser.states[state];

                writeln!(f, "Terminals for reduce:")?;
                for (reduce_token, ruleid) in state.reduce_map.iter() {
                    writeln!(f, "{}: {}", reduce_token, parser.rules[*ruleid])?;
                }
                writeln!(f, "Terminals for shift:")?;
                for (shift_token, next_state) in state.shift_goto_map_term.iter() {
                    writeln!(f, "'{}': {}", shift_token, next_state)?;
                }
                writeln!(f, "NonTerminals for shift:")?;
                for (shift_token, next_state) in state.shift_goto_map_nonterm.iter() {
                    writeln!(f, "{}: {}", shift_token, next_state)?;
                }
                writeln!(f, "Backtrace states:")?;
                for state_id in context.state_stack.iter().rev() {
                    let state = &parser.states[*state_id];
                    for (rule, lookaheads) in state.ruleset.rules.iter() {
                        let shifted = ShiftedRule {
                            rule: parser.rules[rule.rule].clone(),
                            shifted: rule.shifted,
                        };
                        write!(f, "{} / ", shifted)?;
                        for lookahead in lookaheads.iter() {
                            write!(f, "'{}', ", lookahead)?;
                        }
                        writeln!(f)?;
                    }
                    writeln!(f, "{:-^40}", "Prev State")?;
                }
            }
            ParseErrorStr::StateStackEmpty => {
                write!(
                f,
                "State Stack is empty; This should not be happened if DFA is generated correctly"
            )?;
            }
            ParseErrorStr::StateStackNotEnough => {
                write!(
                f,
                "State Stack is not enough for reduce; This should not be happened if DFA is generated correctly"
            )?;
            }
            ParseErrorStr::InvalidState(state) => {
                write!(f, "Invalid State: Goto {}; This should not be happened if DFA is generated correctly", state)?;
            }
        }
        Ok(())
    }
}
impl<'a, Term: Debug + Clone, NonTerm: Debug + Clone> Debug for ParseErrorStr<'a, Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseErrorStr::InvalidNonTerminal(nonterm, parser, context) => {
                writeln!(
                    f,
                    "Invalid Non-Terminal: {:?} at state {}",
                    nonterm,
                    context.state()
                )?;
                let state = context.state();
                let state = &parser.states[state];

                writeln!(f, "Terminals for reduce:")?;
                for (reduce_token, ruleid) in state.reduce_map.iter() {
                    writeln!(f, "{:?}: {:?}", reduce_token, parser.rules[*ruleid])?;
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
                for state_id in context.state_stack.iter().rev() {
                    let state = &parser.states[*state_id];
                    for (rule, lookaheads) in state.ruleset.rules.iter() {
                        let shifted = ShiftedRule {
                            rule: parser.rules[rule.rule].clone(),
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
            ParseErrorStr::InvalidTerminal(term, parser, context) => {
                writeln!(
                    f,
                    "Invalid Terminal: {:?} at state {}",
                    term,
                    context.state()
                )?;
                let state = context.state();
                let state = &parser.states[state];

                writeln!(f, "Terminals for reduce:")?;
                for (reduce_token, ruleid) in state.reduce_map.iter() {
                    writeln!(f, "{:?}: {:?}", reduce_token, parser.rules[*ruleid])?;
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
                for state_id in context.state_stack.iter().rev() {
                    let state = &parser.states[*state_id];
                    for (rule, lookaheads) in state.ruleset.rules.iter() {
                        let shifted = ShiftedRule {
                            rule: parser.rules[rule.rule].clone(),
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
            ParseErrorStr::StateStackEmpty => {
                write!(
                f,
                "State Stack is empty; This should not be happened if DFA is generated correctly"
            )?;
            }
            ParseErrorStr::StateStackNotEnough => {
                write!(
                f,
                "State Stack is not enough for reduce; This should not be happened if DFA is generated correctly"
            )?;
            }
            ParseErrorStr::InvalidState(state) => {
                write!(f, "Invalid State: Goto {}; This should not be happened if DFA is generated correctly", state)?;
            }
        }
        Ok(())
    }
}
