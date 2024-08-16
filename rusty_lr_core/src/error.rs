use std::collections::BTreeSet;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;

use crate::rule::ShiftedRuleRef2;
use crate::state::State;
use crate::ProductionRule;
use crate::ShiftedRuleRef;
use crate::Token;

pub trait GetContext<Term, NonTerm> {
    fn get_state_stack(&self) -> &[usize];
}
pub trait GetParser<Term, NonTerm> {
    fn get_rules(&self) -> &[ProductionRule<Term, NonTerm>];
    fn get_states(&self) -> &[State<Term, NonTerm>];
}

/// Error type for feed(), when invalid terminal is feeded
#[derive(Debug)]
pub struct InvalidTerminalError<Term> {
    /// invalid terminal feeded
    pub term: Term,
    /// expected terminals
    pub expected: Vec<Term>,
}

impl<Term: Display> Display for InvalidTerminalError<Term> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
    }
}

impl<Term: Display + Debug> std::error::Error for InvalidTerminalError<Term> {
    fn cause(&self) -> Option<&dyn std::error::Error> {
        None
    }
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
    fn description(&self) -> &str {
        "Invalid terminal feeded"
    }
}

impl<Term> InvalidTerminalError<Term> {
    /// Generate backtrace information.
    /// Returned `Vec` holds a list of ruleset, each ruleset is what current state was trying to parse.
    /// 0'th index is the latest, that is, the last element of `Vec` will hold the initial state's ruleset
    pub fn backtrace<NonTerm>(
        parser: &impl GetParser<Term, NonTerm>,
        context: &impl GetContext<Term, NonTerm>,
    ) -> Vec<BTreeSet<ShiftedRuleRef>>
    where
        NonTerm: PartialEq,
    {
        let state_stack = context.get_state_stack();
        let rules = parser.get_rules();
        let states = parser.get_states();
        let mut backtrace = Vec::with_capacity(state_stack.len());

        let state = &states[*state_stack.last().unwrap()];
        let mut cur_rules = state.ruleset.clone();
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
                        for rule in states[*prev_state].ruleset.iter() {
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

    /// Generate brief error message.
    pub fn message(&self) -> String
    where
        Term: Display,
    {
        let mut message = format!("Invalid Terminal: {}. ", self.term);

        if self.expected.is_empty() {
            message.push_str("No expected token");
        } else {
            let expected: BTreeSet<String> =
                self.expected.iter().map(|t| format!("{}", t)).collect();
            message.push_str("Expected one of: ");
            let len = expected.len();
            for (id, term) in expected.into_iter().enumerate() {
                message.push_str(&term);
                if id < len - 1 {
                    message.push_str(", ");
                }
            }
        }
        message
    }

    /// Generate long, detailed error message.
    pub fn long_message<NonTerm>(
        &self,
        parser: &impl GetParser<Term, NonTerm>,
        context: &impl GetContext<Term, NonTerm>,
    ) -> String
    where
        Term: Display + Hash + Eq,
        NonTerm: Display + PartialEq,
    {
        let rules = parser.get_rules();
        let mut message = String::new();
        let backtrace = Self::backtrace(parser, context);
        for (id, ruleset) in backtrace.iter().enumerate() {
            if id == 0 {
                message.push_str(&format!("{:-^80}\n", "Backtracing state"));
            } else {
                message.push_str(&format!("{:-^80}\n", "Prev state"));
            }
            for rule in ruleset.iter() {
                let shifted = ShiftedRuleRef2 {
                    rule: &rules[rule.rule],
                    shifted: rule.shifted,
                };
                message.push_str(&format!("{}\n", shifted));
            }
        }
        format!("{}\n{}", &self.message(), message)
    }
}

/// Error type for feed()
#[derive(Debug)]
pub enum ParseError<Term, ReduceActionError> {
    /// Invalid terminal feeded
    InvalidTerminal(InvalidTerminalError<Term>),

    /// Error from reduce action
    ReduceAction(ReduceActionError),
}
impl<Term: Display + Hash + Eq, ReduceActionError: Display> Display
    for ParseError<Term, ReduceActionError>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::InvalidTerminal(err) => {
                write!(f, "{}", err)
            }
            ParseError::ReduceAction(err) => {
                write!(f, "{}", err)
            }
        }
    }
}
impl<Term: Display + Debug + Hash + Eq, ReduceActionError: std::error::Error> std::error::Error
    for ParseError<Term, ReduceActionError>
{
    fn cause(&self) -> Option<&dyn std::error::Error> {
        None
    }
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
    fn description(&self) -> &str {
        "Parse error"
    }
}

impl<Term, ReduceActionError> ParseError<Term, ReduceActionError> {
    /// Generate long, detailed error message.
    pub fn long_message<NonTerm>(
        &self,
        parser: &impl GetParser<Term, NonTerm>,
        context: &impl GetContext<Term, NonTerm>,
    ) -> String
    where
        Term: Display + Hash + Eq,
        ReduceActionError: Display,
        NonTerm: Display + PartialEq,
    {
        match self {
            ParseError::InvalidTerminal(err) => err.long_message(parser, context),
            ParseError::ReduceAction(err) => {
                format!("ReduceActionError: {}", err)
            }
        }
    }
}
