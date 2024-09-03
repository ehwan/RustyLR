use std::hash::Hash;

use super::ParseError;
use super::Parser;
use super::Stack;

#[cfg(feature = "tree")]
use crate::TreeList;

/// Context for LR parser.
/// This hadles actual data stack for reduce action.
pub struct Context<S: Stack> {
    /// state stack
    pub state_stack: Vec<usize>,

    /// The top of state stack before `feed()` called.
    /// This is used for `expected()` method.
    pub(crate) last_state: usize,

    pub(crate) data_stack: S,

    #[cfg(feature = "tree")]
    pub(crate) tree_stack: TreeList<S::Term, S::NonTerm>,
}

impl<S: Stack> Context<S> {
    /// Create a new context.
    /// `state_stack` is initialized with 0 (root state).
    pub fn new() -> Self
    where
        S: Stack,
    {
        Context {
            state_stack: vec![0],
            last_state: 0,

            data_stack: S::new(),

            #[cfg(feature = "tree")]
            tree_stack: TreeList::new(),
        }
    }
    /// pop value from start rule
    #[inline]
    pub fn accept(&mut self) -> S::StartType
    where
        S: Stack,
    {
        self.data_stack.pop_start()
    }

    /// For debugging.
    /// Get `TreeList` that current context holds.
    #[cfg(feature = "tree")]
    pub fn to_tree_list(&self) -> TreeList<S::Term, S::NonTerm>
    where
        S::Term: Clone,
        S::NonTerm: Clone,
    {
        self.tree_stack.clone()
    }
    /// For debugging.
    /// Get `TreeList` that current context holds.
    #[cfg(feature = "tree")]
    pub fn into_tree_list(self) -> TreeList<S::Term, S::NonTerm> {
        self.tree_stack
    }

    /// This function should be called after `feed()` returns `Error`.
    /// Get expected tokens for last `feed()` call.
    pub fn expected<'a, P: Parser<Term = S::Term, NonTerm = S::NonTerm>>(
        &self,
        parser: &'a P,
    ) -> impl Iterator<Item = &'a S::Term>
    where
        S::Term: 'a,
        S::NonTerm: 'a,
    {
        parser.get_states()[self.last_state].expected()
    }

    /// Feed one terminal to parser, and update state stack.
    pub fn feed<P: Parser<Term = S::Term, NonTerm = S::NonTerm>>(
        &mut self,
        parser: &P,
        term: S::Term,
        userdata: &mut S::UserData,
    ) -> Result<(), ParseError<S::Term, S::NonTerm, S::ReduceActionError>>
    where
        S::Term: Hash + Eq + Clone,
        S::NonTerm: Hash + Eq + Clone,
    {
        super::feed(parser, self, term, userdata)
    }

    #[cfg(feature = "error")]
    /// Get backtrace information for current state.
    /// What current state is trying to parse, and where it comes from.
    pub fn backtrace<P: Parser<Term = S::Term, NonTerm = S::NonTerm>>(
        &self,
        parser: &P,
    ) -> crate::Backtrace<S::Term, S::NonTerm>
    where
        S::Term: Clone,
        S::NonTerm: Hash + Eq + Clone,
    {
        use crate::Backtrace;
        use crate::HashSet;
        use crate::ShiftedRule;
        use crate::ShiftedRuleRef;
        use crate::Token;
        use std::collections::BTreeSet;

        if self.state_stack.len() == 1 {
            let state0 = &parser.get_states()[0];
            let mut rules = Vec::with_capacity(state0.ruleset.len());
            for rule in state0.ruleset.iter() {
                rules.push(ShiftedRule {
                    rule: parser.get_rules()[rule.rule].clone(),
                    shifted: rule.shifted,
                });
            }

            return Backtrace {
                traces: vec![rules],
            };
        }

        let mut traces = Vec::new();
        let mut current_rules: BTreeSet<_> = parser.get_states()[*self.state_stack.last().unwrap()]
            .ruleset
            .iter()
            .filter(|rule| rule.shifted > 0)
            .copied()
            .collect();
        let mut next_rules = BTreeSet::new();
        traces.push(current_rules.clone());
        let mut zero_shifted_rules: HashSet<S::NonTerm> = Default::default();

        for state_idx in self.state_stack.iter().rev().skip(1).copied() {
            zero_shifted_rules.clear();
            next_rules.clear();
            for rule in current_rules.iter() {
                if rule.shifted > 0 {
                    next_rules.insert(ShiftedRuleRef {
                        rule: rule.rule,
                        shifted: rule.shifted - 1,
                    });
                    if rule.shifted == 1 {
                        zero_shifted_rules.insert(parser.get_rules()[rule.rule].name.clone());
                    }
                }
            }
            std::mem::swap(&mut current_rules, &mut next_rules);
            if zero_shifted_rules.is_empty() {
                continue;
            }

            loop {
                let len0 = current_rules.len();
                for rule in parser.get_states()[state_idx].ruleset.iter() {
                    let prod_rule = &parser.get_rules()[rule.rule];
                    if let Some(Token::NonTerm(nonterm)) = prod_rule.rule.get(rule.shifted) {
                        if zero_shifted_rules.contains(nonterm) {
                            current_rules.insert(*rule);
                            if rule.shifted == 0 {
                                zero_shifted_rules.insert(prod_rule.name.clone());
                            }
                        }
                    }
                }
                if len0 == current_rules.len() {
                    break;
                }
            }

            traces.push(current_rules.clone());
        }

        Backtrace {
            traces: traces
                .into_iter()
                .map(|rules| {
                    rules
                        .into_iter()
                        .map(|rule| ShiftedRule {
                            rule: parser.get_rules()[rule.rule].clone(),
                            shifted: rule.shifted,
                        })
                        .collect()
                })
                .collect(),
        }
    }
}

impl<S: Stack> Default for Context<S>
where
    S: Stack,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<S: Stack> Clone for Context<S>
where
    S: Clone,
    S::Term: Clone,
    S::NonTerm: Clone,
{
    fn clone(&self) -> Self {
        Context {
            state_stack: self.state_stack.clone(),
            last_state: self.last_state,
            data_stack: self.data_stack.clone(),

            #[cfg(feature = "tree")]
            tree_stack: self.tree_stack.clone(),
        }
    }
}

#[cfg(feature = "tree")]
impl<S: Stack> std::fmt::Display for Context<S>
where
    S::Term: std::fmt::Display + Clone,
    S::NonTerm: std::fmt::Display + Clone,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_tree_list())
    }
}
#[cfg(feature = "tree")]
impl<S: Stack> std::fmt::Debug for Context<S>
where
    S::Term: std::fmt::Debug + Clone,
    S::NonTerm: std::fmt::Debug + Clone,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.to_tree_list())
    }
}

#[cfg(feature = "tree")]
impl<S: Stack> std::ops::Deref for Context<S> {
    type Target = TreeList<S::Term, S::NonTerm>;
    fn deref(&self) -> &Self::Target {
        &self.tree_stack
    }
}

#[cfg(feature = "tree")]
impl<S: Stack> std::ops::DerefMut for Context<S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.tree_stack
    }
}
