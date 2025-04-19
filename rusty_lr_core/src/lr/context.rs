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

    /// Get expected tokens for next `feed()` call.
    pub fn expected<'a, P: Parser<Term = S::Term, NonTerm = S::NonTerm>>(
        &self,
        parser: &'a P,
    ) -> impl Iterator<Item = &'a S::Term>
    where
        S::Term: 'a,
        S::NonTerm: 'a,
    {
        parser.get_states()[*self.state_stack.last().unwrap()].expected()
    }
    /// Get expected non-terminal tokens for next `feed()` call.
    pub fn expected_nonterm<'a, P: Parser<Term = S::Term, NonTerm = S::NonTerm>>(
        &self,
        parser: &'a P,
    ) -> impl Iterator<Item = &'a S::NonTerm>
    where
        S::Term: 'a,
        S::NonTerm: 'a,
    {
        parser.get_states()[*self.state_stack.last().unwrap()].expected_nonterm()
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

    /// Check if `term` can be feeded to current state.
    /// This does not check for reduce action error.
    ///
    /// This does not change the state of the context.
    pub fn can_feed<P: Parser<Term = S::Term, NonTerm = S::NonTerm>>(
        &self,
        parser: &P,
        term: &S::Term,
    ) -> bool
    where
        S::Term: Hash + Eq,
        S::NonTerm: Hash + Eq,
    {
        if parser.get_states()[*self.state_stack.last().unwrap()]
            .shift_goto_term(term)
            .is_some()
        {
            return true;
        }

        let mut state_stack = self.state_stack.clone();
        while let Some(reduce_rule) = parser.get_states()[*state_stack.last().unwrap()].reduce(term)
        {
            let rule = &parser.get_rules()[reduce_rule];
            let new_len = state_stack.len() - rule.rule.len();
            state_stack.truncate(new_len);

            if let Some(next_state) =
                parser.get_states()[*state_stack.last().unwrap()].shift_goto_nonterm(&rule.name)
            {
                state_stack.push(next_state);
            } else {
                return false;
            }
        }

        parser.get_states()[*state_stack.last().unwrap()]
            .shift_goto_term(term)
            .is_some()
    }

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
