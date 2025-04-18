use crate::rule::LookaheadRuleRefSet;
use crate::ShiftedRuleRef;
use crate::Token;

use std::collections::BTreeMap;
use std::collections::BTreeSet;

/// state in DFA building
#[derive(Debug, Clone)]
pub struct State<Term, NonTerm> {
    pub shift_goto_map_term: BTreeMap<Term, usize>,
    pub shift_goto_map_nonterm: BTreeMap<NonTerm, usize>,
    pub reduce_map: BTreeMap<Term, BTreeSet<usize>>,
    pub ruleset: LookaheadRuleRefSet<Term>,

    /// what token is used to reach this state
    pub token: Option<Token<Term, NonTerm>>,
    /// shortest sequence of tokens to reach this state
    pub shortest_path: Vec<(Token<Term, NonTerm>, usize)>,
}
impl<Term, NonTerm> State<Term, NonTerm> {
    pub fn new(
        token: Option<Token<Term, NonTerm>>,
        path: Vec<(Token<Term, NonTerm>, usize)>,
    ) -> Self {
        State {
            shift_goto_map_term: Default::default(),
            shift_goto_map_nonterm: Default::default(),
            reduce_map: Default::default(),
            ruleset: LookaheadRuleRefSet::new(),
            token,
            shortest_path: path,
        }
    }

    /// shift -= 1 for all rules in the ruleset
    pub fn unshifted_ruleset(&self) -> impl Iterator<Item = ShiftedRuleRef> + '_ {
        self.ruleset
            .rules
            .iter()
            .filter(|rule| rule.0.shifted > 0)
            .map(|(rule, _)| {
                let mut rule = *rule;
                rule.shifted -= 1;
                rule
            })
    }

    /// Map terminal and non-terminal symbols to another type.
    /// This is useful when exporting & importing rules.
    pub fn map<NewTerm: Ord, NewNonTerm: Ord>(
        self,
        term_map: impl Fn(Term) -> NewTerm,
        nonterm_map: impl Fn(NonTerm) -> NewNonTerm,
    ) -> State<NewTerm, NewNonTerm> {
        State {
            shift_goto_map_term: self
                .shift_goto_map_term
                .into_iter()
                .map(|(term, state)| (term_map(term), state))
                .collect(),
            shift_goto_map_nonterm: self
                .shift_goto_map_nonterm
                .into_iter()
                .map(|(nonterm, state)| (nonterm_map(nonterm), state))
                .collect(),
            reduce_map: self
                .reduce_map
                .into_iter()
                .map(|(term, rule)| (term_map(term), rule))
                .collect(),
            ruleset: self.ruleset.map(&term_map),

            token: self.token.map(|token| match token {
                Token::Term(term) => Token::Term(term_map(term)),
                Token::NonTerm(nonterm) => Token::NonTerm(nonterm_map(nonterm)),
            }),
            shortest_path: self
                .shortest_path
                .into_iter()
                .map(|(token, state)| match token {
                    Token::Term(term) => (Token::Term(term_map(term)), state),
                    Token::NonTerm(nonterm) => (Token::NonTerm(nonterm_map(nonterm)), state),
                })
                .collect(),
        }
    }

    pub fn conflict_rr(&self) -> impl Iterator<Item = (&BTreeSet<usize>, Vec<&Term>)> {
        let mut reversed_map: BTreeMap<_, Vec<_>> = BTreeMap::new();
        for (term, rules) in self.reduce_map.iter() {
            if rules.len() > 1 {
                reversed_map.entry(rules).or_default().push(term);
            }
        }
        reversed_map.into_iter()
    }
    pub fn conflict_sr<'a>(
        &'a self,
        idx2state: impl Fn(usize) -> &'a State<Term, NonTerm> + 'a,
    ) -> impl Iterator<Item = (&'a Term, &'a BTreeSet<usize>, Vec<ShiftedRuleRef>)> + 'a
    where
        Term: Ord,
    {
        self.shift_goto_map_term
            .iter()
            .filter_map(move |(term, &shift_state)| {
                self.reduce_map.get(term).map(|reduces| {
                    let next_rules: Vec<_> = idx2state(shift_state).unshifted_ruleset().collect();
                    (term, reduces, next_rules)
                })
            })
    }
}

impl<Term, NonTerm> Default for State<Term, NonTerm> {
    fn default() -> Self {
        Self::new(None, Vec::new())
    }
}
