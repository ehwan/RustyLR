use std::ops::Deref;
use std::ops::DerefMut;
use std::rc::Rc;

use super::State;

use crate::nonterminal::TokenData;

/// Iterator for traverse node to root.
/// Note that root node is not included in this iterator.
pub struct NodeRefIterator<'a, Data: TokenData> {
    node: Option<&'a Node<Data>>,
}

impl<'a, Data: TokenData> Iterator for NodeRefIterator<'a, Data> {
    type Item = &'a Node<Data>;

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.node?;
        let parent: Option<Self::Item> = node.parent.as_deref();
        if parent.is_some() {
            self.node = parent;
            Some(node)
        } else {
            // if parent is None, it means this node is root node.
            None
        }
    }
}

/// Node represents single token in the parse tree.
/// To handle multiple paths in the GLR parsing,
/// this constructs Linked List of nodes, where parent node is the previous token in the parse tree.
#[derive(Clone)]
pub struct Node<Data: TokenData> {
    /// parent node
    pub parent: Option<Rc<Node<Data>>>,
    /// index of state in parser
    pub state: usize,

    /// token data for this node
    pub data: Option<(Data, Data::Location)>,
    /// tree representation of this node
    #[cfg(feature = "tree")]
    pub tree: Option<crate::tree::Tree<Data::Term, Data::NonTerm>>,
}

impl<Data: TokenData> Node<Data> {
    /// generate new root node
    pub fn new_root() -> Self {
        Self {
            parent: None,
            state: 0,

            data: None,
            #[cfg(feature = "tree")]
            tree: None,
        }
    }

    /// Get token tree for this node.
    /// This function should not be called from root node.
    #[cfg(feature = "tree")]
    pub fn to_tree(&self) -> &crate::tree::Tree<Data::Term, Data::NonTerm> {
        debug_assert!(self.parent.is_some());
        self.tree.as_ref().unwrap()
    }
    /// Get token tree for this node.
    /// This function should not be called from root node.
    #[cfg(feature = "tree")]
    pub fn into_tree(self) -> crate::tree::Tree<Data::Term, Data::NonTerm> {
        debug_assert!(self.parent.is_some());
        self.tree.unwrap()
    }

    /// Get list of token trees from root to this node.
    /// Unlike other [`Iterator`] functions, the order of returned trees is from root to this node.
    /// This function is not *lazy*.
    /// If you don't want expensive operation (`clone()` or `reverse()`), use `iter()` and `to_tree()` instead.
    #[cfg(feature = "tree")]
    pub fn to_tree_list(&self) -> crate::tree::TreeList<Data::Term, Data::NonTerm>
    where
        Data::Term: Clone,
        Data::NonTerm: Clone,
    {
        let mut trees: Vec<crate::tree::Tree<Data::Term, Data::NonTerm>> =
            self.iter().map(|node| node.to_tree().clone()).collect();
        trees.reverse();
        crate::tree::TreeList { trees }
    }

    /// Get data for this node.
    /// This function should not be called from root node.
    pub fn to_data(&self) -> &Data {
        debug_assert!(self.parent.is_some());
        &self.data.as_ref().unwrap().0
    }
    /// Get data for this node.
    /// This function should not be called from root node.
    pub fn into_data(self) -> Data {
        debug_assert!(self.parent.is_some());
        self.data.unwrap().0
    }
    /// Get list of data from root to this node.
    /// Unlike other [`Iterator`] functions, the order of returned data is from root to this node.
    /// This function is not *lazy*.
    /// If you don't want expensive operation (`clone()` or `reverse()`), use `iter()` and `to_tree()` instead.
    pub fn to_data_list(&self) -> Vec<Data>
    where
        Data: Clone,
    {
        let mut data: Vec<Data> = self.iter().map(|node| node.to_data().clone()).collect();
        data.reverse();
        data
    }

    /// Get iterator for traversing from this node to root.
    /// Note that root node is not included in this iterator.
    pub fn iter(&self) -> NodeRefIterator<'_, Data> {
        NodeRefIterator { node: Some(self) }
    }

    /// Get set of `%trace` non-terminal symbols that current context is trying to parse.
    ///
    /// The order of the returned set does not mean anything.
    /// If the current context is attempting to recognize following grammar:
    ///
    /// Chunk -> Statement -> IfStatement -> ReturnStatement -> ...
    ///
    /// Then the returned set will be:
    /// [`Chunk`, `Statement`, `IfStatement`, `ReturnStatement`]
    pub fn trace<P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
    ) -> crate::hash::HashSet<Data::NonTerm>
    where
        Data::NonTerm: Copy + Eq + std::hash::Hash + crate::nonterminal::NonTerminal,
    {
        use crate::hash::HashSet;
        use crate::nonterminal::NonTerminal;
        use crate::token::Token;
        use std::collections::BTreeSet;

        let rules = parser.get_rules();
        let states = parser.get_states();

        let mut zero_shifted_rules = BTreeSet::new();
        let mut non_zero_shifted_rules = BTreeSet::new();
        {
            let last_state = &states[self.state];
            for rule in last_state.get_rules().iter() {
                if rule.shifted == 0 {
                    zero_shifted_rules.insert(rule.rule);
                } else {
                    non_zero_shifted_rules.insert((rule.rule, rule.shifted));
                }
            }
        }

        let mut node = self;
        let mut ret: HashSet<Data::NonTerm> = Default::default();
        loop {
            let state = &states[node.state];
            let ruleset = &state.get_rules();

            // insert new shifted rule that brings zero_shifted rules in this state
            let mut new_zero_shifted_rules = Vec::new();
            loop {
                let zero_len0 = zero_shifted_rules.len();
                let nonzero_len0 = non_zero_shifted_rules.len();

                new_zero_shifted_rules.clear();

                for &zero_rule in zero_shifted_rules.iter() {
                    let nonterm0 = rules[zero_rule].name;
                    for rule in ruleset.iter() {
                        let prod_rule = &rules[rule.rule];
                        if let Some(Token::NonTerm(nonterm)) = prod_rule.rule.get(rule.shifted) {
                            if &nonterm0 == nonterm {
                                if rule.shifted == 0 {
                                    new_zero_shifted_rules.push(rule.rule);
                                } else {
                                    // insert new shifted rule
                                    non_zero_shifted_rules.insert((rule.rule, rule.shifted));
                                }
                            }
                        }
                    }
                }
                zero_shifted_rules.extend(new_zero_shifted_rules.iter().copied());

                if zero_len0 == zero_shifted_rules.len()
                    && nonzero_len0 == non_zero_shifted_rules.len()
                {
                    break;
                }
            }

            // push nonterminal of zero-shifted-rules into backtrace vector
            for &zero_rule in zero_shifted_rules.iter() {
                let nonterm0 = rules[zero_rule].name;
                // do not insert auto-generated nonterminals
                // since user don't need to know about them
                if nonterm0.is_trace() {
                    ret.insert(nonterm0);
                }
            }

            // shift to next state
            zero_shifted_rules.clear();
            let mut new_non_zero_shifted_rules = BTreeSet::new();
            for (rule, shifted) in non_zero_shifted_rules.into_iter() {
                if shifted == 1 {
                    zero_shifted_rules.insert(rule);
                } else {
                    new_non_zero_shifted_rules.insert((rule, shifted - 1));
                }
            }
            non_zero_shifted_rules = new_non_zero_shifted_rules;

            if let Some(parent) = node.parent.as_ref() {
                node = parent;
            } else {
                break;
            }
        }

        ret
    }

    /// Get backtrace information for current state.
    /// What current state is trying to parse, and where it comes from.
    pub fn backtrace<P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
    ) -> crate::Backtrace<&'static str, Data::NonTerm>
    where
        Data::Term: Clone,
        Data::NonTerm: std::hash::Hash + Eq + Clone,
    {
        use crate::hash::HashSet;
        use crate::rule::ShiftedRule;
        use crate::rule::ShiftedRuleRef;
        use crate::Backtrace;
        use crate::Token;
        use std::collections::BTreeSet;

        if self.parent.is_none() {
            let state0 = &parser.get_states()[0];
            let mut rules = Vec::with_capacity(state0.get_rules().len());
            for rule in state0.get_rules().iter() {
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
        let mut current_rules: BTreeSet<_> = parser.get_states()[self.state]
            .get_rules()
            .iter()
            // .filter(|rule| rule.shifted > 0)
            .copied()
            .collect();
        let mut next_rules = BTreeSet::new();
        traces.push(current_rules.clone());
        let mut zero_shifted_rules: HashSet<Data::NonTerm> = Default::default();

        for node in self.iter().skip(1) {
            let state_idx = node.state;
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
                for rule in parser.get_states()[state_idx].get_rules().iter() {
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

        // node.iter() does not include root node.
        // so explicitly add root node.
        {
            let state_idx = 0;
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
            if !zero_shifted_rules.is_empty() {
                loop {
                    let len0 = current_rules.len();
                    for rule in parser.get_states()[state_idx].get_rules().iter() {
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

    /// Simulate parser and get next expected (terminals, non-terminals) for current context.
    pub fn expected_token<'a, P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        self: &Rc<Self>,
        parser: &'a P,
        terms: &mut std::collections::BTreeSet<usize>,
        nonterms: &mut std::collections::BTreeSet<Data::NonTerm>,
    ) where
        Data::NonTerm: Ord + Copy + std::hash::Hash,
    {
        let s = self.state;
        let s = parser.get_states().get(s).expect("state must exist");

        terms.extend(s.expected_shift_term());
        nonterms.extend(s.expected_shift_nonterm());

        let mut reduce_nonterms = std::collections::BTreeSet::new();
        for reduce_rule in s.expected_reduce_rule() {
            let prod_rule = &parser.get_rules()[reduce_rule];
            reduce_nonterms.insert((prod_rule.name, prod_rule.rule.len()));
        }
        for &(nonterm, len) in reduce_nonterms.iter() {
            let mut node = self;
            for _ in 0..len {
                node = node.parent.as_ref().unwrap();
            }
            let last_state = node.state;
            if let Some(next_state) = parser.get_states()[last_state].shift_goto_nonterm(&nonterm) {
                let next_node = Self {
                    parent: Some(Rc::clone(node)),
                    state: next_state,
                    data: None,
                    #[cfg(feature = "tree")]
                    tree: None,
                };
                let next_node = Rc::new(next_node);
                Self::expected_token(&next_node, parser, terms, nonterms);
            }
        }
    }

    pub fn panic_mode<P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        mut node: &Rc<Node<Data>>,
        parser: &P,
    ) -> Option<Self>
    where
        Data::NonTerm: std::hash::Hash + Eq,
    {
        let node0 = node;
        let error_nonterm = parser.get_error_nonterm()?;
        let mut popped_count = 0;
        loop {
            let last_state = &parser.get_states()[node.state];
            if let Some(error_state) = last_state.shift_goto_nonterm(&error_nonterm) {
                let new_location = super::merge_locations(node0, popped_count);
                // pop all states and data above this state
                let child_node = Node {
                    data: Some((Data::new_error_nonterm(), new_location)),
                    parent: Some(Rc::clone(node)),
                    state: error_state,
                    #[cfg(feature = "tree")]
                    tree: Some(crate::tree::Tree::new_nonterminal(
                        error_nonterm,
                        Vec::new(),
                    )),
                };

                return Some(child_node);
            }

            if let Some(parent) = node.parent.as_ref() {
                popped_count += 1;
                node = parent;
            } else {
                break;
            }
        }
        None
    }

    pub fn can_panic_mode<P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        mut node: &Rc<Node<Data>>,
        parser: &P,
    ) -> bool
    where
        Data::NonTerm: std::hash::Hash + Eq,
    {
        let Some(error_nonterm) = parser.get_error_nonterm() else {
            return false;
        };
        loop {
            let last_state = &parser.get_states()[node.state];
            if last_state.shift_goto_nonterm(&error_nonterm).is_some() {
                return true;
            }

            if let Some(parent) = node.parent.as_ref() {
                node = parent;
            } else {
                break;
            }
        }
        false
    }
}

#[cfg(feature = "tree")]
impl<Data: TokenData> From<Node<Data>> for crate::tree::Tree<Data::Term, Data::NonTerm> {
    fn from(node: Node<Data>) -> Self {
        node.into_tree()
    }
}

#[cfg(feature = "tree")]
impl<Data: TokenData> std::fmt::Display for Node<Data>
where
    Data::Term: std::fmt::Display,
    Data::NonTerm: std::fmt::Display + crate::nonterminal::NonTerminal,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.parent.is_none() {
            write!(f, "Root")
        } else {
            write!(f, "{}", self.to_tree())
        }
    }
}
#[cfg(feature = "tree")]
impl<Data: TokenData> std::fmt::Debug for Node<Data>
where
    Data::Term: std::fmt::Debug,
    Data::NonTerm: std::fmt::Debug + crate::nonterminal::NonTerminal,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.parent.is_none() {
            write!(f, "Root")
        } else {
            write!(f, "{:?}", self.to_tree())
        }
    }
}

impl<Data: TokenData> Deref for Node<Data> {
    type Target = Data;

    fn deref(&self) -> &Self::Target {
        self.to_data()
    }
}
impl<Data: TokenData> DerefMut for Node<Data> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data.as_mut().unwrap().0
    }
}
