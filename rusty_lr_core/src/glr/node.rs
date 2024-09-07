use std::ops::Deref;
use std::ops::DerefMut;
use std::rc::Rc;

#[cfg(feature = "tree")]
use crate::Tree;
#[cfg(feature = "tree")]
use crate::TreeList;

/// Iterator for traverse node to root.
/// Note that root node is not included in this iterator.
pub struct NodeRefIterator<'a, Data: NodeData> {
    node: Option<&'a Node<Data>>,
}

impl<'a, Data: NodeData> Iterator for NodeRefIterator<'a, Data> {
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

/// Trait for user-defined data in node.
pub trait NodeData: Sized {
    type Term;
    type NonTerm;
    type UserData;
    type ReduceActionError;

    type StartType;

    fn new_term(term: Self::Term) -> Self;
    fn new_nonterm(
        rule_index: usize,
        reduce_args: &mut Vec<Self>,
        shift: &mut bool,
        lookahead: &Self::Term,
        userdata: &mut Self::UserData,
    ) -> Result<Self, Self::ReduceActionError>;

    /// get data of start symbol
    fn into_start(self) -> Self::StartType;
}

/// Node represents single shift action in GLR parser.
#[derive(Clone)]
pub struct Node<Data: NodeData> {
    /// parent node
    pub parent: Option<Rc<Node<Data>>>,
    /// index of state in parser
    pub state: usize,

    /// actual data(RuleType) of this node
    pub data: Option<Data>,
    /// tree representation of this node
    #[cfg(feature = "tree")]
    pub tree: Option<Tree<Data::Term, Data::NonTerm>>,
}

impl<Data: NodeData> Node<Data> {
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
    pub fn to_tree(&self) -> &Tree<Data::Term, Data::NonTerm> {
        debug_assert!(self.parent.is_some());
        self.tree.as_ref().unwrap()
    }
    /// Get token tree for this node.
    /// This function should not be called from root node.
    #[cfg(feature = "tree")]
    pub fn into_tree(self) -> Tree<Data::Term, Data::NonTerm> {
        debug_assert!(self.parent.is_some());
        self.tree.unwrap()
    }

    /// Get list of token trees from root to this node.
    /// Unlike other [`Iterator`] functions, the order of returned trees is from root to this node.
    /// This function is not *lazy*.
    /// If you don't want expensive operation (`clone()` or `reverse()`), use `iter()` and `to_tree()` instead.
    #[cfg(feature = "tree")]
    pub fn to_tree_list(&self) -> TreeList<Data::Term, Data::NonTerm>
    where
        Data::Term: Clone,
        Data::NonTerm: Clone,
    {
        let mut trees: Vec<Tree<Data::Term, Data::NonTerm>> =
            self.iter().map(|node| node.to_tree().clone()).collect();
        trees.reverse();
        TreeList { trees }
    }

    /// Get data for this node.
    /// This function should not be called from root node.
    pub fn to_data(&self) -> &Data {
        debug_assert!(self.parent.is_some());
        self.data.as_ref().unwrap()
    }
    /// Get data for this node.
    /// This function should not be called from root node.
    pub fn into_data(self) -> Data {
        debug_assert!(self.parent.is_some());
        self.data.unwrap()
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

    /// Get backtrace information for current state.
    /// What current state is trying to parse, and where it comes from.
    #[cfg(feature = "error")]
    pub fn backtrace<P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
    ) -> crate::Backtrace<Data::Term, Data::NonTerm>
    where
        Data::Term: Clone,
        Data::NonTerm: std::hash::Hash + Eq + Clone,
    {
        use crate::Backtrace;
        use crate::HashSet;
        use crate::ShiftedRule;
        use crate::ShiftedRuleRef;
        use crate::Token;
        use std::collections::BTreeSet;

        if self.parent.is_none() {
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
        let mut current_rules: BTreeSet<_> = parser.get_states()[self.state]
            .ruleset
            .iter()
            .filter(|rule| rule.shifted > 0)
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

    /// get expected terminals for current state.
    pub fn expected<'a, P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &'a P,
    ) -> impl Iterator<Item = &'a Data::Term>
    where
        P::Term: 'a,
        P::NonTerm: 'a,
    {
        parser.get_states()[self.state].expected()
    }
    /// get expected non-terminals for current state.
    pub fn expected_nonterm<'a, P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &'a P,
    ) -> impl Iterator<Item = &'a Data::NonTerm>
    where
        P::Term: 'a,
        P::NonTerm: 'a,
    {
        parser.get_states()[self.state].expected_nonterm()
    }
}

#[cfg(feature = "tree")]
impl<Data: NodeData> From<Node<Data>> for Tree<Data::Term, Data::NonTerm> {
    fn from(node: Node<Data>) -> Self {
        node.into_tree()
    }
}

#[cfg(feature = "tree")]
impl<Data: NodeData> std::fmt::Display for Node<Data>
where
    Data::Term: std::fmt::Display,
    Data::NonTerm: std::fmt::Display,
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
impl<Data: NodeData> std::fmt::Debug for Node<Data>
where
    Data::Term: std::fmt::Debug,
    Data::NonTerm: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.parent.is_none() {
            write!(f, "Root")
        } else {
            write!(f, "{:?}", self.to_tree())
        }
    }
}

impl<Data: NodeData> Deref for Node<Data> {
    type Target = Data;

    fn deref(&self) -> &Self::Target {
        self.to_data()
    }
}
impl<Data: NodeData> DerefMut for Node<Data> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.data.as_mut().unwrap()
    }
}
