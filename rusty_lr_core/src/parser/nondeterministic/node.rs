use crate::nonterminal::TokenData;

/// Node represents single token in the parse tree.
/// To handle multiple paths in the GLR parsing,
/// this constructs Linked List of nodes, where parent node is the previous token in the parse tree.
#[derive(Clone)]
pub struct Node<Data: TokenData> {
    /// parent node
    pub parent: Option<usize>,

    pub reference_count: usize,

    /// index of state in parser
    pub state_stack: Vec<usize>,
    pub data_stack: Vec<Data>,
    pub location_stack: Vec<Data::Location>,
    pub precedence_stack: Vec<Option<usize>>,
    #[cfg(feature = "tree")]
    pub(crate) tree_stack: Vec<crate::tree::Tree<Data::Term, Data::NonTerm>>,
}

impl<Data: TokenData> Default for Node<Data> {
    fn default() -> Self {
        Node {
            parent: None,
            reference_count: 1,
            state_stack: Vec::new(),
            data_stack: Vec::new(),
            location_stack: Vec::new(),
            precedence_stack: Vec::new(),
            #[cfg(feature = "tree")]
            tree_stack: Vec::new(),
        }
    }
}

impl<Data: TokenData> Node<Data> {
    /// Clear this node to `Default::default()`.
    pub fn clear(&mut self) {
        self.parent = None;
        self.reference_count = 1;
        self.state_stack.clear();
        self.data_stack.clear();
        self.location_stack.clear();
        self.precedence_stack.clear();
        #[cfg(feature = "tree")]
        self.tree_stack.clear();
    }
    pub fn len(&self) -> usize {
        self.data_stack.len()
    }
    pub fn is_unique(&self) -> bool {
        self.reference_count == 1
    }

    /*
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
        Data::NonTerm: Ord + Copy + std::hash::Hash + NonTerminal,
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
                    precedence_level: None,
                    state: next_state,
                    data: None,
                    location: None,
                    #[cfg(feature = "tree")]
                    tree: None,
                };
                let next_node = Rc::new(next_node);
                Self::expected_token(&next_node, parser, terms, nonterms);
            }
        }
    }


    /// check if current context can enter panic mode
    pub fn can_panic<P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        mut node: &Rc<Node<Data>>,
        parser: &P,
        error_prec: Option<usize>,
    ) -> bool
    where
        Data::NonTerm: std::hash::Hash + Eq + NonTerminal,
    {
        loop {
            if Self::can_feed_impl(node, parser, TerminalSymbol::Error, error_prec) {
                return true;
            } else {
                node = match node.parent.as_ref() {
                    Some(parent) => parent,
                    None => {
                        // if we reached root node, then panic mode is not possible
                        return false;
                    }
                };
            }
        }
    }
    */
}
