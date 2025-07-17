use std::ops::Deref;
use std::ops::DerefMut;
use std::rc::Rc;

use crate::nonterminal::NonTerminal;
use crate::nonterminal::TokenData;
use crate::parser::State;
use crate::TerminalSymbol;

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
impl<'a, Data: TokenData> Clone for NodeRefIterator<'a, Data> {
    fn clone(&self) -> Self {
        NodeRefIterator { node: self.node }
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
    pub precedence_level: Option<usize>,

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
            precedence_level: None,
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
                    #[cfg(feature = "tree")]
                    tree: None,
                };
                let next_node = Rc::new(next_node);
                Self::expected_token(&next_node, parser, terms, nonterms);
            }
        }
    }

    /// Enter panic mode.
    /// Returns `true` if panic mode was entered successfully.
    pub fn panic_mode<P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        mut self: Rc<Node<Data>>,
        context: &mut super::Context<Data>,
        parser: &P,
        error_prec: Option<usize>,
        userdata: &mut Data::UserData,
    ) -> bool
    where
        Data: Clone,
        Data::Term: Clone,
        Data::NonTerm: std::hash::Hash + Eq + Clone + std::fmt::Debug + NonTerminal,
    {
        use crate::Location;

        let mut error_location =
            Data::Location::new(self.iter().map(|node| &node.data.as_ref().unwrap().1), 0);

        loop {
            match Self::feed_location_impl(
                self,
                context,
                parser,
                TerminalSymbol::Error,
                TerminalSymbol::Error,
                error_prec,
                userdata,
                error_location,
            ) {
                Ok(_) => {
                    return true;
                }
                Err((err_node, _, err_loc)) => {
                    error_location = Data::Location::new(
                        std::iter::once(&err_loc)
                            .chain(err_node.iter().map(|node| &node.data.as_ref().unwrap().1)),
                        2,
                    );
                    self = match err_node.parent.as_ref() {
                        Some(parent) => Rc::clone(parent),
                        None => return false,
                    };
                }
            }
        }
    }

    /// Feed one terminal with location to parser, and update state stack.
    pub fn feed_location<P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        self: Rc<Self>,
        context: &mut super::Context<Data>,
        parser: &P,
        term: P::Term,
        class: usize,
        shift_prec: Option<usize>,
        userdata: &mut Data::UserData,
        location: Data::Location,
    ) -> Result<(), (Rc<Self>, TerminalSymbol<Data::Term>, Data::Location)>
    where
        P::Term: Clone,
        P::NonTerm: std::hash::Hash + Eq + Clone + std::fmt::Debug + NonTerminal,
        Data: Clone,
    {
        let class = TerminalSymbol::Term(class);
        let term = TerminalSymbol::Term(term);

        self.feed_location_impl(context, parser, term, class, shift_prec, userdata, location)
    }
    fn feed_location_impl<P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        self: Rc<Self>,
        context: &mut super::Context<Data>,
        parser: &P,
        term: TerminalSymbol<Data::Term>,
        class: TerminalSymbol<usize>,
        shift_prec: Option<usize>,
        userdata: &mut Data::UserData,
        location: Data::Location,
    ) -> Result<(), (Rc<Self>, TerminalSymbol<Data::Term>, Data::Location)>
    where
        P::Term: Clone,
        P::NonTerm: std::hash::Hash + Eq + Clone + std::fmt::Debug + NonTerminal,
        Data: Clone,
    {
        use crate::parser::State;
        use crate::rule::Precedence;

        let shift_state = parser.get_states()[self.state].shift_goto_class(class);
        if let Some(reduce_rules) = parser.get_states()[self.state].reduce(class) {
            let mut shift = None;
            let mut reduces: smallvec::SmallVec<[_; 2]> = Default::default();

            for reduce_rule in reduce_rules {
                let rule = &parser.get_rules()[reduce_rule];
                let reduce_prec = match rule.precedence {
                    Some(Precedence::Fixed(level)) => Some(level),
                    Some(Precedence::Dynamic(token_index)) => {
                        // fix the value to the offset from current node
                        let ith = rule.rule.len() - token_index - 1;
                        let mut node = &self;
                        for _ in 0..ith {
                            node = node.parent.as_ref().unwrap();
                        }
                        node.precedence_level
                    }
                    None => None,
                };

                // if there is shift/reduce conflict, check for reduce rule's precedence and shift terminal's precedence
                match (shift_state.is_some(), shift_prec, reduce_prec) {
                    (true, Some(shift_prec_), Some(reduce_prec_)) => {
                        match reduce_prec_.cmp(&shift_prec_) {
                            std::cmp::Ordering::Less => {
                                // no reduce
                                shift = shift_state;
                            }
                            std::cmp::Ordering::Equal => {
                                // check for reduce_type
                                use crate::builder::ReduceType;
                                match parser.precedence_types(reduce_prec_) {
                                    Some(ReduceType::Left) => {
                                        // no shift
                                        reduces.push((reduce_rule, reduce_prec));
                                    }
                                    Some(ReduceType::Right) => {
                                        // no reduce
                                        shift = shift_state;
                                    }
                                    None => {
                                        // cannot determine precedence, error
                                        context.no_precedences.push(reduce_rule);
                                    }
                                }
                            }
                            std::cmp::Ordering::Greater => {
                                // no shift
                                reduces.push((reduce_rule, reduce_prec));
                            }
                        }
                    }
                    _ => {
                        // nothing; go for both reduce and shift
                        shift = shift_state;
                        reduces.push((reduce_rule, reduce_prec));
                    }
                }
            }

            let mut shifted = false;
            if !reduces.is_empty() {
                // call every reduce action
                // and check if every reduce action revoked shift
                let mut shift_ = false;
                for (reduce_rule, precedence) in reduces {
                    let mut pass = shift.is_some();
                    match super::reduce(
                        parser,
                        reduce_rule,
                        precedence,
                        Rc::clone(&self),
                        context,
                        &term,
                        &mut pass,
                        userdata,
                    ) {
                        Ok(next_node) => {
                            shift_ |= pass;
                            // reduce recursively
                            shifted |= Self::feed_location_impl(
                                next_node,
                                context,
                                parser,
                                term.clone(),
                                class,
                                shift_prec,
                                userdata,
                                location.clone(),
                            )
                            .is_ok();
                        }
                        Err(err) => {
                            shift_ |= pass;
                            context.reduce_errors.push(err);
                        }
                    }
                }
                // if every reduce action revoked shift,
                // then reset shift to None
                if !shift_ {
                    shift = None;
                }
            }
            if let Some(shift) = shift {
                // some shift action was performed; remove fallback_nodes immediately
                // to avoid cloned Rc nodes
                context.fallback_nodes.clear();

                let next_node = Node {
                    parent: Some(self),
                    state: shift,
                    precedence_level: shift_prec,
                    #[cfg(feature = "tree")]
                    tree: Some(crate::tree::Tree::new_terminal(term.clone())),
                    data: Some((
                        match term {
                            TerminalSymbol::Term(term) => Data::new_terminal(term),
                            TerminalSymbol::Error => Data::new_error(),
                            TerminalSymbol::Eof => Data::new_empty(),
                        },
                        location,
                    )),
                };

                context.next_nodes.push(Rc::new(next_node));
                Ok(())
            } else {
                if shifted {
                    Ok(())
                } else {
                    Err((self, term, location))
                }
            }
        } else if let Some(shift) = shift_state {
            // some shift action was performed; remove fallback_nodes immediately
            // to avoid cloned Rc nodes
            context.fallback_nodes.clear();

            let next_node = Node {
                parent: Some(self),
                state: shift,
                precedence_level: shift_prec,
                #[cfg(feature = "tree")]
                tree: Some(crate::tree::Tree::new_terminal(term.clone())),
                data: Some((
                    match term {
                        TerminalSymbol::Term(term) => Data::new_terminal(term),
                        TerminalSymbol::Error => Data::new_error(),
                        TerminalSymbol::Eof => Data::new_empty(),
                    },
                    location,
                )),
            };

            context.next_nodes.push(Rc::new(next_node));
            Ok(())
        } else {
            // no reduce, no shift
            Err((self, term, location))
        }
    }

    /// Feed one terminal with location to parser, and update state stack.
    pub fn can_feed<P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        self: &Rc<Self>,
        parser: &P,
        class: usize,
        shift_prec: Option<usize>,
    ) -> bool
    where
        P::NonTerm: std::hash::Hash + Eq + NonTerminal,
    {
        let class = TerminalSymbol::Term(class);
        self.can_feed_impl(parser, class, shift_prec)
    }

    /// Feed one terminal with location to parser, and update state stack.
    fn can_feed_impl<P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        self: &Rc<Self>,
        parser: &P,
        class: TerminalSymbol<usize>,
        shift_prec: Option<usize>,
    ) -> bool
    where
        P::NonTerm: std::hash::Hash + Eq + NonTerminal,
    {
        use crate::parser::State;
        use crate::rule::Precedence;

        let shift_state = parser.get_states()[self.state].shift_goto_class(class);
        if let Some(reduce_rules) = parser.get_states()[self.state].reduce(class) {
            let mut shift = None;
            let mut reduces: smallvec::SmallVec<[_; 2]> = Default::default();

            for reduce_rule in reduce_rules {
                let rule = &parser.get_rules()[reduce_rule];
                let reduce_prec = match rule.precedence {
                    Some(Precedence::Fixed(level)) => Some(level),
                    Some(Precedence::Dynamic(token_index)) => {
                        // fix the value to the offset from current node
                        let ith = rule.rule.len() - token_index - 1;
                        let mut node = self;
                        for _ in 0..ith {
                            node = node.parent.as_ref().unwrap();
                        }
                        node.precedence_level
                    }
                    None => None,
                };

                // if there is shift/reduce conflict, check for reduce rule's precedence and shift terminal's precedence
                match (shift_state.is_some(), shift_prec, reduce_prec) {
                    (true, Some(shift_prec_), Some(reduce_prec_)) => {
                        match reduce_prec_.cmp(&shift_prec_) {
                            std::cmp::Ordering::Less => {
                                // no reduce
                                shift = shift_state;
                            }
                            std::cmp::Ordering::Equal => {
                                // check for reduce_type
                                use crate::builder::ReduceType;
                                match parser.precedence_types(reduce_prec_) {
                                    Some(ReduceType::Left) => {
                                        // no shift
                                        reduces.push((reduce_rule, reduce_prec));
                                    }
                                    Some(ReduceType::Right) => {
                                        // no reduce
                                        shift = shift_state;
                                    }
                                    None => {
                                        // cannot determine precedence
                                    }
                                }
                            }
                            std::cmp::Ordering::Greater => {
                                // no shift
                                reduces.push((reduce_rule, reduce_prec));
                            }
                        }
                    }
                    _ => {
                        // nothing; go for both reduce and shift
                        shift = shift_state;
                        reduces.push((reduce_rule, reduce_prec));
                    }
                }
            }

            if shift.is_some() {
                return true;
            }

            // call every reduce action
            for (reduce_rule, precedence) in reduces {
                let rule = &parser.get_rules()[reduce_rule];

                let mut node = self;
                for _ in 0..rule.rule.len() {
                    node = node.parent.as_ref().unwrap();
                }

                if let Some(next_shift_nonterm) =
                    parser.get_states()[node.state].shift_goto_nonterm(&rule.name)
                {
                    let next_node = Self {
                        parent: Some(Rc::clone(node)),
                        state: next_shift_nonterm,
                        precedence_level: precedence,
                        #[cfg(feature = "tree")]
                        tree: None,
                        data: None,
                    };
                    if Self::can_feed_impl(&Rc::new(next_node), parser, class, shift_prec) {
                        return true;
                    }
                } else {
                    // cannot shift non-terminal, so no reduce
                    continue;
                }
            }
            false
        } else if shift_state.is_some() {
            true
        } else {
            false
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

    /// Feed one terminal with location to parser, and update state stack.
    pub fn feed_eof<P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        self: Rc<Self>,
        context: &mut super::Context<Data>,
        parser: &P,
        eof_location: Data::Location,
        userdata: &mut Data::UserData,
    ) -> Result<(), (Rc<Self>, TerminalSymbol<Data::Term>, Data::Location)>
    where
        P::Term: Clone,
        P::NonTerm: std::hash::Hash + Eq + Clone + std::fmt::Debug + NonTerminal,
        Data: Clone,
    {
        self.feed_location_impl(
            context,
            parser,
            TerminalSymbol::Eof,
            TerminalSymbol::Eof,
            None,
            userdata,
            eof_location,
        )
    }

    pub fn can_feed_eof<P: super::Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        self: &Rc<Self>,
        parser: &P,
    ) -> bool
    where
        P::NonTerm: std::hash::Hash + Eq + NonTerminal,
    {
        self.can_feed_impl(parser, TerminalSymbol::Eof, None)
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
