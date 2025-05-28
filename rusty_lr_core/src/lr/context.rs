use std::collections::BTreeSet;
use std::hash::Hash;

use super::ParseError;
use super::Parser;
use super::State;

#[cfg(feature = "tree")]
use crate::TreeList;

use crate::TokenData;

/// A struct that maintains the current state and the values associated with each symbol.
pub struct Context<Data: TokenData> {
    /// State stack
    pub state_stack: Vec<usize>,

    /// Data stack holds the values associated with each symbol.
    pub(crate) data_stack: Vec<Data>,

    /// temporary data stack for reduce action.
    pub(crate) reduce_args: Vec<Data>,

    /// Tree stack for tree representation of the parse.
    #[cfg(feature = "tree")]
    pub(crate) tree_stack: TreeList<Data::Term, Data::NonTerm>,
}

impl<Data: TokenData> Context<Data> {
    /// Create a new context.
    /// `state_stack` is initialized with [0] (root state).
    pub fn new() -> Self {
        Context {
            state_stack: vec![0],

            data_stack: Vec::new(),
            reduce_args: Vec::new(),

            #[cfg(feature = "tree")]
            tree_stack: TreeList::new(),
        }
    }
    /// Create a new context with given capacity of `state_stack` and `data_stack`.
    /// `state_stack` is initialized with [0] (root state).
    pub fn with_capacity(capacity: usize) -> Self {
        let mut state_stack = Vec::with_capacity(capacity);
        state_stack.push(0);
        Context {
            state_stack,

            data_stack: Vec::with_capacity(capacity),
            reduce_args: Vec::new(),

            #[cfg(feature = "tree")]
            tree_stack: TreeList::new(),
        }
    }
    /// Pops the value of the start symbol from the data stack.
    /// This must be called when the parser is in the final state (after feeding EOF).
    #[inline]
    pub fn accept(&mut self) -> Data::StartType
    where
        Data: TryInto<Data::StartType>,
    {
        // data_stack must be <Start> <EOF> in this point
        self.data_stack.pop();
        self.data_stack
            .pop()
            .expect("data stack must have at least one element")
            .try_into()
            .ok()
            .expect("data stack must have <Start> as the last element")
    }

    /// For debugging.
    /// Get `TreeList` that current context holds.
    #[cfg(feature = "tree")]
    pub fn to_tree_list(&self) -> TreeList<Data::Term, Data::NonTerm>
    where
        Data::Term: Clone,
        Data::NonTerm: Clone,
    {
        self.tree_stack.clone()
    }
    /// For debugging.
    /// Get `TreeList` that current context holds.
    #[cfg(feature = "tree")]
    pub fn into_tree_list(self) -> TreeList<Data::Term, Data::NonTerm> {
        self.tree_stack
    }

    /// Get expected terminal classes for next `feed()` call.
    pub fn expected_class<'a, P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &'a P,
    ) -> impl Iterator<Item = usize> + 'a
    where
        Data::Term: 'a,
        Data::NonTerm: 'a,
    {
        parser.get_states()[*self.state_stack.last().unwrap()].expected()
    }
    /// Get expected tokens for next `feed()` call.
    pub fn expected<'a, P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &'a P,
    ) -> impl Iterator<Item = P::TerminalClassElement> + 'a
    where
        Data::Term: 'a,
        Data::NonTerm: 'a,
    {
        parser.get_states()[*self.state_stack.last().unwrap()]
            .expected()
            .flat_map(|class| parser.get_terminals(class).unwrap())
    }
    /// Get expected non-terminal tokens for next `feed()` call.
    pub fn expected_nonterm<'a, P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &'a P,
    ) -> impl Iterator<Item = &'a Data::NonTerm>
    where
        Data::Term: 'a,
        Data::NonTerm: 'a,
    {
        parser.get_states()[*self.state_stack.last().unwrap()].expected_nonterm()
    }

    /// Feed one terminal to parser, and update state stack.
    /// This automatically enters panic mode if needed.
    pub fn feed<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &mut self,
        parser: &P,
        term: Data::Term,
        userdata: &mut Data::UserData,
    ) -> Result<(), ParseError<Data::Term, Data::NonTerm, Data::ReduceActionError>>
    where
        Data::Term: Hash + Eq + Clone,
        Data::NonTerm: Hash + Eq + Copy,
        Data: From<Data::Term>,
    {
        #[cfg(feature = "tree")]
        use crate::Tree;

        let class = parser.to_terminal_class(&term);
        // check if there is any reduce action with given terminal
        while let Some(reduce_rule) =
            parser.get_states()[*self.state_stack.last().unwrap()].reduce(class)
        {
            let rule = &parser.get_rules()[reduce_rule];
            // pop state stack
            self.state_stack
                .truncate(self.state_stack.len() - rule.rule.len());

            let mut shift = false;

            self.reduce_args.clear();
            self.reduce_args.reserve(rule.rule.len());
            for _ in 0..rule.rule.len() {
                self.reduce_args.push(
                    self.data_stack
                        .pop()
                        .expect("data stack must have at least one element"),
                );
            }

            // call reduce action
            let new_data = Data::reduce_action(
                reduce_rule,
                &mut self.reduce_args,
                &mut shift,
                &term,
                userdata,
            )
            .map_err(ParseError::ReduceAction)?;
            self.data_stack.push(new_data);

            // construct tree
            #[cfg(feature = "tree")]
            {
                let mut children = Vec::with_capacity(rule.rule.len());
                for _ in 0..rule.rule.len() {
                    let tree = self.tree_stack.pop().unwrap();
                    children.push(tree);
                }
                children.reverse();

                self.tree_stack
                    .push(Tree::new_nonterminal(rule.name.clone(), children));
            }

            // shift with reduced nonterminal
            if let Some(next_state_id) = parser.get_states()[*self.state_stack.last().unwrap()]
                .shift_goto_nonterm(&rule.name)
            {
                self.state_stack.push(next_state_id);
            } else {
                unreachable!("shift nonterm failed");
            }
        }

        let state = &parser.get_states()[*self.state_stack.last().unwrap()];

        // shift with terminal
        if let Some(next_state_id) = state.shift_goto_class(class) {
            self.state_stack.push(next_state_id);

            #[cfg(feature = "tree")]
            self.tree_stack.push(Tree::new_terminal(term.clone()));

            self.data_stack.push(term.into());

            Ok(())
        } else {
            // enters panic mode
            if self.panic_mode(parser) {
                // check for shift terminal again
                if let Some(next_state_id) =
                    parser.get_states()[*self.state_stack.last().unwrap()].shift_goto_class(class)
                {
                    self.state_stack.push(next_state_id);

                    #[cfg(feature = "tree")]
                    self.tree_stack.push(Tree::new_terminal(term.clone()));

                    self.data_stack.push(term.into());
                }
                Ok(())
            } else {
                #[cfg(feature = "error")]
                let backtrace = self.backtrace(parser);
                let error = super::InvalidTerminalError {
                    term,
                    #[cfg(feature = "error")]
                    backtrace,
                    #[cfg(not(feature = "error"))]
                    _phantom: std::marker::PhantomData,
                };
                Err(ParseError::InvalidTerminal(error))
            }
        }
    }

    /// Check if `term` can be feeded to current state.
    /// This does not check for reduce action error, nor panic mode.
    /// You should call `can_panic_mode()` after this fails to check if panic mode can accept this term.
    ///
    /// This does not change the state of the context.
    pub fn can_feed<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
        term: &Data::Term,
    ) -> bool
    where
        Data::Term: Hash + Eq,
        Data::NonTerm: Hash + Eq,
    {
        let class = parser.to_terminal_class(term);
        if parser.get_states()[*self.state_stack.last().unwrap()]
            .shift_goto_class(class)
            .is_some()
        {
            return true;
        }

        let mut state_stack = self.state_stack.clone();
        while let Some(reduce_rule) =
            parser.get_states()[*state_stack.last().unwrap()].reduce(class)
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
            .shift_goto_class(class)
            .is_some()
    }
    /// Check if current context can enter panic mode.
    pub fn can_panic_mode<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
    ) -> bool
    where
        Data::NonTerm: Hash + Eq,
    {
        let Some(error_nonterm) = parser.get_error_nonterm() else {
            return false;
        };
        for &last_state in self.state_stack.iter().rev() {
            let last_state = &parser.get_states()[last_state];
            if last_state.shift_goto_nonterm(&error_nonterm).is_some() {
                return true;
            }
        }
        false
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
    pub fn trace<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
    ) -> crate::HashSet<Data::NonTerm>
    where
        Data::NonTerm: Copy + Eq + Hash + crate::NonTerminal,
    {
        use crate::token::Token;
        use crate::HashSet;
        use crate::NonTerminal;

        let rules = parser.get_rules();
        let states = parser.get_states();

        let mut zero_shifted_rules = BTreeSet::new();
        let mut non_zero_shifted_rules = BTreeSet::new();
        {
            let last_state = &states[*self.state_stack.last().unwrap()];
            for rule in last_state.get_rules().iter() {
                if rule.shifted == 0 {
                    zero_shifted_rules.insert(rule.rule);
                } else {
                    non_zero_shifted_rules.insert((rule.rule, rule.shifted));
                }
            }
        }

        let mut ret: HashSet<Data::NonTerm> = Default::default();

        for &state_idx in self.state_stack.iter().rev() {
            let state = &states[state_idx];
            let ruleset = state.get_rules();

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
        }

        ret
    }

    /// Panic mode recovery with `error` non-terminal.
    /// Returns true if error is shifted.
    fn panic_mode<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &mut self,
        parser: &P,
    ) -> bool
    where
        Data::NonTerm: Hash + Eq + Copy,
    {
        let Some(error_nonterm) = parser.get_error_nonterm() else {
            return false;
        };
        let mut popped_token = 0;
        for (stack_idx, &last_state) in self.state_stack.iter().enumerate().rev() {
            let last_state = &parser.get_states()[last_state];
            if let Some(error_state) = last_state.shift_goto_nonterm(&error_nonterm) {
                // pop all states above this state
                self.state_stack.truncate(stack_idx + 1);
                // pop all data
                {
                    let new_len = self.data_stack.len() - popped_token;
                    self.data_stack.truncate(new_len);
                }
                #[cfg(feature = "tree")]
                {
                    let new_len = self.tree_stack.len() - popped_token;
                    self.tree_stack.truncate(new_len);
                }

                // shift to error state
                self.state_stack.push(error_state);
                self.data_stack.push(Data::new_error_nonterm());
                #[cfg(feature = "tree")]
                {
                    // push error tree
                    self.tree_stack
                        .push(crate::Tree::new_nonterminal(error_nonterm, Vec::new()));
                }
                return true;
            }

            popped_token += 1;
        }
        false
    }

    /// Get backtrace information for current state.
    /// What current state is trying to parse, and where it comes from.
    pub fn backtrace<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
    ) -> crate::Backtrace<&'static str, Data::NonTerm>
    where
        Data::Term: Clone,
        Data::NonTerm: Hash + Eq + Clone,
    {
        use crate::Backtrace;
        use crate::HashSet;
        use crate::ShiftedRule;
        use crate::ShiftedRuleRef;
        use crate::Token;
        use std::collections::BTreeSet;

        if self.state_stack.len() == 1 {
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
        let mut current_rules: BTreeSet<_> = parser.get_states()[*self.state_stack.last().unwrap()]
            .get_rules()
            .iter()
            // .filter(|rule| rule.shifted > 0)
            .copied()
            .collect();
        let mut next_rules = BTreeSet::new();
        traces.push(current_rules.clone());
        let mut zero_shifted_rules: HashSet<Data::NonTerm> = Default::default();

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

impl<Data: TokenData> Default for Context<Data> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Data: TokenData> Clone for Context<Data>
where
    Data: Clone,
    Data::Term: Clone,
    Data::NonTerm: Clone,
{
    fn clone(&self) -> Self {
        Context {
            state_stack: self.state_stack.clone(),
            data_stack: self.data_stack.clone(),
            reduce_args: Vec::new(),

            #[cfg(feature = "tree")]
            tree_stack: self.tree_stack.clone(),
        }
    }
}

#[cfg(feature = "tree")]
impl<Data: TokenData> std::fmt::Display for Context<Data>
where
    Data::Term: std::fmt::Display + Clone,
    Data::NonTerm: std::fmt::Display + Clone + crate::NonTerminal,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_tree_list())
    }
}
#[cfg(feature = "tree")]
impl<Data: TokenData> std::fmt::Debug for Context<Data>
where
    Data::Term: std::fmt::Debug + Clone,
    Data::NonTerm: std::fmt::Debug + Clone + crate::NonTerminal,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.to_tree_list())
    }
}

#[cfg(feature = "tree")]
impl<Data: TokenData> std::ops::Deref for Context<Data> {
    type Target = TreeList<Data::Term, Data::NonTerm>;
    fn deref(&self) -> &Self::Target {
        &self.tree_stack
    }
}

#[cfg(feature = "tree")]
impl<Data: TokenData> std::ops::DerefMut for Context<Data> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.tree_stack
    }
}
