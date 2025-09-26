use std::collections::BTreeSet;
use std::hash::Hash;

use super::ParseError;

use crate::parser::data_stack::DataStack;
use crate::parser::nonterminal::NonTerminal;
use crate::parser::state::Index;
use crate::parser::terminalclass::TerminalClass;
use crate::parser::Parser;
use crate::parser::Precedence;
use crate::parser::State;
use crate::TerminalSymbol;

/// A struct that maintains the current state and the values associated with each symbol.
pub struct Context<Data: DataStack, StateIndex> {
    /// stacks hold the values associated with each shifted symbol.
    pub state_stack: Vec<StateIndex>,
    pub(crate) data_stack: Data,
    pub(crate) location_stack: Vec<Data::Location>,
    pub(crate) precedence_stack: Vec<Precedence>,
    /// Tree stack for tree representation of the parse.
    #[cfg(feature = "tree")]
    pub(crate) tree_stack: crate::tree::TreeList<Data::Term, Data::NonTerm>,
}

impl<Data: DataStack, StateIndex: Index + Copy> Context<Data, StateIndex> {
    /// Create a new context.
    /// `state_stack` is initialized with [0] (root state).
    pub fn new() -> Self {
        Context {
            state_stack: vec![StateIndex::from_usize_unchecked(0)],

            data_stack: Default::default(),
            location_stack: Vec::new(),
            precedence_stack: Vec::new(),

            #[cfg(feature = "tree")]
            tree_stack: crate::tree::TreeList::new(),
        }
    }
    /// Create a new context with given capacity of `state_stack` and `data_stack`.
    /// `state_stack` is initialized with [0] (root state).
    pub fn with_capacity(capacity: usize) -> Self {
        let mut state_stack = Vec::with_capacity(capacity);
        state_stack.push(
            StateIndex::from_usize_unchecked(0), // root state
        );
        Context {
            state_stack,

            data_stack: Data::with_capacity(capacity),
            location_stack: Vec::with_capacity(capacity),
            precedence_stack: Vec::with_capacity(capacity),

            #[cfg(feature = "tree")]
            tree_stack: crate::tree::TreeList::new(),
        }
    }
    /// End this context and pop the value of the start symbol from the data stack.
    pub fn accept<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        mut self,
        parser: &P,
        userdata: &mut Data::UserData,
    ) -> Result<Data::StartType, ParseError<Data::Term, Data::Location, Data::ReduceActionError>>
    where
        Data::Term: Clone,
        Data::NonTerm: Hash + Eq + Copy + NonTerminal,
    {
        self.feed_eof(parser, userdata)?;

        // data_stack must be <Start> <EOF> in this point
        self.data_stack.pop(); // pop eof
        Ok(self.data_stack.pop_start().unwrap())
    }

    /// Check if current context can be terminated and get the value of the start symbol.
    pub fn can_accept<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
    ) -> bool
    where
        Data::NonTerm: Hash + Eq + NonTerminal,
    {
        let mut extra_state_stack = Vec::new();
        let mut extra_precedence_stack = Vec::new();

        self.can_feed_impl(
            self.precedence_stack.len(),
            &mut extra_state_stack,
            &mut extra_precedence_stack,
            parser,
            P::TermClass::EOF,
            Precedence::none(),
        ) == Some(true)
    }

    /// Get current state index
    #[inline]
    pub fn state(&self) -> usize {
        self.state_stack.last().unwrap().into_usize()
    }
    /// Get iterator of state stack
    pub fn state_stack(&self) -> impl Iterator<Item = usize> + '_ {
        self.state_stack.iter().map(|s| s.into_usize())
    }

    /// For debugging.
    /// Get `TreeList` that current context holds.
    #[cfg(feature = "tree")]
    pub fn to_tree_list(&self) -> crate::tree::TreeList<Data::Term, Data::NonTerm>
    where
        Data::Term: Clone,
        Data::NonTerm: Clone,
    {
        self.tree_stack.clone()
    }
    /// For debugging.
    /// Get `TreeList` that current context holds.
    #[cfg(feature = "tree")]
    pub fn into_tree_list(self) -> crate::tree::TreeList<Data::Term, Data::NonTerm> {
        self.tree_stack
    }

    /// Simulate parser and get next expected (terminals, non-terminals) for current context.
    pub fn expected_token<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
    ) -> (BTreeSet<P::TermClass>, BTreeSet<P::NonTerm>)
    where
        P::TermClass: Ord,
        P::NonTerm: Ord,
    {
        let mut terms = BTreeSet::new();
        let mut nonterms = BTreeSet::new();
        let mut extra_state_stack = Vec::new();
        self.expected_token_impl(
            &mut extra_state_stack,
            self.precedence_stack.len(),
            parser,
            &mut terms,
            &mut nonterms,
        );

        (terms, nonterms)
    }
    /// Same as `expected_token()`, but returns as printable type.
    pub fn expected_token_str<'a, P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
    ) -> (
        impl Iterator<Item = &'static str>,
        impl Iterator<Item = &'static str>,
    )
    where
        P::TermClass: Ord,
        P::NonTerm: Ord,
    {
        let (terms, nonterms) = self.expected_token(parser);
        (
            terms.into_iter().map(|term| term.as_str()),
            nonterms.into_iter().map(|nonterm| nonterm.as_str()),
        )
    }

    fn expected_token_impl<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        extra_state_stack: &mut Vec<StateIndex>,
        stack_len: usize,
        parser: &P,
        terms: &mut BTreeSet<P::TermClass>,
        nonterms: &mut BTreeSet<P::NonTerm>,
    ) where
        P::TermClass: Ord,
        P::NonTerm: Ord,
    {
        let state = &parser.get_states()[extra_state_stack
            .last()
            .copied()
            .unwrap_or_else(|| self.state_stack[stack_len])
            .into_usize()];

        terms.extend(state.expected_shift_term());
        nonterms.extend(state.expected_shift_nonterm());

        let mut reduce_nonterms = BTreeSet::new();
        for reduce_rule in state.expected_reduce_rule() {
            let prod_rule = &parser.get_rules()[reduce_rule.into_usize()];
            reduce_nonterms.insert((prod_rule.rule.len(), prod_rule.name));
        }
        for &(mut tokens_len, nonterm) in reduce_nonterms.iter() {
            let mut stack_len = stack_len;
            let mut extra_state_stack = if tokens_len > extra_state_stack.len() {
                tokens_len -= extra_state_stack.len();
                stack_len -= tokens_len;
                Vec::new()
            } else {
                extra_state_stack[..extra_state_stack.len() - tokens_len].to_vec()
            };
            let state = &parser.get_states()[extra_state_stack
                .last()
                .copied()
                .unwrap_or_else(|| self.state_stack[stack_len])
                .into_usize()];
            if let Some(next_state) = state.shift_goto_nonterm(nonterm) {
                extra_state_stack.push(Index::from_usize_unchecked(next_state.state));
                self.expected_token_impl(
                    &mut extra_state_stack,
                    stack_len,
                    parser,
                    terms,
                    nonterms,
                );
            }
        }
    }

    /// Feed one terminal to parser, and update stacks.
    /// This will use `Default::default()` for location.
    pub fn feed<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &mut self,
        parser: &P,
        term: Data::Term,
        userdata: &mut Data::UserData,
    ) -> Result<(), ParseError<Data::Term, Data::Location, Data::ReduceActionError>>
    where
        Data::Location: Default,
        P::Term: Clone,
    {
        self.feed_location(parser, term, userdata, Default::default())
    }

    /// Feed one terminal with location to parser, and update stacks.
    pub fn feed_location<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &mut self,
        parser: &P,
        term: P::Term,
        userdata: &mut Data::UserData,
        location: Data::Location,
    ) -> Result<(), ParseError<Data::Term, Data::Location, Data::ReduceActionError>>
    where
        P::Term: Clone,
    {
        use crate::Location;
        let class = P::TermClass::from_term(&term);
        let shift_prec = class.precedence();

        match self.feed_location_impl(
            parser,
            TerminalSymbol::Term(term),
            class,
            shift_prec,
            userdata,
            Some(location),
        ) {
            Ok(()) => Ok(()),
            Err(ParseError::NoAction(err)) => {
                // nothing shifted; enters panic mode

                // if `error` token was not used in the grammar, early return here
                if !P::ERROR_USED {
                    return Err(ParseError::NoAction(err));
                }

                let mut error_location = Location::new(self.location_stack.iter().rev(), 0);
                let error = P::TermClass::ERROR;
                let error_prec = error.precedence();

                loop {
                    match self.feed_location_impl(
                        parser,
                        TerminalSymbol::Error,
                        error,
                        error_prec,
                        userdata,
                        Some(error_location),
                    ) {
                        Err(ParseError::NoAction(err)) => {
                            if self.state_stack.len() == 1 {
                                return Err(ParseError::NoAction(super::error::NoActionError {
                                    term: err.term,
                                    location: err.location,
                                    state: err.state,
                                }));
                            }

                            // no action for `error` token, continue to panic mode
                            // merge location with previous
                            error_location = Data::Location::new(
                                std::iter::once(&err.location.unwrap())
                                    .chain(self.location_stack.iter().rev()),
                                2, // error node
                            );
                            self.data_stack.pop();
                            self.precedence_stack.pop();
                            self.location_stack.pop();
                            self.state_stack.pop();

                            #[cfg(feature = "tree")]
                            self.tree_stack.pop(); // pop tree node for `error`
                        }
                        Ok(()) => break,             // successfully shifted `error`
                        Err(err) => return Err(err), // other errors
                    }
                }

                // try shift given term again
                // to check if the given terminal should be merged with `error` token
                // or it can be shift right after the error token
                if let Some(next_state) = parser.get_states()
                    [self.state_stack.last().unwrap().into_usize()]
                .shift_goto_class(class)
                {
                    #[cfg(feature = "tree")]
                    self.tree_stack
                        .push(crate::tree::Tree::new_terminal(err.term.clone()));

                    // shift after `error` token
                    if next_state.push {
                        self.data_stack.push_terminal(err.term.into_term().unwrap());
                    } else {
                        self.data_stack.push_empty();
                    }

                    self.location_stack.push(err.location.unwrap());
                    self.precedence_stack.push(shift_prec);
                    self.state_stack
                        .push(StateIndex::from_usize_unchecked(next_state.state));
                } else {
                    // merge term with previous error

                    let error_location = Data::Location::new(
                        std::iter::once(&err.location.unwrap())
                            .chain(self.location_stack.iter().rev()),
                        2, // error node
                    );
                    if let Some(err_loc) = self.location_stack.last_mut() {
                        *err_loc = error_location;
                    } else {
                        unreachable!("location stack must have at least one element");
                    }
                }
                Ok(())
            }
            Err(err) => Err(err),
        }
    }

    fn feed_location_impl<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &mut self,
        parser: &P,
        term: TerminalSymbol<P::Term>,
        class: P::TermClass,
        shift_prec: Precedence,
        userdata: &mut Data::UserData,
        location: Option<Data::Location>,
    ) -> Result<(), ParseError<Data::Term, Data::Location, Data::ReduceActionError>>
    where
        P::Term: Clone,
    {
        debug_assert!(
            (term.is_eof() && location.is_none()) || (!term.is_eof() && location.is_some())
        );
        use super::super::state::ReduceRules;
        use crate::Location;

        let shift_to = loop {
            let state = &parser.get_states()[self.state_stack.last().unwrap().into_usize()];

            let shift = state.shift_goto_class(class);
            if let Some(reduce_rule) = state.reduce(class) {
                let reduce_rule = reduce_rule.to_iter().next().unwrap();
                let rule = &parser.get_rules()[reduce_rule.into_usize()];
                let tokens_len = rule.rule.len();

                let reduce_prec = match rule.precedence {
                    Some(crate::rule::Precedence::Fixed(level)) => Precedence::new(level as u8),
                    Some(crate::rule::Precedence::Dynamic(token_idx)) => {
                        // get token_idx'th precedence from back of precedence stack
                        let idx = self.precedence_stack.len() - tokens_len + token_idx;
                        self.precedence_stack[idx]
                    }
                    None => Precedence::none(),
                };

                // resolve shift/reduce conflict by precedence
                if shift.is_some() {
                    let shift_prec = shift_prec.unwrap();
                    let reduce_prec = reduce_prec.unwrap();
                    use std::cmp::Ordering;
                    match reduce_prec.cmp(&shift_prec) {
                        Ordering::Less => {
                            // no reduce
                            break shift;
                        }
                        Ordering::Equal => {
                            // check for reduce type
                            use crate::rule::ReduceType;
                            match parser.precedence_types(reduce_prec) {
                                Some(ReduceType::Left) => {
                                    // no shift
                                    // shift = None;
                                }
                                Some(ReduceType::Right) => {
                                    // no reduce
                                    break shift;
                                }
                                None => {
                                    // error
                                    return Err(ParseError::NoPrecedence(
                                        super::error::NoPrecedenceError {
                                            term,
                                            location,
                                            state: self.state_stack.last().unwrap().into_usize(),
                                            rule: reduce_rule.into_usize(),
                                        },
                                    ));
                                }
                            }
                        }
                        Ordering::Greater => {
                            // no shift
                            // shift = None;
                        }
                    }
                }

                // if the code reaches here, it proceed to reduce, without shifting

                // pop state stack
                self.state_stack
                    .truncate(self.state_stack.len() - tokens_len);
                // pop precedence stack
                self.precedence_stack
                    .truncate(self.precedence_stack.len() - tokens_len);
                self.precedence_stack.push(reduce_prec);

                let mut shift = false;

                let mut new_location =
                    Data::Location::new(self.location_stack.iter().rev(), tokens_len);

                // call reduce action
                let non_empty_pushed = match Data::reduce_action(
                    &mut self.data_stack,
                    &mut self.location_stack,
                    reduce_rule.into_usize(),
                    &mut shift,
                    &term,
                    userdata,
                    &mut new_location,
                ) {
                    Ok(ret) => ret,
                    Err(err) => {
                        return Err(ParseError::ReduceAction(super::error::ReduceActionError {
                            term,
                            location,
                            state: self.state(),
                            source: err,
                        }));
                    }
                };
                self.location_stack.push(new_location);

                // construct tree
                #[cfg(feature = "tree")]
                {
                    let mut children = Vec::with_capacity(rule.rule.len());
                    for _ in 0..rule.rule.len() {
                        let tree = self.tree_stack.pop().unwrap();
                        children.push(tree);
                    }
                    children.reverse();

                    self.tree_stack.push(crate::tree::Tree::new_nonterminal(
                        rule.name.clone(),
                        children,
                    ));
                }

                // shift with reduced nonterminal
                if let Some(next_state_id) = parser.get_states()
                    [self.state_stack.last().unwrap().into_usize()]
                .shift_goto_nonterm(rule.name)
                {
                    self.state_stack
                        .push(StateIndex::from_usize_unchecked(next_state_id.state));
                    if !next_state_id.push && non_empty_pushed {
                        self.data_stack.pop();
                        self.data_stack.push_empty();
                    }
                } else {
                    unreachable!("shift nonterm failed");
                }
            } else {
                break shift;
            }
        };

        // shift with terminal
        if let Some(next_state_id) = shift_to {
            self.state_stack
                .push(StateIndex::from_usize_unchecked(next_state_id.state));

            #[cfg(feature = "tree")]
            self.tree_stack
                .push(crate::tree::Tree::new_terminal(term.clone()));

            if next_state_id.push {
                match term {
                    TerminalSymbol::Term(t) => self.data_stack.push_terminal(t),
                    TerminalSymbol::Error | TerminalSymbol::Eof => self.data_stack.push_empty(),
                }
            } else {
                self.data_stack.push_empty();
            }

            // location is `Some` if it is not `Eof`
            if let Some(location) = location {
                self.location_stack.push(location);
            }
            self.precedence_stack.push(shift_prec);

            Ok(())
        } else {
            Err(ParseError::NoAction(super::error::NoActionError {
                term,
                location,
                state: self.state(),
            }))
        }
    }

    /// Check if `term` can be feeded to current state.
    /// This does not simulate for reduce action error, or panic mode.
    /// So this function will return `false` even if term can be shifted as `error` token,
    /// and will return `true` if `Err` variant is returned by `reduce_action`.
    pub fn can_feed<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
        term: &Data::Term,
    ) -> bool {
        let mut extra_state_stack = Vec::new();
        let mut extra_precedence_stack = Vec::new();
        let class = P::TermClass::from_term(term);
        let shift_prec = class.precedence();

        self.can_feed_impl(
            self.precedence_stack.len(),
            &mut extra_state_stack,
            &mut extra_precedence_stack,
            parser,
            class,
            shift_prec,
        ) == Some(true)
    }

    /// Check if current context can enter panic mode
    pub fn can_panic<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
    ) -> bool {
        // if `error` token was not used in the grammar, early return here
        if !P::ERROR_USED {
            return false;
        }

        let mut extra_state_stack = Vec::new();
        let mut extra_precedence_stack = Vec::new();
        let error = P::TermClass::ERROR;
        let error_prec = error.precedence();
        let mut stack_len = self.precedence_stack.len();

        loop {
            match self.can_feed_impl(
                stack_len,
                &mut extra_state_stack,
                &mut extra_precedence_stack,
                parser,
                error,
                error_prec,
            ) {
                Some(true) => break true, // successfully shifted `error`
                Some(false) => {
                    if stack_len == 0 {
                        break false;
                    } else {
                        stack_len -= 1;
                    }
                }
                None => break false,
            }
            extra_state_stack.clear();
            extra_precedence_stack.clear();
        }
    }

    fn can_feed_impl<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        mut stack_len: usize,
        extra_state_stack: &mut Vec<StateIndex>,
        extra_precedence_stack: &mut Vec<Precedence>,
        parser: &P,
        class: P::TermClass,
        shift_prec: Precedence,
    ) -> Option<bool> {
        let shift_to = loop {
            let state = &parser.get_states()[extra_state_stack
                .last()
                .copied()
                .unwrap_or_else(|| self.state_stack[stack_len])
                .into_usize()];

            let shift = state.shift_goto_class(class);
            if let Some(reduce_rule) = state.reduce(class) {
                use super::super::state::ReduceRules;
                let reduce_rule = reduce_rule.to_iter().next().unwrap();
                let rule = &parser.get_rules()[reduce_rule.into_usize()];
                let tokens_len = rule.rule.len();

                let reduce_prec = match rule.precedence {
                    Some(crate::rule::Precedence::Fixed(level)) => Precedence::new(level as u8),
                    Some(crate::rule::Precedence::Dynamic(token_idx)) => {
                        // get token_idx'th precedence from back of precedence stack
                        let idx = stack_len + extra_precedence_stack.len() - tokens_len + token_idx;
                        if idx < stack_len {
                            self.precedence_stack[idx]
                        } else {
                            let idx = idx - stack_len;
                            extra_precedence_stack[idx]
                        }
                    }
                    None => Precedence::none(),
                };

                // resolve shift/reduce conflict by precedence
                if shift.is_some() {
                    let shift_prec = shift_prec.unwrap();
                    let reduce_prec = reduce_prec.unwrap();
                    use std::cmp::Ordering;
                    match reduce_prec.cmp(&shift_prec) {
                        Ordering::Less => {
                            // no reduce
                            break shift;
                        }
                        Ordering::Equal => {
                            // check for reduce type
                            use crate::rule::ReduceType;
                            match parser.precedence_types(reduce_prec) {
                                Some(ReduceType::Left) => {
                                    // no shift
                                    // shift = None;
                                }
                                Some(ReduceType::Right) => {
                                    // no reduce
                                    break shift;
                                }
                                None => {
                                    // error
                                    return None;
                                }
                            }
                        }
                        Ordering::Greater => {
                            // no shift
                            // shift = None;
                        }
                    }
                }

                // pop state stack
                // pop precedence stack
                if tokens_len <= extra_precedence_stack.len() {
                    extra_precedence_stack.truncate(extra_precedence_stack.len() - tokens_len);
                    extra_state_stack.truncate(extra_state_stack.len() - tokens_len);
                } else {
                    let left = tokens_len - extra_precedence_stack.len();
                    extra_precedence_stack.clear();
                    extra_state_stack.clear();
                    stack_len -= left;
                }

                extra_precedence_stack.push(reduce_prec);

                // shift with reduced nonterminal
                if let Some(next_state_id) = parser.get_states()[extra_state_stack
                    .last()
                    .copied()
                    .unwrap_or_else(|| self.state_stack[stack_len])
                    .into_usize()]
                .shift_goto_nonterm(rule.name)
                {
                    extra_state_stack.push(StateIndex::from_usize_unchecked(next_state_id.state));
                } else {
                    unreachable!(
                        "unreachable: nonterminal shift should always succeed after reduce operation. Failed to shift nonterminal '{}' in state {}.",
                        rule.name.as_str(),
                        extra_state_stack
                            .last()
                            .copied()
                            .unwrap_or_else(|| self.state_stack[stack_len])
                            .into_usize()
                    );
                }
            } else {
                break shift;
            }
        };

        // shift with terminal
        Some(shift_to.is_some())
    }

    fn feed_eof<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &mut self,
        parser: &P,
        userdata: &mut Data::UserData,
    ) -> Result<(), ParseError<Data::Term, Data::Location, Data::ReduceActionError>>
    where
        P::Term: Clone,
    {
        self.feed_location_impl(
            parser,
            TerminalSymbol::Eof,
            P::TermClass::EOF,
            Precedence::none(),
            userdata,
            None,
        )
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
    ) -> crate::hash::HashSet<Data::NonTerm>
    where
        P::NonTerm: Hash + Eq,
    {
        use crate::hash::HashSet;
        use crate::token::Token;

        let rules = parser.get_rules();
        let states = parser.get_states();

        let mut zero_shifted_rules = BTreeSet::new();
        let mut non_zero_shifted_rules = BTreeSet::new();
        {
            let last_state = &states[self.state_stack.last().unwrap().into_usize()];
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
            let state = &states[state_idx.into_usize()];
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

    /// Get backtrace information for current state.
    /// What current state is trying to parse, and where it comes from.
    pub fn backtrace<P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>>(
        &self,
        parser: &P,
    ) -> crate::Backtrace<P::TermClass, P::NonTerm>
    where
        P::NonTerm: Hash + Eq,
    {
        use crate::hash::HashSet;
        use crate::rule::ShiftedRule;
        use crate::rule::ShiftedRuleRef;
        use crate::Backtrace;
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
        let mut current_rules: BTreeSet<_> = parser.get_states()
            [self.state_stack.last().unwrap().into_usize()]
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
                for rule in parser.get_states()[state_idx.into_usize()]
                    .get_rules()
                    .iter()
                {
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

impl<Data: DataStack, StateIndex: Index + Copy> Default for Context<Data, StateIndex> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Data: DataStack, StateIndex: Index + Copy> Clone for Context<Data, StateIndex>
where
    Data: Clone,
    Data::Term: Clone,
    Data::NonTerm: Clone,
{
    fn clone(&self) -> Self {
        Context {
            state_stack: self.state_stack.clone(),
            data_stack: self.data_stack.clone(),
            location_stack: self.location_stack.clone(),
            precedence_stack: self.precedence_stack.clone(),

            #[cfg(feature = "tree")]
            tree_stack: self.tree_stack.clone(),
        }
    }
}

#[cfg(feature = "tree")]
impl<Data: DataStack, StateIndex: Index + Copy> std::fmt::Display for Context<Data, StateIndex>
where
    Data::Term: std::fmt::Display + Clone,
    Data::NonTerm: std::fmt::Display + Clone + NonTerminal,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_tree_list())
    }
}
#[cfg(feature = "tree")]
impl<Data: DataStack, StateIndex: Index + Copy> std::fmt::Debug for Context<Data, StateIndex>
where
    Data::Term: std::fmt::Debug + Clone,
    Data::NonTerm: std::fmt::Debug + Clone + NonTerminal,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.to_tree_list())
    }
}

#[cfg(feature = "tree")]
impl<Data: DataStack, StateIndex: Index + Copy> std::ops::Deref for Context<Data, StateIndex> {
    type Target = crate::tree::TreeList<Data::Term, Data::NonTerm>;
    fn deref(&self) -> &Self::Target {
        &self.tree_stack
    }
}

#[cfg(feature = "tree")]
impl<Data: DataStack, StateIndex: Index + Copy> std::ops::DerefMut for Context<Data, StateIndex> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.tree_stack
    }
}
