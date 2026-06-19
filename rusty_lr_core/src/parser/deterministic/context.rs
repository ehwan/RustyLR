use std::collections::BTreeSet;

use super::ParseError;
use crate::Location;

use crate::parser::data_stack::DataStack;
use crate::parser::nonterminal::NonTerminal;
use crate::parser::state::Index;
use crate::parser::terminalclass::TerminalClass;
use crate::parser::Parser;
use crate::parser::Precedence;
use crate::parser::State;
use crate::TerminalSymbol;

/// A struct that maintains the current state and the values associated with each symbol.
pub struct Context<
    P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>,
    Data: DataStack,
    StateIndex,
> {
    /// stacks hold the values associated with each shifted symbol.
    pub state_stack: Vec<StateIndex>,
    pub(crate) data_stack: Data,
    pub(crate) location_stack: Vec<Data::Location>,
    pub(crate) precedence_stack: Vec<Precedence>,
    pub(crate) userdata: Data::UserData,
    /// Tree stack for tree representation of the parse.
    #[cfg(feature = "tree")]
    pub(crate) tree_stack: crate::tree::TreeList<Data::Term, Data::NonTerm>,
    pub(crate) _phantom: std::marker::PhantomData<P>,
}

impl<
        P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>,
        Data: DataStack,
        StateIndex: Index + Copy,
    > Context<P, Data, StateIndex>
{
    /// Create a new context.
    /// `state_stack` is initialized with [0] (root state).
    pub fn new(userdata: Data::UserData) -> Self {
        Context {
            state_stack: vec![StateIndex::from_usize_unchecked(0)],

            data_stack: Default::default(),
            location_stack: Vec::new(),
            precedence_stack: Vec::new(),
            userdata,

            #[cfg(feature = "tree")]
            tree_stack: crate::tree::TreeList::new(),
            _phantom: std::marker::PhantomData,
        }
    }
    /// Create a new context using `Default::default()` as user data.
    pub fn with_default_userdata() -> Self
    where
        Data::UserData: Default,
    {
        Self::new(Default::default())
    }
    /// Create a new context with given capacity of `state_stack` and `data_stack`.
    /// `state_stack` is initialized with [0] (root state).
    pub fn with_capacity(capacity: usize, userdata: Data::UserData) -> Self {
        let mut state_stack = Vec::with_capacity(capacity);
        state_stack.push(
            StateIndex::from_usize_unchecked(0), // root state
        );
        Context {
            state_stack,

            data_stack: Data::with_capacity(capacity),
            location_stack: Vec::with_capacity(capacity),
            precedence_stack: Vec::with_capacity(capacity),
            userdata,

            #[cfg(feature = "tree")]
            tree_stack: crate::tree::TreeList::new(),
            _phantom: std::marker::PhantomData,
        }
    }
    /// Create a new context with capacity using `Default::default()` as user data.
    pub fn with_capacity_and_default_userdata(capacity: usize) -> Self
    where
        Data::UserData: Default,
    {
        Self::with_capacity(capacity, Default::default())
    }
    /// Borrow the user data owned by this context.
    pub fn userdata(&self) -> &Data::UserData {
        &self.userdata
    }

    /// Borrow the user data owned by this context as an iterator.
    pub fn userdata_all(&self) -> impl Iterator<Item = &Data::UserData> {
        std::iter::once(&self.userdata)
    }

    /// Mutably borrow the user data owned by this context.
    pub fn userdata_mut(&mut self) -> &mut Data::UserData {
        &mut self.userdata
    }

    /// Mutably borrow the user data owned by this context as an iterator.
    pub fn userdata_all_mut(&mut self) -> impl Iterator<Item = &mut Data::UserData> {
        std::iter::once(&mut self.userdata)
    }

    /// End this context and pop the value of the start symbol from the data stack.
    pub fn accept(
        mut self,
    ) -> Result<
        (Data::StartType, Data::UserData),
        ParseError<Data::Term, Data::Location, Data::ReduceActionError>,
    >
    where
        Data::Term: Clone,
        Data::NonTerm: std::fmt::Debug,
        P::State: State<StateIndex = StateIndex>,
    {
        self.feed_eof()?;

        // data_stack must be <Start> in this point
        Ok((self.data_stack.pop_start().unwrap(), self.userdata))
    }

    /// End this context and return an iterator with the start symbol and final user data.
    pub fn accept_all(
        self,
    ) -> Result<
        impl Iterator<Item = (Data::StartType, Data::UserData)>,
        ParseError<Data::Term, Data::Location, Data::ReduceActionError>,
    >
    where
        Data::Term: Clone,
        Data::NonTerm: std::fmt::Debug,
        P::State: State<StateIndex = StateIndex>,
    {
        Ok(std::iter::once(self.accept()?))
    }

    /// Check if current context can be terminated and get the value of the start symbol.
    pub fn can_accept(&self) -> bool
    where
        P::State: State<StateIndex = StateIndex>,
    {
        let mut extra_state_stack = Vec::new();
        let mut extra_precedence_stack = Vec::new();

        self.can_feed_impl(
            self.precedence_stack.len(),
            &mut extra_state_stack,
            &mut extra_precedence_stack,
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
    pub fn expected_token(&self) -> (BTreeSet<P::TermClass>, BTreeSet<P::NonTerm>)
    where
        P::TermClass: Ord,
        P::NonTerm: Ord,
        P::State: State<StateIndex = StateIndex>,
    {
        let mut terms = BTreeSet::new();
        let mut nonterms = BTreeSet::new();
        let mut extra_state_stack = Vec::new();
        self.expected_token_impl(
            &mut extra_state_stack,
            self.precedence_stack.len(),
            &mut terms,
            &mut nonterms,
        );

        (terms, nonterms)
    }
    /// Same as `expected_token()`, but returns as printable type.
    pub fn expected_token_str<'a>(
        &self,
    ) -> (
        impl Iterator<Item = &'static str>,
        impl Iterator<Item = &'static str>,
    )
    where
        P::TermClass: Ord,
        P::NonTerm: Ord,
        P::State: State<StateIndex = StateIndex>,
    {
        let (terms, nonterms) = self.expected_token();
        (
            terms.into_iter().map(|term| term.as_str()),
            nonterms.into_iter().map(|nonterm| nonterm.as_str()),
        )
    }

    fn expected_token_impl(
        &self,
        extra_state_stack: &mut Vec<StateIndex>,
        stack_len: usize,
        terms: &mut BTreeSet<P::TermClass>,
        nonterms: &mut BTreeSet<P::NonTerm>,
    ) where
        P::TermClass: Ord,
        P::NonTerm: Ord,
        P::State: State<StateIndex = StateIndex>,
    {
        let state = &P::get_states()[extra_state_stack
            .last()
            .copied()
            .unwrap_or_else(|| self.state_stack[stack_len])
            .into_usize()];

        terms.extend(state.expected_shift_term());
        nonterms.extend(state.expected_shift_nonterm());

        let mut reduce_nonterms = BTreeSet::new();
        for reduce_rule in state.expected_reduce_rule() {
            let prod_rule = &P::get_rules()[reduce_rule.into_usize()];
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
            let state = &P::get_states()[extra_state_stack
                .last()
                .copied()
                .unwrap_or_else(|| self.state_stack[stack_len])
                .into_usize()];
            if let Some(next_state) = state.shift_goto_nonterm(nonterm) {
                extra_state_stack.push(next_state.state);
                self.expected_token_impl(&mut extra_state_stack, stack_len, terms, nonterms);
            }
        }
    }

    /// Feed one terminal to parser, and update stacks.
    /// This will use `Default::default()` for location.
    pub fn feed(
        &mut self,
        term: Data::Term,
    ) -> Result<(), ParseError<Data::Term, Data::Location, Data::ReduceActionError>>
    where
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
        P::State: State<StateIndex = StateIndex>,
        Data::Location: Default,
    {
        self.feed_location(term, Default::default())
    }

    /// Feed one terminal with location to parser, and update stacks.
    pub fn feed_location(
        &mut self,
        term: P::Term,
        location: Data::Location,
    ) -> Result<(), ParseError<Data::Term, Data::Location, Data::ReduceActionError>>
    where
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
        P::State: State<StateIndex = StateIndex>,
    {
        use crate::Location;
        let class = P::TermClass::from_term(&term);
        let shift_prec = class.precedence();

        match self.feed_location_impl(TerminalSymbol::Term(term), class, shift_prec, location) {
            Ok(()) => Ok(()),
            Err(ParseError::NoAction(err)) => {
                // nothing shifted; enters panic mode

                // if `error` token was not used in the grammar, early return here
                if !P::ERROR_USED {
                    return Err(ParseError::NoAction(err));
                }

                let error_prec = P::TermClass::ERROR.precedence();

                let mut extra_state_stack = Vec::new();
                let mut extra_precedence_stack = Vec::new();

                use crate::TriState;
                let mut pop_count = 0;
                let mut found = false;
                for &s in self.state_stack.iter().rev() {
                    match P::get_states()[s.into_usize()].can_accept_error() {
                        TriState::False => {}
                        TriState::Maybe => {
                            extra_precedence_stack.clear();
                            extra_state_stack.clear();

                            if self.can_feed_impl(
                                self.precedence_stack.len() - pop_count,
                                &mut extra_state_stack,
                                &mut extra_precedence_stack,
                                P::TermClass::ERROR,
                                error_prec,
                            ) == Some(true)
                            {
                                found = true;
                                break;
                            }
                        }
                        TriState::True => {
                            found = true;
                            break;
                        }
                    }

                    pop_count += 1;
                }

                if !found {
                    return Err(ParseError::NoAction(err));
                }

                let error_location =
                    Data::Location::new(self.location_stack.iter().rev(), pop_count);
                self.location_stack
                    .truncate(self.location_stack.len() - pop_count);
                self.state_stack
                    .truncate(self.state_stack.len() - pop_count);
                self.data_stack
                    .truncate(self.precedence_stack.len() - pop_count);
                self.precedence_stack
                    .truncate(self.precedence_stack.len() - pop_count);

                #[cfg(feature = "tree")]
                {
                    let l = self.tree_stack.len() - pop_count;
                    self.tree_stack.truncate(l);
                }

                match self.feed_location_impl(
                    TerminalSymbol::Error,
                    P::TermClass::ERROR,
                    error_prec,
                    error_location,
                ) {
                    Ok(()) => {
                        // try shift given term again
                        // to check if the given terminal should be merged with `error` token
                        // or it can be shift right after the error token
                        if let Some(next_state) = P::get_states()
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

                            self.location_stack.push(err.location.clone());
                            self.precedence_stack.push(shift_prec);
                            self.state_stack.push(next_state.state);
                        } else {
                            // merge term with previous error

                            let error_location = Data::Location::new(
                                std::iter::once(&err.location)
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
                    Err(_) => Err(ParseError::NoAction(err)), // other errors
                }
            }
            Err(err) => Err(err),
        }
    }

    fn feed_location_impl(
        &mut self,
        term: TerminalSymbol<P::Term>,
        class: P::TermClass,
        shift_prec: Precedence,
        location: Data::Location,
    ) -> Result<(), ParseError<Data::Term, Data::Location, Data::ReduceActionError>>
    where
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
        P::State: State<StateIndex = StateIndex>,
    {
        use super::super::state::ReduceRules;
        use crate::Location;

        let shift_to = loop {
            let state = &P::get_states()[self.state_stack.last().unwrap().into_usize()];

            let shift = state.shift_goto_class(class);
            if let Some(reduce_rule) = state.reduce(class) {
                let reduce_rule = reduce_rule.to_iter().next().unwrap();
                let rule = &P::get_rules()[reduce_rule.into_usize()];
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
                            match P::precedence_types(reduce_prec) {
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

                let Some(next_nonterm_shift) = P::get_states()
                    [self.state_stack.last().unwrap().into_usize()]
                .shift_goto_nonterm(rule.name) else {
                    unreachable!(
                        "Failed to shift nonterminal: {:?} in state {}",
                        rule.name,
                        self.state_stack.last().unwrap().into_usize()
                    );
                };

                // call reduce action
                match Data::reduce_action(
                    &mut self.data_stack,
                    &mut self.location_stack,
                    next_nonterm_shift.push,
                    reduce_rule.into_usize(),
                    &mut shift,
                    &term,
                    &mut self.userdata,
                    &mut new_location,
                ) {
                    Ok(_) => {}
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
                self.state_stack.push(next_nonterm_shift.state);
            } else {
                break shift;
            }
        };

        // shift with terminal
        if let Some(next_state_id) = shift_to {
            self.state_stack.push(next_state_id.state);

            #[cfg(feature = "tree")]
            self.tree_stack
                .push(crate::tree::Tree::new_terminal(term.clone()));

            if next_state_id.push {
                match term {
                    TerminalSymbol::Term(t) => self.data_stack.push_terminal(t),
                    TerminalSymbol::Error | TerminalSymbol::Eof => self.data_stack.push_empty(),
                }
            } else {
                match term {
                    TerminalSymbol::Term(_) | TerminalSymbol::Error | TerminalSymbol::Eof => {
                        self.data_stack.push_empty()
                    }
                }
            }

            self.location_stack.push(location);
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
    pub fn can_feed(&self, term: &Data::Term) -> bool
    where
        P::State: State<StateIndex = StateIndex>,
    {
        let mut extra_state_stack = Vec::new();
        let mut extra_precedence_stack = Vec::new();
        let class = P::TermClass::from_term(term);
        let shift_prec = class.precedence();

        self.can_feed_impl(
            self.precedence_stack.len(),
            &mut extra_state_stack,
            &mut extra_precedence_stack,
            class,
            shift_prec,
        ) == Some(true)
    }

    /// Check if current context can enter panic mode
    pub fn can_panic(&self) -> bool
    where
        P::State: State<StateIndex = StateIndex>,
    {
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

    fn can_feed_impl(
        &self,
        mut stack_len: usize,
        extra_state_stack: &mut Vec<StateIndex>,
        extra_precedence_stack: &mut Vec<Precedence>,
        class: P::TermClass,
        shift_prec: Precedence,
    ) -> Option<bool>
    where
        P::State: State<StateIndex = StateIndex>,
    {
        let shift_to = loop {
            let state = &P::get_states()[extra_state_stack
                .last()
                .copied()
                .unwrap_or_else(|| self.state_stack[stack_len])
                .into_usize()];

            let shift = state.shift_goto_class(class);
            if let Some(reduce_rule) = state.reduce(class) {
                use super::super::state::ReduceRules;
                let reduce_rule = reduce_rule.to_iter().next().unwrap();
                let rule = &P::get_rules()[reduce_rule.into_usize()];
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
                            match P::precedence_types(reduce_prec) {
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
                if let Some(next_state_id) = P::get_states()[extra_state_stack
                    .last()
                    .copied()
                    .unwrap_or_else(|| self.state_stack[stack_len])
                    .into_usize()]
                .shift_goto_nonterm(rule.name)
                {
                    extra_state_stack.push(next_state_id.state);
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

    fn feed_eof(
        &mut self,
    ) -> Result<(), ParseError<Data::Term, Data::Location, Data::ReduceActionError>>
    where
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
        P::State: State<StateIndex = StateIndex>,
    {
        let eof_location = Data::Location::new(self.location_stack.iter().rev(), 0);
        self.feed_location_impl(
            TerminalSymbol::Eof,
            P::TermClass::EOF,
            Precedence::none(),
            eof_location,
        )
    }
}

impl<
        P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>,
        Data: DataStack,
        StateIndex: Index + Copy,
    > Default for Context<P, Data, StateIndex>
where
    Data::UserData: Default,
{
    fn default() -> Self {
        Self::with_default_userdata()
    }
}

impl<
        P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>,
        Data: DataStack,
        StateIndex: Index + Copy,
    > Clone for Context<P, Data, StateIndex>
where
    Data: Clone,
    Data::Term: Clone,
    Data::NonTerm: Clone,
    Data::UserData: Clone,
{
    fn clone(&self) -> Self {
        Context {
            state_stack: self.state_stack.clone(),
            data_stack: self.data_stack.clone(),
            location_stack: self.location_stack.clone(),
            precedence_stack: self.precedence_stack.clone(),
            userdata: self.userdata.clone(),

            #[cfg(feature = "tree")]
            tree_stack: self.tree_stack.clone(),
            _phantom: std::marker::PhantomData,
        }
    }
}

#[cfg(feature = "tree")]
impl<
        P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>,
        Data: DataStack,
        StateIndex: Index + Copy,
    > std::fmt::Display for Context<P, Data, StateIndex>
where
    Data::Term: std::fmt::Display + Clone,
    Data::NonTerm: std::fmt::Display + Clone + NonTerminal,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_tree_list())
    }
}
#[cfg(feature = "tree")]
impl<
        P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>,
        Data: DataStack,
        StateIndex: Index + Copy,
    > std::fmt::Debug for Context<P, Data, StateIndex>
where
    Data::Term: std::fmt::Debug + Clone,
    Data::NonTerm: std::fmt::Debug + Clone + NonTerminal,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.to_tree_list())
    }
}

#[cfg(feature = "tree")]
impl<
        P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>,
        Data: DataStack,
        StateIndex: Index + Copy,
    > std::ops::Deref for Context<P, Data, StateIndex>
{
    type Target = crate::tree::TreeList<Data::Term, Data::NonTerm>;
    fn deref(&self) -> &Self::Target {
        &self.tree_stack
    }
}

#[cfg(feature = "tree")]
impl<
        P: Parser<Term = Data::Term, NonTerm = Data::NonTerm>,
        Data: DataStack,
        StateIndex: Index + Copy,
    > std::ops::DerefMut for Context<P, Data, StateIndex>
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.tree_stack
    }
}
