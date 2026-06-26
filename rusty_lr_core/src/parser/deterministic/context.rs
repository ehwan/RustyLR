use std::collections::BTreeSet;

use super::ParseError;
use crate::Location;

use crate::parser::nonterminal::NonTerminal;
use crate::parser::semantic_value::SemanticValue;
use crate::parser::table::Index;
use crate::parser::table::ParserTables;
use crate::parser::terminalclass::TerminalClass;
use crate::parser::Parser;

use crate::TerminalSymbol;

/// A struct that maintains the current state and the values associated with each symbol.
pub struct Context<
    P: Parser<Term = Data::Term, NonTerm = Data::NonTerm, StateIndex = StateIndex>,
    Data: SemanticValue,
    StateIndex,
> {
    /// stacks hold the values associated with each shifted symbol.
    pub state_stack: Vec<StateIndex>,
    pub(crate) data_stack: Vec<Data>,
    pub(crate) location_stack: Vec<Data::Location>,
    pub(crate) userdata: Data::UserData,
    /// Decoded parser tables initialized when the context is created.
    ///
    /// Keeping this reference in the context avoids repeated `Parser::get_tables()` calls in the
    /// token-feeding hot path.
    pub(crate) tables: &'static P::Tables,
    /// Tree stack for tree representation of the parse.
    #[cfg(feature = "tree")]
    pub(crate) tree_stack: crate::tree::TreeList<Data::Term, Data::NonTerm>,
    pub(crate) _phantom: std::marker::PhantomData<P>,
}

impl<
        P: Parser<Term = Data::Term, NonTerm = Data::NonTerm, StateIndex = StateIndex>,
        Data: SemanticValue,
        StateIndex: Index + Copy,
    > Context<P, Data, StateIndex>
{
    /// Create a new context.
    /// `state_stack` is initialized with [0] (root state).
    pub fn new(userdata: Data::UserData) -> Self {
        Context {
            state_stack: vec![StateIndex::from_usize_unchecked(0)],

            data_stack: Vec::new(),
            location_stack: Vec::new(),
            userdata,
            tables: P::get_tables(),

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
    /// Create a new context with a virtual start branch.
    pub fn new_with_branch(userdata: Data::UserData, branch_idx: u32) -> Self
    where
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
    {
        let mut ctx = Self::new(userdata);
        let class = P::TermClass::from_virtual_start(branch_idx);
        let shift_to = ctx.tables.shift_goto_class(0, class).unwrap_or_else(|| {
            panic!(
                "Failed to resolve shift for virtual start branch {}",
                branch_idx
            )
        });
        ctx.state_stack.push(shift_to.state);
        ctx.data_stack.push(Data::new_empty());
        ctx.location_stack
            .push(Data::Location::new(std::iter::empty(), 0));
        #[cfg(feature = "tree")]
        {
            ctx.tree_stack.push(crate::tree::Tree::new_terminal(
                TerminalSymbol::VirtualStart(branch_idx),
            ));
        }
        ctx
    }
    /// Create a new context with a virtual start branch using `Default::default()` as user data.
    pub fn with_default_userdata_and_branch(branch_idx: u32) -> Self
    where
        Data::UserData: Default,
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
    {
        Self::new_with_branch(Default::default(), branch_idx)
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

            data_stack: Vec::with_capacity(capacity),
            location_stack: Vec::with_capacity(capacity),
            userdata,
            tables: P::get_tables(),

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
    /// Create a new context with capacity and a virtual start branch.
    pub fn with_capacity_and_branch(
        capacity: usize,
        userdata: Data::UserData,
        branch_idx: u32,
    ) -> Self
    where
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
    {
        let mut ctx = Self::with_capacity(capacity, userdata);
        let class = P::TermClass::from_virtual_start(branch_idx);
        let shift_to = ctx.tables.shift_goto_class(0, class).unwrap_or_else(|| {
            panic!(
                "Failed to resolve shift for virtual start branch {}",
                branch_idx
            )
        });
        ctx.state_stack.push(shift_to.state);
        ctx.data_stack.push(Data::new_empty());
        ctx.location_stack
            .push(Data::Location::new(std::iter::empty(), 0));
        #[cfg(feature = "tree")]
        {
            ctx.tree_stack.push(crate::tree::Tree::new_terminal(
                TerminalSymbol::VirtualStart(branch_idx),
            ));
        }
        ctx
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

    /// Consume this context and return the semantic value stack with final user data.
    pub fn into_data_stack(self) -> (Vec<Data>, Data::UserData) {
        (self.data_stack, self.userdata)
    }

    /// End this context and pop the value of the start symbol from the data stack.
    pub fn accept(
        mut self,
    ) -> Result<
        (Vec<Data>, Data::UserData),
        ParseError<Data::Term, Data::Location, Data::ReduceActionError>,
    >
    where
        Data::Term: Clone,
        Data::NonTerm: std::fmt::Debug,
    {
        self.feed_eof()?;

        Ok(self.into_data_stack())
    }

    /// End this context and return an iterator with the start symbol and final user data.
    pub fn accept_all(
        self,
    ) -> Result<
        impl Iterator<Item = (Vec<Data>, Data::UserData)>,
        ParseError<Data::Term, Data::Location, Data::ReduceActionError>,
    >
    where
        Data::Term: Clone,
        Data::NonTerm: std::fmt::Debug,
    {
        Ok(std::iter::once(self.accept()?))
    }

    pub fn can_accept(&self) -> bool {
        let mut extra_state_stack = Vec::new();

        self.can_feed_impl(
            self.state_stack.len() - 1,
            &mut extra_state_stack,
            P::TermClass::EOF,
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
    {
        let mut terms = BTreeSet::new();
        let mut nonterms = BTreeSet::new();
        let mut extra_state_stack = Vec::new();
        self.expected_token_impl(
            &mut extra_state_stack,
            self.state_stack.len() - 1,
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
    {
        let state = extra_state_stack
            .last()
            .copied()
            .unwrap_or_else(|| self.state_stack[stack_len])
            .into_usize();

        terms.extend(self.tables.expected_shift_term(state));
        nonterms.extend(self.tables.expected_shift_nonterm(state));

        let mut reduce_nonterms = BTreeSet::new();
        for reduce_rule in self.tables.expected_reduce_rule(state) {
            let prod_rule = self.tables.rule(reduce_rule.into_usize());
            reduce_nonterms.insert((prod_rule.len, prod_rule.lhs));
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
            let state = extra_state_stack
                .last()
                .copied()
                .unwrap_or_else(|| self.state_stack[stack_len])
                .into_usize();
            if let Some(next_state) = self.tables.shift_goto_nonterm(state, nonterm) {
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
    {
        use crate::Location;
        let class = P::TermClass::from_term(&term);

        match self.feed_location_impl(TerminalSymbol::Terminal(term), class, location) {
            Ok(()) => Ok(()),
            Err(ParseError::NoAction(err)) => {
                // nothing shifted; enters panic mode

                // if `error` token was not used in the grammar, early return here
                if !P::ERROR_USED {
                    return Err(ParseError::NoAction(err));
                }

                let mut extra_state_stack = Vec::new();

                use crate::TriState;
                let mut pop_count = 0;
                let mut found = false;
                for &s in self.state_stack.iter().rev() {
                    match self.tables.can_accept_error(s.into_usize()) {
                        TriState::False => {}
                        TriState::Maybe => {
                            extra_state_stack.clear();

                            if self.can_feed_impl(
                                self.state_stack.len() - pop_count - 1,
                                &mut extra_state_stack,
                                P::TermClass::ERROR,
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
                self.data_stack
                    .truncate(self.state_stack.len() - pop_count - 1);
                self.state_stack
                    .truncate(self.state_stack.len() - pop_count);

                #[cfg(feature = "tree")]
                {
                    let l = self.tree_stack.len() - pop_count;
                    self.tree_stack.truncate(l);
                }

                match self.feed_location_impl(
                    TerminalSymbol::Error,
                    P::TermClass::ERROR,
                    error_location,
                ) {
                    Ok(()) => {
                        // try shift given term again
                        // to check if the given terminal should be merged with `error` token
                        // or it can be shift right after the error token
                        if let Some(next_state) = self
                            .tables
                            .shift_goto_class(self.state_stack.last().unwrap().into_usize(), class)
                        {
                            #[cfg(feature = "tree")]
                            self.tree_stack
                                .push(crate::tree::Tree::new_terminal(err.term.clone()));

                            // shift after `error` token
                            if next_state.push {
                                self.data_stack
                                    .push(Data::new_terminal(err.term.into_term().unwrap()));
                            } else {
                                self.data_stack.push(Data::new_empty());
                            }

                            self.location_stack.push(err.location.clone());
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
        location: Data::Location,
    ) -> Result<(), ParseError<Data::Term, Data::Location, Data::ReduceActionError>>
    where
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
    {
        use super::super::table::ReduceRules;
        use crate::Location;

        let shift_to = loop {
            let state = self.state_stack.last().unwrap().into_usize();

            let (shift, reduce) = match self.tables.term_action(state, class) {
                Some(action) => (action.shift(), action.reduce()),
                None => (None, None),
            };
            if let Some(reduce_rule) = reduce {
                let reduce_rule = reduce_rule.to_iter().next().unwrap();
                let rule = *self.tables.rule(reduce_rule.into_usize());
                let tokens_len = rule.len;

                // pop state stack
                self.state_stack
                    .truncate(self.state_stack.len() - tokens_len);

                let mut shift = false;

                let mut new_location =
                    Data::Location::new(self.location_stack.iter().rev(), tokens_len);

                let Some(next_nonterm_shift) = self
                    .tables
                    .shift_goto_nonterm(self.state_stack.last().unwrap().into_usize(), rule.lhs)
                else {
                    unreachable!(
                        "Failed to shift nonterminal: {:?} in state {}",
                        rule.lhs,
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
                    let mut children = Vec::with_capacity(rule.len);
                    for _ in 0..rule.len {
                        let tree = self.tree_stack.pop().unwrap();
                        children.push(tree);
                    }
                    children.reverse();

                    self.tree_stack.push(crate::tree::Tree::new_nonterminal(
                        rule.lhs.clone(),
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
                    TerminalSymbol::Terminal(t) => self.data_stack.push(Data::new_terminal(t)),
                    TerminalSymbol::Error
                    | TerminalSymbol::Eof
                    | TerminalSymbol::VirtualStart(_) => self.data_stack.push(Data::new_empty()),
                }
            } else {
                match term {
                    TerminalSymbol::Terminal(_)
                    | TerminalSymbol::Error
                    | TerminalSymbol::Eof
                    | TerminalSymbol::VirtualStart(_) => self.data_stack.push(Data::new_empty()),
                }
            }

            self.location_stack.push(location);

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
    pub fn can_feed(&self, term: &Data::Term) -> bool {
        let mut extra_state_stack = Vec::new();
        let class = P::TermClass::from_term(term);

        self.can_feed_impl(self.state_stack.len() - 1, &mut extra_state_stack, class) == Some(true)
    }

    /// Check if current context can enter panic mode
    pub fn can_panic(&self) -> bool {
        // if `error` token was not used in the grammar, early return here
        if !P::ERROR_USED {
            return false;
        }

        let mut extra_state_stack = Vec::new();
        let error = P::TermClass::ERROR;
        let mut stack_len = self.state_stack.len() - 1;

        loop {
            match self.can_feed_impl(stack_len, &mut extra_state_stack, error) {
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
        }
    }

    fn can_feed_impl(
        &self,
        mut stack_len: usize,
        extra_state_stack: &mut Vec<StateIndex>,
        class: P::TermClass,
    ) -> Option<bool> {
        let shift_to = loop {
            let state = extra_state_stack
                .last()
                .copied()
                .unwrap_or_else(|| self.state_stack[stack_len])
                .into_usize();

            let (shift, reduce) = match self.tables.term_action(state, class) {
                Some(action) => (action.shift(), action.reduce()),
                None => (None, None),
            };
            if let Some(reduce_rule) = reduce {
                use super::super::table::ReduceRules;
                let reduce_rule = reduce_rule.to_iter().next().unwrap();
                let rule = *self.tables.rule(reduce_rule.into_usize());
                let tokens_len = rule.len;

                // pop state stack
                if tokens_len <= extra_state_stack.len() {
                    extra_state_stack.truncate(extra_state_stack.len() - tokens_len);
                } else {
                    let left = tokens_len - extra_state_stack.len();
                    extra_state_stack.clear();
                    stack_len -= left;
                }

                // shift with reduced nonterminal
                let last_state = extra_state_stack
                    .last()
                    .copied()
                    .unwrap_or_else(|| self.state_stack[stack_len])
                    .into_usize();
                if let Some(next_state_id) = self.tables.shift_goto_nonterm(last_state, rule.lhs) {
                    extra_state_stack.push(next_state_id.state);
                } else {
                    unreachable!(
                        "unreachable: nonterminal shift should always succeed after reduce operation. Failed to shift nonterminal '{}' in state {}.",
                        rule.lhs.as_str(),
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

    pub fn feed_eof(
        &mut self,
    ) -> Result<(), ParseError<Data::Term, Data::Location, Data::ReduceActionError>>
    where
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
    {
        let eof_location = Data::Location::new(self.location_stack.iter().rev(), 0);
        self.feed_location_impl(TerminalSymbol::Eof, P::TermClass::EOF, eof_location)
    }
}

impl<
        P: Parser<Term = Data::Term, NonTerm = Data::NonTerm, StateIndex = StateIndex>,
        Data: SemanticValue,
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
        P: Parser<Term = Data::Term, NonTerm = Data::NonTerm, StateIndex = StateIndex>,
        Data: SemanticValue,
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
            userdata: self.userdata.clone(),
            tables: self.tables,

            #[cfg(feature = "tree")]
            tree_stack: self.tree_stack.clone(),
            _phantom: std::marker::PhantomData,
        }
    }
}

#[cfg(feature = "tree")]
impl<
        P: Parser<Term = Data::Term, NonTerm = Data::NonTerm, StateIndex = StateIndex>,
        Data: SemanticValue,
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
        P: Parser<Term = Data::Term, NonTerm = Data::NonTerm, StateIndex = StateIndex>,
        Data: SemanticValue,
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
        P: Parser<Term = Data::Term, NonTerm = Data::NonTerm, StateIndex = StateIndex>,
        Data: SemanticValue,
        StateIndex: Index + Copy,
    > std::ops::DerefMut for Context<P, Data, StateIndex>
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.tree_stack
    }
}
