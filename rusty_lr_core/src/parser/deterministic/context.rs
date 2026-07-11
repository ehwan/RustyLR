use std::cell::RefCell;
use std::collections::BTreeSet;

use super::ParseError;
use crate::Location;

use crate::parser::Parser;
use crate::parser::nonterminal::NonTerminal;
use crate::parser::semantic_value::SemanticValue;
use crate::parser::semantic_value::StartExtractor;
use crate::parser::table::Index;
use crate::parser::table::ParserTables;
use crate::parser::terminalclass::TerminalClass;

use crate::TerminalSymbol;

/// One deterministic reduction captured during pre-feed simulation.
///
/// This stores the production and goto decision so the commit phase does not re-query the LR table
/// for the same reduce step.
struct PlannedReduction<NonTerm, StateIndex> {
    rule_index: usize,
    tokens_len: usize,
    #[allow(dead_code)]
    lhs: NonTerm,
    next_nonterm_shift: crate::parser::table::ShiftTarget<StateIndex>,
}

/// Commit record produced by the deterministic pre-feed CFG simulation.
///
/// `feed_location` builds this before mutating semantic stacks. If terminal shift is impossible,
/// no reduce action has run and the parser stack is left unchanged.
struct FeedPlan<NonTerm, StateIndex> {
    reductions: Vec<PlannedReduction<NonTerm, StateIndex>>,
    terminal_shift: crate::parser::table::ShiftTarget<StateIndex>,
}

/// Minimal syntax failure captured by the simulation before the original terminal is moved.
///
/// Keeping only the failed state lets the caller build `NoAction` without cloning the terminal
/// during planning.
struct FeedPlanError {
    state: usize,
}

struct NoActionFailure<Term, Location> {
    term: TerminalSymbol<Term>,
    location: Location,
    state: usize,
}

enum FeedLocationError<Term, Location, ReduceAction, UserData> {
    NoAction(NoActionFailure<Term, Location>),
    ReduceAction(ParseError<Term, Location, ReduceAction, UserData>),
}

/// A struct that maintains the current state and the values associated with each symbol.
pub struct Context<
    P: Parser<Term = Data::Term, NonTerm = Data::NonTerm, StateIndex = StateIndex>,
    Data: SemanticValue,
    Start: StartExtractor<Data>,
    StateIndex,
> {
    /// stacks hold the values associated with each shifted symbol.
    pub state_stack: Vec<StateIndex>,
    pub(crate) data_stack: Vec<Data>,
    pub(crate) location_stack: Vec<Data::Location>,
    /// User data owned by this deterministic parse path.
    ///
    /// `NoAction` is detected before semantic actions run, so it leaves this value in place and
    /// the context can be fed again. A reduce-action error may leave semantic state partially
    /// mutated, so it moves this value into `ParseError::ReduceAction` and consumes the context.
    pub(crate) userdata: Option<Data::UserData>,
    /// Decoded parser tables initialized when the context is created.
    ///
    /// Keeping this reference in the context avoids repeated `Parser::get_tables()` calls in the
    /// token-feeding hot path.
    pub(crate) tables: &'static P::Tables,
    /// Reusable simulation stack for deterministic pre-feed planning.
    ///
    /// `can_feed` only has `&self`, so this scratch buffer uses interior mutability to avoid
    /// allocating a fresh `Vec` for each grammatical simulation.
    pub(crate) feed_extra_state_stack: RefCell<Vec<StateIndex>>,
    /// Tree stack for tree representation of the parse.
    #[cfg(feature = "tree")]
    pub(crate) tree_stack: crate::tree::TreeList<Data::Term, Data::NonTerm>,
    pub(crate) _phantom: std::marker::PhantomData<(P, Start)>,
}

impl<
    P: Parser<Term = Data::Term, NonTerm = Data::NonTerm, StateIndex = StateIndex>,
    Data: SemanticValue,
    Start: StartExtractor<Data>,
    StateIndex: Index + Copy,
> Context<P, Data, Start, StateIndex>
{
    /// Create a new context.
    /// The state stack is initialized with the selected start state.
    pub fn new(userdata: Data::UserData) -> Self {
        P::__assert_rusty_lr_parser_version_compatible();
        let mut ctx = Context {
            state_stack: Vec::new(),

            data_stack: Vec::new(),
            location_stack: Vec::new(),
            userdata: Some(userdata),
            tables: P::get_tables(),
            feed_extra_state_stack: RefCell::new(Vec::new()),

            #[cfg(feature = "tree")]
            tree_stack: crate::tree::TreeList::new(),
            _phantom: std::marker::PhantomData,
        };
        ctx.init_start_branch(Start::BRANCH_INDEX);
        ctx
    }
    /// Create a new context using `Default::default()` as user data.
    pub fn with_default_userdata() -> Self
    where
        Data::UserData: Default,
    {
        Self::new(Default::default())
    }
    fn init_start_branch(&mut self, branch_idx: u32) {
        let class = P::TermClass::from_virtual_start(branch_idx);
        let shift_to = self.tables.shift_goto_class(0, class).unwrap_or_else(|| {
            panic!(
                "Failed to resolve shift for virtual start branch {}",
                branch_idx
            )
        });
        self.state_stack.push(shift_to.state);
        self.data_stack.push(Data::new_empty());
        self.location_stack
            .push(Data::Location::new(std::iter::empty(), 0));
        #[cfg(feature = "tree")]
        {
            self.tree_stack.push(crate::tree::Tree::new_terminal(
                TerminalSymbol::VirtualStart(branch_idx),
            ));
        }
    }
    /// Create a new context with given capacity of `state_stack` and `data_stack`.
    /// The state stack is initialized with the selected start state.
    pub fn with_capacity(capacity: usize, userdata: Data::UserData) -> Self {
        P::__assert_rusty_lr_parser_version_compatible();
        let mut ctx = Context {
            state_stack: Vec::with_capacity(capacity),

            data_stack: Vec::with_capacity(capacity),
            location_stack: Vec::with_capacity(capacity),
            userdata: Some(userdata),
            tables: P::get_tables(),
            feed_extra_state_stack: RefCell::new(Vec::with_capacity(capacity)),

            #[cfg(feature = "tree")]
            tree_stack: crate::tree::TreeList::new(),
            _phantom: std::marker::PhantomData,
        };
        ctx.init_start_branch(Start::BRANCH_INDEX);
        ctx
    }
    /// Create a new context with capacity using `Default::default()` as user data.
    pub fn with_capacity_and_default_userdata(capacity: usize) -> Self
    where
        Data::UserData: Default,
    {
        Self::with_capacity(capacity, Default::default())
    }
    fn userdata_ref(&self) -> &Data::UserData {
        self.userdata
            .as_ref()
            .expect("parser context userdata was moved into a previous reduce-action parse error")
    }

    fn userdata_mut_ref(&mut self) -> &mut Data::UserData {
        self.userdata
            .as_mut()
            .expect("parser context userdata was moved into a previous reduce-action parse error")
    }

    fn take_userdata(&mut self) -> Data::UserData {
        self.userdata
            .take()
            .expect("parser context userdata was moved into a previous reduce-action parse error")
    }

    fn is_consumed(&self) -> bool {
        self.userdata.is_none()
    }

    /// Borrow the user data owned by this context.
    pub fn userdata(&self) -> &Data::UserData {
        self.userdata_ref()
    }

    /// Borrow the user data owned by this context as an iterator.
    pub fn userdata_all(&self) -> impl Iterator<Item = &Data::UserData> {
        self.userdata.iter()
    }

    /// Mutably borrow the user data owned by this context.
    pub fn userdata_mut(&mut self) -> &mut Data::UserData {
        self.userdata_mut_ref()
    }

    /// Mutably borrow the user data owned by this context as an iterator.
    pub fn userdata_all_mut(&mut self) -> impl Iterator<Item = &mut Data::UserData> {
        self.userdata.iter_mut()
    }

    fn state_from_len(&self, stack_len: usize) -> usize {
        if stack_len == 0 {
            0
        } else {
            self.state_stack[stack_len - 1].into_usize()
        }
    }

    fn simulated_state(&self, extra_state_stack: &[StateIndex], stack_len: usize) -> usize {
        extra_state_stack
            .last()
            .copied()
            .map(Index::into_usize)
            .unwrap_or_else(|| self.state_from_len(stack_len))
    }

    /// End this context and pop the value of the start symbol from the data stack.
    ///
    /// `ParseError::NoAction` leaves the context reusable, so callers may feed more tokens and
    /// call `accept` again. Successful acceptance moves the start value and user data out of the
    /// context, consuming it. A reduce-action failure also consumes the context.
    pub fn accept(
        &mut self,
    ) -> Result<
        (Start::StartType, Data::UserData),
        ParseError<Data::Term, Data::Location, Data::ReduceActionError, Data::UserData>,
    >
    where
        Data::Term: Clone,
        Data::NonTerm: std::fmt::Debug,
    {
        self.feed_eof()?;

        // pop eof
        self.data_stack.pop();
        let start = self.data_stack.pop().unwrap();
        let start = Start::extract(start).unwrap_or_else(|| {
            panic!(
                "Failed to extract start value for branch {}",
                Start::BRANCH_INDEX
            )
        });
        Ok((
            start,
            self.userdata.take().expect(
                "parser context userdata was moved into a previous reduce-action parse error",
            ),
        ))
    }

    /// End this context and return an iterator with the start symbol and final user data.
    pub fn accept_all(
        &mut self,
    ) -> Result<
        impl Iterator<Item = (Start::StartType, Data::UserData)>,
        ParseError<Data::Term, Data::Location, Data::ReduceActionError, Data::UserData>,
    >
    where
        Data::Term: Clone,
        Data::NonTerm: std::fmt::Debug,
    {
        Ok(std::iter::once(self.accept()?))
    }

    pub fn can_accept(&self) -> bool {
        // EOF acceptance uses the same grammatical feed simulation as regular terminals.
        if self.is_consumed() {
            return false;
        }
        self.plan_feed(P::TermClass::EOF).is_ok()
    }

    /// Get current state index
    #[inline]
    pub fn state(&self) -> usize {
        self.state_from_len(self.state_stack.len())
    }
    /// Get iterator of state stack.
    /// The implicit root state is not included.
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
            self.state_stack.len(),
            &mut terms,
            &mut nonterms,
        );

        (terms, nonterms)
    }
    /// Same as `expected_token()`, but returns as printable type.
    pub fn expected_token_str<'a>(
        &self,
    ) -> (
        impl Iterator<Item = &'static str> + use<P, Data, Start, StateIndex>,
        impl Iterator<Item = &'static str> + use<P, Data, Start, StateIndex>,
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
        let state = self.simulated_state(extra_state_stack, stack_len);

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
            let state = self.simulated_state(&extra_state_stack, stack_len);
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
    ) -> Result<(), ParseError<Data::Term, Data::Location, Data::ReduceActionError, Data::UserData>>
    where
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
        Data::Location: Default,
    {
        self.feed_location(term, Default::default())
    }

    /// Feed one terminal with location to parser, and update stacks.
    ///
    /// If this returns `ParseError::NoAction`, no reduce action has run for the lookahead and the
    /// context remains reusable. If this returns `ParseError::ReduceAction`, the user data has
    /// been moved into the error and this context is consumed. Calling `feed` or `accept` on a
    /// consumed context returns `ParseError::ConsumedContext`.
    pub fn feed_location(
        &mut self,
        term: P::Term,
        location: Data::Location,
    ) -> Result<(), ParseError<Data::Term, Data::Location, Data::ReduceActionError, Data::UserData>>
    where
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
    {
        use crate::Location;
        if self.is_consumed() {
            return Err(self.consumed_context_error(TerminalSymbol::Terminal(term), location));
        }

        let class = P::TermClass::from_term(&term);

        match self.feed_location_impl(TerminalSymbol::Terminal(term), class, location) {
            Ok(()) => Ok(()),
            Err(FeedLocationError::NoAction(err)) => {
                // nothing shifted; enters panic mode

                // if `error` token was not used in the grammar, early return here
                if !P::ERROR_USED {
                    return Err(Self::no_action_error(err));
                }

                let mut pop_count = 0;
                let mut error_plan = None;
                for _ in self.state_stack.iter().rev() {
                    // try feeding `error` token to check if we can go to panic mode
                    if let Ok(plan) = self.plan_feed_from_stack_len(
                        self.state_stack.len() - pop_count,
                        P::TermClass::ERROR,
                    ) {
                        error_plan = Some(plan);
                        break;
                    }

                    pop_count += 1;
                }

                let Some(error_plan) = error_plan else {
                    return Err(Self::no_action_error(err));
                };

                let error_location =
                    Data::Location::new(self.location_stack.iter().rev(), pop_count);
                self.location_stack
                    .truncate(self.location_stack.len() - pop_count);
                self.data_stack.truncate(self.data_stack.len() - pop_count);
                self.state_stack
                    .truncate(self.state_stack.len() - pop_count);

                #[cfg(feature = "tree")]
                {
                    let l = self.tree_stack.len() - pop_count;
                    self.tree_stack.truncate(l);
                }

                self.apply_feed_plan(error_plan, TerminalSymbol::Error, error_location)?;
                // `error` was feed, now check with the original terminal symbol again.
                // If the original terminal is still not accepted, it is part of the `error`.
                match self.plan_feed(class) {
                    Ok(term_plan) => self.apply_feed_plan(term_plan, err.term, err.location),
                    Err(_) => {
                        // The terminal symbol is still not feedable, so it belongs to the `error` token.
                        // merge term with previous error
                        let error_location = Data::Location::new(
                            std::iter::once(&err.location).chain(self.location_stack.iter().rev()),
                            2, // error node
                        );
                        if let Some(err_loc) = self.location_stack.last_mut() {
                            *err_loc = error_location;
                        } else {
                            unreachable!("location stack must have at least one element");
                        }
                        Ok(())
                    }
                }
            }
            Err(FeedLocationError::ReduceAction(err)) => Err(err),
        }
    }

    fn feed_location_impl(
        &mut self,
        term: TerminalSymbol<P::Term>,
        class: P::TermClass,
        location: Data::Location,
    ) -> Result<
        (),
        FeedLocationError<Data::Term, Data::Location, Data::ReduceActionError, Data::UserData>,
    >
    where
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
    {
        // First build a complete grammatical feed plan. No semantic action runs until this
        // succeeds, so late `NoAction` cannot leave partially reduced stacks behind.
        let plan = match self.plan_feed(class) {
            Ok(plan) => plan,
            Err(err) => {
                return Err(FeedLocationError::NoAction(NoActionFailure {
                    term,
                    location,
                    state: err.state,
                }));
            }
        };
        self.apply_feed_plan(plan, term, location)
            .map_err(FeedLocationError::ReduceAction)
    }

    fn no_action_error(
        err: NoActionFailure<P::Term, Data::Location>,
    ) -> ParseError<Data::Term, Data::Location, Data::ReduceActionError, Data::UserData> {
        // NoAction is raised by CFG-only planning before any semantic action runs, so user data
        // stays in the context and the caller may recover by feeding another token.
        ParseError::NoAction(super::error::NoActionError {
            term: err.term,
            location: err.location,
            state: err.state,
        })
    }

    fn consumed_context_error(
        &self,
        term: TerminalSymbol<P::Term>,
        location: Data::Location,
    ) -> ParseError<Data::Term, Data::Location, Data::ReduceActionError, Data::UserData> {
        ParseError::ConsumedContext(super::error::ConsumedContextError {
            term,
            location,
            state: self.state(),
        })
    }

    /// Simulate the deterministic LR stack operations needed to feed one terminal.
    ///
    /// The returned plan is a commit log: every reduction records its production, pop length, and
    /// non-terminal goto, followed by the final terminal shift.
    /// Returning `NoAction` if the terminal cannot be shifted after all reductions.
    fn plan_feed(
        &self,
        class: P::TermClass,
    ) -> Result<FeedPlan<P::NonTerm, StateIndex>, FeedPlanError> {
        self.plan_feed_from_stack_len(self.state_stack.len(), class)
    }

    /// Same as `plan_feed`, but simulates on a prefix of the current stack.
    fn plan_feed_from_stack_len(
        &self,
        stack_len: usize,
        class: P::TermClass,
    ) -> Result<FeedPlan<P::NonTerm, StateIndex>, FeedPlanError> {
        let mut extra_state_stack = self.feed_extra_state_stack.borrow_mut();
        extra_state_stack.clear();

        self.plan_feed_with_extra_stack(stack_len, class, &mut extra_state_stack)
    }

    fn plan_feed_with_extra_stack(
        &self,
        mut stack_len: usize,
        class: P::TermClass,
        extra_state_stack: &mut Vec<StateIndex>,
    ) -> Result<FeedPlan<P::NonTerm, StateIndex>, FeedPlanError> {
        use super::super::table::ReduceRules;

        let mut reductions = Vec::new();

        // Walk the same reduce chain that a real feed would perform, but keep all new states in
        // `extra_state_stack` so the real parser stack remains untouched.
        let terminal_shift = loop {
            let state = self.simulated_state(&extra_state_stack, stack_len);
            let (shift, reduce) = match self.tables.term_action(state, class) {
                Some(action) => (action.shift(), action.reduce()),
                None => (None, None),
            };
            if let Some(reduce_rule) = reduce {
                let reduce_rule = reduce_rule.to_iter().next().unwrap();
                let rule = *self.tables.rule(reduce_rule.into_usize());
                let tokens_len = rule.len;

                // Simulate popping the RHS of the production across already-simulated states and
                // the original stack prefix.
                if tokens_len <= extra_state_stack.len() {
                    extra_state_stack.truncate(extra_state_stack.len() - tokens_len);
                } else {
                    let left = tokens_len - extra_state_stack.len();
                    extra_state_stack.clear();
                    stack_len -= left;
                }

                let Some(next_nonterm_shift) = self.tables.shift_goto_nonterm(
                    self.simulated_state(&extra_state_stack, stack_len),
                    rule.lhs,
                ) else {
                    unreachable!(
                        "Failed to shift nonterminal: {} in state {}",
                        rule.lhs.as_str(),
                        self.simulated_state(&extra_state_stack, stack_len)
                    );
                };

                // Record the reduce and goto decision now so commit can replay it without
                // re-running table selection.
                reductions.push(PlannedReduction {
                    rule_index: reduce_rule.into_usize(),
                    tokens_len,
                    lhs: rule.lhs,
                    next_nonterm_shift,
                });
                extra_state_stack.push(next_nonterm_shift.state);
            } else {
                break shift;
            }
        };

        // A feed is grammatically possible only if the simulated reduce chain reaches a terminal
        // shift. Otherwise this is a syntax `NoAction`, not a semantic failure.
        match terminal_shift {
            Some(terminal_shift) => Ok(FeedPlan {
                reductions,
                terminal_shift,
            }),
            None => Err(FeedPlanError {
                state: self.simulated_state(&extra_state_stack, stack_len),
            }),
        }
    }

    /// Apply a previously simulated deterministic feed plan to the real parser stacks.
    ///
    /// At this point CFG-level success is known.
    /// Remaining errors can only come from user reduce actions, so they are reported as semantic reduce-action failures
    fn apply_feed_plan(
        &mut self,
        plan: FeedPlan<P::NonTerm, StateIndex>,
        term: TerminalSymbol<P::Term>,
        location: Data::Location,
    ) -> Result<(), ParseError<Data::Term, Data::Location, Data::ReduceActionError, Data::UserData>>
    where
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
    {
        use crate::Location;

        for reduction in plan.reductions {
            // Replay the simulated RHS pop before invoking the generated reduce action, matching
            // the stack shape that the generated action expects.
            self.state_stack
                .truncate(self.state_stack.len() - reduction.tokens_len);

            let mut shift = false;
            let mut new_location =
                Data::Location::new(self.location_stack.iter().rev(), reduction.tokens_len);

            match Data::reduce_action(
                &mut self.data_stack,
                &mut self.location_stack,
                reduction.next_nonterm_shift.push,
                reduction.rule_index,
                &mut shift,
                &term,
                self.userdata.as_mut().expect(
                    "parser context userdata was moved into a previous reduce-action parse error",
                ),
                &mut new_location,
            ) {
                Ok(_) => {}
                Err(err) => {
                    let state = self.state();
                    // A user reduce action may have mutated stacks or user data before failing.
                    // Move user data into the error and leave the context unusable.
                    let userdata = self.take_userdata();
                    return Err(ParseError::ReduceAction(super::error::ReduceActionError {
                        term,
                        location,
                        state,
                        source: err,
                        userdata,
                    }));
                }
            };
            self.location_stack.push(new_location);

            #[cfg(feature = "tree")]
            {
                // Mirror the same reduction in the optional syntax tree stack.
                let l = self.tree_stack.len() - reduction.tokens_len;
                let children = self.tree_stack.split_off(l);

                self.tree_stack
                    .push(crate::tree::Tree::new_nonterminal(reduction.lhs, children));
            }

            self.state_stack.push(reduction.next_nonterm_shift.state);
        }

        // Commit the terminal shift only after all planned reductions have been replayed.
        self.state_stack.push(plan.terminal_shift.state);

        #[cfg(feature = "tree")]
        self.tree_stack
            .push(crate::tree::Tree::new_terminal(term.clone()));

        if plan.terminal_shift.push {
            match term {
                TerminalSymbol::Terminal(t) => self.data_stack.push(Data::new_terminal(t)),
                TerminalSymbol::Error | TerminalSymbol::Eof | TerminalSymbol::VirtualStart(_) => {
                    self.data_stack.push(Data::new_empty())
                }
            }
        } else {
            self.data_stack.push(Data::new_empty());
        }

        self.location_stack.push(location);

        Ok(())
    }

    /// Check if `term` can be feeded to current state.
    /// This does not simulate for reduce action error, or panic mode.
    /// So this function will return `false` even if term can be shifted as `error` token,
    /// and will return `true` if `Err` variant is returned by `reduce_action`.
    pub fn can_feed(&self, term: &Data::Term) -> bool {
        if self.is_consumed() {
            return false;
        }
        let class = P::TermClass::from_term(term);

        self.plan_feed(class).is_ok()
    }

    /// Check if current context can enter panic mode
    pub fn can_panic(&self) -> bool {
        if self.is_consumed() {
            return false;
        }
        // if `error` token was not used in the grammar, early return here
        if !P::ERROR_USED {
            return false;
        }

        let error = P::TermClass::ERROR;
        let mut stack_len = self.state_stack.len();

        loop {
            match self.plan_feed_from_stack_len(stack_len, error) {
                Ok(_) => break true, // successfully shifted `error`
                Err(_) => {
                    if stack_len == 0 {
                        break false;
                    } else {
                        stack_len -= 1;
                    }
                }
            }
        }
    }

    fn feed_eof(
        &mut self,
    ) -> Result<(), ParseError<Data::Term, Data::Location, Data::ReduceActionError, Data::UserData>>
    where
        P::Term: Clone,
        P::NonTerm: std::fmt::Debug,
    {
        let eof_location = Data::Location::new(self.location_stack.iter().rev(), 0);
        if self.is_consumed() {
            return Err(self.consumed_context_error(TerminalSymbol::Eof, eof_location));
        }

        match self.feed_location_impl(TerminalSymbol::Eof, P::TermClass::EOF, eof_location) {
            Ok(()) => Ok(()),
            Err(FeedLocationError::NoAction(err)) => Err(Self::no_action_error(err)),
            Err(FeedLocationError::ReduceAction(err)) => Err(err),
        }
    }
}

impl<
    P: Parser<Term = Data::Term, NonTerm = Data::NonTerm, StateIndex = StateIndex>,
    Data: SemanticValue,
    Start: StartExtractor<Data>,
    StateIndex: Index + Copy,
> Default for Context<P, Data, Start, StateIndex>
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
    Start: StartExtractor<Data>,
    StateIndex: Index + Copy,
> Clone for Context<P, Data, Start, StateIndex>
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
            feed_extra_state_stack: RefCell::new(Vec::new()),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<
    P: Parser<Term = Data::Term, NonTerm = Data::NonTerm, StateIndex = StateIndex>,
    Data: SemanticValue,
    Start: StartExtractor<Data>,
    StateIndex: Index + Copy,
> std::fmt::Debug for Context<P, Data, Start, StateIndex>
where
    Data: std::fmt::Debug,
    Data::UserData: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let state_stack: Vec<_> = self.state_stack().collect();
        f.debug_struct("Context")
            .field("state", &self.state())
            .field("state_stack", &state_stack)
            .field("data_stack", &self.data_stack)
            .field("userdata", &self.userdata)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::DefaultLocation;
    use crate::parser::table::ShiftTarget;
    use crate::parser::table::TermActionRef;

    #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
    enum TestTermClass {
        Error,
        Eof,
        Start,
        A,
        B,
    }

    impl TerminalClass for TestTermClass {
        type Term = char;

        const ERROR: Self = TestTermClass::Error;
        const EOF: Self = TestTermClass::Eof;

        fn as_str(&self) -> &'static str {
            match self {
                TestTermClass::Error => "error",
                TestTermClass::Eof => "eof",
                TestTermClass::Start => "start",
                TestTermClass::A => "a",
                TestTermClass::B => "b",
            }
        }

        fn to_usize(&self) -> usize {
            *self as usize
        }

        fn from_term(term: &Self::Term) -> Self {
            match term {
                'a' => TestTermClass::A,
                'b' => TestTermClass::B,
                _ => TestTermClass::Error,
            }
        }

        fn from_virtual_start(_branch_idx: u32) -> Self {
            TestTermClass::Start
        }
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
    enum TestNonTerm {
        N,
    }

    impl NonTerminal for TestNonTerm {
        fn nonterm_type(&self) -> Option<crate::parser::nonterminal::NonTerminalType> {
            None
        }

        fn as_str(&self) -> &'static str {
            "N"
        }

        fn to_usize(&self) -> usize {
            0
        }
    }

    struct TestTables;

    impl ParserTables for TestTables {
        type TermClass = TestTermClass;
        type NonTerm = TestNonTerm;
        type ReduceRules = usize;
        type StateIndex = usize;

        fn term_action(
            &self,
            state: usize,
            class: Self::TermClass,
        ) -> Option<TermActionRef<'_, Self::ReduceRules, Self::StateIndex>> {
            // This table intentionally reduces on lookahead `b`, but the state after reducing has
            // no `b` shift. It exercises late `NoAction` after a planned reduction.
            match (state, class) {
                (0, TestTermClass::Start) => Some(TermActionRef::Shift(ShiftTarget::new(1, false))),
                (1, TestTermClass::A) => Some(TermActionRef::Shift(ShiftTarget::new(2, true))),
                (2, TestTermClass::B) => Some(TermActionRef::Reduce(&0)),
                _ => None,
            }
        }

        fn shift_goto_nonterm(
            &self,
            state: usize,
            nonterm: Self::NonTerm,
        ) -> Option<ShiftTarget<Self::StateIndex>> {
            match (state, nonterm) {
                (1, TestNonTerm::N) => Some(ShiftTarget::new(3, true)),
                _ => None,
            }
        }

        fn is_accept(&self, _state: usize) -> bool {
            false
        }

        fn expected_shift_term(&self, _state: usize) -> impl Iterator<Item = Self::TermClass> + '_ {
            std::iter::empty()
        }

        fn expected_shift_nonterm(
            &self,
            _state: usize,
        ) -> impl Iterator<Item = Self::NonTerm> + '_ {
            std::iter::empty()
        }

        fn expected_reduce_rule(&self, _state: usize) -> impl Iterator<Item = impl Index> + '_ {
            std::iter::empty::<usize>()
        }

        fn rule(&self, rule: usize) -> &crate::parser::table::RuleInfo<Self::NonTerm> {
            static RULES: [crate::parser::table::RuleInfo<TestNonTerm>; 1] =
                [crate::parser::table::RuleInfo {
                    lhs: TestNonTerm::N,
                    len: 1,
                }];
            &RULES[rule]
        }

        fn state_count(&self) -> usize {
            4
        }

        fn rule_count(&self) -> usize {
            1
        }
    }

    struct TestParser;

    impl Parser for TestParser {
        const ERROR_USED: bool = false;

        type Term = char;
        type TermClass = TestTermClass;
        type NonTerm = TestNonTerm;
        type StateIndex = usize;
        type ReduceRules = usize;
        type Tables = TestTables;

        fn get_tables() -> &'static Self::Tables {
            &TestTables
        }

        fn __rusty_lr_parser_version() -> (usize, usize, usize) {
            crate::versions::EXPECTED_RUSTY_LR_PARSER_VERSION
        }
    }

    struct RecoveryTables;

    impl ParserTables for RecoveryTables {
        type TermClass = TestTermClass;
        type NonTerm = TestNonTerm;
        type ReduceRules = usize;
        type StateIndex = usize;

        fn term_action(
            &self,
            state: usize,
            class: Self::TermClass,
        ) -> Option<TermActionRef<'_, Self::ReduceRules, Self::StateIndex>> {
            // Recovery from state 2 must reduce before the synthetic `error` token can shift.
            match (state, class) {
                (0, TestTermClass::Start) => Some(TermActionRef::Shift(ShiftTarget::new(1, false))),
                (1, TestTermClass::A) => Some(TermActionRef::Shift(ShiftTarget::new(2, true))),
                (2, TestTermClass::Error) => Some(TermActionRef::Reduce(&0)),
                (3, TestTermClass::Error) => Some(TermActionRef::Shift(ShiftTarget::new(4, false))),
                (4, TestTermClass::B) => Some(TermActionRef::Shift(ShiftTarget::new(5, true))),
                _ => None,
            }
        }

        fn shift_goto_nonterm(
            &self,
            state: usize,
            nonterm: Self::NonTerm,
        ) -> Option<ShiftTarget<Self::StateIndex>> {
            match (state, nonterm) {
                (1, TestNonTerm::N) => Some(ShiftTarget::new(3, true)),
                _ => None,
            }
        }

        fn is_accept(&self, _state: usize) -> bool {
            false
        }

        fn expected_shift_term(&self, _state: usize) -> impl Iterator<Item = Self::TermClass> + '_ {
            std::iter::empty()
        }

        fn expected_shift_nonterm(
            &self,
            _state: usize,
        ) -> impl Iterator<Item = Self::NonTerm> + '_ {
            std::iter::empty()
        }

        fn expected_reduce_rule(&self, _state: usize) -> impl Iterator<Item = impl Index> + '_ {
            std::iter::empty::<usize>()
        }

        fn rule(&self, rule: usize) -> &crate::parser::table::RuleInfo<Self::NonTerm> {
            static RULES: [crate::parser::table::RuleInfo<TestNonTerm>; 1] =
                [crate::parser::table::RuleInfo {
                    lhs: TestNonTerm::N,
                    len: 1,
                }];
            &RULES[rule]
        }

        fn state_count(&self) -> usize {
            6
        }

        fn rule_count(&self) -> usize {
            1
        }
    }

    struct RecoveryParser;

    impl Parser for RecoveryParser {
        const ERROR_USED: bool = true;

        type Term = char;
        type TermClass = TestTermClass;
        type NonTerm = TestNonTerm;
        type StateIndex = usize;
        type ReduceRules = usize;
        type Tables = RecoveryTables;

        fn get_tables() -> &'static Self::Tables {
            &RecoveryTables
        }

        fn __rusty_lr_parser_version() -> (usize, usize, usize) {
            crate::versions::EXPECTED_RUSTY_LR_PARSER_VERSION
        }
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    enum TestData {
        Empty,
        Terminal(char),
        N,
    }

    impl SemanticValue for TestData {
        type Term = char;
        type NonTerm = TestNonTerm;
        type UserData = Vec<&'static str>;
        type ReduceActionError = ();
        type Location = DefaultLocation;

        fn new_empty() -> Self {
            TestData::Empty
        }

        fn new_terminal(term: Self::Term) -> Self {
            TestData::Terminal(term)
        }

        fn reduce_action(
            data_stack: &mut Vec<Self>,
            location_stack: &mut Vec<Self::Location>,
            push_data: bool,
            rule_index: usize,
            _shift: &mut bool,
            _lookahead: &TerminalSymbol<Self::Term>,
            userdata: &mut Self::UserData,
            _location0: &mut Self::Location,
        ) -> Result<(), Self::ReduceActionError> {
            assert_eq!(rule_index, 0);
            assert_eq!(data_stack.pop(), Some(TestData::Terminal('a')));
            location_stack.pop();
            userdata.push("reduced");
            if push_data {
                data_stack.push(TestData::N);
            } else {
                data_stack.push(TestData::Empty);
            }
            Ok(())
        }
    }

    struct TestStart;

    impl StartExtractor<TestData> for TestStart {
        type StartType = ();

        const BRANCH_INDEX: u32 = 0;

        fn extract(_value: TestData) -> Option<Self::StartType> {
            Some(())
        }
    }

    #[test]
    fn feed_failure_after_planned_reductions_does_not_commit_stack_changes() {
        let mut ctx = Context::<TestParser, TestData, TestStart, usize>::new(Vec::new());
        ctx.feed('a').unwrap();

        // `b` would trigger a reduction before discovering that it cannot be shifted. The context
        // must remain byte-for-byte equivalent to its pre-feed state.
        let state_stack = ctx.state_stack.clone();
        let data_stack = ctx.data_stack.clone();
        let location_stack = ctx.location_stack.clone();

        let err = ctx.feed('b').unwrap_err();

        assert_eq!(err.state(), 3);
        assert_eq!(ctx.state_stack, state_stack);
        assert_eq!(ctx.data_stack, data_stack);
        assert_eq!(ctx.location_stack, location_stack);
        assert!(err.userdata().is_none());
        assert!(ctx.userdata.as_ref().unwrap().is_empty());
    }

    #[test]
    fn error_token_recovery_commits_the_precomputed_plan() {
        let mut ctx = Context::<RecoveryParser, TestData, TestStart, usize>::new(Vec::new());
        ctx.feed('a').unwrap();

        // Feeding `b` has no direct action, so recovery shifts `error`. That recovery shift must
        // use the reduction discovered by pre-feed simulation.
        ctx.feed('b').unwrap();

        assert_eq!(ctx.userdata.as_ref().unwrap(), &vec!["reduced"]);
        assert_eq!(ctx.state_stack, vec![1, 3, 4, 5]);
        assert_eq!(
            ctx.data_stack,
            vec![
                TestData::Empty,
                TestData::N,
                TestData::Empty,
                TestData::Terminal('b')
            ]
        );
    }
}
