use std::fmt::Debug;
use std::fmt::Display;

/// Error type for feed()
#[derive(Clone, Debug)]
pub struct ParseError<Term, Location, ReduceActionError, UserData> {
    /// The terminal symbol that caused the error.
    pub term: crate::TerminalSymbol<Term>,
    /// Location of the terminal symbol.
    pub location: Location,
    /// Errors grouped by the GLR branch that produced them.
    pub branch_errors: Vec<ParseErrorBranch<ReduceActionError, UserData>>,
    /// Whether this error was produced by using a context after it had already been consumed.
    pub context_consumed: bool,
}

/// Errors observed while processing one GLR branch for a single lookahead.
///
/// A branch failure is either a grammatical `NoAction` at one parser state or one reduce-action
/// error raised at one parser state. Keeping that pair in a single value makes it explicit that
/// the state and semantic error came from the same nondeterministic branch. Reusing a context after
/// every branch has already been consumed is reported by `ParseError::context_consumed`, because no
/// reusable branch remains to attach branch-local state to.
#[derive(Clone, Debug)]
pub enum ParseErrorBranch<ReduceActionError, UserData> {
    /// No parser-table action was available for this branch.
    ///
    /// This branch did not run a reduce action for the lookahead. Its stack and user data remain
    /// reusable when no branch succeeds and the context restores `NoAction` branches.
    NoAction {
        /// Parser state associated with this branch's failure.
        state: usize,
        /// User data owned by this branch when it failed.
        userdata: UserData,
    },
    /// A reduce action rejected this branch.
    ///
    /// The branch may have partially mutated semantic state before returning `Err`, so this branch
    /// is consumed and its user data is moved into the error.
    ReduceAction {
        /// Parser state associated with this branch's failure.
        state: usize,
        /// Error returned from the reduce action while replaying this branch.
        source: ReduceActionError,
        /// User data owned by this branch when it failed.
        userdata: UserData,
    },
}

/// Successful GLR feed result.
///
/// GLR parsing can keep one branch alive while other branches fail. In that case the feed is a
/// success, but `errors` records the branches that were pruned by `NoAction` or reduce-action
/// failure.
#[derive(Clone, Debug)]
pub struct FeedSuccess<Term, Location, ReduceActionError, UserData> {
    /// Branch errors observed while at least one GLR branch survived this feed.
    ///
    /// This is intentionally optional so callers that only care about fatal parse failure can keep
    /// using `context.feed(token)?` or `context.feed(token).unwrap()`.
    pub errors: Option<ParseError<Term, Location, ReduceActionError, UserData>>,
}

impl<Term, Location, ReduceActionError, UserData>
    ParseError<Term, Location, ReduceActionError, UserData>
{
    pub fn location(&self) -> &Location {
        &self.location
    }
    pub fn term(&self) -> &crate::TerminalSymbol<Term> {
        &self.term
    }
    /// States when the error occurred, flattened across all failed branches.
    pub fn states(&self) -> impl Iterator<Item = usize> + '_ {
        self.branch_errors.iter().flat_map(|branch| branch.states())
    }

    pub fn is_consumed_context(&self) -> bool {
        self.context_consumed
    }
}

impl<ReduceActionError, UserData> ParseErrorBranch<ReduceActionError, UserData> {
    pub(crate) fn no_action(state: usize, userdata: UserData) -> Self {
        ParseErrorBranch::NoAction { state, userdata }
    }

    pub(crate) fn reduce_action(
        state: usize,
        source: ReduceActionError,
        userdata: UserData,
    ) -> Self {
        ParseErrorBranch::ReduceAction {
            state,
            source,
            userdata,
        }
    }

    /// State associated with this branch's failure.
    pub fn state(&self) -> usize {
        match self {
            ParseErrorBranch::NoAction { state, .. } => *state,
            ParseErrorBranch::ReduceAction { state, .. } => *state,
        }
    }

    /// State associated with this branch's failure, returned as an iterator for API symmetry with
    /// `ParseError::states()`.
    pub fn states(&self) -> impl Iterator<Item = usize> + '_ {
        std::iter::once(self.state())
    }

    /// Reduce-action error for this branch, if this branch failed semantically.
    pub fn reduce_action_error(&self) -> Option<&ReduceActionError> {
        match self {
            ParseErrorBranch::NoAction { .. } => None,
            ParseErrorBranch::ReduceAction { source, .. } => Some(source),
        }
    }

    /// User data owned by this branch when it failed.
    pub fn userdata(&self) -> &UserData {
        match self {
            ParseErrorBranch::NoAction { userdata, .. } => userdata,
            ParseErrorBranch::ReduceAction { userdata, .. } => userdata,
        }
    }

    /// Consume this branch error and return its user data.
    pub fn into_userdata(self) -> UserData {
        match self {
            ParseErrorBranch::NoAction { userdata, .. } => userdata,
            ParseErrorBranch::ReduceAction { userdata, .. } => userdata,
        }
    }
}

impl<Term, Location, ReduceActionError, UserData> Display
    for ParseError<Term, Location, ReduceActionError, UserData>
where
    Term: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.context_consumed {
            return write!(f, "ConsumedContext: {}", self.term);
        }
        let states: Vec<_> = self.states().collect();
        write!(f, "ParseError: {}, States: {:?}", self.term, states)
    }
}
