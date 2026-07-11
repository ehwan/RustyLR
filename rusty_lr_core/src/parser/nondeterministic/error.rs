use std::fmt::Debug;
use std::fmt::Display;

/// Error type for feed()
#[derive(Clone, Debug)]
pub struct ParseError<Term, Location, ReduceActionError> {
    /// The terminal symbol that caused the error.
    pub term: crate::TerminalSymbol<Term>,
    /// Location of the terminal symbol.
    pub location: Location,
    /// Errors grouped by the GLR branch that produced them.
    pub branch_errors: Vec<ParseErrorBranch<ReduceActionError>>,
}

/// Errors observed while processing one GLR branch for a single lookahead.
///
/// A branch failure is either a grammatical `NoAction` at one parser state or one reduce-action
/// error raised at one parser state. Keeping that pair in a single value makes it explicit that
/// the state and semantic error came from the same nondeterministic branch.
#[derive(Clone, Debug)]
pub enum ParseErrorBranch<ReduceActionError> {
    /// No parser-table action was available for this branch.
    NoAction {
        /// Parser state associated with this branch's failure.
        state: usize,
    },
    /// A reduce action rejected this branch.
    ReduceAction {
        /// Parser state associated with this branch's failure.
        state: usize,
        /// Error returned from the reduce action while replaying this branch.
        source: ReduceActionError,
    },
}

/// Successful GLR feed result.
///
/// GLR parsing can keep one branch alive while other branches fail. In that case the feed is a
/// success, but `errors` records the branches that were pruned by `NoAction` or reduce-action
/// failure.
#[derive(Clone, Debug)]
pub struct FeedSuccess<Term, Location, ReduceActionError> {
    /// Branch errors observed while at least one GLR branch survived this feed.
    ///
    /// This is intentionally optional so callers that only care about fatal parse failure can keep
    /// using `context.feed(token)?` or `context.feed(token).unwrap()`.
    pub errors: Option<ParseError<Term, Location, ReduceActionError>>,
}

impl<Term, Location, ReduceActionError> ParseError<Term, Location, ReduceActionError> {
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
}

impl<ReduceActionError> ParseErrorBranch<ReduceActionError> {
    pub(crate) fn no_action(state: usize) -> Self {
        ParseErrorBranch::NoAction { state }
    }

    pub(crate) fn reduce_action(state: usize, source: ReduceActionError) -> Self {
        ParseErrorBranch::ReduceAction { state, source }
    }

    /// State associated with this branch's failure.
    pub fn state(&self) -> usize {
        match self {
            ParseErrorBranch::NoAction { state } => *state,
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
}

impl<Term, Location, ReduceActionError> Display for ParseError<Term, Location, ReduceActionError>
where
    Term: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let states: Vec<_> = self.states().collect();
        write!(f, "ParseError: {}, States: {:?}", self.term, states)
    }
}
